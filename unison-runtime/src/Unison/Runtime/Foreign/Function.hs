{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Foreign.Function
  ( ForeignFunc (..),
    ForeignConvention (..),
    mkForeign,
    -- mkForeignExn,
    executeForeign,
    executeForeignExn
  )
where

import Control.Exception (throwIO)
import Data.Char qualified as Char
import Data.Foldable (toList)
import Data.Sequence qualified as Sq
import Data.Tagged (Tagged (..))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word16, Word32, Word64, Word8)
import System.IO (BufferMode (..), IOMode (..), SeekMode (..))
import Unison.Builtin.Decls qualified as Ty
import Unison.Reference (Reference)
import Unison.Runtime.ANF (internalBug)
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Foreign
import Unison.Runtime.MCode
import Unison.Runtime.Stack
import Unison.Type
  ( anyRef,
    listRef,
    textRef,
    typeLinkRef,
  )
import Unison.Runtime.TypeTags
import Unison.Util.Text (Text, pack, unpack)

-- This type considers two sorts of foreign functions.
--
--   1. A 'pure' foreign function accepts a stack and produces a
--      stack, possibly with different contents. This isn't pure in
--      the sense that it could have side effects, but it doesn't
--      throw an observable exception.
--   2. An exceptional foreign function is similar, except it
--      produces a boolean indicating whether its final stack points
--      to an exceptional value. This allows the interpreter to take
--      steps to raise a unison `Exception` directly.
data ForeignFunc where
  FF :: (ForeignConvention i, ForeignConvention o) => (i -> IO o) -> ForeignFunc
  -- Idea: have a variety of FF with a calling convention for direct
  -- exception requests. Some builtins have such a convention already,
  -- and could be efficiently implemented as direct foreign calls.
  -- Conceivably this could also be used to implement `Either Failure`
  -- builtins, with an optimization that turns the wrapper around
  -- _those_ into direct calls to the foreign function.
  -- FFE :: (ForeignArgs i, ForeignConvention e, ForeignConvention o)
  --     => (i -> IO (Either (Failure e) o))
  --     -> ForeignFunc

instance Show ForeignFunc where
  show _ = "ForeignFunc"

instance Eq ForeignFunc where
  _ == _ = internalBug "Eq ForeignFunc"

instance Ord ForeignFunc where
  compare _ _ = internalBug "Ord ForeignFunc"

-- A ForeignConvention explains how to encode foreign values as
-- unison types. Depending on the situation, this can take three
-- forms.
--
--   1. Reading/writing directly from/to the stack
--   2. Reading a tuple directly from the stack
--   3. Translating a standalone value
--
-- The first is used when the value in question is the one that is
-- going to be directly on the stack, to allow for slight
-- optimization (e.g. an `Either` only requires reading/writing the
-- boxed portion of the stack). For compound types, though, it's
-- necessary to be able to de/encode a value that was nested inside
-- something else.
--
-- The second is used for multi-argument foreign functions. The
-- default implementation expects a single argument, and reads at
-- that specific index. But, tuples and the unit type can override
-- to read multiple arguments directly from the stack. This works
-- out better than having a separate class with a default
-- ForeignConvention instance, because the latter requires
-- incoherence to work as expected.
--
-- We can give a default implementation of the stack operations in
-- terms of the other coding.
class ForeignConvention a where
  readAtIndex :: Stack -> Int -> IO a
  readsAt :: Stack -> Args -> IO a
  decodeVal :: Val -> IO a

  readAtIndex stk i = peekOff stk i >>= decodeVal

  readsAt stk (VArg1 i) = readAtIndex stk i
  readsAt _ args = readsAtError "one argument" args

  writeBack :: Stack -> a -> IO ()
  encodeVal :: a -> Val

  writeBack stk v = poke stk (encodeVal v)

readsAtError :: String -> Args -> IO a
readsAtError expect args = throwIO $ Panic msg Nothing
  where
    msg = "readsAt: expected " ++ expect ++ ", got: " ++ show args

foreignConventionError :: String -> Val -> IO a
foreignConventionError ty v = throwIO $ Panic msg (Just v)
  where
    msg = "mismatched foreign calling convention for `" ++ ty ++ "`"

instance
  ( ForeignConvention a,
    ForeignConvention b
  ) => ForeignConvention (Either a b) where
  decodeVal (BoxedVal (Data1 _ t v))
    | t == leftTag = Left <$> decodeVal v
    | otherwise = Right <$> decodeVal v
  decodeVal v = foreignConventionError "Either" v

  encodeVal (Left x) =
    BoxedVal . Data1 Ty.eitherRef leftTag $ encodeVal x
  encodeVal (Right y) =
    BoxedVal . Data1 Ty.eitherRef rightTag $ encodeVal y

  readAtIndex stk i = bpeekOff stk i >>= \case
    Data1 _ t v
      | t == leftTag -> Left <$> decodeVal v
      | otherwise -> Right <$> decodeVal v
    c -> foreignConventionError "Either" (BoxedVal c)

  writeBack stk (Left x) =
    bpoke stk . Data1 Ty.eitherRef leftTag $ encodeVal x
  writeBack stk (Right y) =
    bpoke stk . Data1 Ty.eitherRef rightTag $ encodeVal y

instance ForeignConvention a => ForeignConvention (Maybe a) where
  decodeVal (BoxedVal (Enum _ _)) = pure Nothing
  decodeVal (BoxedVal (Data1 _ _ v)) = Just <$> decodeVal v
  decodeVal v = foreignConventionError "Maybe" v

  encodeVal Nothing = noneVal
  encodeVal (Just v) = someVal (encodeVal v)

  readAtIndex stk i = bpeekOff stk i >>= \case
    Data1 _ _ v -> Just <$> decodeVal v
    Enum _ _ -> pure Nothing
    c -> foreignConventionError "Maybe" (BoxedVal c)

  writeBack stk Nothing = bpoke stk noneClo
  writeBack stk (Just v) = bpoke stk (someClo (encodeVal v))

noneClo :: Closure
noneClo = Enum Ty.optionalRef noneTag

noneVal :: Val
noneVal = BoxedVal noneClo

someClo :: Val -> Closure
someClo v = Data1 Ty.optionalRef someTag v

someVal :: Val -> Val
someVal v = BoxedVal (someClo v)

instance ForeignConvention Int where
  decodeVal (IntVal v) = pure v
  decodeVal v = foreignConventionError "Int" v
  encodeVal = IntVal

  readAtIndex stk i = upeekOff stk i
  writeBack stk v = upokeT stk v intTypeTag

-- We don't have a clear mapping from these types to Unison types, most are just mapped to Nats.

instance ForeignConvention Word8 where
  decodeVal (NatVal v) = pure $ fromIntegral v
  decodeVal v = foreignConventionError "Word8" v
  encodeVal w = NatVal $ fromIntegral w

  readAtIndex stk i = fromIntegral <$> peekOffN stk i
  writeBack stk v = pokeN stk $ fromIntegral v

instance ForeignConvention Word16 where
  decodeVal (NatVal v) = pure $ fromIntegral v
  decodeVal v = foreignConventionError "Word16" v
  encodeVal w = NatVal $ fromIntegral w

  readAtIndex stk i = fromIntegral <$> peekOffN stk i
  writeBack stk v = pokeN stk $ fromIntegral v

instance ForeignConvention Word32 where
  decodeVal (NatVal v) = pure $ fromIntegral v
  decodeVal v = foreignConventionError "Word32" v
  encodeVal w = NatVal $ fromIntegral w

  readAtIndex stk i = fromIntegral <$> upeekOff stk i
  writeBack stk v = pokeN stk $ fromIntegral v

instance ForeignConvention Word64 where
  decodeVal (NatVal w) = pure w
  decodeVal v = foreignConventionError "Word64" v
  encodeVal w = NatVal w

  readAtIndex stk i = peekOffN stk i
  writeBack stk w = pokeN stk w

instance ForeignConvention Char where
  decodeVal (CharVal c) = pure c
  decodeVal v = foreignConventionError "Char" v

  encodeVal c = CharVal c

  readAtIndex stk i = Char.chr <$> upeekOff stk i
  writeBack stk v = upokeT stk (Char.ord v) charTypeTag

unitClo :: Closure
unitClo = Enum Ty.unitRef unitTag

unitVal :: Val
unitVal = BoxedVal unitClo

instance ForeignConvention () where
  decodeVal _ = pure ()
  encodeVal _ = unitVal

  readsAt _ ZArgs = pure ()
  readsAt _ as = readsAtError "zero arguments" as

  readAtIndex _ _ = pure ()
  writeBack stk _ = bpoke stk $ unitClo

decodeTup2 :: (ForeignConvention a, ForeignConvention b) => Closure -> IO (a, b)
decodeTup2 (Data2 _ _ x (BoxedVal (Data2 _ _ y _))) =
  (,) <$> decodeVal x <*> decodeVal y
decodeTup2 c = foreignConventionError "Pair" (BoxedVal c)

encodeTup2 :: (ForeignConvention a, ForeignConvention b) => (a, b) -> Closure
encodeTup2 (x,y) =
  Data2 Ty.pairRef pairTag (encodeVal x) (encodeVal y)

instance
  ( ForeignConvention a,
    ForeignConvention b
  ) => ForeignConvention (a, b) where
  decodeVal (BoxedVal v) = decodeTup2 v
  decodeVal v = foreignConventionError "Pair" v
  encodeVal p = BoxedVal $ encodeTup2 p

  readsAt stk (VArg2 i j) =
    (,) <$> readAtIndex stk i
        <*> readAtIndex stk j
  readsAt _ as = readsAtError "two arguments" as

  readAtIndex stk i = bpeekOff stk i >>= decodeTup2
  writeBack stk p = bpoke stk $ encodeTup2 p

decodeTup3 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c) => Closure -> IO (a, b, c)
decodeTup3 (Data2 _ _ x (BoxedVal (Data2 _ _ y (BoxedVal (Data2 _ _ z _))))) =
  (,,) <$> decodeVal x <*> decodeVal y <*> decodeVal z
decodeTup3 c = foreignConventionError "Triple" (BoxedVal c)

encodeTup3 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c) => (a, b, c) -> Closure
encodeTup3 (x,y,z) =
  Data2 Ty.pairRef pairTag (encodeVal x) (BoxedVal $ encodeTup2 (y,z))

instance
  ( ForeignConvention a,
    ForeignConvention b,
    ForeignConvention c
  ) => ForeignConvention (a, b, c) where
  decodeVal (BoxedVal v) = decodeTup3 v
  decodeVal v = foreignConventionError "Triple" v
  encodeVal p = BoxedVal $ encodeTup3 p

  readsAt stk (VArgN v) =
    (,,) <$> readAtIndex stk (PA.indexPrimArray v 0)
         <*> readAtIndex stk (PA.indexPrimArray v 1)
         <*> readAtIndex stk (PA.indexPrimArray v 2)
  readsAt _ as = readsAtError "three arguments" as

  readAtIndex stk i = bpeekOff stk i >>= decodeTup3
  writeBack stk p = bpoke stk $ encodeTup3 p

decodeTup4 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c, ForeignConvention d) => Closure -> IO (a, b, c, d)
decodeTup4 (Data2 _ _ w (BoxedVal (Data2 _ _ x (BoxedVal (Data2 _ _ y (BoxedVal (Data2 _ _ z _))))))) =
  (,,,) <$> decodeVal w <*> decodeVal x <*> decodeVal y <*> decodeVal z
decodeTup4 c = foreignConventionError "Quadruple" (BoxedVal c)

encodeTup4 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c, ForeignConvention d) => (a, b, c, d) -> Closure
encodeTup4 (w,x,y,z) =
  Data2 Ty.pairRef pairTag (encodeVal w) (BoxedVal $ encodeTup3 (x,y,z))

instance
  ( ForeignConvention a,
    ForeignConvention b,
    ForeignConvention c,
    ForeignConvention d
  ) => ForeignConvention (a, b, c, d) where
  decodeVal (BoxedVal v) = decodeTup4 v
  decodeVal v = foreignConventionError "Quadruple" v

  encodeVal p = BoxedVal $ encodeTup4 p

  readsAt stk (VArgN v) =
    (,,,) <$> readAtIndex stk (PA.indexPrimArray v 0)
          <*> readAtIndex stk (PA.indexPrimArray v 1)
          <*> readAtIndex stk (PA.indexPrimArray v 2)
          <*> readAtIndex stk (PA.indexPrimArray v 3)
  readsAt _ as = readsAtError "four arguments" as

  readAtIndex stk i = bpeekOff stk i >>= decodeTup4
  writeBack stk p = bpoke stk $ encodeTup4 p

decodeTup5 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c, ForeignConvention d, ForeignConvention e) => Closure -> IO (a, b, c, d, e)
decodeTup5 (Data2 _ _ v (BoxedVal (Data2 _ _ w (BoxedVal (Data2 _ _ x (BoxedVal (Data2 _ _ y (BoxedVal (Data2 _ _ z _))))))))) =
  (,,,,) <$> decodeVal v <*> decodeVal w <*> decodeVal x <*> decodeVal y <*> decodeVal z
decodeTup5 c = foreignConventionError "Quintuple" (BoxedVal c)

encodeTup5 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c, ForeignConvention d, ForeignConvention e) => (a, b, c, d, e) -> Closure
encodeTup5 (v,w,x,y,z) =
  Data2 Ty.pairRef pairTag (encodeVal v) (BoxedVal $ encodeTup4 (w,x,y,z))

instance
  ( ForeignConvention a,
    ForeignConvention b,
    ForeignConvention c,
    ForeignConvention d,
    ForeignConvention e
  ) =>
  ForeignConvention (a, b, c, d, e)
  where
  decodeVal (BoxedVal c) = decodeTup5 c
  decodeVal v = foreignConventionError "Quintuple" v

  encodeVal = BoxedVal . encodeTup5

  readsAt stk (VArgN v) =
    (,,,,) <$> readAtIndex stk (PA.indexPrimArray v 0)
           <*> readAtIndex stk (PA.indexPrimArray v 1)
           <*> readAtIndex stk (PA.indexPrimArray v 2)
           <*> readAtIndex stk (PA.indexPrimArray v 3)
           <*> readAtIndex stk (PA.indexPrimArray v 4)
  readsAt _ as = readsAtError "five arguments" as

  readAtIndex stk i = bpeekOff stk i >>= decodeTup5
  writeBack stk p = bpoke stk $ encodeTup5 p


decodeFailure :: ForeignConvention a => Closure -> IO (Failure a)
decodeFailure (DataG _ _ (_, args)) =
  Failure
    <$> decodeTypeLink (PA.indexArray args 0)
    <*> decodeText (PA.indexArray args 1)
    <*> decodeAny (PA.indexArray args 2)
decodeFailure c = foreignConventionError "Failure" (BoxedVal c)

encodeFailure :: ForeignConvention a => Failure a -> Closure
encodeFailure (Failure r msg v) = DataG Ty.failureRef failureTag payload
  where
    payload = boxedSeg [encodeTypeLink r, encodeText msg, encodeAny v]

boxedSeg :: [Closure] -> Seg
boxedSeg cs = (useg (0 <$ cs), bseg cs)

decodeTypeLink :: Closure -> IO Reference
decodeTypeLink = marshalUnwrapForeignIO

encodeTypeLink :: Reference -> Closure
encodeTypeLink rf = Foreign (Wrap typeLinkRef rf)

encodeAny :: ForeignConvention a => a -> Closure
encodeAny v = Data1 anyRef anyTag (encodeVal v)

decodeAny :: ForeignConvention a => Closure -> IO a
decodeAny (Data1 _ _ v) = decodeVal v
decodeAny c = foreignConventionError "Any" (BoxedVal c)

decodeText :: Closure -> IO Text
decodeText = marshalUnwrapForeignIO

encodeText :: Text -> Closure
encodeText tx = Foreign (Wrap textRef tx)

instance ForeignConvention a => ForeignConvention (Failure a) where
  decodeVal (BoxedVal v) = decodeFailure v
  decodeVal v = foreignConventionError "Failure" v
  encodeVal v = BoxedVal $ encodeFailure v

  readAtIndex stk i = bpeekOff stk i >>= decodeFailure
  writeBack stk f = bpoke stk $ encodeFailure f

decodeForeignClo :: String -> Closure -> IO a
decodeForeignClo _ (Foreign x) = pure $ unwrapForeign x
decodeForeignClo ty c = foreignConventionError ty (BoxedVal c)

encodeForeignClo :: Reference -> a -> Closure
encodeForeignClo r = Foreign . Wrap r

decodeBuiltin :: forall a. BuiltinForeign a => Val -> IO a
decodeBuiltin v
  | BoxedVal c <- v = decodeForeignClo ty c
  | otherwise = foreignConventionError ty v
  where
    Tagged ty = foreignName :: Tagged a String

encodeBuiltin :: forall a. BuiltinForeign a => a -> Val
encodeBuiltin = BoxedVal . encodeForeignClo r
  where
    Tagged r = foreignRef :: Tagged a Reference

readBuiltinAt :: forall a. BuiltinForeign a => Stack -> Int -> IO a
readBuiltinAt stk i = bpeekOff stk i >>= decodeForeignClo ty
  where
    Tagged ty = foreignName :: Tagged a String

writeBuiltin :: forall a. BuiltinForeign a => Stack -> a -> IO ()
writeBuiltin stk = bpoke stk . encodeForeignClo r
  where
    Tagged r = foreignRef :: Tagged a Reference

decodeAsBuiltin :: BuiltinForeign t => (t -> a) -> Val -> IO a
decodeAsBuiltin k = fmap k . decodeBuiltin

encodeAsBuiltin :: BuiltinForeign t => (a -> t) -> a -> Val
encodeAsBuiltin k = encodeBuiltin . k

readAsBuiltin
  :: BuiltinForeign t => (t -> a) -> Stack -> Int -> IO a
readAsBuiltin k stk i = k <$> readBuiltinAt stk i

writeAsBuiltin :: BuiltinForeign t => (a -> t) -> Stack -> a -> IO ()
writeAsBuiltin k stk = writeBuiltin stk . k

instance ForeignConvention POSIXTime where
  decodeVal (IntVal i) = pure (fromIntegral i)
  decodeVal v = foreignConventionError "POSIXTime" v
  encodeVal pt = IntVal (round pt)
  readAtIndex stk i = fromIntegral <$> peekOffI stk i
  writeBack stk pt = pokeI stk (round pt)


mkForeign ::
  ForeignConvention a =>
  ForeignConvention r =>
  (a -> IO r) -> ForeignFunc
mkForeign f = FF f
{-# inline mkForeign #-}

-- mkForeignExn ::
--   ForeignArgs a =>
--   ForeignConvention e =>
--   ForeignConvention r =>
--   (a -> IO (Either (Failure e) r)) ->
--   ForeignFunc
-- mkForeignExn f = FFE f

executeForeign ::
  ForeignConvention a =>
  ForeignConvention r =>
  (a -> IO r) ->
  Stack -> Args -> IO Stack
executeForeign ev stk args = do
  r <- ev =<< readsAt stk args
  stk <- bump stk
  stk <$ writeBack stk r
{-# inlinable executeForeign #-}

executeForeignExn ::
  ForeignConvention a =>
  ForeignConvention e =>
  ForeignConvention r =>
  (a -> IO (Either (Failure e) r)) ->
  Stack -> Args -> IO (Bool, Stack)
executeForeignExn ev stk args =
  readsAt stk args >>= ev >>= \case
    Left fail -> wb True fail
    Right r -> wb False r
  where
    wb :: ForeignConvention s => v -> s -> IO (v, Stack)
    wb v s = do
      stk <- bump stk
      (v, stk) <$ writeBack stk s

-- TODO: was this ever actually used? Mapping IO exceptions to numbers.
--
-- ioeDecode :: Int -> IOErrorType
-- ioeDecode 0 = AlreadyExists
-- ioeDecode 1 = NoSuchThing
-- ioeDecode 2 = ResourceBusy
-- ioeDecode 3 = ResourceExhausted
-- ioeDecode 4 = EOF
-- ioeDecode 5 = IllegalOperation
-- ioeDecode 6 = PermissionDenied
-- ioeDecode 7 = UserError
-- ioeDecode _ = internalBug "ioeDecode"

-- ioeEncode :: IOErrorType -> Int
-- ioeEncode AlreadyExists = 0
-- ioeEncode NoSuchThing = 1
-- ioeEncode ResourceBusy = 2
-- ioeEncode ResourceExhausted = 3
-- ioeEncode EOF = 4
-- ioeEncode IllegalOperation = 5
-- ioeEncode PermissionDenied = 6
-- ioeEncode UserError = 7
-- ioeEncode _ = internalBug "ioeDecode"

-- instance ForeignConvention IOException where
--   readForeign = readForeignAs (bld . ioeDecode)
--     where
--       bld t = IOError Nothing t "" "" Nothing Nothing
--
--   writeForeign = writeForeignAs (ioeEncode . ioe_type)

decodeBufferMode :: Closure -> IO BufferMode
decodeBufferMode (Enum _ t)
  | t == noBufTag = pure NoBuffering
  | t == lineBufTag = pure LineBuffering
  | t == blockBufTag = pure $ BlockBuffering Nothing
decodeBufferMode (Data1 _ t (IntVal i))
  | t == sizedBlockBufTag = pure . BlockBuffering $ Just i
decodeBufferMode c = foreignConventionError "BufferMode" (BoxedVal c)

encodeBufferMode :: BufferMode -> Closure
encodeBufferMode NoBuffering = no'buf
encodeBufferMode LineBuffering = line'buf
encodeBufferMode (BlockBuffering Nothing) = block'buf
encodeBufferMode (BlockBuffering (Just n)) =
  Data1 Ty.bufferModeRef sizedBlockBufTag . NatVal $ fromIntegral n

no'buf, line'buf, block'buf :: Closure
no'buf = Enum Ty.bufferModeRef noBufTag
line'buf = Enum Ty.bufferModeRef lineBufTag
block'buf = Enum Ty.bufferModeRef blockBufTag

instance ForeignConvention BufferMode where
  decodeVal (BoxedVal c) = decodeBufferMode c
  decodeVal v = foreignConventionError "BufferMode" v

  encodeVal = BoxedVal . encodeBufferMode

  readAtIndex stk i = bpeekOff stk i >>= decodeBufferMode
  writeBack stk bm = bpoke stk (encodeBufferMode bm)

decodeIOMode :: Closure -> IO IOMode
decodeIOMode (Enum _ t)
  | t == readModeTag = pure ReadMode
  | t == writeModeTag = pure WriteMode
  | t == appendModeTag = pure AppendMode
  | t == readWriteModeTag = pure ReadWriteMode
decodeIOMode c = foreignConventionError "IOMode" (BoxedVal c)

encodeIOMode :: IOMode -> Closure
encodeIOMode ReadMode = read'mode
encodeIOMode WriteMode = write'mode
encodeIOMode AppendMode = append'mode
encodeIOMode ReadWriteMode = read'write'mode

read'mode, write'mode, append'mode, read'write'mode :: Closure
read'mode = Enum Ty.bufferModeRef readModeTag
write'mode = Enum Ty.bufferModeRef writeModeTag
append'mode = Enum Ty.bufferModeRef appendModeTag
read'write'mode = Enum Ty.bufferModeRef readWriteModeTag

instance ForeignConvention IOMode where
  decodeVal (BoxedVal c) = decodeIOMode c
  decodeVal v = foreignConventionError "IOMode" v

  encodeVal = BoxedVal . encodeIOMode

  readAtIndex stk i = bpeekOff stk i >>= decodeIOMode
  writeBack stk im = bpoke stk (encodeIOMode im)

decodeSeekMode :: Closure -> IO SeekMode
decodeSeekMode (Enum _ t)
  | t == seekAbsoluteTag = pure AbsoluteSeek
  | t == seekRelativeTag = pure RelativeSeek
  | t == seekEndTag = pure SeekFromEnd
decodeSeekMode v = foreignConventionError "SeekMode" (BoxedVal v)

encodeSeekMode :: SeekMode -> Closure
encodeSeekMode AbsoluteSeek = absolute'seek
encodeSeekMode RelativeSeek = relative'seek
encodeSeekMode SeekFromEnd = seek'from'end

absolute'seek, relative'seek, seek'from'end :: Closure
absolute'seek = Enum Ty.seekModeRef seekAbsoluteTag
relative'seek = Enum Ty.seekModeRef seekRelativeTag
seek'from'end = Enum Ty.seekModeRef seekEndTag

instance ForeignConvention SeekMode where
  decodeVal (BoxedVal c) = decodeSeekMode c
  decodeVal v = foreignConventionError "SeekMode" v

  encodeVal = BoxedVal . encodeSeekMode

  readAtIndex stk i = bpeekOff stk i >>= decodeSeekMode
  writeBack stk sm = bpoke stk (encodeSeekMode sm)

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
-- instance {-# OVERLAPPING #-} ForeignConvention [Val] where
--   decodeVal = decode
--   readForeign (i : args) stk =
--     (args,) . toList <$> peekOffS stk i
--   readForeign _ _ = foreignCCError "[Val]"
--   writeForeign stk l = do
--     stk <- bump stk
--     stk <$ pokeS stk (Sq.fromList l)

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
-- instance {-# OVERLAPPING #-} ForeignConvention [Closure] where
--   readForeign (i : args) stk =
--     (args,) . fmap getBoxedVal . toList <$> peekOffS stk i
--   readForeign _ _ = foreignCCError "[Closure]"
--   writeForeign stk l = do
--     stk <- bump stk
--     stk <$ pokeS stk (Sq.fromList . fmap BoxedVal $ l)
--
-- instance ForeignConvention [Foreign] where
--   readForeign = readForeignAs (fmap marshalToForeign)
--   writeForeign = writeForeignAs (fmap Foreign)
--

instance {-# overlapping #-} ForeignConvention String where
  decodeVal = decodeAsBuiltin unpack
  encodeVal = encodeAsBuiltin pack

  readAtIndex = readAsBuiltin unpack
  writeBack = writeAsBuiltin pack

instance ForeignConvention Bool where
  decodeVal (BoolVal b) = pure b
  decodeVal v = foreignConventionError "Bool" v

  encodeVal = BoolVal

  readAtIndex = peekOffBool
  writeBack = pokeBool

instance ForeignConvention Double where
  decodeVal (DoubleVal d) = pure d
  decodeVal v = foreignConventionError "Double" v

  encodeVal = DoubleVal

  readAtIndex = peekOffD
  writeBack = pokeD

instance ForeignConvention Val where
  decodeVal = pure
  encodeVal = id

  readAtIndex = peekOff
  writeBack = poke

instance ForeignConvention Foreign where
  decodeVal (BoxedVal (Foreign f)) = pure f
  decodeVal v = foreignConventionError "Foreign" v
  encodeVal f = BoxedVal (Foreign f)

  readAtIndex stk i = bpeekOff stk i >>= \case
    Foreign f -> pure f
    c -> foreignConventionError "Foreign" (BoxedVal c)
  writeBack stk f = bpoke stk (Foreign f)

instance ForeignConvention a => ForeignConvention [a] where
  decodeVal (BoxedVal (Foreign f))
    | (sq :: Sq.Seq Val) <- unwrapForeign f = traverse decodeVal (toList sq)
  decodeVal v = foreignConventionError "List" v

  encodeVal l =
    BoxedVal . Foreign . Wrap listRef . Sq.fromList $ encodeVal <$> l

  readAtIndex stk i = traverse decodeVal . toList =<< peekOffS stk i

  writeBack stk sq = pokeS stk . Sq.fromList $ encodeVal <$> sq

instance {-# overlappable #-} (BuiltinForeign b) => ForeignConvention b where
  decodeVal = decodeBuiltin
  encodeVal = encodeBuiltin
  readAtIndex = readBuiltinAt
  writeBack = writeBuiltin
