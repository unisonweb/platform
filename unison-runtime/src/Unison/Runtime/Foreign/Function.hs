{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Foreign.Function () where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TVar)
import Control.Exception (evaluate)
import Data.Atomics (Ticket)
import Data.Foldable (toList)
import Data.IORef (IORef)
import Data.Sequence qualified as Sq
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Socket (Socket)
import Network.UDP (UDPSocket)
import System.IO (BufferMode (..), Handle, IOMode, SeekMode)
import Unison.Builtin.Decls qualified as Ty
import Unison.Reference (Reference)
import Unison.Runtime.ANF (Code, PackedTag (..), Value, internalBug)
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Exception
import Unison.Runtime.Foreign
import Unison.Runtime.Stack
import Unison.Type
  ( iarrayRef,
    ibytearrayRef,
    marrayRef,
    mbytearrayRef,
    mvarRef,
    promiseRef,
    refRef,
    ticketRef,
    tvarRef,
    typeLinkRef,
  )
import Unison.Util.Bytes (Bytes)
import Unison.Util.RefPromise (Promise)
import Unison.Util.Text (Text, pack, unpack)

class ForeignConvention a where
  readForeign ::
    [Int] -> Stack -> IO ([Int], a)
  writeForeign ::
    Stack -> a -> IO Stack

instance ForeignConvention Int where
  readForeign (i : args) stk = (args,) <$> peekOffI stk i
  readForeign [] _ = foreignCCError "Int"
  {-# INLINE readForeign #-}
  writeForeign stk i = do
    stk <- bump stk
    stk <$ pokeI stk i
  {-# INLINE writeForeign #-}

instance ForeignConvention Word64 where
  readForeign (i : args) stk = (args,) <$> peekOffN stk i
  readForeign [] _ = foreignCCError "Word64"
  {-# INLINE readForeign #-}
  writeForeign stk n = do
    stk <- bump stk
    stk <$ pokeN stk n
  {-# INLINE writeForeign #-}

-- We don't have a clear mapping from these types to Unison types, most are just mapped to Nats.

instance ForeignConvention Word8 where
  readForeign = readForeignAs (fromIntegral :: Word64 -> Word8)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (fromIntegral :: Word8 -> Word64)
  {-# INLINE writeForeign #-}

instance ForeignConvention Word16 where
  readForeign = readForeignAs (fromIntegral :: Word64 -> Word16)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (fromIntegral :: Word16 -> Word64)
  {-# INLINE writeForeign #-}

instance ForeignConvention Word32 where
  readForeign = readForeignAs (fromIntegral :: Word64 -> Word32)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (fromIntegral :: Word32 -> Word64)
  {-# INLINE writeForeign #-}

instance ForeignConvention Char where
  readForeign (i : args) stk = (args,) <$> peekOffC stk i
  readForeign [] _ = foreignCCError "Char"
  {-# INLINE readForeign #-}
  writeForeign stk ch = do
    stk <- bump stk
    stk <$ pokeC stk ch
  {-# INLINE writeForeign #-}

instance ForeignConvention Val where
  readForeign (i : args) stk = (args,) <$> peekOff stk i
  readForeign [] _ = foreignCCError "Val"
  {-# INLINE readForeign #-}
  writeForeign stk v = do
    stk <- bump stk
    stk <$ (poke stk =<< evaluate v)
  {-# INLINE writeForeign #-}

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
instance ForeignConvention Closure where
  readForeign (i : args) stk = (args,) <$> bpeekOff stk i
  readForeign [] _ = foreignCCError "Closure"
  {-# INLINE readForeign #-}
  writeForeign stk c = do
    stk <- bump stk
    stk <$ (bpoke stk =<< evaluate c)
  {-# INLINE writeForeign #-}

instance ForeignConvention Text where
  readForeign = readForeignBuiltin
  {-# INLINE readForeign #-}
  writeForeign = writeForeignBuiltin
  {-# INLINE writeForeign #-}

instance ForeignConvention Bytes where
  readForeign = readForeignBuiltin
  {-# INLINE readForeign #-}
  writeForeign = writeForeignBuiltin
  {-# INLINE writeForeign #-}

instance ForeignConvention Socket where
  readForeign = readForeignBuiltin
  {-# INLINE readForeign #-}
  writeForeign = writeForeignBuiltin
  {-# INLINE writeForeign #-}

instance ForeignConvention UDPSocket where
  readForeign = readForeignBuiltin
  {-# INLINE readForeign #-}
  writeForeign = writeForeignBuiltin
  {-# INLINE writeForeign #-}

instance ForeignConvention ThreadId where
  readForeign = readForeignBuiltin
  {-# INLINE readForeign #-}
  writeForeign = writeForeignBuiltin
  {-# INLINE writeForeign #-}

instance ForeignConvention Handle where
  readForeign = readForeignBuiltin
  {-# INLINE readForeign #-}
  writeForeign = writeForeignBuiltin
  {-# INLINE writeForeign #-}

instance ForeignConvention POSIXTime where
  readForeign = readForeignAs (fromIntegral :: Int -> POSIXTime)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (round :: POSIXTime -> Int)
  {-# INLINE writeForeign #-}

instance (ForeignConvention a) => ForeignConvention (Maybe a) where
  readForeign (i : args) stk =
    upeekOff stk i >>= \case
      0 -> pure (args, Nothing)
      1 -> fmap Just <$> readForeign args stk
      _ -> foreignCCError "Maybe"
  readForeign [] _ = foreignCCError "Maybe"
  {-# INLINE readForeign #-}

  writeForeign stk Nothing = do
    stk <- bump stk
    stk <$ pokeTag stk 0
  writeForeign stk (Just x) = do
    stk <- writeForeign stk x
    stk <- bump stk
    stk <$ pokeTag stk 1
  {-# INLINE writeForeign #-}

instance
  (ForeignConvention a, ForeignConvention b) =>
  ForeignConvention (Either a b)
  where
  readForeign (i : args) stk =
    peekTagOff stk i >>= \case
      0 -> readForeignAs Left args stk
      1 -> readForeignAs Right args stk
      _ -> foreignCCError "Either"
  readForeign _ _ = foreignCCError "Either"
  {-# INLINE readForeign #-}

  writeForeign stk (Left a) = do
    stk <- writeForeign stk a
    stk <- bump stk
    stk <$ pokeTag stk 0
  writeForeign stk (Right b) = do
    stk <- writeForeign stk b
    stk <- bump stk
    stk <$ pokeTag stk 1
  {-# INLINE writeForeign #-}

ioeDecode :: Int -> IOErrorType
ioeDecode 0 = AlreadyExists
ioeDecode 1 = NoSuchThing
ioeDecode 2 = ResourceBusy
ioeDecode 3 = ResourceExhausted
ioeDecode 4 = EOF
ioeDecode 5 = IllegalOperation
ioeDecode 6 = PermissionDenied
ioeDecode 7 = UserError
ioeDecode _ = internalBug "ioeDecode"

ioeEncode :: IOErrorType -> Int
ioeEncode AlreadyExists = 0
ioeEncode NoSuchThing = 1
ioeEncode ResourceBusy = 2
ioeEncode ResourceExhausted = 3
ioeEncode EOF = 4
ioeEncode IllegalOperation = 5
ioeEncode PermissionDenied = 6
ioeEncode UserError = 7
ioeEncode _ = internalBug "ioeDecode"

instance ForeignConvention IOException where
  readForeign = readForeignAs (bld . ioeDecode)
    where
      bld t = IOError Nothing t "" "" Nothing Nothing
  {-# INLINE readForeign #-}

  writeForeign = writeForeignAs (ioeEncode . ioe_type)
  {-# INLINE writeForeign #-}

readForeignAs ::
  (ForeignConvention a) =>
  (a -> b) ->
  [Int] ->
  Stack ->
  IO ([Int], b)
readForeignAs f args stk = fmap f <$> readForeign args stk

writeForeignAs ::
  (ForeignConvention b) =>
  (a -> b) ->
  Stack ->
  a ->
  IO Stack
writeForeignAs f stk x = writeForeign stk (f x)

readForeignEnum ::
  (Enum a) =>
  [Int] ->
  Stack ->
  IO ([Int], a)
readForeignEnum = readForeignAs toEnum

writeForeignEnum ::
  (Enum a) =>
  Stack ->
  a ->
  IO Stack
writeForeignEnum = writeForeignAs fromEnum

readForeignBuiltin ::
  (BuiltinForeign b) =>
  [Int] ->
  Stack ->
  IO ([Int], b)
readForeignBuiltin = readForeignAs (unwrapBuiltin . marshalToForeign)

writeForeignBuiltin ::
  (BuiltinForeign b) =>
  Stack ->
  b ->
  IO Stack
writeForeignBuiltin = writeForeignAs (Foreign . wrapBuiltin)

writeTypeLink ::
  Stack ->
  Reference ->
  IO Stack
writeTypeLink = writeForeignAs (Foreign . Wrap typeLinkRef)

readTypelink ::
  [Int] ->
  Stack ->
  IO ([Int], Reference)
readTypelink = readForeignAs (unwrapForeign . marshalToForeign)

instance ForeignConvention Double where
  readForeign (i : args) stk = (args,) <$> peekOffD stk i
  readForeign _ _ = foreignCCError "Double"
  {-# INLINE readForeign #-}
  writeForeign stk d =
    bump stk >>= \stk -> do
      pokeD stk d
      pure stk
  {-# INLINE writeForeign #-}

instance ForeignConvention Bool where
  readForeign (i : args) stk = do
    b <- peekOffBool stk i
    pure (args, b)
  readForeign _ _ = foreignCCError "Bool"
  {-# INLINE readForeign #-}
  writeForeign stk b = do
    stk <- bump stk
    pokeBool stk b
    pure stk
  {-# INLINE writeForeign #-}

instance ForeignConvention String where
  readForeign = readForeignAs unpack
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs pack
  {-# INLINE writeForeign #-}

instance ForeignConvention SeekMode where
  readForeign = readForeignEnum
  {-# INLINE readForeign #-}
  writeForeign = writeForeignEnum
  {-# INLINE writeForeign #-}

instance ForeignConvention IOMode where
  readForeign = readForeignEnum
  {-# INLINE readForeign #-}
  writeForeign = writeForeignEnum
  {-# INLINE writeForeign #-}

instance ForeignConvention () where
  readForeign args _ = pure (args, ())
  {-# INLINE readForeign #-}
  writeForeign stk _ = pure stk
  {-# INLINE writeForeign #-}

instance
  (ForeignConvention a, ForeignConvention b) =>
  ForeignConvention (a, b)
  where
  readForeign args stk = do
    (args, a) <- readForeign args stk
    (args, b) <- readForeign args stk
    pure (args, (a, b))
  {-# INLINE readForeign #-}

  writeForeign stk (x, y) = do
    stk <- writeForeign stk y
    writeForeign stk x
  {-# INLINE writeForeign #-}

instance (ForeignConvention a) => ForeignConvention (Failure a) where
  readForeign args stk = do
    (args, typeref) <- readTypelink args stk
    (args, message) <- readForeign args stk
    (args, any) <- readForeign args stk
    pure (args, Failure typeref message any)
  {-# INLINE readForeign #-}

  writeForeign stk (Failure typeref message any) = do
    stk <- writeForeign stk any
    stk <- writeForeign stk message
    writeTypeLink stk typeref
  {-# INLINE writeForeign #-}

instance
  ( ForeignConvention a,
    ForeignConvention b,
    ForeignConvention c
  ) =>
  ForeignConvention (a, b, c)
  where
  readForeign args stk = do
    (args, a) <- readForeign args stk
    (args, b) <- readForeign args stk
    (args, c) <- readForeign args stk
    pure (args, (a, b, c))
  {-# INLINE readForeign #-}

  writeForeign stk (a, b, c) = do
    stk <- writeForeign stk c
    stk <- writeForeign stk b
    writeForeign stk a
  {-# INLINE writeForeign #-}

instance
  ( ForeignConvention a,
    ForeignConvention b,
    ForeignConvention c,
    ForeignConvention d
  ) =>
  ForeignConvention (a, b, c, d)
  where
  readForeign args stk = do
    (args, a) <- readForeign args stk
    (args, b) <- readForeign args stk
    (args, c) <- readForeign args stk
    (args, d) <- readForeign args stk
    pure (args, (a, b, c, d))
  {-# INLINE readForeign #-}

  writeForeign stk (a, b, c, d) = do
    stk <- writeForeign stk d
    stk <- writeForeign stk c
    stk <- writeForeign stk b
    writeForeign stk a
  {-# INLINE writeForeign #-}

instance
  ( ForeignConvention a,
    ForeignConvention b,
    ForeignConvention c,
    ForeignConvention d,
    ForeignConvention e
  ) =>
  ForeignConvention (a, b, c, d, e)
  where
  readForeign args stk = do
    (args, a) <- readForeign args stk
    (args, b) <- readForeign args stk
    (args, c) <- readForeign args stk
    (args, d) <- readForeign args stk
    (args, e) <- readForeign args stk
    pure (args, (a, b, c, d, e))
  {-# INLINE readForeign #-}

  writeForeign stk (a, b, c, d, e) = do
    stk <- writeForeign stk e
    stk <- writeForeign stk d
    stk <- writeForeign stk c
    stk <- writeForeign stk b
    writeForeign stk a
  {-# INLINE writeForeign #-}

no'buf, line'buf, block'buf, sblock'buf :: Word64
no'buf = fromIntegral Ty.bufferModeNoBufferingId
line'buf = fromIntegral Ty.bufferModeLineBufferingId
block'buf = fromIntegral Ty.bufferModeBlockBufferingId
sblock'buf = fromIntegral Ty.bufferModeSizedBlockBufferingId

instance ForeignConvention BufferMode where
  readForeign (i : args) stk =
    peekOffN stk i >>= \case
      t
        | t == no'buf -> pure (args, NoBuffering)
        | t == line'buf -> pure (args, LineBuffering)
        | t == block'buf -> pure (args, BlockBuffering Nothing)
        | t == sblock'buf ->
            fmap (BlockBuffering . Just)
              <$> readForeign args stk
        | otherwise ->
            foreignCCError $
              "BufferMode (unknown tag: " <> show t <> ")"
  readForeign _ _ = foreignCCError $ "BufferMode (empty stack)"
  {-# INLINE readForeign #-}

  writeForeign stk bm =
    bump stk >>= \stk ->
      case bm of
        NoBuffering -> stk <$ pokeN stk no'buf
        LineBuffering -> stk <$ pokeN stk line'buf
        BlockBuffering Nothing -> stk <$ pokeN stk block'buf
        BlockBuffering (Just n) -> do
          pokeI stk n
          stk <- bump stk
          stk <$ pokeN stk sblock'buf
  {-# INLINE writeForeign #-}

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
instance {-# OVERLAPPING #-} ForeignConvention [Val] where
  readForeign (i : args) stk =
    (args,) . toList <$> peekOffS stk i
  readForeign _ _ = foreignCCError "[Val]"
  {-# INLINE readForeign #-}
  writeForeign stk l = do
    stk <- bump stk
    stk <$ pokeS stk (Sq.fromList l)
  {-# INLINE writeForeign #-}

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
instance {-# OVERLAPPING #-} ForeignConvention [Closure] where
  readForeign (i : args) stk =
    (args,) . fmap getBoxedVal . toList <$> peekOffS stk i
  readForeign _ _ = foreignCCError "[Closure]"
  {-# INLINE readForeign #-}
  writeForeign stk l = do
    stk <- bump stk
    stk <$ pokeS stk (Sq.fromList . fmap BoxedVal $ l)
  {-# INLINE writeForeign #-}

instance ForeignConvention [Foreign] where
  readForeign = readForeignAs (fmap marshalToForeign)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (fmap Foreign)
  {-# INLINE writeForeign #-}

instance ForeignConvention (MVar Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (Foreign . Wrap mvarRef)
  {-# INLINE writeForeign #-}

instance ForeignConvention (TVar Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (Foreign . Wrap tvarRef)
  {-# INLINE writeForeign #-}

instance ForeignConvention (IORef Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (Foreign . Wrap refRef)
  {-# INLINE writeForeign #-}

instance ForeignConvention (Ticket Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (Foreign . Wrap ticketRef)
  {-# INLINE writeForeign #-}

instance ForeignConvention (Promise Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (Foreign . Wrap promiseRef)
  {-# INLINE writeForeign #-}

instance ForeignConvention Code where
  readForeign = readForeignBuiltin
  {-# INLINE readForeign #-}
  writeForeign = writeForeignBuiltin
  {-# INLINE writeForeign #-}

instance ForeignConvention Value where
  readForeign = readForeignBuiltin
  {-# INLINE readForeign #-}
  writeForeign = writeForeignBuiltin
  {-# INLINE writeForeign #-}

instance ForeignConvention Foreign where
  readForeign = readForeignAs marshalToForeign
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs Foreign
  {-# INLINE writeForeign #-}

instance ForeignConvention (PA.MutableArray s Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (Foreign . Wrap marrayRef)
  {-# INLINE writeForeign #-}

instance ForeignConvention (PA.MutableByteArray s) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (Foreign . Wrap mbytearrayRef)
  {-# INLINE writeForeign #-}

instance ForeignConvention (PA.Array Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (Foreign . Wrap iarrayRef)
  {-# INLINE writeForeign #-}

instance ForeignConvention PA.ByteArray where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  {-# INLINE readForeign #-}
  writeForeign = writeForeignAs (Foreign . Wrap ibytearrayRef)
  {-# INLINE writeForeign #-}

instance {-# OVERLAPPABLE #-} (BuiltinForeign b) => ForeignConvention b where
  readForeign = readForeignBuiltin
  {-# INLINE readForeign #-}
  writeForeign = writeForeignBuiltin
  {-# INLINE writeForeign #-}

fromUnisonPair :: (BuiltinForeign a, BuiltinForeign b) => Closure -> (a, b)
fromUnisonPair (DataC _ _ [BoxedVal x, BoxedVal (DataC _ _ [BoxedVal y, BoxedVal _unit])]) =
  (unwrapForeignClosure x, unwrapForeignClosure y)
fromUnisonPair _ = error "fromUnisonPair: invalid closure"

toUnisonPair ::
  (BuiltinForeign a, BuiltinForeign b) => (a, b) -> Closure
toUnisonPair (x, y) =
  DataC
    Ty.pairRef
    (PackedTag 0)
    [BoxedVal $ wr x, BoxedVal $ DataC Ty.pairRef (PackedTag 0) [BoxedVal $ wr y, BoxedVal $ un]]
  where
    un = DataC Ty.unitRef (PackedTag 0) []
    wr z = Foreign $ wrapBuiltin z

unwrapForeignClosure :: Closure -> a
unwrapForeignClosure = unwrapForeign . marshalToForeign

instance {-# OVERLAPPABLE #-} (BuiltinForeign a, BuiltinForeign b) => ForeignConvention [(a, b)] where
  readForeign (i : args) stk =
    (args,)
      . fmap (fromUnisonPair . getBoxedVal)
      . toList
      <$> peekOffS stk i
  readForeign _ _ = foreignCCError "[(a,b)]"
  {-# INLINE readForeign #-}

  writeForeign stk l = do
    stk <- bump stk
    stk <$ pokeS stk (boxedVal . toUnisonPair <$> Sq.fromList l)

instance {-# OVERLAPPABLE #-} (BuiltinForeign b) => ForeignConvention [b] where
  readForeign (i : args) stk =
    (args,)
      . fmap (unwrapForeignClosure . getBoxedVal)
      . toList
      <$> peekOffS stk i
  readForeign _ _ = foreignCCError "[b]"
  {-# INLINE readForeign #-}
  writeForeign stk l = do
    stk <- bump stk
    stk <$ pokeS stk (boxedVal . Foreign . wrapBuiltin <$> Sq.fromList l)
  {-# INLINE writeForeign #-}

foreignCCError :: String -> IO a
foreignCCError nm =
  die $ "mismatched foreign calling convention for `" ++ nm ++ "`"
