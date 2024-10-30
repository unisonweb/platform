{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Foreign.Function
  ( ForeignFunc (..),
    ForeignConvention (..),
    mkForeign,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TVar)
import Control.Exception (evaluate)
import Data.Atomics (Ticket)
import Data.Char qualified as Char
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
import Unison.Runtime.ANF (Code, Value, internalBug)
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Exception
import Unison.Runtime.Foreign
import Unison.Runtime.MCode
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

-- Foreign functions operating on stacks
data ForeignFunc where
  FF ::
    (Stack -> Args -> IO a) ->
    (Stack -> r -> IO Stack) ->
    (a -> IO r) ->
    ForeignFunc

instance Show ForeignFunc where
  show _ = "ForeignFunc"

instance Eq ForeignFunc where
  _ == _ = internalBug "Eq ForeignFunc"

instance Ord ForeignFunc where
  compare _ _ = internalBug "Ord ForeignFunc"

class ForeignConvention a where
  readForeign ::
    [Int] -> Stack -> IO ([Int], a)
  writeForeign ::
    Stack -> a -> IO Stack

mkForeign ::
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO r) ->
  ForeignFunc
mkForeign ev = FF readArgs writeForeign ev
  where
    readArgs stk (argsToLists -> args) =
      readForeign args stk >>= \case
        ([], a) -> pure a
        _ ->
          internalBug
            "mkForeign: too many arguments for foreign function"

instance ForeignConvention Int where
  readForeign (i : args) stk = (args,) <$> upeekOff stk i
  readForeign [] _ = foreignCCError "Int"
  writeForeign stk i = do
    stk <- bump stk
    stk <$ upoke stk i

instance ForeignConvention Word64 where
  readForeign (i : args) stk = (args,) <$> peekOffN stk i
  readForeign [] _ = foreignCCError "Word64"
  writeForeign stk n = do
    stk <- bump stk
    stk <$ pokeN stk n

instance ForeignConvention Word8 where
  readForeign = readForeignAs (fromIntegral :: Word64 -> Word8)
  writeForeign = writeForeignAs (fromIntegral :: Word8 -> Word64)

instance ForeignConvention Word16 where
  readForeign = readForeignAs (fromIntegral :: Word64 -> Word16)
  writeForeign = writeForeignAs (fromIntegral :: Word16 -> Word64)

instance ForeignConvention Word32 where
  readForeign = readForeignAs (fromIntegral :: Word64 -> Word32)
  writeForeign = writeForeignAs (fromIntegral :: Word32 -> Word64)

instance ForeignConvention Char where
  readForeign (i : args) stk = (args,) . Char.chr <$> upeekOff stk i
  readForeign [] _ = foreignCCError "Char"
  writeForeign stk ch = do
    stk <- bump stk
    stk <$ upoke stk (Char.ord ch)

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
instance ForeignConvention Closure where
  readForeign (i : args) stk = (args,) <$> bpeekOff stk i
  readForeign [] _ = foreignCCError "Closure"
  writeForeign stk c = do
    stk <- bump stk
    stk <$ (bpoke stk =<< evaluate c)

instance ForeignConvention Text where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention Bytes where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention Socket where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention UDPSocket where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention ThreadId where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention Handle where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention POSIXTime where
  readForeign = readForeignAs (fromIntegral :: Int -> POSIXTime)
  writeForeign = writeForeignAs (round :: POSIXTime -> Int)

instance (ForeignConvention a) => ForeignConvention (Maybe a) where
  readForeign (i : args) stk =
    upeekOff stk i >>= \case
      0 -> pure (args, Nothing)
      1 -> fmap Just <$> readForeign args stk
      _ -> foreignCCError "Maybe"
  readForeign [] _ = foreignCCError "Maybe"

  writeForeign stk Nothing = do
    stk <- bump stk
    stk <$ upoke stk 0
  writeForeign stk (Just x) = do
    stk <- writeForeign stk x
    stk <- bump stk
    stk <$ upoke stk 1

instance
  (ForeignConvention a, ForeignConvention b) =>
  ForeignConvention (Either a b)
  where
  readForeign (i : args) stk =
    upeekOff stk i >>= \case
      0 -> readForeignAs Left args stk
      1 -> readForeignAs Right args stk
      _ -> foreignCCError "Either"
  readForeign _ _ = foreignCCError "Either"

  writeForeign stk (Left a) = do
    stk <- writeForeign stk a
    stk <- bump stk
    stk <$ upoke stk 0
  writeForeign stk (Right b) = do
    stk <- writeForeign stk b
    stk <- bump stk
    stk <$ upoke stk 1

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

  writeForeign = writeForeignAs (ioeEncode . ioe_type)

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
  writeForeign stk d =
    bump stk >>= \stk -> do
      pokeD stk d
      pure stk

instance ForeignConvention Bool where
  readForeign = readForeignEnum
  writeForeign = writeForeignEnum

instance ForeignConvention String where
  readForeign = readForeignAs unpack
  writeForeign = writeForeignAs pack

instance ForeignConvention SeekMode where
  readForeign = readForeignEnum
  writeForeign = writeForeignEnum

instance ForeignConvention IOMode where
  readForeign = readForeignEnum
  writeForeign = writeForeignEnum

instance ForeignConvention () where
  readForeign args _ = pure (args, ())
  writeForeign stk _ = pure stk

instance
  (ForeignConvention a, ForeignConvention b) =>
  ForeignConvention (a, b)
  where
  readForeign args stk = do
    (args, a) <- readForeign args stk
    (args, b) <- readForeign args stk
    pure (args, (a, b))

  writeForeign stk (x, y) = do
    stk <- writeForeign stk y
    writeForeign stk x

instance (ForeignConvention a) => ForeignConvention (Failure a) where
  readForeign args stk = do
    (args, typeref) <- readTypelink args stk
    (args, message) <- readForeign args stk
    (args, any) <- readForeign args stk
    pure (args, Failure typeref message any)

  writeForeign stk (Failure typeref message any) = do
    stk <- writeForeign stk any
    stk <- writeForeign stk message
    writeTypeLink stk typeref

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

  writeForeign stk (a, b, c) = do
    stk <- writeForeign stk c
    stk <- writeForeign stk b
    writeForeign stk a

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

  writeForeign stk (a, b, c, d) = do
    stk <- writeForeign stk d
    stk <- writeForeign stk c
    stk <- writeForeign stk b
    writeForeign stk a

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

  writeForeign stk (a, b, c, d, e) = do
    stk <- writeForeign stk e
    stk <- writeForeign stk d
    stk <- writeForeign stk c
    stk <- writeForeign stk b
    writeForeign stk a

no'buf, line'buf, block'buf, sblock'buf :: Int
no'buf = fromIntegral Ty.bufferModeNoBufferingId
line'buf = fromIntegral Ty.bufferModeLineBufferingId
block'buf = fromIntegral Ty.bufferModeBlockBufferingId
sblock'buf = fromIntegral Ty.bufferModeSizedBlockBufferingId

instance ForeignConvention BufferMode where
  readForeign (i : args) stk =
    upeekOff stk i >>= \case
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

  writeForeign stk bm =
    bump stk >>= \stk ->
      case bm of
        NoBuffering -> stk <$ upoke stk no'buf
        LineBuffering -> stk <$ upoke stk line'buf
        BlockBuffering Nothing -> stk <$ upoke stk block'buf
        BlockBuffering (Just n) -> do
          upoke stk n
          stk <- bump stk
          stk <$ upoke stk sblock'buf

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
instance ForeignConvention [Closure] where
  readForeign (i : args) stk =
    (args,) . toList <$> peekOffS stk i
  readForeign _ _ = foreignCCError "[Closure]"
  writeForeign stk l = do
    stk <- bump stk
    stk <$ pokeS stk (Sq.fromList l)

instance ForeignConvention [Foreign] where
  readForeign = readForeignAs (fmap marshalToForeign)
  writeForeign = writeForeignAs (fmap Foreign)

instance ForeignConvention (MVar Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap mvarRef)

instance ForeignConvention (TVar Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap tvarRef)

instance ForeignConvention (IORef Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap refRef)

instance ForeignConvention (Ticket Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap ticketRef)

instance ForeignConvention (Promise Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap promiseRef)

instance ForeignConvention Code where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention Value where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention Foreign where
  readForeign = readForeignAs marshalToForeign
  writeForeign = writeForeignAs Foreign

instance ForeignConvention (PA.MutableArray s Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap marrayRef)

instance ForeignConvention (PA.MutableByteArray s) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap mbytearrayRef)

instance ForeignConvention (PA.Array Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap iarrayRef)

instance ForeignConvention PA.ByteArray where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap ibytearrayRef)

instance {-# OVERLAPPABLE #-} (BuiltinForeign b) => ForeignConvention b where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

fromUnisonPair :: Closure -> (a, b)
fromUnisonPair (DataC _ _ [Right x, Right (DataC _ _ [Right y, Right _])]) =
  (unwrapForeignClosure x, unwrapForeignClosure y)
fromUnisonPair _ = error "fromUnisonPair: invalid closure"

toUnisonPair ::
  (BuiltinForeign a, BuiltinForeign b) => (a, b) -> Closure
toUnisonPair (x, y) =
  DataC
    Ty.pairRef
    0
    [Right $ wr x, Right $ DataC Ty.pairRef 0 [Right $ wr y, Right $ un]]
  where
    un = DataC Ty.unitRef 0 []
    wr z = Foreign $ wrapBuiltin z

unwrapForeignClosure :: Closure -> a
unwrapForeignClosure = unwrapForeign . marshalToForeign

instance {-# OVERLAPPABLE #-} (BuiltinForeign a, BuiltinForeign b) => ForeignConvention [(a, b)] where
  readForeign (i : args) stk =
    (args,)
      . fmap fromUnisonPair
      . toList
      <$> peekOffS stk i
  readForeign _ _ = foreignCCError "[(a,b)]"

  writeForeign stk l = do
    stk <- bump stk
    stk <$ pokeS stk (toUnisonPair <$> Sq.fromList l)

instance {-# OVERLAPPABLE #-} (BuiltinForeign b) => ForeignConvention [b] where
  readForeign (i : args) stk =
    (args,)
      . fmap unwrapForeignClosure
      . toList
      <$> peekOffS stk i
  readForeign _ _ = foreignCCError "[b]"
  writeForeign stk l = do
    stk <- bump stk
    stk <$ pokeS stk (Foreign . wrapBuiltin <$> Sq.fromList l)

foreignCCError :: String -> IO a
foreignCCError nm =
  die $ "mismatched foreign calling convention for `" ++ nm ++ "`"
