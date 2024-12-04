{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Foreign.Function
  ( ForeignFunc (..),
    ForeignConvention (..),
    ForeignFunc' (..),
    mkForeign,
  )
where

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

-- | Enum representing every foreign call.
data ForeignFunc'
  = IO_UDP_clientSocket_impl_v1
  | IO_UDP_UDPSocket_recv_impl_v1
  | IO_UDP_UDPSocket_send_impl_v1
  | IO_UDP_UDPSocket_close_impl_v1
  | IO_UDP_ListenSocket_close_impl_v1
  | IO_UDP_UDPSocket_toText_impl_v1
  | IO_UDP_serverSocket_impl_v1
  | IO_UDP_ListenSocket_toText_impl_v1
  | IO_UDP_ListenSocket_recvFrom_impl_v1
  | IO_UDP_ClientSockAddr_toText_v1
  | IO_UDP_ListenSocket_sendTo_impl_v1
  | IO_openFile_impl_v3
  | IO_closeFile_impl_v3
  | IO_isFileEOF_impl_v3
  | IO_isFileOpen_impl_v3
  | IO_getEcho_impl_v1
  | IO_ready_impl_v1
  | IO_getChar_impl_v1
  | IO_isSeekable_impl_v3
  | IO_seekHandle_impl_v3
  | IO_handlePosition_impl_v3
  | IO_getBuffering_impl_v3
  | IO_setBuffering_impl_v3
  | IO_setEcho_impl_v1
  | IO_getLine_impl_v1
  | IO_getBytes_impl_v3
  | IO_getSomeBytes_impl_v1
  | IO_putBytes_impl_v3
  | IO_systemTime_impl_v3
  | IO_systemTimeMicroseconds_v1
  | Clock_internals_monotonic_v1
  | Clock_internals_realtime_v1
  | Clock_internals_processCPUTime_v1
  | Clock_internals_threadCPUTime_v1
  | Clock_internals_sec_v1
  | Clock_internals_nsec_v1
  | Clock_internals_systemTimeZone_v1
  | IO_getTempDirectory_impl_v3
  | IO_createTempDirectory_impl_v3
  | IO_getCurrentDirectory_impl_v3
  | IO_setCurrentDirectory_impl_v3
  | IO_fileExists_impl_v3
  | IO_getEnv_impl_v1
  | IO_getArgs_impl_v1
  | IO_isDirectory_impl_v3
  | IO_createDirectory_impl_v3
  | IO_removeDirectory_impl_v3
  | IO_renameDirectory_impl_v3
  | IO_directoryContents_impl_v3
  | IO_removeFile_impl_v3
  | IO_renameFile_impl_v3
  | IO_getFileTimestamp_impl_v3
  | IO_getFileSize_impl_v3
  | IO_serverSocket_impl_v3
  | Socket_toText
  | Handle_toText
  | ThreadId_toText
  | IO_socketPort_impl_v3
  | IO_listen_impl_v3
  | IO_clientSocket_impl_v3
  | IO_closeSocket_impl_v3
  | IO_socketAccept_impl_v3
  | IO_socketSend_impl_v3
  | IO_socketReceive_impl_v3
  | IO_kill_impl_v3
  | IO_delay_impl_v3
  | IO_stdHandle
  | IO_process_call
  | IO_process_start
  | IO_process_kill
  | IO_process_wait
  | IO_process_exitCode
  | MVar_new
  | MVar_newEmpty_v2
  | MVar_take_impl_v3
  | MVar_tryTake
  | MVar_put_impl_v3
  | MVar_tryPut_impl_v3
  | MVar_swap_impl_v3
  | MVar_isEmpty
  | MVar_read_impl_v3
  | MVar_tryRead_impl_v3
  | Char_toText
  | Text_repeat
  | Text_reverse
  | Text_toUppercase
  | Text_toLowercase
  | Text_toUtf8
  | Text_fromUtf8_impl_v3
  | Tls_ClientConfig_default
  | Tls_ServerConfig_default
  | Tls_ClientConfig_certificates_set
  | Tls_ServerConfig_certificates_set
  | TVar_new
  | TVar_read
  | TVar_write
  | TVar_newIO
  | TVar_readIO
  | TVar_swap
  | STM_retry
  | Promise_new
  | Promise_read
  | Promise_tryRead
  | Promise_write
  | Tls_newClient_impl_v3
  | Tls_newServer_impl_v3
  | Tls_handshake_impl_v3
  | Tls_send_impl_v3
  | Tls_decodeCert_impl_v3
  | Tls_encodeCert
  | Tls_decodePrivateKey
  | Tls_encodePrivateKey
  | Tls_receive_impl_v3
  | Tls_terminate_impl_v3
  | Code_validateLinks
  | Code_dependencies
  | Code_serialize
  | Code_deserialize
  | Code_display
  | Value_dependencies
  | Value_serialize
  | Value_deserialize
  | Crypto_HashAlgorithm_Sha3_512
  | Crypto_HashAlgorithm_Sha3_256
  | Crypto_HashAlgorithm_Sha2_512
  | Crypto_HashAlgorithm_Sha2_256
  | Crypto_HashAlgorithm_Sha1
  | Crypto_HashAlgorithm_Blake2b_512
  | Crypto_HashAlgorithm_Blake2b_256
  | Crypto_HashAlgorithm_Blake2s_256
  | Crypto_HashAlgorithm_Md5
  | Crypto_hashBytes
  | Crypto_hmacBytes
  | Crypto_hash
  | Crypto_hmac
  | Crypto_Ed25519_sign_impl
  | Crypto_Ed25519_verify_impl
  | Crypto_Rsa_sign_impl
  | Crypto_Rsa_verify_impl
  | Universal_murmurHash
  | IO_randomBytes
  | Bytes_zlib_compress
  | Bytes_gzip_compress
  | Bytes_zlib_decompress
  | Bytes_gzip_decompress
  | Bytes_toBase16
  | Bytes_toBase32
  | Bytes_toBase64
  | Bytes_toBase64UrlUnpadded
  | Bytes_fromBase16
  | Bytes_fromBase32
  | Bytes_fromBase64
  | Bytes_fromBase64UrlUnpadded
  | Bytes_decodeNat64be
  | Bytes_decodeNat64le
  | Bytes_decodeNat32be
  | Bytes_decodeNat32le
  | Bytes_decodeNat16be
  | Bytes_decodeNat16le
  | Bytes_encodeNat64be
  | Bytes_encodeNat64le
  | Bytes_encodeNat32be
  | Bytes_encodeNat32le
  | Bytes_encodeNat16be
  | Bytes_encodeNat16le
  | MutableArray_copyTo_force
  | MutableByteArray_copyTo_force
  | ImmutableArray_copyTo_force
  | ImmutableArray_size
  | MutableArray_size
  | ImmutableByteArray_size
  | MutableByteArray_size
  | ImmutableByteArray_copyTo_force
  | MutableArray_read
  | MutableByteArray_read8
  | MutableByteArray_read16be
  | MutableByteArray_read24be
  | MutableByteArray_read32be
  | MutableByteArray_read40be
  | MutableByteArray_read64be
  | MutableArray_write
  | MutableByteArray_write8
  | MutableByteArray_write16be
  | MutableByteArray_write32be
  | MutableByteArray_write64be
  | ImmutableArray_read
  | ImmutableByteArray_read8
  | ImmutableByteArray_read16be
  | ImmutableByteArray_read24be
  | ImmutableByteArray_read32be
  | ImmutableByteArray_read40be
  | ImmutableByteArray_read64be
  | MutableByteArray_freeze_force
  | MutableArray_freeze_force
  | MutableByteArray_freeze
  | MutableArray_freeze
  | MutableByteArray_length
  | ImmutableByteArray_length
  | IO_array
  | IO_arrayOf
  | IO_bytearray
  | IO_bytearrayOf
  | Scope_array
  | Scope_arrayOf
  | Scope_bytearray
  | Scope_bytearrayOf
  | Text_patterns_literal
  | Text_patterns_digit
  | Text_patterns_letter
  | Text_patterns_space
  | Text_patterns_punctuation
  | Text_patterns_anyChar
  | Text_patterns_eof
  | Text_patterns_charRange
  | Text_patterns_notCharRange
  | Text_patterns_charIn
  | Text_patterns_notCharIn
  | Pattern_many
  | Pattern_many_corrected
  | Pattern_capture
  | Pattern_captureAs
  | Pattern_join
  | Pattern_or
  | Pattern_replicate
  | Pattern_run
  | Pattern_isMatch
  | Char_Class_any
  | Char_Class_not
  | Char_Class_and
  | Char_Class_or
  | Char_Class_range
  | Char_Class_anyOf
  | Char_Class_alphanumeric
  | Char_Class_upper
  | Char_Class_lower
  | Char_Class_whitespace
  | Char_Class_control
  | Char_Class_printable
  | Char_Class_mark
  | Char_Class_number
  | Char_Class_punctuation
  | Char_Class_symbol
  | Char_Class_separator
  | Char_Class_letter
  | Char_Class_is
  | Text_patterns_char

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
  readForeign (i : args) stk = (args,) <$> peekOffI stk i
  readForeign [] _ = foreignCCError "Int"
  writeForeign stk i = do
    stk <- bump stk
    stk <$ pokeI stk i

instance ForeignConvention Word64 where
  readForeign (i : args) stk = (args,) <$> peekOffN stk i
  readForeign [] _ = foreignCCError "Word64"
  writeForeign stk n = do
    stk <- bump stk
    stk <$ pokeN stk n

-- We don't have a clear mapping from these types to Unison types, most are just mapped to Nats.

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
  readForeign (i : args) stk = (args,) <$> peekOffC stk i
  readForeign [] _ = foreignCCError "Char"
  writeForeign stk ch = do
    stk <- bump stk
    stk <$ pokeC stk ch

instance ForeignConvention Val where
  readForeign (i : args) stk = (args,) <$> peekOff stk i
  readForeign [] _ = foreignCCError "Val"
  writeForeign stk v = do
    stk <- bump stk
    stk <$ (poke stk =<< evaluate v)

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
    stk <$ pokeTag stk 0
  writeForeign stk (Just x) = do
    stk <- writeForeign stk x
    stk <- bump stk
    stk <$ pokeTag stk 1

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

  writeForeign stk (Left a) = do
    stk <- writeForeign stk a
    stk <- bump stk
    stk <$ pokeTag stk 0
  writeForeign stk (Right b) = do
    stk <- writeForeign stk b
    stk <- bump stk
    stk <$ pokeTag stk 1

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
  readForeign (i : args) stk = do
    b <- peekOffBool stk i
    pure (args, b)
  readForeign _ _ = foreignCCError "Bool"
  writeForeign stk b = do
    stk <- bump stk
    pokeBool stk b
    pure stk

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

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
instance {-# OVERLAPPING #-} ForeignConvention [Val] where
  readForeign (i : args) stk =
    (args,) . toList <$> peekOffS stk i
  readForeign _ _ = foreignCCError "[Val]"
  writeForeign stk l = do
    stk <- bump stk
    stk <$ pokeS stk (Sq.fromList l)

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
instance {-# OVERLAPPING #-} ForeignConvention [Closure] where
  readForeign (i : args) stk =
    (args,) . fmap getBoxedVal . toList <$> peekOffS stk i
  readForeign _ _ = foreignCCError "[Closure]"
  writeForeign stk l = do
    stk <- bump stk
    stk <$ pokeS stk (Sq.fromList . fmap BoxedVal $ l)

instance ForeignConvention [Foreign] where
  readForeign = readForeignAs (fmap marshalToForeign)
  writeForeign = writeForeignAs (fmap Foreign)

instance ForeignConvention (MVar Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap mvarRef)

instance ForeignConvention (TVar Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap tvarRef)

instance ForeignConvention (IORef Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap refRef)

instance ForeignConvention (Ticket Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap ticketRef)

instance ForeignConvention (Promise Val) where
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

instance ForeignConvention (PA.MutableArray s Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap marrayRef)

instance ForeignConvention (PA.MutableByteArray s) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap mbytearrayRef)

instance ForeignConvention (PA.Array Val) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap iarrayRef)

instance ForeignConvention PA.ByteArray where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap ibytearrayRef)

instance {-# OVERLAPPABLE #-} (BuiltinForeign b) => ForeignConvention b where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

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
  writeForeign stk l = do
    stk <- bump stk
    stk <$ pokeS stk (boxedVal . Foreign . wrapBuiltin <$> Sq.fromList l)

foreignCCError :: String -> IO a
foreignCCError nm =
  die $ "mismatched foreign calling convention for `" ++ nm ++ "`"
