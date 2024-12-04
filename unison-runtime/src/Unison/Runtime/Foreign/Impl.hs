{-# OPTIONS_GHC -Wno-unused-imports #-}

module Unison.Runtime.Foreign.Impl (foreignCall) where

import Control.Concurrent (ThreadId)
import Control.Concurrent as SYS
  ( killThread,
    threadDelay,
  )
import Control.Concurrent.MVar as SYS
import Control.Concurrent.STM qualified as STM
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Exception (evaluate)
import Control.Exception.Safe qualified as Exception
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Primitive qualified as PA
import Control.Monad.Reader (ReaderT (..), ask, runReaderT)
import Control.Monad.State.Strict (State, execState, modify)
import Crypto.Error (CryptoError (..), CryptoFailable (..))
import Crypto.Hash qualified as Hash
import Crypto.MAC.HMAC qualified as HMAC
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Crypto.PubKey.RSA.PKCS15 qualified as RSA
import Crypto.Random (getRandomBytes)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteArray qualified as BA
import Data.ByteString (hGet, hGetSome, hPut)
import Data.ByteString.Lazy qualified as L
import Data.Default (def)
import Data.Digest.Murmur64 (asWord64, hash64)
import Data.IP (IP)
import Data.Map qualified as Map
import Data.PEM (PEM, pemContent, pemParseLBS)
import Data.Set (insert)
import Data.Set qualified as Set
import Data.Text qualified
import Data.Text.IO qualified as Text.IO
import Data.Time.Clock.POSIX as SYS
  ( getPOSIXTime,
    posixSecondsToUTCTime,
    utcTimeToPOSIXSeconds,
  )
import Data.Time.LocalTime (TimeZone (..), getTimeZone)
import Data.X509 qualified as X
import Data.X509.CertificateStore qualified as X
import Data.X509.Memory qualified as X
import GHC.Conc qualified as STM
import GHC.IO (IO (IO))
import Network.Simple.TCP as SYS
  ( HostPreference (..),
    bindSock,
    closeSock,
    connectSock,
    listenSock,
    recv,
    send,
  )
import Network.Socket as SYS
  ( PortNumber,
    Socket,
    accept,
    socketPort,
  )
import Network.TLS as TLS
import Network.TLS.Extra.Cipher as Cipher
import Network.UDP as UDP
  ( ClientSockAddr,
    ListenSocket,
    UDPSocket (..),
    clientSocket,
    close,
    recv,
    recvFrom,
    send,
    sendTo,
    serverSocket,
    stop,
  )
import System.Clock (Clock (..), getTime, nsec, sec)
import System.Directory as SYS
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesPathExist,
    getCurrentDirectory,
    getDirectoryContents,
    getFileSize,
    getModificationTime,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
    renameDirectory,
    renameFile,
    setCurrentDirectory,
  )
import System.Environment as SYS
  ( getArgs,
    getEnv,
  )
import System.Exit as SYS (ExitCode (..))
import System.FilePath (isPathSeparator)
import System.IO (Handle)
import System.IO as SYS
  ( IOMode (..),
    hClose,
    hGetBuffering,
    hGetChar,
    hGetEcho,
    hIsEOF,
    hIsOpen,
    hIsSeekable,
    hReady,
    hSeek,
    hSetBuffering,
    hSetEcho,
    hTell,
    openFile,
    stderr,
    stdin,
    stdout,
  )
import System.IO.Temp (createTempDirectory)
import System.Process as SYS
  ( getProcessExitCode,
    proc,
    runInteractiveProcess,
    terminateProcess,
    waitForProcess,
    withCreateProcess,
  )
import System.X509 qualified as X
import Unison.ABT.Normalized hiding (TTm)
import Unison.Builtin.Decls qualified as Ty
import Unison.Prelude hiding (Text, some)
import Unison.Reference
import Unison.Referent (Referent, pattern Ref)
import Unison.Runtime.ANF as ANF
import Unison.Runtime.ANF.Rehash (checkGroupHashes)
import Unison.Runtime.ANF.Serialize as ANF
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Builtin
import Unison.Runtime.Builtin.Types
import Unison.Runtime.Crypto.Rsa as Rsa
import Unison.Runtime.Exception (die)
import Unison.Runtime.Foreign
import Unison.Runtime.Foreign
  ( Foreign (Wrap),
    HashAlgorithm (..),
    pattern Failure,
  )
import Unison.Runtime.Foreign qualified as F
import Unison.Runtime.Foreign.Function hiding (mkForeign)
import Unison.Runtime.MCode
import Unison.Runtime.Stack
import Unison.Runtime.Stack (UnboxedTypeTag (..), Val (..), emptyVal, unboxedTypeTagToInt)
import Unison.Runtime.Stack qualified as Closure
import Unison.Symbol
import Unison.Type qualified as Ty
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.EnumContainers as EC
import Unison.Util.RefPromise
  ( Promise,
    newPromise,
    readPromise,
    tryReadPromise,
    writePromise,
  )
import Unison.Util.Text (Text)
import Unison.Util.Text qualified as Util.Text
import Unison.Util.Text.Pattern qualified as TPat
import Unison.Var
import UnliftIO qualified

foreignCall :: ForeignFunc' -> Args -> Stack -> IO Stack
foreignCall = \case
  IO_UDP_clientSocket_impl_v1 -> mkForeignIOF $ \(host :: Util.Text.Text, port :: Util.Text.Text) ->
    let hostStr = Util.Text.toString host
        portStr = Util.Text.toString port
     in UDP.clientSocket hostStr portStr True
  IO_UDP_UDPSocket_recv_impl_v1 -> mkForeignIOF $ \(sock :: UDPSocket) -> Bytes.fromArray <$> UDP.recv sock
  IO_UDP_UDPSocket_send_impl_v1 -> mkForeignIOF $
    \(sock :: UDPSocket, bytes :: Bytes.Bytes) ->
      UDP.send sock (Bytes.toArray bytes)
  IO_UDP_UDPSocket_close_impl_v1 -> mkForeignIOF $
    \(sock :: UDPSocket) -> UDP.close sock
  IO_UDP_ListenSocket_close_impl_v1 -> mkForeignIOF $
    \(sock :: ListenSocket) -> UDP.stop sock
  IO_UDP_UDPSocket_toText_impl_v1 -> mkForeign $
    \(sock :: UDPSocket) -> pure $ show sock
  IO_UDP_serverSocket_impl_v1 -> mkForeignIOF $
    \(ip :: Util.Text.Text, port :: Util.Text.Text) ->
      let maybeIp = readMaybe $ Util.Text.toString ip :: Maybe IP
          maybePort = readMaybe $ Util.Text.toString port :: Maybe PortNumber
       in case (maybeIp, maybePort) of
            (Nothing, _) -> fail "Invalid IP Address"
            (_, Nothing) -> fail "Invalid Port Number"
            (Just ip, Just pt) -> UDP.serverSocket (ip, pt)
  IO_UDP_ListenSocket_toText_impl_v1 -> mkForeign $
    \(sock :: ListenSocket) -> pure $ show sock
  IO_UDP_ListenSocket_recvFrom_impl_v1 ->
    mkForeignIOF $
      fmap (first Bytes.fromArray) <$> UDP.recvFrom
  IO_UDP_ClientSockAddr_toText_v1 -> mkForeign $
    \(sock :: ClientSockAddr) -> pure $ show sock
  IO_UDP_ListenSocket_sendTo_impl_v1 -> mkForeignIOF $
    \(socket :: ListenSocket, bytes :: Bytes.Bytes, addr :: ClientSockAddr) ->
      UDP.sendTo socket (Bytes.toArray bytes) addr
  IO_openFile_impl_v3 -> mkForeignIOF $ \(fnameText :: Util.Text.Text, n :: Int) ->
    let fname = Util.Text.toString fnameText
        mode = case n of
          0 -> ReadMode
          1 -> WriteMode
          2 -> AppendMode
          _ -> ReadWriteMode
     in openFile fname mode
  IO_closeFile_impl_v3 -> mkForeignIOF hClose
  IO_isFileEOF_impl_v3 -> mkForeignIOF hIsEOF
  IO_isFileOpen_impl_v3 -> mkForeignIOF hIsOpen
  IO_getEcho_impl_v1 -> mkForeignIOF hGetEcho
  IO_ready_impl_v1 -> mkForeignIOF hReady
  IO_getChar_impl_v1 -> mkForeignIOF hGetChar
  IO_isSeekable_impl_v3 -> mkForeignIOF hIsSeekable
  IO_seekHandle_impl_v3 -> mkForeignIOF $
    \(h, sm, n) -> hSeek h sm (fromIntegral (n :: Int))
  IO_handlePosition_impl_v3 ->
    -- TODO: truncating integer
    mkForeignIOF $
      \h -> fromInteger @Word64 <$> hTell h
  IO_getBuffering_impl_v3 -> mkForeignIOF hGetBuffering
  IO_setBuffering_impl_v3 ->
    mkForeignIOF $
      uncurry hSetBuffering
  IO_setEcho_impl_v1 -> mkForeignIOF $ uncurry hSetEcho
  IO_getLine_impl_v1 ->
    mkForeignIOF $
      fmap Util.Text.fromText . Text.IO.hGetLine
  IO_getBytes_impl_v3 -> mkForeignIOF $
    \(h, n) -> Bytes.fromArray <$> hGet h n
  IO_getSomeBytes_impl_v1 -> mkForeignIOF $
    \(h, n) -> Bytes.fromArray <$> hGetSome h n
  IO_putBytes_impl_v3 -> mkForeignIOF $ \(h, bs) -> hPut h (Bytes.toArray bs)
  IO_systemTime_impl_v3 -> mkForeignIOF $
    \() -> getPOSIXTime
  IO_systemTimeMicroseconds_v1 -> mkForeign $
    \() -> fmap (1e6 *) getPOSIXTime
  Clock_internals_monotonic_v1 -> mkForeignIOF $
    \() -> getTime Monotonic
  Clock_internals_realtime_v1 -> mkForeignIOF $
    \() -> getTime Realtime
  Clock_internals_processCPUTime_v1 -> mkForeignIOF $
    \() -> getTime ProcessCPUTime
  Clock_internals_threadCPUTime_v1 -> mkForeignIOF $
    \() -> getTime ThreadCPUTime
  Clock_internals_sec_v1 -> mkForeign (\n -> pure (fromIntegral $ sec n :: Word64))
  Clock_internals_nsec_v1 -> mkForeign (\n -> pure (fromIntegral $ nsec n :: Word64))
  Clock_internals_systemTimeZone_v1 ->
    mkForeign
      ( \secs -> do
          TimeZone offset summer name <- getTimeZone (posixSecondsToUTCTime (fromIntegral (secs :: Int)))
          pure (offset :: Int, summer, name)
      )
  IO_getTempDirectory_impl_v3 ->
    mkForeignIOF $
      \() -> chop <$> getTemporaryDirectory
  IO_createTempDirectory_impl_v3 -> mkForeignIOF $ \prefix -> do
    temp <- getTemporaryDirectory
    chop <$> createTempDirectory temp prefix
  IO_getCurrentDirectory_impl_v3 -> mkForeignIOF $
    \() -> getCurrentDirectory
  IO_setCurrentDirectory_impl_v3 -> mkForeignIOF setCurrentDirectory
  IO_fileExists_impl_v3 -> mkForeignIOF doesPathExist
  IO_getEnv_impl_v1 -> mkForeignIOF getEnv
  IO_getArgs_impl_v1 -> mkForeignIOF $
    \() -> fmap Util.Text.pack <$> SYS.getArgs
  IO_isDirectory_impl_v3 -> mkForeignIOF doesDirectoryExist
  IO_createDirectory_impl_v3 ->
    mkForeignIOF $
      createDirectoryIfMissing True
  IO_removeDirectory_impl_v3 -> mkForeignIOF removeDirectoryRecursive
  IO_renameDirectory_impl_v3 ->
    mkForeignIOF $
      uncurry renameDirectory
  IO_directoryContents_impl_v3 ->
    mkForeignIOF $
      (fmap Util.Text.pack <$>) . getDirectoryContents
  IO_removeFile_impl_v3 -> mkForeignIOF removeFile
  IO_renameFile_impl_v3 ->
    mkForeignIOF $
      uncurry renameFile
  IO_getFileTimestamp_impl_v3 ->
    mkForeignIOF $
      fmap utcTimeToPOSIXSeconds . getModificationTime
  IO_getFileSize_impl_v3 ->
    -- TODO: truncating integer
    mkForeignIOF $
      \fp -> fromInteger @Word64 <$> getFileSize fp
  IO_serverSocket_impl_v3 ->
    mkForeignIOF $
      \( mhst :: Maybe Util.Text.Text,
         port
         ) ->
          fst <$> SYS.bindSock (hostPreference mhst) port
  Socket_toText -> mkForeign $
    \(sock :: Socket) -> pure $ show sock
  Handle_toText -> mkForeign $
    \(hand :: Handle) -> pure $ show hand
  ThreadId_toText -> mkForeign $
    \(threadId :: ThreadId) -> pure $ show threadId
  IO_socketPort_impl_v3 -> mkForeignIOF $
    \(handle :: Socket) -> do
      n <- SYS.socketPort handle
      return (fromIntegral n :: Word64)
  IO_listen_impl_v3 -> mkForeignIOF $
    \sk -> SYS.listenSock sk 2048
  IO_clientSocket_impl_v3 ->
    mkForeignIOF $
      fmap fst . uncurry SYS.connectSock
  IO_closeSocket_impl_v3 -> mkForeignIOF SYS.closeSock
  IO_socketAccept_impl_v3 ->
    mkForeignIOF $
      fmap fst . SYS.accept
  IO_socketSend_impl_v3 -> mkForeignIOF $
    \(sk, bs) -> SYS.send sk (Bytes.toArray bs)
  IO_socketReceive_impl_v3 -> mkForeignIOF $
    \(hs, n) ->
      maybe mempty Bytes.fromArray <$> SYS.recv hs n
  IO_kill_impl_v3 -> mkForeignIOF killThread
  IO_delay_impl_v3 -> mkForeignIOF customDelay
  IO_stdHandle -> mkForeign $
    \(n :: Int) -> case n of
      0 -> pure SYS.stdin
      1 -> pure SYS.stdout
      2 -> pure SYS.stderr
      _ -> die "IO.stdHandle: invalid input."
  IO_process_call -> mkForeign $
    \(exe, map Util.Text.unpack -> args) ->
      withCreateProcess (proc exe args) $ \_ _ _ p ->
        exitDecode <$> waitForProcess p
  IO_process_start -> mkForeign $ \(exe, map Util.Text.unpack -> args) ->
    runInteractiveProcess exe args Nothing Nothing
  IO_process_kill -> mkForeign $ terminateProcess
  IO_process_wait -> mkForeign $
    \ph -> exitDecode <$> waitForProcess ph
  IO_process_exitCode ->
    mkForeign $
      fmap (fmap exitDecode) . getProcessExitCode
  MVar_new -> mkForeign $
    \(c :: Val) -> newMVar c
  MVar_newEmpty_v2 -> mkForeign $
    \() -> newEmptyMVar @Val
  MVar_take_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val) -> takeMVar mv
  MVar_tryTake -> mkForeign $
    \(mv :: MVar Val) -> tryTakeMVar mv
  MVar_put_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val, x) -> putMVar mv x
  MVar_tryPut_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val, x) -> tryPutMVar mv x
  MVar_swap_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val, x) -> swapMVar mv x
  MVar_isEmpty -> mkForeign $
    \(mv :: MVar Val) -> isEmptyMVar mv
  MVar_read_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val) -> readMVar mv
  MVar_tryRead_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val) -> tryReadMVar mv
  Char_toText -> mkForeign $
    \(ch :: Char) -> pure (Util.Text.singleton ch)
  Text_repeat -> mkForeign $
    \(n :: Word64, txt :: Util.Text.Text) -> pure (Util.Text.replicate (fromIntegral n) txt)
  Text_reverse ->
    mkForeign $
      pure . Util.Text.reverse
  Text_toUppercase ->
    mkForeign $
      pure . Util.Text.toUppercase
  Text_toLowercase ->
    mkForeign $
      pure . Util.Text.toLowercase
  Text_toUtf8 ->
    mkForeign $
      pure . Util.Text.toUtf8
  Text_fromUtf8_impl_v3 ->
    mkForeign $
      pure . mapLeft (\t -> Failure Ty.ioFailureRef (Util.Text.pack t) unitValue) . Util.Text.fromUtf8
  Tls_ClientConfig_default -> mkForeign $
    \(hostName :: Util.Text.Text, serverId :: Bytes.Bytes) ->
      fmap
        ( \store ->
            (defaultParamsClient (Util.Text.unpack hostName) (Bytes.toArray serverId))
              { TLS.clientSupported = def {TLS.supportedCiphers = Cipher.ciphersuite_strong},
                TLS.clientShared = def {TLS.sharedCAStore = store}
              }
        )
        X.getSystemCertificateStore
  Tls_ServerConfig_default ->
    mkForeign $
      \(certs :: [X.SignedCertificate], key :: X.PrivKey) ->
        pure $
          (def :: TLS.ServerParams)
            { TLS.serverSupported = def {TLS.supportedCiphers = Cipher.ciphersuite_strong},
              TLS.serverShared = def {TLS.sharedCredentials = Credentials [(X.CertificateChain certs, key)]}
            }
  Tls_ClientConfig_certificates_set ->
    let updateClient :: X.CertificateStore -> TLS.ClientParams -> TLS.ClientParams
        updateClient certs client = client {TLS.clientShared = ((clientShared client) {TLS.sharedCAStore = certs})}
     in mkForeign $
          \(certs :: [X.SignedCertificate], params :: ClientParams) -> pure $ updateClient (X.makeCertificateStore certs) params
  Tls_ServerConfig_certificates_set ->
    let updateServer :: X.CertificateStore -> TLS.ServerParams -> TLS.ServerParams
        updateServer certs client = client {TLS.serverShared = ((serverShared client) {TLS.sharedCAStore = certs})}
     in mkForeign $
          \(certs :: [X.SignedCertificate], params :: ServerParams) -> pure $ updateServer (X.makeCertificateStore certs) params
  TVar_new -> mkForeign $
    \(c :: Val) -> unsafeSTMToIO $ STM.newTVar c
  TVar_read -> mkForeign $
    \(v :: STM.TVar Val) -> unsafeSTMToIO $ STM.readTVar v
  TVar_write -> mkForeign $
    \(v :: STM.TVar Val, c :: Val) ->
      unsafeSTMToIO $ STM.writeTVar v c
  TVar_newIO -> mkForeign $
    \(c :: Val) -> STM.newTVarIO c
  TVar_readIO -> mkForeign $
    \(v :: STM.TVar Val) -> STM.readTVarIO v
  TVar_swap -> mkForeign $
    \(v, c :: Val) -> unsafeSTMToIO $ STM.swapTVar v c
  STM_retry -> mkForeign $
    \() -> unsafeSTMToIO STM.retry :: IO Val
  Promise_new -> mkForeign $
    \() -> newPromise @Val
  Promise_read -> mkForeign $
    \(p :: Promise Val) -> readPromise p
  Promise_tryRead -> mkForeign $
    \(p :: Promise Val) -> tryReadPromise p
  Promise_write -> mkForeign $
    \(p :: Promise Val, a :: Val) -> writePromise p a
  Tls_newClient_impl_v3 ->
    mkForeignTls $
      \( config :: TLS.ClientParams,
         socket :: SYS.Socket
         ) -> TLS.contextNew socket config
  Tls_newServer_impl_v3 ->
    mkForeignTls $
      \( config :: TLS.ServerParams,
         socket :: SYS.Socket
         ) -> TLS.contextNew socket config
  Tls_handshake_impl_v3 -> mkForeignTls $
    \(tls :: TLS.Context) -> TLS.handshake tls
  Tls_send_impl_v3 ->
    mkForeignTls $
      \( tls :: TLS.Context,
         bytes :: Bytes.Bytes
         ) -> TLS.sendData tls (Bytes.toLazyByteString bytes)
  Tls_decodeCert_impl_v3 -> undefined
  Tls_encodeCert -> undefined
  Tls_decodePrivateKey -> undefined
  Tls_encodePrivateKey -> undefined
  Tls_receive_impl_v3 -> undefined
  Tls_terminate_impl_v3 -> undefined
  Code_validateLinks -> undefined
  Code_dependencies -> undefined
  Code_serialize -> undefined
  Code_deserialize -> undefined
  Code_display -> undefined
  Value_dependencies -> undefined
  Value_serialize -> undefined
  Value_deserialize -> undefined
  Crypto_HashAlgorithm_Sha3_512 -> undefined
  Crypto_HashAlgorithm_Sha3_256 -> undefined
  Crypto_HashAlgorithm_Sha2_512 -> undefined
  Crypto_HashAlgorithm_Sha2_256 -> undefined
  Crypto_HashAlgorithm_Sha1 -> undefined
  Crypto_HashAlgorithm_Blake2b_512 -> undefined
  Crypto_HashAlgorithm_Blake2b_256 -> undefined
  Crypto_HashAlgorithm_Blake2s_256 -> undefined
  Crypto_HashAlgorithm_Md5 -> undefined
  Crypto_hashBytes -> undefined
  Crypto_hmacBytes -> undefined
  Crypto_hash -> undefined
  Crypto_hmac -> undefined
  Crypto_Ed25519_sign_impl -> undefined
  Crypto_Ed25519_verify_impl -> undefined
  Crypto_Rsa_sign_impl -> undefined
  Crypto_Rsa_verify_impl -> undefined
  Universal_murmurHash -> undefined
  IO_randomBytes -> undefined
  Bytes_zlib_compress -> undefined
  Bytes_gzip_compress -> undefined
  Bytes_zlib_decompress -> undefined
  Bytes_gzip_decompress -> undefined
  Bytes_toBase16 -> undefined
  Bytes_toBase32 -> undefined
  Bytes_toBase64 -> undefined
  Bytes_toBase64UrlUnpadded -> undefined
  Bytes_fromBase16 -> undefined
  Bytes_fromBase32 -> undefined
  Bytes_fromBase64 -> undefined
  Bytes_fromBase64UrlUnpadded -> undefined
  Bytes_decodeNat64be -> undefined
  Bytes_decodeNat64le -> undefined
  Bytes_decodeNat32be -> undefined
  Bytes_decodeNat32le -> undefined
  Bytes_decodeNat16be -> undefined
  Bytes_decodeNat16le -> undefined
  Bytes_encodeNat64be -> undefined
  Bytes_encodeNat64le -> undefined
  Bytes_encodeNat32be -> undefined
  Bytes_encodeNat32le -> undefined
  Bytes_encodeNat16be -> undefined
  Bytes_encodeNat16le -> undefined
  MutableArray_copyTo_force -> undefined
  MutableByteArray_copyTo_force -> undefined
  ImmutableArray_copyTo_force -> undefined
  ImmutableArray_size -> undefined
  MutableArray_size -> undefined
  ImmutableByteArray_size -> undefined
  MutableByteArray_size -> undefined
  ImmutableByteArray_copyTo_force -> undefined
  MutableArray_read -> undefined
  MutableByteArray_read8 -> undefined
  MutableByteArray_read16be -> undefined
  MutableByteArray_read24be -> undefined
  MutableByteArray_read32be -> undefined
  MutableByteArray_read40be -> undefined
  MutableByteArray_read64be -> undefined
  MutableArray_write -> undefined
  MutableByteArray_write8 -> undefined
  MutableByteArray_write16be -> undefined
  MutableByteArray_write32be -> undefined
  MutableByteArray_write64be -> undefined
  ImmutableArray_read -> undefined
  ImmutableByteArray_read8 -> undefined
  ImmutableByteArray_read16be -> undefined
  ImmutableByteArray_read24be -> undefined
  ImmutableByteArray_read32be -> undefined
  ImmutableByteArray_read40be -> undefined
  ImmutableByteArray_read64be -> undefined
  MutableByteArray_freeze_force -> undefined
  MutableArray_freeze_force -> undefined
  MutableByteArray_freeze -> undefined
  MutableArray_freeze -> undefined
  MutableByteArray_length -> undefined
  ImmutableByteArray_length -> undefined
  IO_array -> undefined
  IO_arrayOf -> undefined
  IO_bytearray -> undefined
  IO_bytearrayOf -> undefined
  Scope_array -> undefined
  Scope_arrayOf -> undefined
  Scope_bytearray -> undefined
  Scope_bytearrayOf -> undefined
  Text_patterns_literal -> undefined
  Text_patterns_digit -> undefined
  Text_patterns_letter -> undefined
  Text_patterns_space -> undefined
  Text_patterns_punctuation -> undefined
  Text_patterns_anyChar -> undefined
  Text_patterns_eof -> undefined
  Text_patterns_charRange -> undefined
  Text_patterns_notCharRange -> undefined
  Text_patterns_charIn -> undefined
  Text_patterns_notCharIn -> undefined
  Pattern_many -> undefined
  Pattern_many_corrected -> undefined
  Pattern_capture -> undefined
  Pattern_captureAs -> undefined
  Pattern_join -> undefined
  Pattern_or -> undefined
  Pattern_replicate -> undefined
  Pattern_run -> undefined
  Pattern_isMatch -> undefined
  Char_Class_any -> undefined
  Char_Class_not -> undefined
  Char_Class_and -> undefined
  Char_Class_or -> undefined
  Char_Class_range -> undefined
  Char_Class_anyOf -> undefined
  Char_Class_alphanumeric -> undefined
  Char_Class_upper -> undefined
  Char_Class_lower -> undefined
  Char_Class_whitespace -> undefined
  Char_Class_control -> undefined
  Char_Class_printable -> undefined
  Char_Class_mark -> undefined
  Char_Class_number -> undefined
  Char_Class_punctuation -> undefined
  Char_Class_symbol -> undefined
  Char_Class_separator -> undefined
  Char_Class_letter -> undefined
  Char_Class_is -> undefined
  Text_patterns_char -> undefined
  where
    chop = reverse . dropWhile isPathSeparator . reverse

    hostPreference :: Maybe Util.Text.Text -> SYS.HostPreference
    hostPreference Nothing = SYS.HostAny
    hostPreference (Just host) = SYS.Host $ Util.Text.unpack host

    mx :: Word64
    mx = fromIntegral (maxBound :: Int)

    customDelay :: Word64 -> IO ()
    customDelay n
      | n < mx = threadDelay (fromIntegral n)
      | otherwise = threadDelay maxBound >> customDelay (n - mx)

    exitDecode ExitSuccess = 0
    exitDecode (ExitFailure n) = n
{-# INLINE foreignCall #-}

mkForeign :: (ForeignConvention a, ForeignConvention b) => (a -> IO b) -> Args -> Stack -> IO Stack
mkForeign f args stk = do
  args <- decodeArgs args stk
  res <- f args
  writeForeign stk res
  where
    decodeArgs :: (ForeignConvention x) => Args -> Stack -> IO x
    decodeArgs args stk =
      readForeign (argsToLists args) stk >>= \case
        ([], a) -> pure a
        _ ->
          error
            "mkForeign: too many arguments for foreign function"
{-# INLINE mkForeign #-}

mkForeignIOF ::
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO r) ->
  Args ->
  Stack ->
  IO Stack
mkForeignIOF f = mkForeign $ \a -> tryIOE (f a)
  where
    tryIOE :: IO a -> IO (Either (F.Failure Val) a)
    tryIOE = fmap handleIOE . UnliftIO.try
    handleIOE :: Either IOException a -> Either (F.Failure Val) a
    handleIOE (Left e) = Left $ Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue
    handleIOE (Right a) = Right a
{-# INLINE mkForeignIOF #-}

mkForeignTls ::
  forall a r.
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO r) ->
  Args ->
  Stack ->
  IO Stack
mkForeignTls f = mkForeign $ \a -> fmap flatten (tryIO2 (tryIO1 (f a)))
  where
    tryIO1 :: IO r -> IO (Either TLS.TLSException r)
    tryIO1 = UnliftIO.try
    tryIO2 :: IO (Either TLS.TLSException r) -> IO (Either IOException (Either TLS.TLSException r))
    tryIO2 = UnliftIO.try
    flatten :: Either IOException (Either TLS.TLSException r) -> Either ((F.Failure Val)) r
    flatten (Left e) = Left (Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Left e)) = Left (Failure Ty.tlsFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Right a)) = Right a

mkForeignTlsE ::
  forall a r.
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO (Either (F.Failure Val) r)) ->
  Args ->
  Stack ->
  IO Stack
mkForeignTlsE f = mkForeign $ \a -> fmap flatten (tryIO2 (tryIO1 (f a)))
  where
    tryIO1 :: IO (Either (F.Failure Val) r) -> IO (Either TLS.TLSException (Either (F.Failure Val) r))
    tryIO1 = UnliftIO.try
    tryIO2 :: IO (Either TLS.TLSException (Either (F.Failure Val) r)) -> IO (Either IOException (Either TLS.TLSException (Either (F.Failure Val) r)))
    tryIO2 = UnliftIO.try
    flatten :: Either IOException (Either TLS.TLSException (Either (F.Failure Val) r)) -> Either (F.Failure Val) r
    flatten (Left e) = Left (Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Left e)) = Left (Failure Ty.tlsFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Right (Left e))) = Left e
    flatten (Right (Right (Right a))) = Right a

unsafeSTMToIO :: STM.STM a -> IO a
unsafeSTMToIO (STM.STM m) = IO m
