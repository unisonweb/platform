{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Runtime.Foreign.Impl (foreignCall) where

import Control.Concurrent (ThreadId)
import Control.Concurrent as SYS
  ( killThread,
    threadDelay,
  )
import Control.Concurrent.MVar as SYS
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Exception.Safe qualified as Exception
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Primitive qualified as PA
import Crypto.Error (CryptoError (..), CryptoFailable (..))
import Crypto.Hash qualified as Hash
import Crypto.MAC.HMAC qualified as HMAC
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Crypto.PubKey.RSA.PKCS15 qualified as RSA
import Crypto.Random (getRandomBytes)
import Data.Atomics (Ticket)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteArray qualified as BA
import Data.ByteString (hGet, hGetSome, hPut)
import Data.ByteString.Lazy qualified as L
import Data.Default (def)
import Data.Digest.Murmur64 (asWord64, hash64)
import Data.IORef (IORef)
import Data.IP (IP)
import Data.PEM (PEM, pemContent, pemParseLBS)
import Data.Sequence qualified as Sq
import Data.Text qualified
import Data.Text.IO qualified as Text.IO
import Data.Time.Clock.POSIX (POSIXTime)
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
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Simple.TCP as SYS
  ( HostPreference (..),
    bindSock,
    closeSock,
    connectSock,
    listenSock,
    recv,
    send,
  )
import Network.Socket (Socket)
import Network.Socket as SYS
  ( PortNumber,
    Socket,
    accept,
    socketPort,
  )
import Network.TLS as TLS
import Network.TLS.Extra.Cipher as Cipher
import Network.UDP (UDPSocket)
import Network.UDP as UDP
  ( ClientSockAddr,
    ListenSocket,
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
import System.IO (BufferMode (..), Handle, IOMode, SeekMode)
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
import Unison.Builtin.Decls qualified as Ty
import Unison.Prelude hiding (Text, some)
import Unison.Reference
import Unison.Referent (Referent, pattern Ref)
import Unison.Runtime.ANF (Code, PackedTag (..), Value, internalBug)
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.ANF.Rehash (checkGroupHashes)
import Unison.Runtime.ANF.Serialize qualified as ANF
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Builtin
import Unison.Runtime.Crypto.Rsa qualified as Rsa
import Unison.Runtime.Exception
import Unison.Runtime.Foreign hiding (Failure)
import Unison.Runtime.Foreign qualified as F
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (..))
import Unison.Runtime.MCode
import Unison.Runtime.Stack
import Unison.Symbol
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
import Unison.Type qualified as Ty
import Unison.Util.Bytes (Bytes)
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.RefPromise
  ( Promise,
    newPromise,
    readPromise,
    tryReadPromise,
    writePromise,
  )
import Unison.Util.Text (Text, pack, unpack)
import Unison.Util.Text qualified as Util.Text
import Unison.Util.Text.Pattern qualified as TPat
import UnliftIO qualified

foreignCall :: ForeignFunc -> Args -> XStack -> IOXStack
foreignCall !ff !args !xstk =
  stackIOToIOX $ foreignCallHelper ff args (packXStack xstk)
{-# INLINE foreignCall #-}

foreignCallHelper :: ForeignFunc -> Args -> Stack -> IO Stack
foreignCallHelper = \case
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
      pure . mapLeft (\t -> F.Failure Ty.ioFailureRef (Util.Text.pack t) unitValue) . Util.Text.fromUtf8
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
  Tls_decodeCert_impl_v3 ->
    let wrapFailure t = F.Failure Ty.tlsFailureRef (Util.Text.pack t) unitValue
        decoded :: Bytes.Bytes -> Either String PEM
        decoded bytes = case pemParseLBS $ Bytes.toLazyByteString bytes of
          Right (pem : _) -> Right pem
          Right [] -> Left "no PEM found"
          Left l -> Left l
        asCert :: PEM -> Either String X.SignedCertificate
        asCert pem = X.decodeSignedCertificate $ pemContent pem
     in mkForeignTlsE $
          \(bytes :: Bytes.Bytes) -> pure $ mapLeft wrapFailure $ (decoded >=> asCert) bytes
  Tls_encodeCert -> mkForeign $
    \(cert :: X.SignedCertificate) -> pure $ Bytes.fromArray $ X.encodeSignedObject cert
  Tls_decodePrivateKey -> mkForeign $
    \(bytes :: Bytes.Bytes) -> pure $ X.readKeyFileFromMemory $ L.toStrict $ Bytes.toLazyByteString bytes
  Tls_encodePrivateKey -> mkForeign $
    \(privateKey :: X.PrivKey) -> pure $ Util.Text.toUtf8 $ Util.Text.pack $ show privateKey
  Tls_receive_impl_v3 -> mkForeignTls $
    \(tls :: TLS.Context) -> do
      bs <- TLS.recvData tls
      pure $ Bytes.fromArray bs
  Tls_terminate_impl_v3 -> mkForeignTls $
    \(tls :: TLS.Context) -> TLS.bye tls
  Code_validateLinks -> mkForeign $
    \(lsgs0 :: [(Referent, ANF.Code)]) -> do
      let f (msg, rs) =
            F.Failure Ty.miscFailureRef (Util.Text.fromText msg) rs
      pure . first f $ checkGroupHashes lsgs0
  Code_dependencies -> mkForeign $
    \(ANF.CodeRep sg _) ->
      pure $ Wrap Ty.termLinkRef . Ref <$> ANF.groupTermLinks sg
  Code_serialize -> mkForeign $
    \(co :: ANF.Code) ->
      pure . Bytes.fromArray $ ANF.serializeCode builtinForeignNames co
  Code_deserialize ->
    mkForeign $
      pure . ANF.deserializeCode . Bytes.toArray
  Code_display -> mkForeign $
    \(nm, (ANF.CodeRep sg _)) ->
      pure $ ANF.prettyGroup @Symbol (Util.Text.unpack nm) sg ""
  Value_dependencies ->
    mkForeign $
      pure . fmap (Wrap Ty.termLinkRef . Ref) . ANF.valueTermLinks
  Value_serialize ->
    mkForeign $
      pure . Bytes.fromArray . ANF.serializeValue
  Value_deserialize ->
    mkForeign $
      pure . ANF.deserializeValue . Bytes.toArray
  Crypto_HashAlgorithm_Sha3_512 -> mkHashAlgorithm "Sha3_512" Hash.SHA3_512
  Crypto_HashAlgorithm_Sha3_256 -> mkHashAlgorithm "Sha3_256" Hash.SHA3_256
  Crypto_HashAlgorithm_Sha2_512 -> mkHashAlgorithm "Sha2_512" Hash.SHA512
  Crypto_HashAlgorithm_Sha2_256 -> mkHashAlgorithm "Sha2_256" Hash.SHA256
  Crypto_HashAlgorithm_Sha1 -> mkHashAlgorithm "Sha1" Hash.SHA1
  Crypto_HashAlgorithm_Blake2b_512 -> mkHashAlgorithm "Blake2b_512" Hash.Blake2b_512
  Crypto_HashAlgorithm_Blake2b_256 -> mkHashAlgorithm "Blake2b_256" Hash.Blake2b_256
  Crypto_HashAlgorithm_Blake2s_256 -> mkHashAlgorithm "Blake2s_256" Hash.Blake2s_256
  Crypto_HashAlgorithm_Md5 -> mkHashAlgorithm "Md5" Hash.MD5
  Crypto_hashBytes -> mkForeign $
    \(HashAlgorithm _ alg, b :: Bytes.Bytes) ->
      let ctx = Hash.hashInitWith alg
       in pure . Bytes.fromArray . Hash.hashFinalize $ Hash.hashUpdates ctx (Bytes.byteStringChunks b)
  Crypto_hmacBytes -> mkForeign $
    \(HashAlgorithm _ alg, key :: Bytes.Bytes, msg :: Bytes.Bytes) ->
      let out = u alg $ HMAC.hmac (Bytes.toArray @BA.Bytes key) (Bytes.toArray @BA.Bytes msg)
          u :: a -> HMAC.HMAC a -> HMAC.HMAC a
          u _ h = h -- to help typechecker along
       in pure $ Bytes.fromArray out
  Crypto_hash -> mkForeign $
    \(HashAlgorithm _ alg, x) ->
      let hashlazy ::
            (Hash.HashAlgorithm a) =>
            a ->
            L.ByteString ->
            Hash.Digest a
          hashlazy _ l = Hash.hashlazy l
       in pure . Bytes.fromArray . hashlazy alg $ ANF.serializeValueForHash x
  Crypto_hmac -> mkForeign $
    \(HashAlgorithm _ alg, key, x) ->
      let hmac ::
            (Hash.HashAlgorithm a) => a -> L.ByteString -> HMAC.HMAC a
          hmac _ s =
            HMAC.finalize
              . HMAC.updates
                (HMAC.initialize $ Bytes.toArray @BA.Bytes key)
              $ L.toChunks s
       in pure . Bytes.fromArray . hmac alg $ ANF.serializeValueForHash x
  Crypto_Ed25519_sign_impl ->
    mkForeign $
      pure . signEd25519Wrapper
  Crypto_Ed25519_verify_impl ->
    mkForeign $
      pure . verifyEd25519Wrapper
  Crypto_Rsa_sign_impl ->
    mkForeign $
      pure . signRsaWrapper
  Crypto_Rsa_verify_impl ->
    mkForeign $
      pure . verifyRsaWrapper
  Universal_murmurHash ->
    mkForeign $
      pure . asWord64 . hash64 . ANF.serializeValueForHash
  IO_randomBytes -> mkForeign $
    \n -> Bytes.fromArray <$> getRandomBytes @IO @ByteString n
  Bytes_zlib_compress -> mkForeign $ pure . Bytes.zlibCompress
  Bytes_gzip_compress -> mkForeign $ pure . Bytes.gzipCompress
  Bytes_zlib_decompress -> mkForeign $ \bs ->
    catchAll (pure (Bytes.zlibDecompress bs))
  Bytes_gzip_decompress -> mkForeign $ \bs ->
    catchAll (pure (Bytes.gzipDecompress bs))
  Bytes_toBase16 -> mkForeign $ pure . Bytes.toBase16
  Bytes_toBase32 -> mkForeign $ pure . Bytes.toBase32
  Bytes_toBase64 -> mkForeign $ pure . Bytes.toBase64
  Bytes_toBase64UrlUnpadded -> mkForeign $ pure . Bytes.toBase64UrlUnpadded
  Bytes_fromBase16 ->
    mkForeign $
      pure . mapLeft Util.Text.fromText . Bytes.fromBase16
  Bytes_fromBase32 ->
    mkForeign $
      pure . mapLeft Util.Text.fromText . Bytes.fromBase32
  Bytes_fromBase64 ->
    mkForeign $
      pure . mapLeft Util.Text.fromText . Bytes.fromBase64
  Bytes_fromBase64UrlUnpadded ->
    mkForeign $
      pure . mapLeft Util.Text.fromText . Bytes.fromBase64UrlUnpadded
  Bytes_decodeNat64be -> mkForeign $ pure . Bytes.decodeNat64be
  Bytes_decodeNat64le -> mkForeign $ pure . Bytes.decodeNat64le
  Bytes_decodeNat32be -> mkForeign $ pure . Bytes.decodeNat32be
  Bytes_decodeNat32le -> mkForeign $ pure . Bytes.decodeNat32le
  Bytes_decodeNat16be -> mkForeign $ pure . Bytes.decodeNat16be
  Bytes_decodeNat16le -> mkForeign $ pure . Bytes.decodeNat16le
  Bytes_encodeNat64be -> mkForeign $ pure . Bytes.encodeNat64be
  Bytes_encodeNat64le -> mkForeign $ pure . Bytes.encodeNat64le
  Bytes_encodeNat32be -> mkForeign $ pure . Bytes.encodeNat32be
  Bytes_encodeNat32le -> mkForeign $ pure . Bytes.encodeNat32le
  Bytes_encodeNat16be -> mkForeign $ pure . Bytes.encodeNat16be
  Bytes_encodeNat16le -> mkForeign $ pure . Bytes.encodeNat16le
  MutableArray_copyTo_force -> mkForeign $
    \(dst, doff, src, soff, l) ->
      let name = "MutableArray.copyTo!"
       in if l == 0
            then pure (Right ())
            else
              checkBounds name (PA.sizeofMutableArray dst) (doff + l - 1) $
                checkBounds name (PA.sizeofMutableArray src) (soff + l - 1) $
                  Right
                    <$> PA.copyMutableArray @IO @Val
                      dst
                      (fromIntegral doff)
                      src
                      (fromIntegral soff)
                      (fromIntegral l)
  MutableByteArray_copyTo_force -> mkForeign $
    \(dst, doff, src, soff, l) ->
      let name = "MutableByteArray.copyTo!"
       in if l == 0
            then pure (Right ())
            else
              checkBoundsPrim name (PA.sizeofMutableByteArray dst) (doff + l) 0 $
                checkBoundsPrim name (PA.sizeofMutableByteArray src) (soff + l) 0 $
                  Right
                    <$> PA.copyMutableByteArray @IO
                      dst
                      (fromIntegral doff)
                      src
                      (fromIntegral soff)
                      (fromIntegral l)
  ImmutableArray_copyTo_force -> mkForeign $
    \(dst, doff, src, soff, l) ->
      let name = "ImmutableArray.copyTo!"
       in if l == 0
            then pure (Right ())
            else
              checkBounds name (PA.sizeofMutableArray dst) (doff + l - 1) $
                checkBounds name (PA.sizeofArray src) (soff + l - 1) $
                  Right
                    <$> PA.copyArray @IO @Val
                      dst
                      (fromIntegral doff)
                      src
                      (fromIntegral soff)
                      (fromIntegral l)
  ImmutableArray_size ->
    mkForeign $
      pure . fromIntegral @Int @Word64 . PA.sizeofArray @Val
  MutableArray_size ->
    mkForeign $
      pure . fromIntegral @Int @Word64 . PA.sizeofMutableArray @PA.RealWorld @Val
  ImmutableByteArray_size ->
    mkForeign $
      pure . fromIntegral @Int @Word64 . PA.sizeofByteArray
  MutableByteArray_size ->
    mkForeign $
      pure . fromIntegral @Int @Word64 . PA.sizeofMutableByteArray @PA.RealWorld
  ImmutableByteArray_copyTo_force -> mkForeign $
    \(dst, doff, src, soff, l) ->
      let name = "ImmutableByteArray.copyTo!"
       in if l == 0
            then pure (Right ())
            else
              checkBoundsPrim name (PA.sizeofMutableByteArray dst) (doff + l) 0 $
                checkBoundsPrim name (PA.sizeofByteArray src) (soff + l) 0 $
                  Right
                    <$> PA.copyByteArray @IO
                      dst
                      (fromIntegral doff)
                      src
                      (fromIntegral soff)
                      (fromIntegral l)
  MutableArray_read ->
    mkForeign $
      checkedRead "MutableArray.read"
  MutableByteArray_read8 ->
    mkForeign $
      checkedRead8 "MutableByteArray.read8"
  MutableByteArray_read16be ->
    mkForeign $
      checkedRead16 "MutableByteArray.read16be"
  MutableByteArray_read24be ->
    mkForeign $
      checkedRead24 "MutableByteArray.read24be"
  MutableByteArray_read32be ->
    mkForeign $
      checkedRead32 "MutableByteArray.read32be"
  MutableByteArray_read40be ->
    mkForeign $
      checkedRead40 "MutableByteArray.read40be"
  MutableByteArray_read64be ->
    mkForeign $
      checkedRead64 "MutableByteArray.read64be"
  MutableArray_write ->
    mkForeign $
      checkedWrite "MutableArray.write"
  MutableByteArray_write8 ->
    mkForeign $
      checkedWrite8 "MutableByteArray.write8"
  MutableByteArray_write16be ->
    mkForeign $
      checkedWrite16 "MutableByteArray.write16be"
  MutableByteArray_write32be ->
    mkForeign $
      checkedWrite32 "MutableByteArray.write32be"
  MutableByteArray_write64be ->
    mkForeign $
      checkedWrite64 "MutableByteArray.write64be"
  ImmutableArray_read ->
    mkForeign $
      checkedIndex "ImmutableArray.read"
  ImmutableByteArray_read8 ->
    mkForeign $
      checkedIndex8 "ImmutableByteArray.read8"
  ImmutableByteArray_read16be ->
    mkForeign $
      checkedIndex16 "ImmutableByteArray.read16be"
  ImmutableByteArray_read24be ->
    mkForeign $
      checkedIndex24 "ImmutableByteArray.read24be"
  ImmutableByteArray_read32be ->
    mkForeign $
      checkedIndex32 "ImmutableByteArray.read32be"
  ImmutableByteArray_read40be ->
    mkForeign $
      checkedIndex40 "ImmutableByteArray.read40be"
  ImmutableByteArray_read64be ->
    mkForeign $
      checkedIndex64 "ImmutableByteArray.read64be"
  MutableByteArray_freeze_force ->
    mkForeign $
      PA.unsafeFreezeByteArray
  MutableArray_freeze_force ->
    mkForeign $
      PA.unsafeFreezeArray @IO @Val
  MutableByteArray_freeze -> mkForeign $
    \(src, off, len) ->
      if len == 0
        then fmap Right . PA.unsafeFreezeByteArray =<< PA.newByteArray 0
        else
          checkBoundsPrim
            "MutableByteArray.freeze"
            (PA.sizeofMutableByteArray src)
            (off + len)
            0
            $ Right <$> PA.freezeByteArray src (fromIntegral off) (fromIntegral len)
  MutableArray_freeze -> mkForeign $
    \(src :: PA.MutableArray PA.RealWorld Val, off, len) ->
      if len == 0
        then fmap Right . PA.unsafeFreezeArray =<< PA.newArray 0 emptyVal
        else
          checkBounds
            "MutableArray.freeze"
            (PA.sizeofMutableArray src)
            (off + len - 1)
            $ Right <$> PA.freezeArray src (fromIntegral off) (fromIntegral len)
  MutableByteArray_length ->
    mkForeign $
      pure . PA.sizeofMutableByteArray @PA.RealWorld
  ImmutableByteArray_length ->
    mkForeign $
      pure . PA.sizeofByteArray
  IO_array -> mkForeign $
    \n -> PA.newArray n emptyVal
  IO_arrayOf -> mkForeign $
    \(v :: Val, n) -> PA.newArray n v
  IO_bytearray -> mkForeign $ PA.newByteArray
  IO_bytearrayOf -> mkForeign $
    \(init, sz) -> do
      arr <- PA.newByteArray sz
      PA.fillByteArray arr 0 sz init
      pure arr
  Scope_array -> mkForeign $
    \n -> PA.newArray n emptyVal
  Scope_arrayOf -> mkForeign $
    \(v :: Val, n) -> PA.newArray n v
  Scope_bytearray -> mkForeign $ PA.newByteArray
  Scope_bytearrayOf -> mkForeign $
    \(init, sz) -> do
      arr <- PA.newByteArray sz
      PA.fillByteArray arr 0 sz init
      pure arr
  Text_patterns_literal -> mkForeign $
    \txt -> evaluate . TPat.cpattern $ TPat.Literal txt
  Text_patterns_digit ->
    mkForeign $
      let v = TPat.cpattern (TPat.Char (TPat.CharRange '0' '9')) in \() -> pure v
  Text_patterns_letter ->
    mkForeign $
      let v = TPat.cpattern (TPat.Char (TPat.CharClass TPat.Letter)) in \() -> pure v
  Text_patterns_space ->
    mkForeign $
      let v = TPat.cpattern (TPat.Char (TPat.CharClass TPat.Whitespace)) in \() -> pure v
  Text_patterns_punctuation ->
    mkForeign $
      let v = TPat.cpattern (TPat.Char (TPat.CharClass TPat.Punctuation)) in \() -> pure v
  Text_patterns_anyChar ->
    mkForeign $
      let v = TPat.cpattern (TPat.Char TPat.Any) in \() -> pure v
  Text_patterns_eof ->
    mkForeign $
      let v = TPat.cpattern TPat.Eof in \() -> pure v
  Text_patterns_charRange -> mkForeign $
    \(beg, end) -> evaluate . TPat.cpattern . TPat.Char $ TPat.CharRange beg end
  Text_patterns_notCharRange -> mkForeign $
    \(beg, end) -> evaluate . TPat.cpattern . TPat.Char . TPat.Not $ TPat.CharRange beg end
  Text_patterns_charIn -> mkForeign $ \ccs -> do
    cs <- for ccs $ \case
      CharVal c -> pure c
      _ -> die "Text.patterns.charIn: non-character closure"
    evaluate . TPat.cpattern . TPat.Char $ TPat.CharSet cs
  Text_patterns_notCharIn -> mkForeign $ \ccs -> do
    cs <- for ccs $ \case
      CharVal c -> pure c
      _ -> die "Text.patterns.notCharIn: non-character closure"
    evaluate . TPat.cpattern . TPat.Char . TPat.Not $ TPat.CharSet cs
  Pattern_many -> mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.Many False p
  Pattern_many_corrected -> mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.Many True p
  Pattern_capture -> mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.Capture p
  Pattern_captureAs -> mkForeign $
    \(t, (TPat.CP p _)) -> evaluate . TPat.cpattern $ TPat.CaptureAs t p
  Pattern_join -> mkForeign $ \ps ->
    evaluate . TPat.cpattern . TPat.Join $ map (\(TPat.CP p _) -> p) ps
  Pattern_or -> mkForeign $
    \(TPat.CP l _, TPat.CP r _) -> evaluate . TPat.cpattern $ TPat.Or l r
  Pattern_replicate -> mkForeign $
    \(m0 :: Word64, n0 :: Word64, TPat.CP p _) ->
      let m = fromIntegral m0; n = fromIntegral n0
       in evaluate . TPat.cpattern $ TPat.Replicate m n p
  Pattern_run -> mkForeign $
    \(TPat.CP _ matcher, input :: Text) -> pure $ matcher input
  Pattern_isMatch -> mkForeign $
    \(TPat.CP _ matcher, input :: Text) -> pure . isJust $ matcher input
  Char_Class_any -> mkForeign $ \() -> pure TPat.Any
  Char_Class_not -> mkForeign $ pure . TPat.Not
  Char_Class_and -> mkForeign $ \(a, b) -> pure $ TPat.Intersect a b
  Char_Class_or -> mkForeign $ \(a, b) -> pure $ TPat.Union a b
  Char_Class_range -> mkForeign $ \(a, b) -> pure $ TPat.CharRange a b
  Char_Class_anyOf -> mkForeign $ \ccs -> do
    cs <- for ccs $ \case
      CharVal c -> pure c
      _ -> die "Text.patterns.charIn: non-character closure"
    evaluate $ TPat.CharSet cs
  Char_Class_alphanumeric -> mkForeign $ \() -> pure (TPat.CharClass TPat.AlphaNum)
  Char_Class_upper -> mkForeign $ \() -> pure (TPat.CharClass TPat.Upper)
  Char_Class_lower -> mkForeign $ \() -> pure (TPat.CharClass TPat.Lower)
  Char_Class_whitespace -> mkForeign $ \() -> pure (TPat.CharClass TPat.Whitespace)
  Char_Class_control -> mkForeign $ \() -> pure (TPat.CharClass TPat.Control)
  Char_Class_printable -> mkForeign $ \() -> pure (TPat.CharClass TPat.Printable)
  Char_Class_mark -> mkForeign $ \() -> pure (TPat.CharClass TPat.MarkChar)
  Char_Class_number -> mkForeign $ \() -> pure (TPat.CharClass TPat.Number)
  Char_Class_punctuation -> mkForeign $ \() -> pure (TPat.CharClass TPat.Punctuation)
  Char_Class_symbol -> mkForeign $ \() -> pure (TPat.CharClass TPat.Symbol)
  Char_Class_separator -> mkForeign $ \() -> pure (TPat.CharClass TPat.Separator)
  Char_Class_letter -> mkForeign $ \() -> pure (TPat.CharClass TPat.Letter)
  Char_Class_is -> mkForeign $ \(cl, c) -> evaluate $ TPat.charPatternPred cl c
  Text_patterns_char -> mkForeign $ \c ->
    let v = TPat.cpattern (TPat.Char c) in pure v
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

    mkHashAlgorithm :: forall alg. (Hash.HashAlgorithm alg) => Data.Text.Text -> alg -> Args -> Stack -> IO Stack
    mkHashAlgorithm txt alg =
      let algoRef = Builtin ("crypto.HashAlgorithm." <> txt)
       in mkForeign $ \() -> pure (HashAlgorithm algoRef alg)

    catchAll :: (MonadCatch m, MonadIO m, NFData a) => m a -> m (Either Util.Text.Text a)
    catchAll e = do
      e <- Exception.tryAnyDeep e
      pure $ case e of
        Left se -> Left (Util.Text.pack (show se))
        Right a -> Right a
{-# INLINE foreignCallHelper #-}

mkForeign :: (ForeignConvention a, ForeignConvention b) => (a -> IO b) -> Args -> Stack -> IO Stack
mkForeign !f !args !stk = do
  args <- decodeArgs args stk
  res <- f args
  writeForeign stk res
  where
    decodeArgs :: (ForeignConvention x) => Args -> Stack -> IO x
    decodeArgs !args !stk =
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
    handleIOE (Left e) = Left $ F.Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue
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
    flatten (Left e) = Left (F.Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Left e)) = Left (F.Failure Ty.tlsFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Right a)) = Right a
{-# INLINE mkForeignTls #-}

mkForeignTlsE ::
  forall a r.
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO (Either Failure r)) ->
  Args ->
  Stack ->
  IO Stack
mkForeignTlsE f = mkForeign $ \a -> fmap flatten (tryIO2 (tryIO1 (f a)))
  where
    tryIO1 :: IO (Either Failure r) -> IO (Either TLS.TLSException (Either Failure r))
    tryIO1 = UnliftIO.try
    tryIO2 :: IO (Either TLS.TLSException (Either Failure r)) -> IO (Either IOException (Either TLS.TLSException (Either Failure r)))
    tryIO2 = UnliftIO.try
    flatten :: Either IOException (Either TLS.TLSException (Either Failure r)) -> Either Failure r
    flatten (Left e) = Left (F.Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Left e)) = Left (F.Failure Ty.tlsFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Right (Left e))) = Left e
    flatten (Right (Right (Right a))) = Right a
{-# INLINE mkForeignTlsE #-}

unsafeSTMToIO :: STM.STM a -> IO a
unsafeSTMToIO (STM.STM m) = IO m
{-# INLINE unsafeSTMToIO #-}

signEd25519Wrapper ::
  (Bytes.Bytes, Bytes.Bytes, Bytes.Bytes) -> Either Failure Bytes.Bytes
signEd25519Wrapper (secret0, public0, msg0) = case validated of
  CryptoFailed err ->
    Left (F.Failure Ty.cryptoFailureRef (errMsg err) unitValue)
  CryptoPassed (secret, public) ->
    Right . Bytes.fromArray $ Ed25519.sign secret public msg
  where
    msg = Bytes.toArray msg0 :: ByteString
    validated =
      (,)
        <$> Ed25519.secretKey (Bytes.toArray secret0 :: ByteString)
        <*> Ed25519.publicKey (Bytes.toArray public0 :: ByteString)

    errMsg CryptoError_PublicKeySizeInvalid =
      "ed25519: Public key size invalid"
    errMsg CryptoError_SecretKeySizeInvalid =
      "ed25519: Secret key size invalid"
    errMsg CryptoError_SecretKeyStructureInvalid =
      "ed25519: Secret key structure invalid"
    errMsg _ = "ed25519: unexpected error"

verifyEd25519Wrapper ::
  (Bytes.Bytes, Bytes.Bytes, Bytes.Bytes) -> Either Failure Bool
verifyEd25519Wrapper (public0, msg0, sig0) = case validated of
  CryptoFailed err ->
    Left $ F.Failure Ty.cryptoFailureRef (errMsg err) unitValue
  CryptoPassed (public, sig) ->
    Right $ Ed25519.verify public msg sig
  where
    msg = Bytes.toArray msg0 :: ByteString
    validated =
      (,)
        <$> Ed25519.publicKey (Bytes.toArray public0 :: ByteString)
        <*> Ed25519.signature (Bytes.toArray sig0 :: ByteString)

    errMsg CryptoError_PublicKeySizeInvalid =
      "ed25519: Public key size invalid"
    errMsg CryptoError_SecretKeySizeInvalid =
      "ed25519: Secret key size invalid"
    errMsg CryptoError_SecretKeyStructureInvalid =
      "ed25519: Secret key structure invalid"
    errMsg _ = "ed25519: unexpected error"

signRsaWrapper ::
  (Bytes.Bytes, Bytes.Bytes) -> Either Failure Bytes.Bytes
signRsaWrapper (secret0, msg0) = case validated of
  Left err ->
    Left (F.Failure Ty.cryptoFailureRef err unitValue)
  Right secret ->
    case RSA.sign Nothing (Just Hash.SHA256) secret msg of
      Left err -> Left (F.Failure Ty.cryptoFailureRef (Rsa.rsaErrorToText err) unitValue)
      Right signature -> Right $ Bytes.fromByteString signature
  where
    msg = Bytes.toArray msg0 :: ByteString
    validated = Rsa.parseRsaPrivateKey (Bytes.toArray secret0 :: ByteString)

verifyRsaWrapper ::
  (Bytes.Bytes, Bytes.Bytes, Bytes.Bytes) -> Either Failure Bool
verifyRsaWrapper (public0, msg0, sig0) = case validated of
  Left err ->
    Left $ F.Failure Ty.cryptoFailureRef err unitValue
  Right public ->
    Right $ RSA.verify (Just Hash.SHA256) public msg sig
  where
    msg = Bytes.toArray msg0 :: ByteString
    sig = Bytes.toArray sig0 :: ByteString
    validated = Rsa.parseRsaPublicKey (Bytes.toArray public0 :: ByteString)

type Failure = F.Failure Val

checkBounds :: Text -> Int -> Word64 -> IO (Either Failure b) -> IO (Either Failure b)
checkBounds name l w act
  | w < fromIntegral l = act
  | otherwise = pure $ Left err
  where
    msg = name <> ": array index out of bounds"
    err = F.Failure Ty.arrayFailureRef msg (natValue w)

-- Performs a bounds check on a byte array. Strategy is as follows:
--
--   isz = signed array size-in-bytes
--   off = unsigned byte offset into the array
--   esz = unsigned number of bytes to be read
--
--   1. Turn the signed size-in-bytes of the array unsigned
--   2. Add the offset to the to-be-read number to get the maximum size needed
--   3. Check that the actual array size is at least as big as the needed size
--   4. Check that the offset is less than the size
--
-- Step 4 ensures that step 3 has not overflowed. Since an actual array size can
-- only be 63 bits (since it is signed), the only way for 3 to overflow is if
-- the offset is larger than a possible array size, since it would need to be
-- 2^64-k, where k is the small (<=8) number of bytes to be read.
checkBoundsPrim ::
  Text -> Int -> Word64 -> Word64 -> IO (Either Failure b) -> IO (Either Failure b)
checkBoundsPrim name isz off esz act
  | w > bsz || off > bsz = pure $ Left err
  | otherwise = act
  where
    msg = name <> ": array index out of bounds"
    err = F.Failure Ty.arrayFailureRef msg (natValue off)

    bsz = fromIntegral isz
    w = off + esz

type RW = PA.PrimState IO

checkedRead ::
  Text -> (PA.MutableArray RW Val, Word64) -> IO (Either Failure Val)
checkedRead name (arr, w) =
  checkBounds
    name
    (PA.sizeofMutableArray arr)
    w
    (Right <$> PA.readArray arr (fromIntegral w))

checkedWrite ::
  Text -> (PA.MutableArray RW Val, Word64, Val) -> IO (Either Failure ())
checkedWrite name (arr, w, v) =
  checkBounds
    name
    (PA.sizeofMutableArray arr)
    w
    (Right <$> PA.writeArray arr (fromIntegral w) v)

checkedIndex ::
  Text -> (PA.Array Val, Word64) -> IO (Either Failure Val)
checkedIndex name (arr, w) =
  checkBounds
    name
    (PA.sizeofArray arr)
    w
    (Right <$> PA.indexArrayM arr (fromIntegral w))

checkedRead8 :: Text -> (PA.MutableByteArray RW, Word64) -> IO (Either Failure Word64)
checkedRead8 name (arr, i) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 1 $
    (Right . fromIntegral) <$> PA.readByteArray @Word8 arr j
  where
    j = fromIntegral i

checkedRead16 :: Text -> (PA.MutableByteArray RW, Word64) -> IO (Either Failure Word64)
checkedRead16 name (arr, i) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 2 $
    mk16
      <$> PA.readByteArray @Word8 arr j
      <*> PA.readByteArray @Word8 arr (j + 1)
  where
    j = fromIntegral i

checkedRead24 :: Text -> (PA.MutableByteArray RW, Word64) -> IO (Either Failure Word64)
checkedRead24 name (arr, i) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 3 $
    mk24
      <$> PA.readByteArray @Word8 arr j
      <*> PA.readByteArray @Word8 arr (j + 1)
      <*> PA.readByteArray @Word8 arr (j + 2)
  where
    j = fromIntegral i

checkedRead32 :: Text -> (PA.MutableByteArray RW, Word64) -> IO (Either Failure Word64)
checkedRead32 name (arr, i) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 4 $
    mk32
      <$> PA.readByteArray @Word8 arr j
      <*> PA.readByteArray @Word8 arr (j + 1)
      <*> PA.readByteArray @Word8 arr (j + 2)
      <*> PA.readByteArray @Word8 arr (j + 3)
  where
    j = fromIntegral i

checkedRead40 :: Text -> (PA.MutableByteArray RW, Word64) -> IO (Either Failure Word64)
checkedRead40 name (arr, i) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 6 $
    mk40
      <$> PA.readByteArray @Word8 arr j
      <*> PA.readByteArray @Word8 arr (j + 1)
      <*> PA.readByteArray @Word8 arr (j + 2)
      <*> PA.readByteArray @Word8 arr (j + 3)
      <*> PA.readByteArray @Word8 arr (j + 4)
  where
    j = fromIntegral i

checkedRead64 :: Text -> (PA.MutableByteArray RW, Word64) -> IO (Either Failure Word64)
checkedRead64 name (arr, i) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 8 $
    mk64
      <$> PA.readByteArray @Word8 arr j
      <*> PA.readByteArray @Word8 arr (j + 1)
      <*> PA.readByteArray @Word8 arr (j + 2)
      <*> PA.readByteArray @Word8 arr (j + 3)
      <*> PA.readByteArray @Word8 arr (j + 4)
      <*> PA.readByteArray @Word8 arr (j + 5)
      <*> PA.readByteArray @Word8 arr (j + 6)
      <*> PA.readByteArray @Word8 arr (j + 7)
  where
    j = fromIntegral i

mk16 :: Word8 -> Word8 -> Either Failure Word64
mk16 b0 b1 = Right $ (fromIntegral b0 `shiftL` 8) .|. (fromIntegral b1)

mk24 :: Word8 -> Word8 -> Word8 -> Either Failure Word64
mk24 b0 b1 b2 =
  Right $
    (fromIntegral b0 `shiftL` 16)
      .|. (fromIntegral b1 `shiftL` 8)
      .|. (fromIntegral b2)

mk32 :: Word8 -> Word8 -> Word8 -> Word8 -> Either Failure Word64
mk32 b0 b1 b2 b3 =
  Right $
    (fromIntegral b0 `shiftL` 24)
      .|. (fromIntegral b1 `shiftL` 16)
      .|. (fromIntegral b2 `shiftL` 8)
      .|. (fromIntegral b3)

mk40 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Either Failure Word64
mk40 b0 b1 b2 b3 b4 =
  Right $
    (fromIntegral b0 `shiftL` 32)
      .|. (fromIntegral b1 `shiftL` 24)
      .|. (fromIntegral b2 `shiftL` 16)
      .|. (fromIntegral b3 `shiftL` 8)
      .|. (fromIntegral b4)

mk64 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Either Failure Word64
mk64 b0 b1 b2 b3 b4 b5 b6 b7 =
  Right $
    (fromIntegral b0 `shiftL` 56)
      .|. (fromIntegral b1 `shiftL` 48)
      .|. (fromIntegral b2 `shiftL` 40)
      .|. (fromIntegral b3 `shiftL` 32)
      .|. (fromIntegral b4 `shiftL` 24)
      .|. (fromIntegral b5 `shiftL` 16)
      .|. (fromIntegral b6 `shiftL` 8)
      .|. (fromIntegral b7)

checkedWrite8 :: Text -> (PA.MutableByteArray RW, Word64, Word64) -> IO (Either Failure ())
checkedWrite8 name (arr, i, v) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 1 $ do
    PA.writeByteArray arr j (fromIntegral v :: Word8)
    pure (Right ())
  where
    j = fromIntegral i

checkedWrite16 :: Text -> (PA.MutableByteArray RW, Word64, Word64) -> IO (Either Failure ())
checkedWrite16 name (arr, i, v) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 2 $ do
    PA.writeByteArray arr j (fromIntegral $ v `shiftR` 8 :: Word8)
    PA.writeByteArray arr (j + 1) (fromIntegral v :: Word8)
    pure (Right ())
  where
    j = fromIntegral i

checkedWrite32 :: Text -> (PA.MutableByteArray RW, Word64, Word64) -> IO (Either Failure ())
checkedWrite32 name (arr, i, v) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 4 $ do
    PA.writeByteArray arr j (fromIntegral $ v `shiftR` 24 :: Word8)
    PA.writeByteArray arr (j + 1) (fromIntegral $ v `shiftR` 16 :: Word8)
    PA.writeByteArray arr (j + 2) (fromIntegral $ v `shiftR` 8 :: Word8)
    PA.writeByteArray arr (j + 3) (fromIntegral v :: Word8)
    pure (Right ())
  where
    j = fromIntegral i

checkedWrite64 :: Text -> (PA.MutableByteArray RW, Word64, Word64) -> IO (Either Failure ())
checkedWrite64 name (arr, i, v) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 8 $ do
    PA.writeByteArray arr j (fromIntegral $ v `shiftR` 56 :: Word8)
    PA.writeByteArray arr (j + 1) (fromIntegral $ v `shiftR` 48 :: Word8)
    PA.writeByteArray arr (j + 2) (fromIntegral $ v `shiftR` 40 :: Word8)
    PA.writeByteArray arr (j + 3) (fromIntegral $ v `shiftR` 32 :: Word8)
    PA.writeByteArray arr (j + 4) (fromIntegral $ v `shiftR` 24 :: Word8)
    PA.writeByteArray arr (j + 5) (fromIntegral $ v `shiftR` 16 :: Word8)
    PA.writeByteArray arr (j + 6) (fromIntegral $ v `shiftR` 8 :: Word8)
    PA.writeByteArray arr (j + 7) (fromIntegral v :: Word8)
    pure (Right ())
  where
    j = fromIntegral i

-- index single byte
checkedIndex8 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex8 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 1 . pure $
    let j = fromIntegral i
     in Right . fromIntegral $ PA.indexByteArray @Word8 arr j

-- index 16 big-endian
checkedIndex16 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex16 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 2 . pure $
    let j = fromIntegral i
     in mk16 (PA.indexByteArray arr j) (PA.indexByteArray arr (j + 1))

-- index 32 big-endian
checkedIndex24 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex24 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 3 . pure $
    let j = fromIntegral i
     in mk24
          (PA.indexByteArray arr j)
          (PA.indexByteArray arr (j + 1))
          (PA.indexByteArray arr (j + 2))

-- index 32 big-endian
checkedIndex32 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex32 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 4 . pure $
    let j = fromIntegral i
     in mk32
          (PA.indexByteArray arr j)
          (PA.indexByteArray arr (j + 1))
          (PA.indexByteArray arr (j + 2))
          (PA.indexByteArray arr (j + 3))

-- index 40 big-endian
checkedIndex40 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex40 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 5 . pure $
    let j = fromIntegral i
     in mk40
          (PA.indexByteArray arr j)
          (PA.indexByteArray arr (j + 1))
          (PA.indexByteArray arr (j + 2))
          (PA.indexByteArray arr (j + 3))
          (PA.indexByteArray arr (j + 4))

-- index 64 big-endian
checkedIndex64 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex64 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 8 . pure $
    let j = fromIntegral i
     in mk64
          (PA.indexByteArray arr j)
          (PA.indexByteArray arr (j + 1))
          (PA.indexByteArray arr (j + 2))
          (PA.indexByteArray arr (j + 3))
          (PA.indexByteArray arr (j + 4))
          (PA.indexByteArray arr (j + 5))
          (PA.indexByteArray arr (j + 6))
          (PA.indexByteArray arr (j + 7))

class ForeignConvention a where
  readForeign ::
    [Int] -> Stack -> IO ([Int], a)
  writeForeign ::
    Stack -> a -> IO Stack

instance ForeignConvention Int where
  readForeign !(i : args) !stk = (args,) <$> peekOffI stk i
  readForeign ![] !_ = foreignCCError "Int"
  {-# INLINE readForeign #-}
  writeForeign !stk !i = do
    stk <- bump stk
    stk <$ pokeI stk i
  {-# INLINE writeForeign #-}

instance ForeignConvention Word64 where
  readForeign !(i : args) !stk = (args,) <$> peekOffN stk i
  readForeign ![] !_ = foreignCCError "Word64"
  {-# INLINE readForeign #-}
  writeForeign !stk !n = do
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
  readForeign !(i : args) !stk = (args,) <$> peekOffC stk i
  readForeign ![] !_ = foreignCCError "Char"
  {-# INLINE readForeign #-}
  writeForeign !stk !ch = do
    stk <- bump stk
    stk <$ pokeC stk ch
  {-# INLINE writeForeign #-}

instance ForeignConvention Val where
  readForeign !(i : args) !stk = (args,) <$> peekOff stk i
  readForeign ![] !_ = foreignCCError "Val"
  {-# INLINE readForeign #-}
  writeForeign !stk !v = do
    stk <- bump stk
    stk <$ (poke stk =<< evaluate v)
  {-# INLINE writeForeign #-}

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
instance ForeignConvention Closure where
  readForeign !(i : args) !stk = (args,) <$> bpeekOff stk i
  readForeign ![] !_ = foreignCCError "Closure"
  {-# INLINE readForeign #-}
  writeForeign !stk !c = do
    stk <- bump stk
    stk <$ (bpoke stk =<< evaluate c)
  {-# INLINE writeForeign #-}

instance ForeignConvention Text where
  readForeign = readForeignBuiltin
  {-# INLINE readForeign #-}
  writeForeign = writeForeignBuiltin
  {-# INLINE writeForeign #-}

instance ForeignConvention Unison.Util.Bytes.Bytes where
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
  readForeign !(i : args) !stk =
    upeekOff stk i >>= \case
      0 -> pure (args, Nothing)
      1 -> fmap Just <$> readForeign args stk
      _ -> foreignCCError "Maybe"
  readForeign ![] !_ = foreignCCError "Maybe"
  {-# INLINE readForeign #-}

  writeForeign !stk !Nothing = do
    stk <- bump stk
    stk <$ pokeTag stk 0
  writeForeign !stk !(Just x) = do
    stk <- writeForeign stk x
    stk <- bump stk
    stk <$ pokeTag stk 1
  {-# INLINE writeForeign #-}

instance
  (ForeignConvention a, ForeignConvention b) =>
  ForeignConvention (Either a b)
  where
  readForeign !(i : args) !stk =
    peekTagOff stk i >>= \case
      0 -> readForeignAs Left args stk
      1 -> readForeignAs Right args stk
      _ -> foreignCCError "Either"
  readForeign !_ !_ = foreignCCError "Either"
  {-# INLINE readForeign #-}

  writeForeign !stk !(Left a) = do
    stk <- writeForeign stk a
    stk <- bump stk
    stk <$ pokeTag stk 0
  writeForeign !stk !(Right b) = do
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
readForeignAs !f !args !stk = fmap f <$> readForeign args stk
{-# INLINE readForeignAs #-}

writeForeignAs ::
  (ForeignConvention b) =>
  (a -> b) ->
  Stack ->
  a ->
  IO Stack
writeForeignAs !f !stk !x = writeForeign stk (f x)
{-# INLINE writeForeignAs #-}

readForeignEnum ::
  (Enum a) =>
  [Int] ->
  Stack ->
  IO ([Int], a)
readForeignEnum = readForeignAs toEnum
{-# INLINE readForeignEnum #-}

writeForeignEnum ::
  (Enum a) =>
  Stack ->
  a ->
  IO Stack
writeForeignEnum = writeForeignAs fromEnum
{-# INLINE writeForeignEnum #-}

readForeignBuiltin ::
  (BuiltinForeign b) =>
  [Int] ->
  Stack ->
  IO ([Int], b)
readForeignBuiltin = readForeignAs (unwrapBuiltin . marshalToForeign)
{-# INLINE readForeignBuiltin #-}

writeForeignBuiltin ::
  (BuiltinForeign b) =>
  Stack ->
  b ->
  IO Stack
writeForeignBuiltin = writeForeignAs (Foreign . wrapBuiltin)
{-# INLINE writeForeignBuiltin #-}

writeTypeLink ::
  Stack ->
  Reference ->
  IO Stack
writeTypeLink = writeForeignAs (Foreign . Wrap typeLinkRef)
{-# INLINE writeTypeLink #-}

readTypelink ::
  [Int] ->
  Stack ->
  IO ([Int], Reference)
readTypelink = readForeignAs (unwrapForeign . marshalToForeign)
{-# INLINE readTypelink #-}

instance ForeignConvention Double where
  readForeign !(i : args) !stk = (args,) <$> peekOffD stk i
  readForeign !_ !_ = foreignCCError "Double"
  {-# INLINE readForeign #-}
  writeForeign !stk !d =
    bump stk >>= \(!stk) -> do
      pokeD stk d
      pure stk
  {-# INLINE writeForeign #-}

instance ForeignConvention Bool where
  readForeign !(i : args) !stk = do
    b <- peekOffBool stk i
    pure (args, b)
  readForeign !_ !_ = foreignCCError "Bool"
  {-# INLINE readForeign #-}
  writeForeign !stk !b = do
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
  readForeign args !_ = pure (args, ())
  {-# INLINE readForeign #-}
  writeForeign !stk !_ = pure stk
  {-# INLINE writeForeign #-}

instance
  (ForeignConvention a, ForeignConvention b) =>
  ForeignConvention (a, b)
  where
  readForeign !args !stk = do
    (args, a) <- readForeign args stk
    (args, b) <- readForeign args stk
    pure (args, (a, b))
  {-# INLINE readForeign #-}

  writeForeign !stk !(x, y) = do
    stk <- writeForeign stk y
    writeForeign stk x
  {-# INLINE writeForeign #-}

instance (ForeignConvention a) => ForeignConvention (F.Failure a) where
  readForeign !args !stk = do
    (args, typeref) <- readTypelink args stk
    (args, message) <- readForeign args stk
    (args, any) <- readForeign args stk
    pure (args, F.Failure typeref message any)
  {-# INLINE readForeign #-}

  writeForeign !stk !(F.Failure typeref message any) = do
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
  readForeign args !stk = do
    (args, a) <- readForeign args stk
    (args, b) <- readForeign args stk
    (args, c) <- readForeign args stk
    pure (args, (a, b, c))
  {-# INLINE readForeign #-}

  writeForeign !stk !(a, b, c) = do
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
  readForeign !args !stk = do
    (args, a) <- readForeign args stk
    (args, b) <- readForeign args stk
    (args, c) <- readForeign args stk
    (args, d) <- readForeign args stk
    pure (args, (a, b, c, d))
  {-# INLINE readForeign #-}

  writeForeign !stk !(a, b, c, d) = do
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
  readForeign !args !stk = do
    (args, a) <- readForeign args stk
    (args, b) <- readForeign args stk
    (args, c) <- readForeign args stk
    (args, d) <- readForeign args stk
    (args, e) <- readForeign args stk
    pure (args, (a, b, c, d, e))
  {-# INLINE readForeign #-}

  writeForeign !stk !(a, b, c, d, e) = do
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
  readForeign !(i : args) !stk =
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
  readForeign !_ !_ = foreignCCError $ "BufferMode (empty stack)"
  {-# INLINE readForeign #-}

  writeForeign !stk !bm =
    bump stk >>= \(!stk) ->
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
  readForeign !(i : args) !stk =
    (args,) . toList <$> peekOffS stk i
  readForeign !_ !_ = foreignCCError "[Val]"
  {-# INLINE readForeign #-}
  writeForeign !stk !l = do
    stk <- bump stk
    stk <$ pokeS stk (Sq.fromList l)
  {-# INLINE writeForeign #-}

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
instance {-# OVERLAPPING #-} ForeignConvention [Closure] where
  readForeign !(i : args) !stk =
    (args,) . fmap getBoxedVal . toList <$> peekOffS stk i
  readForeign !_ !_ = foreignCCError "[Closure]"
  {-# INLINE readForeign #-}
  writeForeign !stk !l = do
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
  readForeign !(i : args) !stk =
    (args,)
      . fmap (fromUnisonPair . getBoxedVal)
      . toList
      <$> peekOffS stk i
  readForeign !_ !_ = foreignCCError "[(a,b)]"
  {-# INLINE readForeign #-}

  writeForeign !stk !l = do
    stk <- bump stk
    stk <$ pokeS stk (boxedVal . toUnisonPair <$> Sq.fromList l)

instance {-# OVERLAPPABLE #-} (BuiltinForeign b) => ForeignConvention [b] where
  readForeign !(i : args) !stk =
    (args,)
      . fmap (unwrapForeignClosure . getBoxedVal)
      . toList
      <$> peekOffS stk i
  readForeign !_ !_ = foreignCCError "[b]"
  {-# INLINE readForeign #-}
  writeForeign !stk !l = do
    stk <- bump stk
    stk <$ pokeS stk (boxedVal . Foreign . wrapBuiltin <$> Sq.fromList l)
  {-# INLINE writeForeign #-}

foreignCCError :: String -> IO a
foreignCCError nm =
  die $ "mismatched foreign calling convention for `" ++ nm ++ "`"
