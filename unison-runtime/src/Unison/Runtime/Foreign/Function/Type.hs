module Unison.Runtime.Foreign.Function.Type
  ( ForeignFunc (..),
    foreignFuncBuiltinName,
  )
where

import Data.Text (Text)

-- | Enum representing every foreign call.
data ForeignFunc
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
  deriving (Show, Eq, Ord, Enum, Bounded)

foreignFuncBuiltinName :: ForeignFunc -> Text
foreignFuncBuiltinName = \case
  IO_UDP_clientSocket_impl_v1 -> "IO.UDP.clientSocket.impl.v1"
  IO_UDP_UDPSocket_recv_impl_v1 -> "IO.UDP.UDPSocket.recv.impl.v1"
  IO_UDP_UDPSocket_send_impl_v1 -> "IO.UDP.UDPSocket.send.impl.v1"
  IO_UDP_UDPSocket_close_impl_v1 -> "IO.UDP.UDPSocket.close.impl.v1"
  IO_UDP_ListenSocket_close_impl_v1 -> "IO.UDP.ListenSocket.close.impl.v1"
  IO_UDP_UDPSocket_toText_impl_v1 -> "IO.UDP.UDPSocket.toText.impl.v1"
  IO_UDP_serverSocket_impl_v1 -> "IO.UDP.serverSocket.impl.v1"
  IO_UDP_ListenSocket_toText_impl_v1 -> "IO.UDP.ListenSocket.toText.impl.v1"
  IO_UDP_ListenSocket_recvFrom_impl_v1 -> "IO.UDP.ListenSocket.recvFrom.impl.v1"
  IO_UDP_ClientSockAddr_toText_v1 -> "IO.UDP.ClientSockAddr.toText.v1"
  IO_UDP_ListenSocket_sendTo_impl_v1 -> "IO.UDP.ListenSocket.sendTo.impl.v1"
  IO_openFile_impl_v3 -> "IO.openFile.impl.v3"
  IO_closeFile_impl_v3 -> "IO.closeFile.impl.v3"
  IO_isFileEOF_impl_v3 -> "IO.isFileEOF.impl.v3"
  IO_isFileOpen_impl_v3 -> "IO.isFileOpen.impl.v3"
  IO_getEcho_impl_v1 -> "IO.getEcho.impl.v1"
  IO_ready_impl_v1 -> "IO.ready.impl.v1"
  IO_getChar_impl_v1 -> "IO.getChar.impl.v1"
  IO_isSeekable_impl_v3 -> "IO.isSeekable.impl.v3"
  IO_seekHandle_impl_v3 -> "IO.seekHandle.impl.v3"
  IO_handlePosition_impl_v3 -> "IO.handlePosition.impl.v3"
  IO_getBuffering_impl_v3 -> "IO.getBuffering.impl.v3"
  IO_setBuffering_impl_v3 -> "IO.setBuffering.impl.v3"
  IO_setEcho_impl_v1 -> "IO.setEcho.impl.v1"
  IO_getLine_impl_v1 -> "IO.getLine.impl.v1"
  IO_getBytes_impl_v3 -> "IO.getBytes.impl.v3"
  IO_getSomeBytes_impl_v1 -> "IO.getSomeBytes.impl.v1"
  IO_putBytes_impl_v3 -> "IO.putBytes.impl.v3"
  IO_systemTime_impl_v3 -> "IO.systemTime.impl.v3"
  IO_systemTimeMicroseconds_v1 -> "IO.systemTimeMicroseconds.v1"
  Clock_internals_monotonic_v1 -> "Clock.internals.monotonic.v1"
  Clock_internals_realtime_v1 -> "Clock.internals.realtime.v1"
  Clock_internals_processCPUTime_v1 -> "Clock.internals.processCPUTime.v1"
  Clock_internals_threadCPUTime_v1 -> "Clock.internals.threadCPUTime.v1"
  Clock_internals_sec_v1 -> "Clock.internals.sec.v1"
  Clock_internals_nsec_v1 -> "Clock.internals.nsec.v1"
  Clock_internals_systemTimeZone_v1 -> "Clock.internals.systemTimeZone.v1"
  IO_getTempDirectory_impl_v3 -> "IO.getTempDirectory.impl.v3"
  IO_createTempDirectory_impl_v3 -> "IO.createTempDirectory.impl.v3"
  IO_getCurrentDirectory_impl_v3 -> "IO.getCurrentDirectory.impl.v3"
  IO_setCurrentDirectory_impl_v3 -> "IO.setCurrentDirectory.impl.v3"
  IO_fileExists_impl_v3 -> "IO.fileExists.impl.v3"
  IO_getEnv_impl_v1 -> "IO.getEnv.impl.v1"
  IO_getArgs_impl_v1 -> "IO.getArgs.impl.v1"
  IO_isDirectory_impl_v3 -> "IO.isDirectory.impl.v3"
  IO_createDirectory_impl_v3 -> "IO.createDirectory.impl.v3"
  IO_removeDirectory_impl_v3 -> "IO.removeDirectory.impl.v3"
  IO_renameDirectory_impl_v3 -> "IO.renameDirectory.impl.v3"
  IO_directoryContents_impl_v3 -> "IO.directoryContents.impl.v3"
  IO_removeFile_impl_v3 -> "IO.removeFile.impl.v3"
  IO_renameFile_impl_v3 -> "IO.renameFile.impl.v3"
  IO_getFileTimestamp_impl_v3 -> "IO.getFileTimestamp.impl.v3"
  IO_getFileSize_impl_v3 -> "IO.getFileSize.impl.v3"
  IO_serverSocket_impl_v3 -> "IO.serverSocket.impl.v3"
  Socket_toText -> "Socket.toText"
  Handle_toText -> "Handle.toText"
  ThreadId_toText -> "ThreadId.toText"
  IO_socketPort_impl_v3 -> "IO.socketPort.impl.v3"
  IO_listen_impl_v3 -> "IO.listen.impl.v3"
  IO_clientSocket_impl_v3 -> "IO.clientSocket.impl.v3"
  IO_closeSocket_impl_v3 -> "IO.closeSocket.impl.v3"
  IO_socketAccept_impl_v3 -> "IO.socketAccept.impl.v3"
  IO_socketSend_impl_v3 -> "IO.socketSend.impl.v3"
  IO_socketReceive_impl_v3 -> "IO.socketReceive.impl.v3"
  IO_kill_impl_v3 -> "IO.kill.impl.v3"
  IO_delay_impl_v3 -> "IO.delay.impl.v3"
  IO_stdHandle -> "IO.stdHandle"
  IO_process_call -> "IO.process.call"
  IO_process_start -> "IO.process.start"
  IO_process_kill -> "IO.process.kill"
  IO_process_wait -> "IO.process.wait"
  IO_process_exitCode -> "IO.process.exitCode"
  MVar_new -> "MVar.new"
  MVar_newEmpty_v2 -> "MVar.newEmpty.v2"
  MVar_take_impl_v3 -> "MVar.take.impl.v3"
  MVar_tryTake -> "MVar.tryTake"
  MVar_put_impl_v3 -> "MVar.put.impl.v3"
  MVar_tryPut_impl_v3 -> "MVar.tryPut.impl.v3"
  MVar_swap_impl_v3 -> "MVar.swap.impl.v3"
  MVar_isEmpty -> "MVar.isEmpty"
  MVar_read_impl_v3 -> "MVar.read.impl.v3"
  MVar_tryRead_impl_v3 -> "MVar.tryRead.impl.v3"
  Char_toText -> "Char.toText"
  Text_repeat -> "Text.repeat"
  Text_reverse -> "Text.reverse"
  Text_toUppercase -> "Text.toUppercase"
  Text_toLowercase -> "Text.toLowercase"
  Text_toUtf8 -> "Text.toUtf8"
  Text_fromUtf8_impl_v3 -> "Text.fromUtf8.impl.v3"
  Tls_ClientConfig_default -> "Tls.ClientConfig.default"
  Tls_ServerConfig_default -> "Tls.ServerConfig.default"
  Tls_ClientConfig_certificates_set -> "Tls.ClientConfig.certificates.set"
  Tls_ServerConfig_certificates_set -> "Tls.ServerConfig.certificates.set"
  TVar_new -> "TVar.new"
  TVar_read -> "TVar.read"
  TVar_write -> "TVar.write"
  TVar_newIO -> "TVar.newIO"
  TVar_readIO -> "TVar.readIO"
  TVar_swap -> "TVar.swap"
  STM_retry -> "STM.retry"
  Promise_new -> "Promise.new"
  Promise_read -> "Promise.read"
  Promise_tryRead -> "Promise.tryRead"
  Promise_write -> "Promise.write"
  Tls_newClient_impl_v3 -> "Tls.newClient.impl.v3"
  Tls_newServer_impl_v3 -> "Tls.newServer.impl.v3"
  Tls_handshake_impl_v3 -> "Tls.handshake.impl.v3"
  Tls_send_impl_v3 -> "Tls.send.impl.v3"
  Tls_decodeCert_impl_v3 -> "Tls.decodeCert.impl.v3"
  Tls_encodeCert -> "Tls.encodeCert"
  Tls_decodePrivateKey -> "Tls.decodePrivateKey"
  Tls_encodePrivateKey -> "Tls.encodePrivateKey"
  Tls_receive_impl_v3 -> "Tls.receive.impl.v3"
  Tls_terminate_impl_v3 -> "Tls.terminate.impl.v3"
  Code_validateLinks -> "Code.validateLinks"
  Code_dependencies -> "Code.dependencies"
  Code_serialize -> "Code.serialize"
  Code_deserialize -> "Code.deserialize"
  Code_display -> "Code.display"
  Value_dependencies -> "Value.dependencies"
  Value_serialize -> "Value.serialize"
  Value_deserialize -> "Value.deserialize"
  Crypto_HashAlgorithm_Sha3_512 -> "crypto.HashAlgorithm.Sha3_512"
  Crypto_HashAlgorithm_Sha3_256 -> "crypto.HashAlgorithm.Sha3_256"
  Crypto_HashAlgorithm_Sha2_512 -> "crypto.HashAlgorithm.Sha2_512"
  Crypto_HashAlgorithm_Sha2_256 -> "crypto.HashAlgorithm.Sha2_256"
  Crypto_HashAlgorithm_Sha1 -> "crypto.HashAlgorithm.Sha1"
  Crypto_HashAlgorithm_Blake2b_512 -> "crypto.HashAlgorithm.Blake2b_512"
  Crypto_HashAlgorithm_Blake2b_256 -> "crypto.HashAlgorithm.Blake2b_256"
  Crypto_HashAlgorithm_Blake2s_256 -> "crypto.HashAlgorithm.Blake2s_256"
  Crypto_HashAlgorithm_Md5 -> "crypto.HashAlgorithm.Md5"
  Crypto_hashBytes -> "crypto.hashBytes"
  Crypto_hmacBytes -> "crypto.hmacBytes"
  Crypto_hash -> "crypto.hash"
  Crypto_hmac -> "crypto.hmac"
  Crypto_Ed25519_sign_impl -> "crypto.Ed25519.sign.impl"
  Crypto_Ed25519_verify_impl -> "crypto.Ed25519.verify.impl"
  Crypto_Rsa_sign_impl -> "crypto.Rsa.sign.impl"
  Crypto_Rsa_verify_impl -> "crypto.Rsa.verify.impl"
  Universal_murmurHash -> "Universal.murmurHash"
  IO_randomBytes -> "IO.randomBytes"
  Bytes_zlib_compress -> "Bytes.zlib.compress"
  Bytes_gzip_compress -> "Bytes.gzip.compress"
  Bytes_zlib_decompress -> "Bytes.zlib.decompress"
  Bytes_gzip_decompress -> "Bytes.gzip.decompress"
  Bytes_toBase16 -> "Bytes.toBase16"
  Bytes_toBase32 -> "Bytes.toBase32"
  Bytes_toBase64 -> "Bytes.toBase64"
  Bytes_toBase64UrlUnpadded -> "Bytes.toBase64UrlUnpadded"
  Bytes_fromBase16 -> "Bytes.fromBase16"
  Bytes_fromBase32 -> "Bytes.fromBase32"
  Bytes_fromBase64 -> "Bytes.fromBase64"
  Bytes_fromBase64UrlUnpadded -> "Bytes.fromBase64UrlUnpadded"
  Bytes_decodeNat64be -> "Bytes.decodeNat64be"
  Bytes_decodeNat64le -> "Bytes.decodeNat64le"
  Bytes_decodeNat32be -> "Bytes.decodeNat32be"
  Bytes_decodeNat32le -> "Bytes.decodeNat32le"
  Bytes_decodeNat16be -> "Bytes.decodeNat16be"
  Bytes_decodeNat16le -> "Bytes.decodeNat16le"
  Bytes_encodeNat64be -> "Bytes.encodeNat64be"
  Bytes_encodeNat64le -> "Bytes.encodeNat64le"
  Bytes_encodeNat32be -> "Bytes.encodeNat32be"
  Bytes_encodeNat32le -> "Bytes.encodeNat32le"
  Bytes_encodeNat16be -> "Bytes.encodeNat16be"
  Bytes_encodeNat16le -> "Bytes.encodeNat16le"
  MutableArray_copyTo_force -> "MutableArray.copyTo!"
  MutableByteArray_copyTo_force -> "MutableByteArray.copyTo!"
  ImmutableArray_copyTo_force -> "ImmutableArray.copyTo!"
  ImmutableArray_size -> "ImmutableArray.size"
  MutableArray_size -> "MutableArray.size"
  ImmutableByteArray_size -> "ImmutableByteArray.size"
  MutableByteArray_size -> "MutableByteArray.size"
  ImmutableByteArray_copyTo_force -> "ImmutableByteArray.copyTo!"
  MutableArray_read -> "MutableArray.read"
  MutableByteArray_read8 -> "MutableByteArray.read8"
  MutableByteArray_read16be -> "MutableByteArray.read16be"
  MutableByteArray_read24be -> "MutableByteArray.read24be"
  MutableByteArray_read32be -> "MutableByteArray.read32be"
  MutableByteArray_read40be -> "MutableByteArray.read40be"
  MutableByteArray_read64be -> "MutableByteArray.read64be"
  MutableArray_write -> "MutableArray.write"
  MutableByteArray_write8 -> "MutableByteArray.write8"
  MutableByteArray_write16be -> "MutableByteArray.write16be"
  MutableByteArray_write32be -> "MutableByteArray.write32be"
  MutableByteArray_write64be -> "MutableByteArray.write64be"
  ImmutableArray_read -> "ImmutableArray.read"
  ImmutableByteArray_read8 -> "ImmutableByteArray.read8"
  ImmutableByteArray_read16be -> "ImmutableByteArray.read16be"
  ImmutableByteArray_read24be -> "ImmutableByteArray.read24be"
  ImmutableByteArray_read32be -> "ImmutableByteArray.read32be"
  ImmutableByteArray_read40be -> "ImmutableByteArray.read40be"
  ImmutableByteArray_read64be -> "ImmutableByteArray.read64be"
  MutableByteArray_freeze_force -> "MutableByteArray.freeze!"
  MutableArray_freeze_force -> "MutableArray.freeze!"
  MutableByteArray_freeze -> "MutableByteArray.freeze"
  MutableArray_freeze -> "MutableArray.freeze"
  MutableByteArray_length -> "MutableByteArray.length"
  ImmutableByteArray_length -> "ImmutableByteArray.length"
  IO_array -> "IO.array"
  IO_arrayOf -> "IO.arrayOf"
  IO_bytearray -> "IO.bytearray"
  IO_bytearrayOf -> "IO.bytearrayOf"
  Scope_array -> "Scope.array"
  Scope_arrayOf -> "Scope.arrayOf"
  Scope_bytearray -> "Scope.bytearray"
  Scope_bytearrayOf -> "Scope.bytearrayOf"
  Text_patterns_literal -> "Text.patterns.literal"
  Text_patterns_digit -> "Text.patterns.digit"
  Text_patterns_letter -> "Text.patterns.letter"
  Text_patterns_space -> "Text.patterns.space"
  Text_patterns_punctuation -> "Text.patterns.punctuation"
  Text_patterns_anyChar -> "Text.patterns.anyChar"
  Text_patterns_eof -> "Text.patterns.eof"
  Text_patterns_charRange -> "Text.patterns.charRange"
  Text_patterns_notCharRange -> "Text.patterns.notCharRange"
  Text_patterns_charIn -> "Text.patterns.charIn"
  Text_patterns_notCharIn -> "Text.patterns.notCharIn"
  Pattern_many -> "Pattern.many"
  Pattern_many_corrected -> "Pattern.many.corrected"
  Pattern_capture -> "Pattern.capture"
  Pattern_captureAs -> "Pattern.captureAs"
  Pattern_join -> "Pattern.join"
  Pattern_or -> "Pattern.or"
  Pattern_replicate -> "Pattern.replicate"
  Pattern_run -> "Pattern.run"
  Pattern_isMatch -> "Pattern.isMatch"
  Char_Class_any -> "Char.Class.any"
  Char_Class_not -> "Char.Class.not"
  Char_Class_and -> "Char.Class.and"
  Char_Class_or -> "Char.Class.or"
  Char_Class_range -> "Char.Class.range"
  Char_Class_anyOf -> "Char.Class.anyOf"
  Char_Class_alphanumeric -> "Char.Class.alphanumeric"
  Char_Class_upper -> "Char.Class.upper"
  Char_Class_lower -> "Char.Class.lower"
  Char_Class_whitespace -> "Char.Class.whitespace"
  Char_Class_control -> "Char.Class.control"
  Char_Class_printable -> "Char.Class.printable"
  Char_Class_mark -> "Char.Class.mark"
  Char_Class_number -> "Char.Class.number"
  Char_Class_punctuation -> "Char.Class.punctuation"
  Char_Class_symbol -> "Char.Class.symbol"
  Char_Class_separator -> "Char.Class.separator"
  Char_Class_letter -> "Char.Class.letter"
  Char_Class_is -> "Char.Class.is"
  Text_patterns_char -> "Text.patterns.char"
