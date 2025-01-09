{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Runtime.Foreign
  ( Foreign (..),
    HashAlgorithm (..),
    unwrapForeign,
    maybeUnwrapForeign,
    wrapBuiltin,
    maybeUnwrapBuiltin,
    unwrapBuiltin,
    BuiltinForeign (..),
    Tls (..),
    Failure (..),
  )
where

import Control.Concurrent (MVar, ThreadId)
import Control.Concurrent.STM (TVar)
import Crypto.Hash qualified as Hash
import Data.Atomics qualified as Atomic
import Data.IORef (IORef)
import Data.Tagged (Tagged (..))
import Data.X509 qualified as X509
import Network.Socket (Socket)
import Network.TLS qualified as TLS (ClientParams, Context, ServerParams)
import Network.UDP (ClientSockAddr, ListenSocket, UDPSocket)
import System.Clock (TimeSpec)
import System.IO (Handle)
import System.Process (ProcessHandle)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Runtime.ANF (Code, Value)
import Unison.Runtime.Array
import Unison.Type qualified as Ty
import Unison.Util.Bytes (Bytes)
import Unison.Util.RefPromise (Promise)
import Unison.Util.Text (Text)
import Unison.Util.Text.Pattern (CPattern, CharPattern)
import Unsafe.Coerce

data Foreign where
  Wrap :: Reference -> !e -> Foreign

promote :: (a -> a -> r) -> b -> c -> r
promote (~~) x y = unsafeCoerce x ~~ unsafeCoerce y

-- These functions are explicit aliases of the overloaded function.
-- When the overloaded function is used in their place, it seems to
-- cause issues with regard to `promote` above. Somehow, the
-- unsafeCoerce can cause memory faults, even when the values are
-- being coerced to appropriate types. Having an explicit, noinline
-- alias seems to prevent the faults.
txtEq :: Text -> Text -> Bool
txtEq l r = l == r
{-# NOINLINE txtEq #-}

txtCmp :: Text -> Text -> Ordering
txtCmp l r = compare l r
{-# NOINLINE txtCmp #-}

bytesEq :: Bytes -> Bytes -> Bool
bytesEq l r = l == r
{-# NOINLINE bytesEq #-}

bytesCmp :: Bytes -> Bytes -> Ordering
bytesCmp l r = compare l r
{-# NOINLINE bytesCmp #-}

mvarEq :: MVar () -> MVar () -> Bool
mvarEq l r = l == r
{-# NOINLINE mvarEq #-}

tvarEq :: TVar () -> TVar () -> Bool
tvarEq l r = l == r
{-# NOINLINE tvarEq #-}

socketEq :: Socket -> Socket -> Bool
socketEq l r = l == r
{-# NOINLINE socketEq #-}

udpSocketEq :: UDPSocket -> UDPSocket -> Bool
udpSocketEq l r = l == r
{-# NOINLINE udpSocketEq #-}

refEq :: IORef () -> IORef () -> Bool
refEq l r = l == r
{-# NOINLINE refEq #-}

tidEq :: ThreadId -> ThreadId -> Bool
tidEq l r = l == r
{-# NOINLINE tidEq #-}

tidCmp :: ThreadId -> ThreadId -> Ordering
tidCmp l r = compare l r
{-# NOINLINE tidCmp #-}

marrEq :: MutableArray () () -> MutableArray () () -> Bool
marrEq l r = l == r
{-# NOINLINE marrEq #-}

mbarrEq :: MutableByteArray () -> MutableByteArray () -> Bool
mbarrEq l r = l == r
{-# NOINLINE mbarrEq #-}

barrEq :: ByteArray -> ByteArray -> Bool
barrEq l r = l == r
{-# NOINLINE barrEq #-}

barrCmp :: ByteArray -> ByteArray -> Ordering
barrCmp l r = compare l r
{-# NOINLINE barrCmp #-}

cpatEq :: CPattern -> CPattern -> Bool
cpatEq l r = l == r
{-# NOINLINE cpatEq #-}

cpatCmp :: CPattern -> CPattern -> Ordering
cpatCmp l r = compare l r
{-# NOINLINE cpatCmp #-}

charClassEq :: CharPattern -> CharPattern -> Bool
charClassEq l r = l == r
{-# NOINLINE charClassEq #-}

charClassCmp :: CharPattern -> CharPattern -> Ordering
charClassCmp = compare
{-# NOINLINE charClassCmp #-}

codeEq :: Code -> Code -> Bool
codeEq co1 co2 = co1 == co2
{-# NOINLINE codeEq #-}

tylEq :: Reference -> Reference -> Bool
tylEq r l = r == l
{-# NOINLINE tylEq #-}

tmlEq :: Referent -> Referent -> Bool
tmlEq r l = r == l
{-# NOINLINE tmlEq #-}

tylCmp :: Reference -> Reference -> Ordering
tylCmp r l = compare r l
{-# NOINLINE tylCmp #-}

tmlCmp :: Referent -> Referent -> Ordering
tmlCmp r l = compare r l
{-# NOINLINE tmlCmp #-}

ref2eq :: Reference -> Maybe (a -> b -> Bool)
ref2eq r
  | r == Ty.textRef = Just $ promote txtEq
  | r == Ty.termLinkRef = Just $ promote tmlEq
  | r == Ty.typeLinkRef = Just $ promote tylEq
  | r == Ty.bytesRef = Just $ promote bytesEq
  -- Note: MVar equality is just reference equality, so it shouldn't
  -- matter what type the MVar holds.
  | r == Ty.mvarRef = Just $ promote mvarEq
  -- Ditto
  | r == Ty.tvarRef = Just $ promote tvarEq
  | r == Ty.socketRef = Just $ promote socketEq
  | r == Ty.udpSocketRef = Just $ promote udpSocketEq
  | r == Ty.refRef = Just $ promote refEq
  | r == Ty.threadIdRef = Just $ promote tidEq
  | r == Ty.marrayRef = Just $ promote marrEq
  | r == Ty.mbytearrayRef = Just $ promote mbarrEq
  | r == Ty.ibytearrayRef = Just $ promote barrEq
  | r == Ty.patternRef = Just $ promote cpatEq
  | r == Ty.charClassRef = Just $ promote charClassEq
  | r == Ty.codeRef = Just $ promote codeEq
  | otherwise = Nothing

ref2cmp :: Reference -> Maybe (a -> b -> Ordering)
ref2cmp r
  | r == Ty.textRef = Just $ promote txtCmp
  | r == Ty.termLinkRef = Just $ promote tmlCmp
  | r == Ty.typeLinkRef = Just $ promote tylCmp
  | r == Ty.bytesRef = Just $ promote bytesCmp
  | r == Ty.threadIdRef = Just $ promote tidCmp
  | r == Ty.ibytearrayRef = Just $ promote barrCmp
  | r == Ty.patternRef = Just $ promote cpatCmp
  | r == Ty.charClassRef = Just $ promote charClassCmp
  | otherwise = Nothing

instance Eq Foreign where
  Wrap rl t == Wrap rr u
    | rl == rr, Just (~~) <- ref2eq rl = t ~~ u
  Wrap rl1 _ == Wrap rl2 _ =
    error $
      "Attempting to check equality of two values of different types: "
        <> show (rl1, rl2)

instance Ord Foreign where
  Wrap rl t `compare` Wrap rr u
    | rl == rr, Just cmp <- ref2cmp rl = cmp t u
  compare (Wrap rl1 _) (Wrap rl2 _) =
    error $
      "Attempting to compare two values of different types: "
        <> show (rl1, rl2)

instance Show Foreign where
  showsPrec p !(Wrap r v) =
    showParen (p > 9) $
      showString "Wrap " . showsPrec 10 r . showString " " . contents
    where
      contents
        | r == Ty.textRef = shows @Text (unsafeCoerce v)
        | otherwise = showString "_"

unwrapForeign :: Foreign -> a
unwrapForeign (Wrap _ e) = unsafeCoerce e

maybeUnwrapForeign :: Reference -> Foreign -> Maybe a
maybeUnwrapForeign rt (Wrap r e)
  | rt == r = Just (unsafeCoerce e)
  | otherwise = Nothing
{-# NOINLINE maybeUnwrapForeign #-}

class BuiltinForeign f where
  foreignName :: Tagged f String
  foreignRef :: Tagged f Reference

instance BuiltinForeign Text where
  foreignName = Tagged "Text"
  foreignRef :: Tagged Text Reference
  foreignRef = Tagged Ty.textRef

instance BuiltinForeign Bytes where
  foreignName = Tagged "Bytes"
  foreignRef = Tagged Ty.bytesRef

instance BuiltinForeign Handle where
  foreignName = Tagged "Handle"
  foreignRef = Tagged Ty.fileHandleRef

instance BuiltinForeign ProcessHandle where
  foreignName = Tagged "ProcessHandle"
  foreignRef = Tagged Ty.processHandleRef

instance BuiltinForeign Referent where
  foreignName = Tagged "Referent"
  foreignRef = Tagged Ty.termLinkRef

instance BuiltinForeign Socket where
  foreignName = Tagged "Socket"
  foreignRef = Tagged Ty.socketRef

instance BuiltinForeign ListenSocket where
  foreignName = Tagged "ListenSocket"
  foreignRef = Tagged Ty.udpListenSocketRef

instance BuiltinForeign ClientSockAddr where
  foreignName = Tagged "ClientSockAddr"
  foreignRef = Tagged Ty.udpClientSockAddrRef

instance BuiltinForeign UDPSocket where
  foreignName = Tagged "UDPSocket"
  foreignRef = Tagged Ty.udpSocketRef

instance BuiltinForeign ThreadId where
  foreignName = Tagged "ThreadId"
  foreignRef = Tagged Ty.threadIdRef

instance BuiltinForeign TLS.ClientParams where
  foreignName = Tagged "ClientParams"
  foreignRef = Tagged Ty.tlsClientConfigRef

instance BuiltinForeign TLS.ServerParams where
  foreignName = Tagged "ServerParams"
  foreignRef = Tagged Ty.tlsServerConfigRef

instance BuiltinForeign X509.SignedCertificate where
  foreignName = Tagged "X509.SignedCertificate"
  foreignRef = Tagged Ty.tlsSignedCertRef

instance BuiltinForeign X509.PrivKey where
  foreignName = Tagged "X509.PrivKey"
  foreignRef = Tagged Ty.tlsPrivateKeyRef

instance BuiltinForeign FilePath where
  foreignName = Tagged "FilePath"
  foreignRef = Tagged Ty.filePathRef

instance BuiltinForeign TLS.Context where
  foreignName = Tagged "TLS.Context"
  foreignRef = Tagged Ty.tlsRef

instance BuiltinForeign Code where
  foreignName = Tagged "Code"
  foreignRef = Tagged Ty.codeRef

instance BuiltinForeign Value where
  foreignName = Tagged "Value"
  foreignRef = Tagged Ty.valueRef

instance BuiltinForeign TimeSpec where
  foreignName = Tagged "TimeSpec"
  foreignRef = Tagged Ty.timeSpecRef

instance BuiltinForeign (Atomic.Ticket a) where
  foreignName = Tagged "Ticket"
  foreignRef = Tagged Ty.ticketRef

instance BuiltinForeign (MVar a) where
  foreignName = Tagged "MVar"
  foreignRef = Tagged Ty.mvarRef

instance BuiltinForeign (TVar a) where
  foreignName = Tagged "TVar"
  foreignRef = Tagged Ty.tvarRef

instance BuiltinForeign (Promise a) where
  foreignName = Tagged "Promise"
  foreignRef = Tagged Ty.promiseRef

instance BuiltinForeign (MutableArray s e) where
  foreignName = Tagged "MutableArray"
  foreignRef = Tagged Ty.marrayRef

instance BuiltinForeign (Array e) where
  foreignName = Tagged "Array"
  foreignRef = Tagged Ty.iarrayRef

instance BuiltinForeign (MutableByteArray s) where
  foreignName = Tagged "MutableByteArray"
  foreignRef = Tagged Ty.mbytearrayRef

instance BuiltinForeign ByteArray where
  foreignName = Tagged "ByteArray"
  foreignRef = Tagged Ty.ibytearrayRef

data HashAlgorithm where
  -- Reference is a reference to the hash algorithm
  HashAlgorithm :: (Hash.HashAlgorithm a) => Reference -> a -> HashAlgorithm

newtype Tls = Tls TLS.Context

data Failure a = Failure Reference Text a

instance BuiltinForeign HashAlgorithm where
  foreignName = Tagged "HashAlgorithm"
  foreignRef = Tagged Ty.hashAlgorithmRef

instance BuiltinForeign CPattern where
  foreignName = Tagged "CPattern"
  foreignRef = Tagged Ty.patternRef

instance BuiltinForeign CharPattern where
  foreignName = Tagged "CharPattern"
  foreignRef = Tagged Ty.charClassRef

wrapBuiltin :: forall f. (BuiltinForeign f) => f -> Foreign
wrapBuiltin x = Wrap r x
  where
    Tagged r = foreignRef :: Tagged f Reference

unwrapBuiltin :: (BuiltinForeign f) => Foreign -> f
unwrapBuiltin (Wrap _ x) = unsafeCoerce x

maybeUnwrapBuiltin :: forall f. (BuiltinForeign f) => Foreign -> Maybe f
maybeUnwrapBuiltin (Wrap r x)
  | r == r0 = Just (unsafeCoerce x)
  | otherwise = Nothing
  where
    Tagged r0 = foreignRef :: Tagged f Reference
