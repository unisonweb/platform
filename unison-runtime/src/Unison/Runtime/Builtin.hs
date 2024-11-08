{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Unison.Runtime.Builtin
  ( builtinLookup,
    builtinTermNumbering,
    builtinTypeNumbering,
    builtinTermBackref,
    builtinTypeBackref,
    builtinForeigns,
    builtinArities,
    builtinInlineInfo,
    sandboxedForeigns,
    numberedTermLookup,
    Sandbox (..),
    baseSandboxInfo,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent as SYS
  ( killThread,
    threadDelay,
  )
import Control.Concurrent.MVar as SYS
import Control.Concurrent.STM qualified as STM
import Control.DeepSeq (NFData)
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
import Unison.Runtime.Builtin.Types
import Unison.Runtime.Crypto.Rsa as Rsa
import Unison.Runtime.Exception (die)
import Unison.Runtime.Foreign
  ( Foreign (Wrap),
    HashAlgorithm (..),
    pattern Failure,
  )
import Unison.Runtime.Foreign qualified as F
import Unison.Runtime.Foreign.Function
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

type Failure = F.Failure Val

freshes :: (Var v) => Int -> [v]
freshes = freshes' mempty

freshes' :: (Var v) => Set v -> Int -> [v]
freshes' avoid0 = go avoid0 []
  where
    go _ vs 0 = vs
    go avoid vs n =
      let v = freshIn avoid $ typed ANFBlank
       in go (insert v avoid) (v : vs) (n - 1)

class Fresh t where fresh :: t

fresh1 :: (Var v) => v
fresh1 = head $ freshes 1

instance (Var v) => Fresh (v, v) where
  fresh = (v1, v2)
    where
      [v1, v2] = freshes 2

instance (Var v) => Fresh (v, v, v) where
  fresh = (v1, v2, v3)
    where
      [v1, v2, v3] = freshes 3

instance (Var v) => Fresh (v, v, v, v) where
  fresh = (v1, v2, v3, v4)
    where
      [v1, v2, v3, v4] = freshes 4

instance (Var v) => Fresh (v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5)
    where
      [v1, v2, v3, v4, v5] = freshes 5

instance (Var v) => Fresh (v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6)
    where
      [v1, v2, v3, v4, v5, v6] = freshes 6

instance (Var v) => Fresh (v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7)
    where
      [v1, v2, v3, v4, v5, v6, v7] = freshes 7

instance (Var v) => Fresh (v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8] = freshes 8

instance (Var v) => Fresh (v, v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8, v9)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8, v9] = freshes 9

instance (Var v) => Fresh (v, v, v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10] = freshes 10

instance (Var v) => Fresh (v, v, v, v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11] = freshes 11

instance (Var v) => Fresh (v, v, v, v, v, v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13] = freshes 13

instance (Var v) => Fresh (v, v, v, v, v, v, v, v, v, v, v, v, v, v) where
  fresh = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)
    where
      [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14] = freshes 14

fls, tru :: (Var v) => ANormal v
fls = TCon Ty.booleanRef 0 []
tru = TCon Ty.booleanRef 1 []

none :: (Var v) => ANormal v
none = TCon Ty.optionalRef (fromIntegral Ty.noneId) []

some, left, right :: (Var v) => v -> ANormal v
some a = TCon Ty.optionalRef (fromIntegral Ty.someId) [a]
left x = TCon Ty.eitherRef (fromIntegral Ty.eitherLeftId) [x]
right x = TCon Ty.eitherRef (fromIntegral Ty.eitherRightId) [x]

seqViewEmpty :: (Var v) => ANormal v
seqViewEmpty = TCon Ty.seqViewRef (fromIntegral Ty.seqViewEmpty) []

seqViewElem :: (Var v) => v -> v -> ANormal v
seqViewElem l r = TCon Ty.seqViewRef (fromIntegral Ty.seqViewElem) [l, r]

boolift :: (Var v) => v -> ANormal v
boolift v =
  TMatch v $ MatchIntegral (mapFromList [(0, fls), (1, tru)]) Nothing

notlift :: (Var v) => v -> ANormal v
notlift v =
  TMatch v $ MatchIntegral (mapFromList [(1, fls), (0, tru)]) Nothing

unenum :: (Var v) => Int -> v -> Reference -> v -> ANormal v -> ANormal v
unenum n v0 r v nx =
  TMatch v0 $ MatchData r cases Nothing
  where
    mkCase i = (toEnum i, ([], TLetD v UN (TLit . I $ fromIntegral i) nx))
    cases = mapFromList . fmap mkCase $ [0 .. n - 1]

unop0 :: (Var v) => Int -> ([v] -> ANormal v) -> SuperNormal v
unop0 n f =
  Lambda [BX]
    . TAbss [x0]
    $ f xs
  where
    xs@(x0 : _) = freshes (1 + n)

binop0 :: (Var v) => Int -> ([v] -> ANormal v) -> SuperNormal v
binop0 n f =
  Lambda [BX, BX]
    . TAbss [x0, y0]
    $ f xs
  where
    xs@(x0 : y0 : _) = freshes (2 + n)

unop :: (Var v) => POp -> SuperNormal v
unop pop =
  unop0 0 $ \[x] ->
    (TPrm pop [x])

binop ::
  (Var v) =>
  POp ->
  SuperNormal v
binop pop =
  binop0 0 $ \[x, y] -> TPrm pop [x, y]

-- | Like `binop`, but swaps the arguments.
binopSwap :: (Var v) => POp -> SuperNormal v
binopSwap pop =
  binop0 0 $ \[x, y] -> TPrm pop [y, x]

-- | Like `binop`, but swaps the arguments.
cmpopb :: (Var v) => POp -> SuperNormal v
cmpopb pop =
  binop0 1 $ \[x, y, b] ->
    TLetD b UN (TPrm pop [y, x]) $
      boolift b

-- | Like `binop`, but swaps arguments then negates the result.
cmpopbn :: (Var v) => POp -> SuperNormal v
cmpopbn pop =
  binop0 1 $ \[x, y, b] ->
    TLetD b UN (TPrm pop [y, x]) $
      notlift b

addi, subi, muli, divi, modi, shli, shri, powi :: (Var v) => SuperNormal v
addi = binop ADDI
subi = binop SUBI
muli = binop MULI
divi = binop DIVI
modi = binop MODI
shli = binop SHLI
shri = binop SHRI
powi = binop POWI

addn, subn, muln, divn, modn, shln, shrn, pown, dropn :: (Var v) => SuperNormal v
addn = binop ADDN
subn = binop SUBN
muln = binop MULN
divn = binop DIVN
modn = binop MODN
shln = binop SHLN
shrn = binop SHRN
pown = binop POWN
dropn = binop DRPN

eqi, eqn, lti, ltn, lei, len :: (Var v) => SuperNormal v
eqi = binop EQLI
lti = binop LESI
lei = binop LEQI
eqn = binop EQLN
ltn = binop LESN
len = binop LEQN

gti, gtn, gei, gen :: (Var v) => SuperNormal v
gti = binopSwap LESI
gei = cmpopb LEQI
gtn = binopSwap LESN
gen = cmpopb LEQN

inci, incn :: (Var v) => SuperNormal v
inci = unop INCI
incn = unop INCN

sgni, negi :: (Var v) => SuperNormal v
sgni = unop SGNI
negi = unop NEGI

lzeron, tzeron, lzeroi, tzeroi, popn, popi :: (Var v) => SuperNormal v
lzeron = unop LZRO
tzeron = unop TZRO
popn = unop POPC
popi = unop POPC
lzeroi = unop LZRO
tzeroi = unop TZRO

andn, orn, xorn, compln, andi, ori, xori, compli :: (Var v) => SuperNormal v
andn = binop ANDN
orn = binop IORN
xorn = binop XORN
compln = unop COMN
andi = binop ANDI
ori = binop IORI
xori = binop XORI
compli = unop COMI

addf,
  subf,
  mulf,
  divf,
  powf,
  sqrtf,
  logf,
  logbf ::
    (Var v) => SuperNormal v
addf = binop ADDF
subf = binop SUBF
mulf = binop MULF
divf = binop DIVF
powf = binop POWF
sqrtf = unop SQRT
logf = unop LOGF
logbf = binop LOGB

expf, absf :: (Var v) => SuperNormal v
expf = unop EXPF
absf = unop ABSF

cosf, sinf, tanf, acosf, asinf, atanf :: (Var v) => SuperNormal v
cosf = unop COSF
sinf = unop SINF
tanf = unop TANF
acosf = unop ACOS
asinf = unop ASIN
atanf = unop ATAN

coshf,
  sinhf,
  tanhf,
  acoshf,
  asinhf,
  atanhf,
  atan2f ::
    (Var v) => SuperNormal v
coshf = unop COSH
sinhf = unop SINH
tanhf = unop TANH
acoshf = unop ACSH
asinhf = unop ASNH
atanhf = unop ATNH
atan2f = binop ATN2

ltf, gtf, lef, gef, eqf, neqf :: (Var v) => SuperNormal v
ltf = cmpopbn LEQF
gtf = binopSwap LESF
lef = binop LEQF
gef = cmpopb LEQF
eqf = binop EQLF
neqf = binop NEQF

minf, maxf :: (Var v) => SuperNormal v
minf = binop MINF
maxf = binop MAXF

ceilf, floorf, truncf, roundf, i2f, n2f :: (Var v) => SuperNormal v
ceilf = unop CEIL
floorf = unop FLOR
truncf = unop TRNF
roundf = unop RNDF
i2f = unop ITOF
n2f = unop NTOF

trni :: (Var v) => SuperNormal v
trni = unop TRNC

modular :: (Var v) => POp -> (Bool -> ANormal v) -> SuperNormal v
modular pop ret =
  unop0 2 $ \[x, m, t] ->
    TLetD t UN (TLit $ I 2)
      . TLetD m UN (TPrm pop [x, t])
      . TMatch m
      $ MatchIntegral
        (mapSingleton 1 $ ret True)
        (Just $ ret False)

evni, evnn, oddi, oddn :: (Var v) => SuperNormal v
evni = modular MODI (\b -> if b then fls else tru)
oddi = modular MODI (\b -> if b then tru else fls)
evnn = modular MODN (\b -> if b then fls else tru)
oddn = modular MODN (\b -> if b then tru else fls)

appendt, taket, dropt, indext, indexb, sizet, unconst, unsnoct :: (Var v) => SuperNormal v
appendt = binop0 0 $ \[x, y] -> TPrm CATT [x, y]
taket = binop0 0 $ \[x, y] ->
  TPrm TAKT [x, y]
dropt = binop0 0 $ \[x, y] ->
  TPrm DRPT [x, y]

atb = binop0 2 $ \[n, b, t, r] ->
  TLetD t UN (TPrm IDXB [n, b])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs r $ some r
          )
        )
      ]

indext = binop0 2 $ \[x, y, t, r] ->
  TLetD t UN (TPrm IXOT [x, y])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs r $ some r
          )
        )
      ]

indexb = binop0 2 $ \[x, y, t, r] ->
  TLetD t UN (TPrm IXOB [x, y])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs r $ some r
          )
        )
      ]

sizet = unop0 0 $ \[x] -> TPrm SIZT [x]

unconst = unop0 6 $ \[x, t, c, y, p, u, yp] ->
  TLetD t UN (TPrm UCNS [x])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN, BX],
            TAbss [c, y]
              . TLetD u BX (TCon Ty.unitRef 0 [])
              . TLetD yp BX (TCon Ty.pairRef 0 [y, u])
              . TLetD p BX (TCon Ty.pairRef 0 [c, yp])
              $ some p
          )
        )
      ]

unsnoct = unop0 6 $ \[x, t, c, y, p, u, cp] ->
  TLetD t UN (TPrm USNC [x])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [BX, UN],
            TAbss [y, c]
              . TLetD u BX (TCon Ty.unitRef 0 [])
              . TLetD cp BX (TCon Ty.pairRef 0 [c, u])
              . TLetD p BX (TCon Ty.pairRef 0 [y, cp])
              $ some p
          )
        )
      ]

appends, conss, snocs :: (Var v) => SuperNormal v
appends = binop0 0 $ \[x, y] -> TPrm CATS [x, y]
conss = binop0 0 $ \[x, y] -> TPrm CONS [x, y]
snocs = binop0 0 $ \[x, y] -> TPrm SNOC [x, y]

takes, drops, sizes, ats, emptys :: (Var v) => SuperNormal v
takes = binop0 0 $ \[x, y] -> TPrm TAKS [x, y]
drops = binop0 0 $ \[x, y] -> TPrm DRPS [x, y]
sizes = unop0 0 $ \[x] -> (TPrm SIZS [x])
ats = binop0 2 $ \[x, y, t, r] ->
  TLetD t UN (TPrm IDXS [x, y])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        (1, ([BX], TAbs r $ some r))
      ]
emptys = Lambda [] $ TPrm BLDS []

viewls, viewrs :: (Var v) => SuperNormal v
viewls = unop0 3 $ \[s, u, h, t] ->
  TLetD u UN (TPrm VWLS [s])
    . TMatch u
    . MatchSum
    $ mapFromList
      [ (0, ([], seqViewEmpty)),
        (1, ([BX, BX], TAbss [h, t] $ seqViewElem h t))
      ]
viewrs = unop0 3 $ \[s, u, i, l] ->
  TLetD u UN (TPrm VWRS [s])
    . TMatch u
    . MatchSum
    $ mapFromList
      [ (0, ([], seqViewEmpty)),
        (1, ([BX, BX], TAbss [i, l] $ seqViewElem i l))
      ]

splitls, splitrs :: (Var v) => SuperNormal v
splitls = binop0 3 $ \[n, s, t, l, r] ->
  TLetD t UN (TPrm SPLL [n, s])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], seqViewEmpty)),
        (1, ([BX, BX], TAbss [l, r] $ seqViewElem l r))
      ]
splitrs = binop0 3 $ \[n, s, t, l, r] ->
  TLetD t UN (TPrm SPLR [n, s])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], seqViewEmpty)),
        (1, ([BX, BX], TAbss [l, r] $ seqViewElem l r))
      ]

eqt, neqt, leqt, geqt, lesst, great :: SuperNormal Symbol
eqt = binop0 1 $ \[x, y, b] ->
  TLetD b UN (TPrm EQLT [x, y]) $
    boolift b
neqt = binop0 1 $ \[x, y, b] ->
  TLetD b UN (TPrm EQLT [x, y]) $
    notlift b
leqt = binop0 1 $ \[x, y, b] ->
  TLetD b UN (TPrm LEQT [x, y]) $
    boolift b
geqt = binop0 1 $ \[x, y, b] ->
  TLetD b UN (TPrm LEQT [y, x]) $
    boolift b
lesst = binop0 1 $ \[x, y, b] ->
  TLetD b UN (TPrm LEQT [y, x]) $
    notlift b
great = binop0 1 $ \[x, y, b] ->
  TLetD b UN (TPrm LEQT [x, y]) $
    notlift b

packt, unpackt :: SuperNormal Symbol
packt = unop0 0 $ \[s] -> TPrm PAKT [s]
unpackt = unop0 0 $ \[t] -> TPrm UPKT [t]

packb, unpackb, emptyb, appendb :: SuperNormal Symbol
packb = unop0 0 $ \[s] -> TPrm PAKB [s]
unpackb = unop0 0 $ \[b] -> TPrm UPKB [b]
emptyb =
  Lambda []
    . TLetD es BX (TPrm BLDS [])
    $ TPrm PAKB [es]
  where
    es = fresh1
appendb = binop0 0 $ \[x, y] -> TPrm CATB [x, y]

takeb, dropb, atb, sizeb, flattenb :: SuperNormal Symbol
takeb = binop0 0 $ \[n, b] -> TPrm TAKB [n, b]
dropb = binop0 0 $ \[n, b] -> TPrm DRPB [n, b]
sizeb = unop0 0 $ \[b] -> (TPrm SIZB [b])
flattenb = unop0 0 $ \[b] -> TPrm FLTB [b]

i2t, n2t, f2t :: SuperNormal Symbol
i2t = unop0 0 $ \[n] -> TPrm ITOT [n]
n2t = unop0 0 $ \[n] -> TPrm NTOT [n]
f2t = unop0 0 $ \[f] -> TPrm FTOT [f]

t2i, t2n, t2f :: SuperNormal Symbol
t2i = unop0 2 $ \[x, t, n] ->
  TLetD t UN (TPrm TTOI [x])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs n $ some n
          )
        )
      ]
t2n = unop0 2 $ \[x, t, n] ->
  TLetD t UN (TPrm TTON [x])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs n $ some n
          )
        )
      ]
t2f = unop0 2 $ \[x, t, f] ->
  TLetD t UN (TPrm TTOF [x])
    . TMatch t
    . MatchSum
    $ mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN],
            TAbs f $ some f
          )
        )
      ]

equ :: SuperNormal Symbol
equ = binop0 1 $ \[x, y, b] ->
  TLetD b UN (TPrm EQLU [x, y]) $
    boolift b

cmpu :: SuperNormal Symbol
cmpu = binop0 1 $ \[x, y, c] ->
  TLetD c UN (TPrm CMPU [x, y]) $
    (TPrm DECI [c])

ltu :: SuperNormal Symbol
ltu = binop0 1 $ \[x, y, c] ->
  TLetD c UN (TPrm CMPU [x, y])
    . TMatch c
    $ MatchIntegral
      (mapFromList [(0, TCon Ty.booleanRef 1 [])])
      (Just $ TCon Ty.booleanRef 0 [])

gtu :: SuperNormal Symbol
gtu = binop0 1 $ \[x, y, c] ->
  TLetD c UN (TPrm CMPU [x, y])
    . TMatch c
    $ MatchIntegral
      (mapFromList [(2, TCon Ty.booleanRef 1 [])])
      (Just $ TCon Ty.booleanRef 0 [])

geu :: SuperNormal Symbol
geu = binop0 1 $ \[x, y, c] ->
  TLetD c UN (TPrm CMPU [x, y])
    . TMatch c
    $ MatchIntegral
      (mapFromList [(0, TCon Ty.booleanRef 0 [])])
      (Just $ TCon Ty.booleanRef 1 [])

leu :: SuperNormal Symbol
leu = binop0 1 $ \[x, y, c] ->
  TLetD c UN (TPrm CMPU [x, y])
    . TMatch c
    $ MatchIntegral
      (mapFromList [(2, TCon Ty.booleanRef 0 [])])
      (Just $ TCon Ty.booleanRef 1 [])

notb :: SuperNormal Symbol
notb = unop0 0 $ \[b] ->
  TMatch b . flip (MatchData Ty.booleanRef) Nothing $
    mapFromList [(0, ([], tru)), (1, ([], fls))]

orb :: SuperNormal Symbol
orb = binop0 0 $ \[p, q] ->
  TMatch p . flip (MatchData Ty.booleanRef) Nothing $
    mapFromList [(1, ([], tru)), (0, ([], TVar q))]

andb :: SuperNormal Symbol
andb = binop0 0 $ \[p, q] ->
  TMatch p . flip (MatchData Ty.booleanRef) Nothing $
    mapFromList [(0, ([], fls)), (1, ([], TVar q))]

-- A runtime type-cast. Used to unsafely coerce between unboxed
-- types at runtime without changing their representation.
coerceType :: UnboxedTypeTag -> SuperNormal Symbol
coerceType destType =
  unop0 1 $ \[v, tag] ->
    TLetD tag UN (TLit $ I $ fromIntegral $ unboxedTypeTagToInt destType) $
      TPrm CAST [v, tag]

-- This version of unsafeCoerce is the identity function. It works
-- only if the two types being coerced between are actually the same,
-- because it keeps the same representation. It is not capable of
-- e.g. correctly translating between two types with compatible bit
-- representations, because tagging information will be retained.
poly'coerce :: SuperNormal Symbol
poly'coerce = unop0 0 $ \[x] -> TVar x

jumpk :: SuperNormal Symbol
jumpk = binop0 0 $ \[k, a] -> TKon k [a]

scope'run :: SuperNormal Symbol
scope'run =
  unop0 1 $ \[e, un] ->
    TLetD un BX (TCon Ty.unitRef 0 []) $
      TApp (FVar e) [un]

fork'comp :: SuperNormal Symbol
fork'comp =
  Lambda [BX]
    . TAbs act
    . TLetD unit BX (TCon Ty.unitRef 0 [])
    . TName lz (Right act) [unit]
    $ TPrm FORK [lz]
  where
    (act, unit, lz) = fresh

try'eval :: SuperNormal Symbol
try'eval =
  Lambda [BX]
    . TAbs act
    . TLetD unit BX (TCon Ty.unitRef 0 [])
    . TName lz (Right act) [unit]
    . TLetD ta UN (TPrm TFRC [lz])
    . TMatch ta
    . MatchSum
    $ mapFromList
      [ exnCase lnk msg xtra any fail,
        (1, ([BX], TAbs r (TVar r)))
      ]
  where
    (act, unit, lz, ta, lnk, msg, xtra, any, fail, r) = fresh

bug :: Util.Text.Text -> SuperNormal Symbol
bug name =
  unop0 1 $ \[x, n] ->
    TLetD n BX (TLit $ T name) $
      TPrm EROR [n, x]

watch :: SuperNormal Symbol
watch =
  binop0 0 $ \[t, v] ->
    TLets Direct [] [] (TPrm PRNT [t]) $
      TVar v

raise :: SuperNormal Symbol
raise =
  unop0 3 $ \[r, f, n, k] ->
    TMatch r
      . flip MatchRequest (TAbs f $ TVar f)
      . Map.singleton Ty.exceptionRef
      $ mapSingleton
        0
        ( [BX],
          TAbs f
            . TShift Ty.exceptionRef k
            . TLetD n BX (TLit $ T "builtin.raise")
            $ TPrm EROR [n, f]
        )

gen'trace :: SuperNormal Symbol
gen'trace =
  binop0 0 $ \[t, v] ->
    TLets Direct [] [] (TPrm TRCE [t, v]) $
      TCon Ty.unitRef 0 []

debug'text :: SuperNormal Symbol
debug'text =
  unop0 3 $ \[c, r, t, e] ->
    TLetD r UN (TPrm DBTX [c])
      . TMatch r
      . MatchSum
      $ mapFromList
        [ (0, ([], none)),
          (1, ([BX], TAbs t . TLetD e BX (left t) $ some e)),
          (2, ([BX], TAbs t . TLetD e BX (right t) $ some e))
        ]

code'missing :: SuperNormal Symbol
code'missing =
  unop0 1 $ \[link, b] ->
    TLetD b UN (TPrm MISS [link]) $
      boolift b

code'cache :: SuperNormal Symbol
code'cache = unop0 0 $ \[new] -> TPrm CACH [new]

code'lookup :: SuperNormal Symbol
code'lookup =
  unop0 2 $ \[link, t, r] ->
    TLetD t UN (TPrm LKUP [link])
      . TMatch t
      . MatchSum
      $ mapFromList
        [ (0, ([], none)),
          (1, ([BX], TAbs r $ some r))
        ]

code'validate :: SuperNormal Symbol
code'validate =
  unop0 6 $ \[item, t, ref, msg, extra, any, fail] ->
    TLetD t UN (TPrm CVLD [item])
      . TMatch t
      . MatchSum
      $ mapFromList
        [ ( 1,
            ([BX, BX, BX],)
              . TAbss [ref, msg, extra]
              . TLetD any BX (TCon Ty.anyRef 0 [extra])
              . TLetD fail BX (TCon Ty.failureRef 0 [ref, msg, any])
              $ some fail
          ),
          ( 0,
            ([],) $
              none
          )
        ]

term'link'to'text :: SuperNormal Symbol
term'link'to'text =
  unop0 0 $ \[link] -> TPrm TLTT [link]

value'load :: SuperNormal Symbol
value'load =
  unop0 2 $ \[vlu, t, r] ->
    TLetD t UN (TPrm LOAD [vlu])
      . TMatch t
      . MatchSum
      $ mapFromList
        [ (0, ([BX], TAbs r $ left r)),
          (1, ([BX], TAbs r $ right r))
        ]

value'create :: SuperNormal Symbol
value'create = unop0 0 $ \[x] -> TPrm VALU [x]

check'sandbox :: SuperNormal Symbol
check'sandbox =
  Lambda [BX, BX]
    . TAbss [refs, val]
    . TLetD b UN (TPrm SDBX [refs, val])
    $ boolift b
  where
    (refs, val, b) = fresh

sandbox'links :: SuperNormal Symbol
sandbox'links = Lambda [BX] . TAbs ln $ TPrm SDBL [ln]
  where
    ln = fresh1

value'sandbox :: SuperNormal Symbol
value'sandbox =
  Lambda [BX, BX]
    . TAbss [refs, val]
    $ TPrm SDBV [refs, val]
  where
    (refs, val) = fresh

stm'atomic :: SuperNormal Symbol
stm'atomic =
  Lambda [BX]
    . TAbs act
    . TLetD unit BX (TCon Ty.unitRef 0 [])
    . TName lz (Right act) [unit]
    $ TPrm ATOM [lz]
  where
    (act, unit, lz) = fresh

type ForeignOp = FOp -> ([Mem], ANormal Symbol)

standard'handle :: ForeignOp
standard'handle instr =
  ([BX],)
    . TAbss [h0]
    . unenum 3 h0 Ty.stdHandleRef h
    $ TFOp instr [h]
  where
    (h0, h) = fresh

any'construct :: SuperNormal Symbol
any'construct =
  unop0 0 $ \[v] ->
    TCon Ty.anyRef 0 [v]

any'extract :: SuperNormal Symbol
any'extract =
  unop0 1 $
    \[v, v1] ->
      TMatch v $
        MatchData Ty.anyRef (mapSingleton 0 $ ([BX], TAbs v1 (TVar v1))) Nothing

-- Refs

-- The docs for IORef state that IORef operations can be observed
-- out of order ([1]) but actually GHC does emit the appropriate
-- load and store barriers nowadays ([2], [3]).
--
-- [1] https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-IORef.html#g:2
-- [2] https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs#L286
-- [3] https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs#L298
ref'read :: SuperNormal Symbol
ref'read =
  unop0 0 $ \[ref] -> (TPrm REFR [ref])

ref'write :: SuperNormal Symbol
ref'write =
  binop0 0 $ \[ref, val] -> (TPrm REFW [ref, val])

-- In GHC, CAS returns both a Boolean and the current value of the
-- IORef, which can be used to retry a failed CAS.
-- This strategy is more efficient than returning a Boolean only
-- because it uses a single call to cmpxchg in assembly (see [1]) to
-- avoid an extra read per CAS iteration, however it's not supported
-- in Scheme.
-- Therefore, we adopt the more common signature that only returns a
-- Boolean, which doesn't even suffer from spurious failures because
-- GHC issues loads of mutable variables with memory_order_acquire
-- (see [2])
--
-- [1]: https://github.com/ghc/ghc/blob/master/rts/PrimOps.cmm#L697
-- [2]: https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs#L285
ref'cas :: SuperNormal Symbol
ref'cas =
  Lambda [BX, BX, BX]
    . TAbss [x, y, z]
    . TLetD b UN (TPrm RCAS [x, y, z])
    $ boolift b
  where
    (x, y, z, b) = fresh

ref'ticket'read :: SuperNormal Symbol
ref'ticket'read = unop0 0 $ TPrm TIKR

ref'readForCas :: SuperNormal Symbol
ref'readForCas = unop0 0 $ TPrm RRFC

ref'new :: SuperNormal Symbol
ref'new = unop0 0 $ TPrm REFN

seek'handle :: ForeignOp
seek'handle instr =
  ([BX, BX, BX],)
    . TAbss [arg1, arg2, arg3]
    . unenum 3 arg2 Ty.seekModeRef seek
    . TLetD result UN (TFOp instr [arg1, seek, arg3])
    $ outIoFailUnit stack1 stack2 stack3 unit fail result
  where
    (arg1, arg2, arg3, seek, stack1, stack2, stack3, unit, fail, result) = fresh

no'buf, line'buf, block'buf, sblock'buf :: (Enum e) => e
no'buf = toEnum $ fromIntegral Ty.bufferModeNoBufferingId
line'buf = toEnum $ fromIntegral Ty.bufferModeLineBufferingId
block'buf = toEnum $ fromIntegral Ty.bufferModeBlockBufferingId
sblock'buf = toEnum $ fromIntegral Ty.bufferModeSizedBlockBufferingId

infixr 0 -->

(-->) :: a -> b -> (a, b)
x --> y = (x, y)

time'zone :: ForeignOp
time'zone instr =
  ([BX],)
    . TAbss [secs]
    . TLets Direct [offset, summer, name] [UN, UN, BX] (TFOp instr [secs])
    . TLetD un BX (TCon Ty.unitRef 0 [])
    . TLetD p2 BX (TCon Ty.pairRef 0 [name, un])
    . TLetD p1 BX (TCon Ty.pairRef 0 [summer, p2])
    $ TCon Ty.pairRef 0 [offset, p1]
  where
    (secs, offset, summer, name, un, p2, p1) = fresh

start'process :: ForeignOp
start'process instr =
  ([BX, BX],)
    . TAbss [exe, args]
    . TLets Direct [hin, hout, herr, hproc] [BX, BX, BX, BX] (TFOp instr [exe, args])
    . TLetD un BX (TCon Ty.unitRef 0 [])
    . TLetD p3 BX (TCon Ty.pairRef 0 [hproc, un])
    . TLetD p2 BX (TCon Ty.pairRef 0 [herr, p3])
    . TLetD p1 BX (TCon Ty.pairRef 0 [hout, p2])
    $ TCon Ty.pairRef 0 [hin, p1]
  where
    (exe, args, hin, hout, herr, hproc, un, p3, p2, p1) = fresh

set'buffering :: ForeignOp
set'buffering instr =
  ([BX, BX],)
    . TAbss [handle, bmode]
    . TMatch bmode
    . MatchDataCover Ty.bufferModeRef
    $ mapFromList
      [ no'buf --> [] --> k1 no'buf,
        line'buf --> [] --> k1 line'buf,
        block'buf --> [] --> k1 block'buf,
        sblock'buf
          --> [BX]
          --> TAbs n
          . TMatch n
          . MatchDataCover Ty.bufferModeRef
          $ mapFromList
            [ 0
                --> [UN]
                --> TAbs w
                . TLetD tag UN (TLit (N sblock'buf))
                $ k2 [tag, w]
            ]
      ]
  where
    k1 num =
      TLetD tag UN (TLit (N num)) $
        k2 [tag]
    k2 args =
      TLetD r UN (TFOp instr (handle : args)) $
        outIoFailUnit s1 s2 s3 u f r
    (handle, bmode, tag, n, w, s1, s2, s3, u, f, r) = fresh

get'buffering'output :: forall v. (Var v) => v -> v -> v -> v -> v -> v -> v -> v -> ANormal v
get'buffering'output eitherResult stack1 stack2 stack3 resultTag anyVar failVar successVar =
  TMatch eitherResult . MatchSum $
    mapFromList
      [ failureCase stack1 stack2 stack3 anyVar failVar,
        ( 1,
          ([UN],)
            . TAbs resultTag
            . TMatch resultTag
            . MatchSum
            $ mapFromList
              [ no'buf
                  --> []
                  --> TLetD successVar BX (TCon Ty.bufferModeRef no'buf [])
                  $ right successVar,
                line'buf
                  --> []
                  --> TLetD successVar BX (TCon Ty.bufferModeRef line'buf [])
                  $ right successVar,
                block'buf
                  --> []
                  --> TLetD successVar BX (TCon Ty.bufferModeRef block'buf [])
                  $ right successVar,
                sblock'buf
                  --> [UN]
                  --> TAbs stack1
                  . TLetD successVar BX (TCon Ty.bufferModeRef sblock'buf [stack1])
                  $ right successVar
              ]
        )
      ]

get'buffering :: ForeignOp
get'buffering =
  in1 arg1 eitherResult $
    get'buffering'output eitherResult n n2 n3 resultTag anyVar failVar successVar
  where
    (arg1, eitherResult, n, n2, n3, resultTag, anyVar, failVar, successVar) = fresh

crypto'hash :: ForeignOp
crypto'hash instr =
  ([BX, BX],)
    . TAbss [alg, x]
    . TLetD vl BX (TPrm VALU [x])
    $ TFOp instr [alg, vl]
  where
    (alg, x, vl) = fresh

murmur'hash :: ForeignOp
murmur'hash instr =
  ([BX],)
    . TAbss [x]
    . TLetD vl BX (TPrm VALU [x])
    $ TFOp instr [vl]
  where
    (x, vl) = fresh

crypto'hmac :: ForeignOp
crypto'hmac instr =
  ([BX, BX, BX],)
    . TAbss [alg, by, x]
    . TLetD vl BX (TPrm VALU [x])
    $ TFOp instr [alg, by, vl]
  where
    (alg, by, x, vl) = fresh

-- Input Shape -- these represent different argument lists a
-- foreign might expect
--
-- They are named according to their shape:
--   inUnit : one input arg, unit output
--   in1 : one input arg
--
-- All of these functions will have take (at least) the same three arguments
--
--   instr : the foreign instruction to call
--   result : a variable containing the result of the foreign call
--   cont : a term which will be evaluated when a result from the foreign call is on the stack
--

-- () -> ...
inUnit :: forall v. (Var v) => v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inUnit unit result cont instr =
  ([BX], TAbs unit $ TLetD result UN (TFOp instr []) cont)

inN :: forall v. (Var v) => [v] -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inN args result cont instr =
  (args $> BX,)
    . TAbss args
    $ TLetD result UN (TFOp instr args) cont

-- a -> ...
in1 :: forall v. (Var v) => v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
in1 arg result cont instr = inN [arg] result cont instr

-- a -> b -> ...
in2 :: forall v. (Var v) => v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
in2 arg1 arg2 result cont instr = inN [arg1, arg2] result cont instr

-- a -> b -> c -> ...
in3 :: forall v. (Var v) => v -> v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
in3 arg1 arg2 arg3 result cont instr = inN [arg1, arg2, arg3] result cont instr

-- Maybe a -> b -> ...
inMaybeBx :: forall v. (Var v) => v -> v -> v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inMaybeBx arg1 arg2 arg3 mb result cont instr =
  ([BX, BX],)
    . TAbss [arg1, arg2]
    . TMatch arg1
    . flip (MatchData Ty.optionalRef) Nothing
    $ mapFromList
      [ ( fromIntegral Ty.noneId,
          ( [],
            TLetD mb UN (TLit $ I 0) $
              TLetD result UN (TFOp instr [mb, arg2]) cont
          )
        ),
        (fromIntegral Ty.someId, ([BX], TAbs arg3 . TLetD mb UN (TLit $ I 1) $ TLetD result UN (TFOp instr [mb, arg3, arg2]) cont))
      ]

set'echo :: ForeignOp
set'echo instr =
  ([BX, BX],)
    . TAbss [arg1, arg2]
    . unenum 2 arg2 Ty.booleanRef bol
    . TLetD result UN (TFOp instr [arg1, bol])
    $ outIoFailUnit stack1 stack2 stack3 unit fail result
  where
    (arg1, arg2, bol, stack1, stack2, stack3, unit, fail, result) = fresh

-- a -> IOMode -> ...
inIomr :: forall v. (Var v) => v -> v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inIomr arg1 arg2 fm result cont instr =
  ([BX, BX],)
    . TAbss [arg1, arg2]
    . unenum 4 arg2 Ty.fileModeRef fm
    $ TLetD result UN (TFOp instr [arg1, fm]) cont

-- Output Shape -- these will represent different ways of translating
-- the result of a foreign call to a Unison Term
--
-- They will be named according to the output type
--   outInt    : a foreign function returning an Int
--   outBool   : a foreign function returning a boolean
--   outIOFail : a function returning (Either Failure a)
--
-- All of these functions will take a Var named result containing the
-- result of the foreign call
--

outMaybe :: forall v. (Var v) => v -> v -> ANormal v
outMaybe tag result =
  TMatch tag . MatchSum $
    mapFromList
      [ (0, ([], none)),
        (1, ([BX], TAbs result $ some result))
      ]

outMaybeNTup :: forall v. (Var v) => v -> v -> v -> v -> v -> v -> ANormal v
outMaybeNTup a b u bp p result =
  TMatch result . MatchSum $
    mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [UN, BX],
            TAbss [a, b]
              . TLetD u BX (TCon Ty.unitRef 0 [])
              . TLetD bp BX (TCon Ty.pairRef 0 [b, u])
              . TLetD p BX (TCon Ty.pairRef 0 [a, bp])
              $ some p
          )
        )
      ]

outMaybeTup :: (Var v) => v -> v -> v -> v -> v -> v -> ANormal v
outMaybeTup a b u bp ap result =
  TMatch result . MatchSum $
    mapFromList
      [ (0, ([], none)),
        ( 1,
          ( [BX, BX],
            TAbss [a, b]
              . TLetD u BX (TCon Ty.unitRef 0 [])
              . TLetD bp BX (TCon Ty.pairRef 0 [b, u])
              . TLetD ap BX (TCon Ty.pairRef 0 [a, bp])
              $ some ap
          )
        )
      ]

-- Note: the Io part doesn't really do anything. There's no actual
-- representation of `IO`.
outIoFail :: forall v. (Var v) => v -> v -> v -> v -> v -> v -> ANormal v
outIoFail stack1 stack2 stack3 any fail result =
  TMatch result . MatchSum $
    mapFromList
      [ failureCase stack1 stack2 stack3 any fail,
        (1, ([BX], TAbs stack1 $ right stack1))
      ]

outIoFailChar :: forall v. (Var v) => v -> v -> v -> v -> v -> v -> ANormal v
outIoFailChar stack1 stack2 stack3 fail extra result =
  TMatch result . MatchSum $
    mapFromList
      [ failureCase stack1 stack2 stack3 extra fail,
        ( 1,
          ([UN],)
            . TAbs extra
            $ right extra
        )
      ]

failureCase ::
  (Var v) => v -> v -> v -> v -> v -> (Word64, ([Mem], ANormal v))
failureCase stack1 stack2 stack3 any fail =
  (0,)
    . ([BX, BX, BX],)
    . TAbss [stack1, stack2, stack3]
    . TLetD any BX (TCon Ty.anyRef 0 [stack3])
    . TLetD fail BX (TCon Ty.failureRef 0 [stack1, stack2, any])
    $ left fail

exnCase ::
  (Var v) => v -> v -> v -> v -> v -> (Word64, ([Mem], ANormal v))
exnCase stack1 stack2 stack3 any fail =
  (0,)
    . ([BX, BX, BX],)
    . TAbss [stack1, stack2, stack3]
    . TLetD any BX (TCon Ty.anyRef 0 [stack3])
    . TLetD fail BX (TCon Ty.failureRef 0 [stack1, stack2, any])
    $ TReq Ty.exceptionRef 0 [fail]

outIoExnUnit ::
  forall v. (Var v) => v -> v -> v -> v -> v -> v -> ANormal v
outIoExnUnit stack1 stack2 stack3 any fail result =
  TMatch result . MatchSum $
    mapFromList
      [ exnCase stack1 stack2 stack3 any fail,
        (1, ([], TCon Ty.unitRef 0 []))
      ]

outIoExn ::
  (Var v) => v -> v -> v -> v -> v -> v -> ANormal v
outIoExn stack1 stack2 stack3 any fail result =
  TMatch result . MatchSum $
    mapFromList
      [ exnCase stack1 stack2 stack3 any fail,
        (1, ([BX], TAbs stack1 $ TVar stack1))
      ]

outIoExnEither ::
  (Var v) => v -> v -> v -> v -> v -> v -> v -> v -> ANormal v
outIoExnEither stack1 stack2 stack3 any fail t0 t1 res =
  TMatch t0 . MatchSum $
    mapFromList
      [ exnCase stack1 stack2 stack3 any fail,
        ( 1,
          ([UN],)
            . TAbs t1
            . TMatch t1
            . MatchSum
            $ mapFromList
              [ (0, ([BX], TAbs res $ left res)),
                (1, ([BX], TAbs res $ right res))
              ]
        )
      ]

outIoFailUnit :: forall v. (Var v) => v -> v -> v -> v -> v -> v -> ANormal v
outIoFailUnit stack1 stack2 stack3 extra fail result =
  TMatch result . MatchSum $
    mapFromList
      [ failureCase stack1 stack2 stack3 extra fail,
        ( 1,
          ([],)
            . TLetD extra BX (TCon Ty.unitRef 0 [])
            $ right extra
        )
      ]

outIoFailBool :: forall v. (Var v) => v -> v -> v -> v -> v -> v -> ANormal v
outIoFailBool stack1 stack2 stack3 extra fail result =
  TMatch result . MatchSum $
    mapFromList
      [ failureCase stack1 stack2 stack3 extra fail,
        ( 1,
          ([UN],)
            . TAbs stack3
            . TLet (Indirect 1) extra BX (boolift stack3)
            $ right extra
        )
      ]

outIoFailTup :: forall v. (Var v) => v -> v -> v -> v -> v -> v -> v -> v -> ANormal v
outIoFailTup stack1 stack2 stack3 stack4 stack5 extra fail result =
  TMatch result . MatchSum $
    mapFromList
      [ failureCase stack1 stack2 stack3 extra fail,
        ( 1,
          ( [BX, BX],
            TAbss [stack1, stack2]
              . TLetD stack3 BX (TCon Ty.unitRef 0 [])
              . TLetD stack4 BX (TCon Ty.pairRef 0 [stack2, stack3])
              . TLetD stack5 BX (TCon Ty.pairRef 0 [stack1, stack4])
              $ right stack5
          )
        )
      ]

outIoFailG ::
  (Var v) =>
  v ->
  v ->
  v ->
  v ->
  v ->
  v ->
  ((ANormal v -> ANormal v) -> ([Mem], ANormal v)) ->
  ANormal v
outIoFailG stack1 stack2 stack3 fail result output k =
  TMatch result . MatchSum $
    mapFromList
      [ failureCase stack1 stack2 stack3 output fail,
        ( 1,
          k $ \t ->
            TLetD output BX t $
              right output
        )
      ]

-- Input / Output glue
--
-- These are pairings of input and output functions to handle a
-- foreign call.  The input function represents the numbers and types
-- of the inputs to a foreign call.  The output function takes the
-- result of the foreign call and turns it into a Unison type.
--

-- a
direct :: ForeignOp
direct instr = ([], TFOp instr [])

-- () -> r
unitToR :: ForeignOp
unitToR =
  inUnit unit result $ TVar result
  where
    (unit, result) = fresh

-- () -> Either Failure a
unitToEF :: ForeignOp
unitToEF =
  inUnit unit result $
    outIoFail stack1 stack2 stack3 any fail result
  where
    (unit, stack1, stack2, stack3, fail, any, result) = fresh

argIomrToEF :: ForeignOp
argIomrToEF =
  inIomr arg1 arg2 enum result $
    outIoFail stack1 stack2 stack3 any fail result
  where
    (arg1, arg2, enum, stack1, stack2, stack3, any, fail, result) = fresh

-- a -> ()
argToUnit :: ForeignOp
argToUnit = in1 arg result (TCon Ty.unitRef 0 [])
  where
    (arg, result) = fresh

-- a -> b ->{E} ()
arg2To0 :: ForeignOp
arg2To0 instr =
  ([BX, BX],)
    . TAbss [arg1, arg2]
    . TLets Direct [] [] (TFOp instr [arg1, arg2])
    $ TCon Ty.unitRef 0 []
  where
    (arg1, arg2) = fresh

-- ... -> Bool
argNToBool :: Int -> ForeignOp
argNToBool n instr =
  (replicate n BX,)
    . TAbss args
    $ TLetD result UN (TFOp instr args) (boolift result)
  where
    (result : args) = freshes (n + 1)

argNDirect :: Int -> ForeignOp
argNDirect n instr =
  (replicate n BX,)
    . TAbss args
    $ TFOp instr args
  where
    args = freshes n

--  () -> a
--
--  Unit is unique in that we don't actually pass it as an arg
unitDirect :: ForeignOp
unitDirect instr = ([BX],) . TAbs arg $ TFOp instr [] where arg = fresh1

-- a -> Either Failure b
argToEF :: ForeignOp
argToEF =
  in1 arg result $
    outIoFail stack1 stack2 stack3 any fail result
  where
    (arg, result, stack1, stack2, stack3, any, fail) = fresh

-- a -> Either Failure (b, c)
argToEFTup :: ForeignOp
argToEFTup =
  in1 arg result $
    outIoFailTup stack1 stack2 stack3 stack4 stack5 extra fail result
  where
    (arg, result, stack1, stack2, stack3, stack4, stack5, extra, fail) = fresh

-- a -> Either Failure (Maybe b)
argToEFM :: ForeignOp
argToEFM =
  in1 arg result
    . outIoFailG stack1 stack2 stack3 fail result output
    $ \k ->
      ( [UN],
        TAbs stack3 . TMatch stack3 . MatchSum $
          mapFromList
            [ (0, ([], k $ none)),
              (1, ([BX], TAbs stack4 . k $ some stack4))
            ]
      )
  where
    (arg, result, stack1, stack2, stack3, stack4, fail, output) = fresh

-- a -> Maybe b
argToMaybe :: ForeignOp
argToMaybe = in1 arg tag $ outMaybe tag result
  where
    (arg, tag, result) = fresh

-- a -> Maybe (Nat, b)
argToMaybeNTup :: ForeignOp
argToMaybeNTup =
  in1 arg result $ outMaybeNTup a b u bp p result
  where
    (arg, a, b, u, bp, p, result) = fresh

-- a -> b -> Maybe (c, d)
arg2ToMaybeTup :: ForeignOp
arg2ToMaybeTup =
  in2 arg1 arg2 result $ outMaybeTup a b u bp ap result
  where
    (arg1, arg2, a, b, u, bp, ap, result) = fresh

-- a -> Either Failure Bool
argToEFBool :: ForeignOp
argToEFBool =
  in1 arg result $
    outIoFailBool stack1 stack2 stack3 bool fail result
  where
    (arg, stack1, stack2, stack3, bool, fail, result) = fresh

-- a -> Either Failure Char
argToEFChar :: ForeignOp
argToEFChar =
  in1 arg result $
    outIoFailChar stack1 stack2 stack3 bool fail result
  where
    (arg, stack1, stack2, stack3, bool, fail, result) = fresh

-- a -> b -> Either Failure Bool
arg2ToEFBool :: ForeignOp
arg2ToEFBool =
  in2 arg1 arg2 result $
    outIoFailBool stack1 stack2 stack3 bool fail result
  where
    (arg1, arg2, stack1, stack2, stack3, bool, fail, result) = fresh

-- a -> b -> c -> Either Failure Bool
arg3ToEFBool :: ForeignOp
arg3ToEFBool =
  in3 arg1 arg2 arg3 result $
    outIoFailBool stack1 stack2 stack3 bool fail result
  where
    (arg1, arg2, arg3, stack1, stack2, stack3, bool, fail, result) = fresh

-- a -> Either Failure ()
argToEF0 :: ForeignOp
argToEF0 =
  in1 arg result $
    outIoFailUnit stack1 stack2 stack3 unit fail result
  where
    (arg, result, stack1, stack2, stack3, unit, fail) = fresh

-- a -> b -> Either Failure ()
arg2ToEF0 :: ForeignOp
arg2ToEF0 =
  in2 arg1 arg2 result $
    outIoFailUnit stack1 stack2 stack3 fail unit result
  where
    (arg1, arg2, result, stack1, stack2, stack3, fail, unit) = fresh

-- a -> b -> c -> Either Failure ()
arg3ToEF0 :: ForeignOp
arg3ToEF0 =
  in3 arg1 arg2 arg3 result $
    outIoFailUnit stack1 stack2 stack3 fail unit result
  where
    (arg1, arg2, arg3, result, stack1, stack2, stack3, fail, unit) = fresh

-- a -> Either Failure b
argToEFNat :: ForeignOp
argToEFNat =
  in1 arg result $
    outIoFail stack1 stack2 stack3 nat fail result
  where
    (arg, result, stack1, stack2, stack3, nat, fail) = fresh

-- Maybe a -> b -> Either Failure c
maybeToEF :: ForeignOp
maybeToEF =
  inMaybeBx arg1 arg2 arg3 mb result $
    outIoFail stack1 stack2 stack3 any fail result
  where
    (arg1, arg2, arg3, mb, result, stack1, stack2, stack3, any, fail) = fresh

-- a -> b -> Either Failure c
arg2ToEF :: ForeignOp
arg2ToEF =
  in2 arg1 arg2 result $
    outIoFail stack1 stack2 stack3 any fail result
  where
    (arg1, arg2, result, stack1, stack2, stack3, any, fail) = fresh

-- a -> b -> c -> Either Failure d
arg3ToEF :: ForeignOp
arg3ToEF =
  in3 arg1 arg2 arg3 result $
    outIoFail stack1 stack2 stack3 any fail result
  where
    (arg1, arg2, arg3, result, stack1, stack2, stack3, any, fail) = fresh

-- a -> b ->{Exception} c
arg2ToExn :: ForeignOp
arg2ToExn =
  in2 arg1 arg2 result $
    outIoExn stack1 stack2 stack3 any fail result
  where
    (arg1, arg2, stack1, stack2, stack3, any, fail, result) = fresh

-- a -> b -> c ->{Exception} ()
arg3ToExnUnit :: ForeignOp
arg3ToExnUnit =
  in3 arg1 arg2 arg3 result $
    outIoExnUnit stack1 stack2 stack3 any fail result
  where
    (arg1, arg2, arg3, stack1, stack2, stack3, any, fail, result) = fresh

-- a -> Nat -> Nat ->{Exception} b
arg3ToExn :: ForeignOp
arg3ToExn =
  in3 arg1 arg2 arg3 result $
    outIoExn stack1 stack2 stack3 any fail result
  where
    (arg1, arg2, arg3, result, stack1, stack2, stack3, any, fail) = fresh

-- a -> Nat -> b -> Nat -> Nat ->{Exception} ()
arg5ToExnUnit :: ForeignOp
arg5ToExnUnit instr =
  ([BX, BX, BX, BX, BX],)
    . TAbss [a0, ua1, a2, ua3, ua4]
    . TLetD result UN (TFOp instr [a0, ua1, a2, ua3, ua4])
    $ outIoExnUnit stack1 stack2 stack3 any fail result
  where
    (a0, a2, ua1, ua3, ua4, result, stack1, stack2, stack3, any, fail) = fresh

-- a ->{Exception} Either b c
argToExnE :: ForeignOp
argToExnE instr =
  ([BX],)
    . TAbs a
    . TLetD t0 UN (TFOp instr [a])
    $ outIoExnEither stack1 stack2 stack3 any fail t0 t1 result
  where
    (a, stack1, stack2, stack3, any, fail, t0, t1, result) = fresh

-- Nat -> Either Failure ()
argToEFUnit :: ForeignOp
argToEFUnit =
  in1 nat result
    . TMatch result
    . MatchSum
    $ mapFromList
      [ failureCase stack1 stack2 stack3 unit fail,
        ( 1,
          ([],)
            . TLetD unit BX (TCon Ty.unitRef 0 [])
            $ right unit
        )
      ]
  where
    (nat, result, fail, stack1, stack2, stack3, unit) = fresh

-- a -> Either b c
argToEither :: ForeignOp
argToEither instr =
  ([BX],)
    . TAbss [b]
    . TLetD e UN (TFOp instr [b])
    . TMatch e
    . MatchSum
    $ mapFromList
      [ (0, ([BX], TAbs ev $ left ev)),
        (1, ([BX], TAbs ev $ right ev))
      ]
  where
    (e, b, ev) = fresh

builtinLookup :: Map.Map Reference (Sandbox, SuperNormal Symbol)
builtinLookup =
  Map.fromList
    . map (\(t, f) -> (Builtin t, f))
    $ [ ("Int.+", (Untracked, addi)),
        ("Int.-", (Untracked, subi)),
        ("Int.*", (Untracked, muli)),
        ("Int./", (Untracked, divi)),
        ("Int.mod", (Untracked, modi)),
        ("Int.==", (Untracked, eqi)),
        ("Int.<", (Untracked, lti)),
        ("Int.<=", (Untracked, lei)),
        ("Int.>", (Untracked, gti)),
        ("Int.>=", (Untracked, gei)),
        ("Int.fromRepresentation", (Untracked, coerceType IntTag)),
        ("Int.toRepresentation", (Untracked, coerceType NatTag)),
        ("Int.increment", (Untracked, inci)),
        ("Int.signum", (Untracked, sgni)),
        ("Int.negate", (Untracked, negi)),
        ("Int.truncate0", (Untracked, trni)),
        ("Int.isEven", (Untracked, evni)),
        ("Int.isOdd", (Untracked, oddi)),
        ("Int.shiftLeft", (Untracked, shli)),
        ("Int.shiftRight", (Untracked, shri)),
        ("Int.trailingZeros", (Untracked, tzeroi)),
        ("Int.leadingZeros", (Untracked, lzeroi)),
        ("Int.and", (Untracked, andi)),
        ("Int.or", (Untracked, ori)),
        ("Int.xor", (Untracked, xori)),
        ("Int.complement", (Untracked, compli)),
        ("Int.pow", (Untracked, powi)),
        ("Int.toText", (Untracked, i2t)),
        ("Int.fromText", (Untracked, t2i)),
        ("Int.toFloat", (Untracked, i2f)),
        ("Int.popCount", (Untracked, popi)),
        ("Nat.+", (Untracked, addn)),
        ("Nat.-", (Untracked, subn)),
        ("Nat.sub", (Untracked, subn)),
        ("Nat.*", (Untracked, muln)),
        ("Nat./", (Untracked, divn)),
        ("Nat.mod", (Untracked, modn)),
        ("Nat.==", (Untracked, eqn)),
        ("Nat.<", (Untracked, ltn)),
        ("Nat.<=", (Untracked, len)),
        ("Nat.>", (Untracked, gtn)),
        ("Nat.>=", (Untracked, gen)),
        ("Nat.increment", (Untracked, incn)),
        ("Nat.isEven", (Untracked, evnn)),
        ("Nat.isOdd", (Untracked, oddn)),
        ("Nat.shiftLeft", (Untracked, shln)),
        ("Nat.shiftRight", (Untracked, shrn)),
        ("Nat.trailingZeros", (Untracked, tzeron)),
        ("Nat.leadingZeros", (Untracked, lzeron)),
        ("Nat.and", (Untracked, andn)),
        ("Nat.or", (Untracked, orn)),
        ("Nat.xor", (Untracked, xorn)),
        ("Nat.complement", (Untracked, compln)),
        ("Nat.pow", (Untracked, pown)),
        ("Nat.drop", (Untracked, dropn)),
        ("Nat.toInt", (Untracked, coerceType IntTag)),
        ("Nat.toFloat", (Untracked, n2f)),
        ("Nat.toText", (Untracked, n2t)),
        ("Nat.fromText", (Untracked, t2n)),
        ("Nat.popCount", (Untracked, popn)),
        ("Float.+", (Untracked, addf)),
        ("Float.-", (Untracked, subf)),
        ("Float.*", (Untracked, mulf)),
        ("Float./", (Untracked, divf)),
        ("Float.pow", (Untracked, powf)),
        ("Float.log", (Untracked, logf)),
        ("Float.logBase", (Untracked, logbf)),
        ("Float.sqrt", (Untracked, sqrtf)),
        ("Float.fromRepresentation", (Untracked, coerceType FloatTag)),
        ("Float.toRepresentation", (Untracked, coerceType NatTag)),
        ("Float.min", (Untracked, minf)),
        ("Float.max", (Untracked, maxf)),
        ("Float.<", (Untracked, ltf)),
        ("Float.>", (Untracked, gtf)),
        ("Float.<=", (Untracked, lef)),
        ("Float.>=", (Untracked, gef)),
        ("Float.==", (Untracked, eqf)),
        ("Float.!=", (Untracked, neqf)),
        ("Float.acos", (Untracked, acosf)),
        ("Float.asin", (Untracked, asinf)),
        ("Float.atan", (Untracked, atanf)),
        ("Float.cos", (Untracked, cosf)),
        ("Float.sin", (Untracked, sinf)),
        ("Float.tan", (Untracked, tanf)),
        ("Float.acosh", (Untracked, acoshf)),
        ("Float.asinh", (Untracked, asinhf)),
        ("Float.atanh", (Untracked, atanhf)),
        ("Float.cosh", (Untracked, coshf)),
        ("Float.sinh", (Untracked, sinhf)),
        ("Float.tanh", (Untracked, tanhf)),
        ("Float.exp", (Untracked, expf)),
        ("Float.abs", (Untracked, absf)),
        ("Float.ceiling", (Untracked, ceilf)),
        ("Float.floor", (Untracked, floorf)),
        ("Float.round", (Untracked, roundf)),
        ("Float.truncate", (Untracked, truncf)),
        ("Float.atan2", (Untracked, atan2f)),
        ("Float.toText", (Untracked, f2t)),
        ("Float.fromText", (Untracked, t2f)),
        -- text
        ("Text.empty", (Untracked, Lambda [] $ TLit (T ""))),
        ("Text.++", (Untracked, appendt)),
        ("Text.take", (Untracked, taket)),
        ("Text.drop", (Untracked, dropt)),
        ("Text.indexOf", (Untracked, indext)),
        ("Text.size", (Untracked, sizet)),
        ("Text.==", (Untracked, eqt)),
        ("Text.!=", (Untracked, neqt)),
        ("Text.<=", (Untracked, leqt)),
        ("Text.>=", (Untracked, geqt)),
        ("Text.<", (Untracked, lesst)),
        ("Text.>", (Untracked, great)),
        ("Text.uncons", (Untracked, unconst)),
        ("Text.unsnoc", (Untracked, unsnoct)),
        ("Text.toCharList", (Untracked, unpackt)),
        ("Text.fromCharList", (Untracked, packt)),
        ("Boolean.not", (Untracked, notb)),
        ("Boolean.or", (Untracked, orb)),
        ("Boolean.and", (Untracked, andb)),
        ("bug", (Untracked, bug "builtin.bug")),
        ("todo", (Untracked, bug "builtin.todo")),
        ("Debug.watch", (Tracked, watch)),
        ("Debug.trace", (Tracked, gen'trace)),
        ("Debug.toText", (Tracked, debug'text)),
        ("unsafe.coerceAbilities", (Untracked, poly'coerce)),
        ("Char.toNat", (Untracked, coerceType NatTag)),
        ("Char.fromNat", (Untracked, coerceType CharTag)),
        ("Bytes.empty", (Untracked, emptyb)),
        ("Bytes.fromList", (Untracked, packb)),
        ("Bytes.toList", (Untracked, unpackb)),
        ("Bytes.++", (Untracked, appendb)),
        ("Bytes.take", (Untracked, takeb)),
        ("Bytes.drop", (Untracked, dropb)),
        ("Bytes.at", (Untracked, atb)),
        ("Bytes.indexOf", (Untracked, indexb)),
        ("Bytes.size", (Untracked, sizeb)),
        ("Bytes.flatten", (Untracked, flattenb)),
        ("List.take", (Untracked, takes)),
        ("List.drop", (Untracked, drops)),
        ("List.size", (Untracked, sizes)),
        ("List.++", (Untracked, appends)),
        ("List.at", (Untracked, ats)),
        ("List.cons", (Untracked, conss)),
        ("List.snoc", (Untracked, snocs)),
        ("List.empty", (Untracked, emptys)),
        ("List.viewl", (Untracked, viewls)),
        ("List.viewr", (Untracked, viewrs)),
        ("List.splitLeft", (Untracked, splitls)),
        ("List.splitRight", (Untracked, splitrs)),
        --
        --   , B "Debug.watch" $ forall1 "a" (\a -> text --> a --> a)
        ("Universal.==", (Untracked, equ)),
        ("Universal.compare", (Untracked, cmpu)),
        ("Universal.>", (Untracked, gtu)),
        ("Universal.<", (Untracked, ltu)),
        ("Universal.>=", (Untracked, geu)),
        ("Universal.<=", (Untracked, leu)),
        -- internal stuff
        ("jumpCont", (Untracked, jumpk)),
        ("raise", (Untracked, raise)),
        ("IO.forkComp.v2", (Tracked, fork'comp)),
        ("Scope.run", (Untracked, scope'run)),
        ("Code.isMissing", (Tracked, code'missing)),
        ("Code.cache_", (Tracked, code'cache)),
        ("Code.lookup", (Tracked, code'lookup)),
        ("Code.validate", (Tracked, code'validate)),
        ("Value.load", (Tracked, value'load)),
        ("Value.value", (Tracked, value'create)),
        ("Any.Any", (Untracked, any'construct)),
        ("Any.unsafeExtract", (Untracked, any'extract)),
        ("Link.Term.toText", (Untracked, term'link'to'text)),
        ("STM.atomically", (Tracked, stm'atomic)),
        ("validateSandboxed", (Untracked, check'sandbox)),
        ("Value.validateSandboxed", (Tracked, value'sandbox)),
        ("sandboxLinks", (Tracked, sandbox'links)),
        ("IO.tryEval", (Tracked, try'eval)),
        ("Ref.read", (Tracked, ref'read)),
        ("Ref.write", (Tracked, ref'write)),
        ("Ref.cas", (Tracked, ref'cas)),
        ("Ref.Ticket.read", (Tracked, ref'ticket'read)),
        ("Ref.readForCas", (Tracked, ref'readForCas)),
        ("Scope.ref", (Untracked, ref'new)),
        ("IO.ref", (Tracked, ref'new))
      ]
      ++ foreignWrappers

type FDecl v =
  ReaderT Bool (State (Word64, [(Data.Text.Text, (Sandbox, SuperNormal v))], EnumMap Word64 (Data.Text.Text, ForeignFunc)))

-- Data type to determine whether a builtin should be tracked for
-- sandboxing. Untracked means that it can be freely used, and Tracked
-- means that the sandboxing check will by default consider them
-- disallowed.
data Sandbox = Tracked | Untracked
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

bomb :: Data.Text.Text -> a -> IO r
bomb name _ = die $ "attempted to use sandboxed operation: " ++ Data.Text.unpack name

declareForeign ::
  Sandbox ->
  Data.Text.Text ->
  ForeignOp ->
  ForeignFunc ->
  FDecl Symbol ()
declareForeign sand name op func0 = do
  sanitize <- ask
  modify $ \(w, codes, funcs) ->
    let func
          | sanitize,
            Tracked <- sand,
            FF r w _ <- func0 =
              FF r w (bomb name)
          | otherwise = func0
        code = (name, (sand, uncurry Lambda (op w)))
     in (w + 1, code : codes, mapInsert w (name, func) funcs)

mkForeignIOF ::
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO r) ->
  ForeignFunc
mkForeignIOF f = mkForeign $ \a -> tryIOE (f a)
  where
    tryIOE :: IO a -> IO (Either Failure a)
    tryIOE = fmap handleIOE . try
    handleIOE :: Either IOException a -> Either Failure a
    handleIOE (Left e) = Left $ Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue
    handleIOE (Right a) = Right a

unitValue :: Val
unitValue = BoxedVal $ Closure.Enum Ty.unitRef (PackedTag 0)

natValue :: Word64 -> Val
natValue w = NatVal w

mkForeignTls ::
  forall a r.
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO r) ->
  ForeignFunc
mkForeignTls f = mkForeign $ \a -> fmap flatten (tryIO2 (tryIO1 (f a)))
  where
    tryIO1 :: IO r -> IO (Either TLS.TLSException r)
    tryIO1 = try
    tryIO2 :: IO (Either TLS.TLSException r) -> IO (Either IOException (Either TLS.TLSException r))
    tryIO2 = try
    flatten :: Either IOException (Either TLS.TLSException r) -> Either (Failure) r
    flatten (Left e) = Left (Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Left e)) = Left (Failure Ty.tlsFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Right a)) = Right a

mkForeignTlsE ::
  forall a r.
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO (Either Failure r)) ->
  ForeignFunc
mkForeignTlsE f = mkForeign $ \a -> fmap flatten (tryIO2 (tryIO1 (f a)))
  where
    tryIO1 :: IO (Either Failure r) -> IO (Either TLS.TLSException (Either Failure r))
    tryIO1 = try
    tryIO2 :: IO (Either TLS.TLSException (Either Failure r)) -> IO (Either IOException (Either TLS.TLSException (Either Failure r)))
    tryIO2 = try
    flatten :: Either IOException (Either TLS.TLSException (Either Failure r)) -> Either Failure r
    flatten (Left e) = Left (Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Left e)) = Left (Failure Ty.tlsFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Right (Left e))) = Left e
    flatten (Right (Right (Right a))) = Right a

declareUdpForeigns :: FDecl Symbol ()
declareUdpForeigns = do
  declareForeign Tracked "IO.UDP.clientSocket.impl.v1" arg2ToEF
    . mkForeignIOF
    $ \(host :: Util.Text.Text, port :: Util.Text.Text) ->
      let hostStr = Util.Text.toString host
          portStr = Util.Text.toString port
       in UDP.clientSocket hostStr portStr True

  declareForeign Tracked "IO.UDP.UDPSocket.recv.impl.v1" argToEF
    . mkForeignIOF
    $ \(sock :: UDPSocket) -> Bytes.fromArray <$> UDP.recv sock

  declareForeign Tracked "IO.UDP.UDPSocket.send.impl.v1" arg2ToEF0
    . mkForeignIOF
    $ \(sock :: UDPSocket, bytes :: Bytes.Bytes) ->
      UDP.send sock (Bytes.toArray bytes)

  declareForeign Tracked "IO.UDP.UDPSocket.close.impl.v1" argToEF0
    . mkForeignIOF
    $ \(sock :: UDPSocket) -> UDP.close sock

  declareForeign Tracked "IO.UDP.ListenSocket.close.impl.v1" argToEF0
    . mkForeignIOF
    $ \(sock :: ListenSocket) -> UDP.stop sock

  declareForeign Tracked "IO.UDP.UDPSocket.toText.impl.v1" (argNDirect 1)
    . mkForeign
    $ \(sock :: UDPSocket) -> pure $ show sock

  declareForeign Tracked "IO.UDP.serverSocket.impl.v1" arg2ToEF
    . mkForeignIOF
    $ \(ip :: Util.Text.Text, port :: Util.Text.Text) ->
      let maybeIp = readMaybe $ Util.Text.toString ip :: Maybe IP
          maybePort = readMaybe $ Util.Text.toString port :: Maybe PortNumber
       in case (maybeIp, maybePort) of
            (Nothing, _) -> fail "Invalid IP Address"
            (_, Nothing) -> fail "Invalid Port Number"
            (Just ip, Just pt) -> UDP.serverSocket (ip, pt)

  declareForeign Tracked "IO.UDP.ListenSocket.toText.impl.v1" (argNDirect 1)
    . mkForeign
    $ \(sock :: ListenSocket) -> pure $ show sock

  declareForeign Tracked "IO.UDP.ListenSocket.recvFrom.impl.v1" argToEFTup
    . mkForeignIOF
    $ fmap (first Bytes.fromArray) <$> UDP.recvFrom

  declareForeign Tracked "IO.UDP.ClientSockAddr.toText.v1" (argNDirect 1)
    . mkForeign
    $ \(sock :: ClientSockAddr) -> pure $ show sock

  declareForeign Tracked "IO.UDP.ListenSocket.sendTo.impl.v1" arg3ToEF0
    . mkForeignIOF
    $ \(socket :: ListenSocket, bytes :: Bytes.Bytes, addr :: ClientSockAddr) ->
      UDP.sendTo socket (Bytes.toArray bytes) addr

declareForeigns :: FDecl Symbol ()
declareForeigns = do
  declareUdpForeigns
  declareForeign Tracked "IO.openFile.impl.v3" argIomrToEF $
    mkForeignIOF $ \(fnameText :: Util.Text.Text, n :: Int) ->
      let fname = Util.Text.toString fnameText
          mode = case n of
            0 -> ReadMode
            1 -> WriteMode
            2 -> AppendMode
            _ -> ReadWriteMode
       in openFile fname mode

  declareForeign Tracked "IO.closeFile.impl.v3" argToEF0 $ mkForeignIOF hClose
  declareForeign Tracked "IO.isFileEOF.impl.v3" argToEFBool $ mkForeignIOF hIsEOF
  declareForeign Tracked "IO.isFileOpen.impl.v3" argToEFBool $ mkForeignIOF hIsOpen
  declareForeign Tracked "IO.getEcho.impl.v1" argToEFBool $ mkForeignIOF hGetEcho
  declareForeign Tracked "IO.ready.impl.v1" argToEFBool $ mkForeignIOF hReady
  declareForeign Tracked "IO.getChar.impl.v1" argToEFChar $ mkForeignIOF hGetChar
  declareForeign Tracked "IO.isSeekable.impl.v3" argToEFBool $ mkForeignIOF hIsSeekable

  declareForeign Tracked "IO.seekHandle.impl.v3" seek'handle
    . mkForeignIOF
    $ \(h, sm, n) -> hSeek h sm (fromIntegral (n :: Int))

  declareForeign Tracked "IO.handlePosition.impl.v3" argToEFNat
    -- TODO: truncating integer
    . mkForeignIOF
    $ \h -> fromInteger @Word64 <$> hTell h

  declareForeign Tracked "IO.getBuffering.impl.v3" get'buffering $
    mkForeignIOF hGetBuffering

  declareForeign Tracked "IO.setBuffering.impl.v3" set'buffering
    . mkForeignIOF
    $ uncurry hSetBuffering

  declareForeign Tracked "IO.setEcho.impl.v1" set'echo . mkForeignIOF $ uncurry hSetEcho

  declareForeign Tracked "IO.getLine.impl.v1" argToEF $
    mkForeignIOF $
      fmap Util.Text.fromText . Text.IO.hGetLine

  declareForeign Tracked "IO.getBytes.impl.v3" arg2ToEF . mkForeignIOF $
    \(h, n) -> Bytes.fromArray <$> hGet h n

  declareForeign Tracked "IO.getSomeBytes.impl.v1" arg2ToEF . mkForeignIOF $
    \(h, n) -> Bytes.fromArray <$> hGetSome h n

  declareForeign Tracked "IO.putBytes.impl.v3" arg2ToEF0 . mkForeignIOF $ \(h, bs) -> hPut h (Bytes.toArray bs)

  declareForeign Tracked "IO.systemTime.impl.v3" unitToEF $
    mkForeignIOF $
      \() -> getPOSIXTime

  declareForeign Tracked "IO.systemTimeMicroseconds.v1" unitToR $
    mkForeign $
      \() -> fmap (1e6 *) getPOSIXTime

  declareForeign Tracked "Clock.internals.monotonic.v1" unitToEF $
    mkForeignIOF $
      \() -> getTime Monotonic

  declareForeign Tracked "Clock.internals.realtime.v1" unitToEF $
    mkForeignIOF $
      \() -> getTime Realtime

  declareForeign Tracked "Clock.internals.processCPUTime.v1" unitToEF $
    mkForeignIOF $
      \() -> getTime ProcessCPUTime

  declareForeign Tracked "Clock.internals.threadCPUTime.v1" unitToEF $
    mkForeignIOF $
      \() -> getTime ThreadCPUTime

  declareForeign Tracked "Clock.internals.sec.v1" (argNDirect 1) $
    mkForeign (\n -> pure (fromIntegral $ sec n :: Word64))

  -- A TimeSpec that comes from getTime never has negative nanos,
  -- so we can safely cast to Nat
  declareForeign Tracked "Clock.internals.nsec.v1" (argNDirect 1) $
    mkForeign (\n -> pure (fromIntegral $ nsec n :: Word64))

  declareForeign Tracked "Clock.internals.systemTimeZone.v1" time'zone $
    mkForeign
      ( \secs -> do
          TimeZone offset summer name <- getTimeZone (posixSecondsToUTCTime (fromIntegral (secs :: Int)))
          pure (offset :: Int, summer, name)
      )

  let chop = reverse . dropWhile isPathSeparator . reverse

  declareForeign Tracked "IO.getTempDirectory.impl.v3" unitToEF $
    mkForeignIOF $
      \() -> chop <$> getTemporaryDirectory

  declareForeign Tracked "IO.createTempDirectory.impl.v3" argToEF $
    mkForeignIOF $ \prefix -> do
      temp <- getTemporaryDirectory
      chop <$> createTempDirectory temp prefix

  declareForeign Tracked "IO.getCurrentDirectory.impl.v3" unitToEF
    . mkForeignIOF
    $ \() -> getCurrentDirectory

  declareForeign Tracked "IO.setCurrentDirectory.impl.v3" argToEF0 $
    mkForeignIOF setCurrentDirectory

  declareForeign Tracked "IO.fileExists.impl.v3" argToEFBool $
    mkForeignIOF doesPathExist

  declareForeign Tracked "IO.getEnv.impl.v1" argToEF $
    mkForeignIOF getEnv

  declareForeign Tracked "IO.getArgs.impl.v1" unitToEF $
    mkForeignIOF $
      \() -> fmap Util.Text.pack <$> SYS.getArgs

  declareForeign Tracked "IO.isDirectory.impl.v3" argToEFBool $
    mkForeignIOF doesDirectoryExist

  declareForeign Tracked "IO.createDirectory.impl.v3" argToEF0 $
    mkForeignIOF $
      createDirectoryIfMissing True

  declareForeign Tracked "IO.removeDirectory.impl.v3" argToEF0 $
    mkForeignIOF removeDirectoryRecursive

  declareForeign Tracked "IO.renameDirectory.impl.v3" arg2ToEF0 $
    mkForeignIOF $
      uncurry renameDirectory

  declareForeign Tracked "IO.directoryContents.impl.v3" argToEF $
    mkForeignIOF $
      (fmap Util.Text.pack <$>) . getDirectoryContents

  declareForeign Tracked "IO.removeFile.impl.v3" argToEF0 $
    mkForeignIOF removeFile

  declareForeign Tracked "IO.renameFile.impl.v3" arg2ToEF0 $
    mkForeignIOF $
      uncurry renameFile

  declareForeign Tracked "IO.getFileTimestamp.impl.v3" argToEFNat
    . mkForeignIOF
    $ fmap utcTimeToPOSIXSeconds . getModificationTime

  declareForeign Tracked "IO.getFileSize.impl.v3" argToEFNat
    -- TODO: truncating integer
    . mkForeignIOF
    $ \fp -> fromInteger @Word64 <$> getFileSize fp

  declareForeign Tracked "IO.serverSocket.impl.v3" maybeToEF
    . mkForeignIOF
    $ \( mhst :: Maybe Util.Text.Text,
         port
         ) ->
        fst <$> SYS.bindSock (hostPreference mhst) port

  declareForeign Tracked "Socket.toText" (argNDirect 1)
    . mkForeign
    $ \(sock :: Socket) -> pure $ show sock

  declareForeign Tracked "Handle.toText" (argNDirect 1)
    . mkForeign
    $ \(hand :: Handle) -> pure $ show hand

  declareForeign Tracked "ThreadId.toText" (argNDirect 1)
    . mkForeign
    $ \(threadId :: ThreadId) -> pure $ show threadId

  declareForeign Tracked "IO.socketPort.impl.v3" argToEFNat
    . mkForeignIOF
    $ \(handle :: Socket) -> do
      n <- SYS.socketPort handle
      return (fromIntegral n :: Word64)

  declareForeign Tracked "IO.listen.impl.v3" argToEF0
    . mkForeignIOF
    $ \sk -> SYS.listenSock sk 2048

  declareForeign Tracked "IO.clientSocket.impl.v3" arg2ToEF
    . mkForeignIOF
    $ fmap fst . uncurry SYS.connectSock

  declareForeign Tracked "IO.closeSocket.impl.v3" argToEF0 $
    mkForeignIOF SYS.closeSock

  declareForeign Tracked "IO.socketAccept.impl.v3" argToEF
    . mkForeignIOF
    $ fmap fst . SYS.accept

  declareForeign Tracked "IO.socketSend.impl.v3" arg2ToEF0
    . mkForeignIOF
    $ \(sk, bs) -> SYS.send sk (Bytes.toArray bs)

  declareForeign Tracked "IO.socketReceive.impl.v3" arg2ToEF
    . mkForeignIOF
    $ \(hs, n) ->
      maybe mempty Bytes.fromArray <$> SYS.recv hs n

  declareForeign Tracked "IO.kill.impl.v3" argToEF0 $ mkForeignIOF killThread

  let mx :: Word64
      mx = fromIntegral (maxBound :: Int)

      customDelay :: Word64 -> IO ()
      customDelay n
        | n < mx = threadDelay (fromIntegral n)
        | otherwise = threadDelay maxBound >> customDelay (n - mx)

  declareForeign Tracked "IO.delay.impl.v3" argToEFUnit $
    mkForeignIOF customDelay

  declareForeign Tracked "IO.stdHandle" standard'handle
    . mkForeign
    $ \(n :: Int) -> case n of
      0 -> pure SYS.stdin
      1 -> pure SYS.stdout
      2 -> pure SYS.stderr
      _ -> die "IO.stdHandle: invalid input."

  let exitDecode ExitSuccess = 0
      exitDecode (ExitFailure n) = n

  declareForeign Tracked "IO.process.call" (argNDirect 2) . mkForeign $
    \(exe, map Util.Text.unpack -> args) ->
      withCreateProcess (proc exe args) $ \_ _ _ p ->
        exitDecode <$> waitForProcess p

  declareForeign Tracked "IO.process.start" start'process . mkForeign $
    \(exe, map Util.Text.unpack -> args) ->
      runInteractiveProcess exe args Nothing Nothing

  declareForeign Tracked "IO.process.kill" argToUnit . mkForeign $
    terminateProcess

  declareForeign Tracked "IO.process.wait" (argNDirect 1) . mkForeign $
    \ph -> exitDecode <$> waitForProcess ph

  declareForeign Tracked "IO.process.exitCode" argToMaybe . mkForeign $
    fmap (fmap exitDecode) . getProcessExitCode

  declareForeign Tracked "MVar.new" (argNDirect 1)
    . mkForeign
    $ \(c :: Val) -> newMVar c

  declareForeign Tracked "MVar.newEmpty.v2" unitDirect
    . mkForeign
    $ \() -> newEmptyMVar @Val

  declareForeign Tracked "MVar.take.impl.v3" argToEF
    . mkForeignIOF
    $ \(mv :: MVar Val) -> takeMVar mv

  declareForeign Tracked "MVar.tryTake" argToMaybe
    . mkForeign
    $ \(mv :: MVar Val) -> tryTakeMVar mv

  declareForeign Tracked "MVar.put.impl.v3" arg2ToEF0
    . mkForeignIOF
    $ \(mv :: MVar Val, x) -> putMVar mv x

  declareForeign Tracked "MVar.tryPut.impl.v3" arg2ToEFBool
    . mkForeignIOF
    $ \(mv :: MVar Val, x) -> tryPutMVar mv x

  declareForeign Tracked "MVar.swap.impl.v3" arg2ToEF
    . mkForeignIOF
    $ \(mv :: MVar Val, x) -> swapMVar mv x

  declareForeign Tracked "MVar.isEmpty" (argNToBool 1)
    . mkForeign
    $ \(mv :: MVar Val) -> isEmptyMVar mv

  declareForeign Tracked "MVar.read.impl.v3" argToEF
    . mkForeignIOF
    $ \(mv :: MVar Val) -> readMVar mv

  declareForeign Tracked "MVar.tryRead.impl.v3" argToEFM
    . mkForeignIOF
    $ \(mv :: MVar Val) -> tryReadMVar mv

  declareForeign Untracked "Char.toText" (argNDirect 1) . mkForeign $
    \(ch :: Char) -> pure (Util.Text.singleton ch)

  declareForeign Untracked "Text.repeat" (argNDirect 2) . mkForeign $
    \(n :: Word64, txt :: Util.Text.Text) -> pure (Util.Text.replicate (fromIntegral n) txt)

  declareForeign Untracked "Text.reverse" (argNDirect 1) . mkForeign $
    pure . Util.Text.reverse

  declareForeign Untracked "Text.toUppercase" (argNDirect 1) . mkForeign $
    pure . Util.Text.toUppercase

  declareForeign Untracked "Text.toLowercase" (argNDirect 1) . mkForeign $
    pure . Util.Text.toLowercase

  declareForeign Untracked "Text.toUtf8" (argNDirect 1) . mkForeign $
    pure . Util.Text.toUtf8

  declareForeign Untracked "Text.fromUtf8.impl.v3" argToEF . mkForeign $
    pure . mapLeft (\t -> Failure Ty.ioFailureRef (Util.Text.pack t) unitValue) . Util.Text.fromUtf8

  declareForeign Tracked "Tls.ClientConfig.default" (argNDirect 2) . mkForeign $
    \(hostName :: Util.Text.Text, serverId :: Bytes.Bytes) ->
      fmap
        ( \store ->
            (defaultParamsClient (Util.Text.unpack hostName) (Bytes.toArray serverId))
              { TLS.clientSupported = def {TLS.supportedCiphers = Cipher.ciphersuite_strong},
                TLS.clientShared = def {TLS.sharedCAStore = store}
              }
        )
        X.getSystemCertificateStore

  declareForeign Tracked "Tls.ServerConfig.default" (argNDirect 2) $
    mkForeign $
      \(certs :: [X.SignedCertificate], key :: X.PrivKey) ->
        pure $
          (def :: TLS.ServerParams)
            { TLS.serverSupported = def {TLS.supportedCiphers = Cipher.ciphersuite_strong},
              TLS.serverShared = def {TLS.sharedCredentials = Credentials [(X.CertificateChain certs, key)]}
            }

  let updateClient :: X.CertificateStore -> TLS.ClientParams -> TLS.ClientParams
      updateClient certs client = client {TLS.clientShared = ((clientShared client) {TLS.sharedCAStore = certs})}
   in declareForeign Tracked "Tls.ClientConfig.certificates.set" (argNDirect 2) . mkForeign $
        \(certs :: [X.SignedCertificate], params :: ClientParams) -> pure $ updateClient (X.makeCertificateStore certs) params

  let updateServer :: X.CertificateStore -> TLS.ServerParams -> TLS.ServerParams
      updateServer certs client = client {TLS.serverShared = ((serverShared client) {TLS.sharedCAStore = certs})}
   in declareForeign Tracked "Tls.ServerConfig.certificates.set" (argNDirect 2) . mkForeign $
        \(certs :: [X.SignedCertificate], params :: ServerParams) -> pure $ updateServer (X.makeCertificateStore certs) params

  declareForeign Tracked "TVar.new" (argNDirect 1) . mkForeign $
    \(c :: Val) -> unsafeSTMToIO $ STM.newTVar c

  declareForeign Tracked "TVar.read" (argNDirect 1) . mkForeign $
    \(v :: STM.TVar Val) -> unsafeSTMToIO $ STM.readTVar v

  declareForeign Tracked "TVar.write" arg2To0 . mkForeign $
    \(v :: STM.TVar Val, c :: Val) ->
      unsafeSTMToIO $ STM.writeTVar v c

  declareForeign Tracked "TVar.newIO" (argNDirect 1) . mkForeign $
    \(c :: Val) -> STM.newTVarIO c

  declareForeign Tracked "TVar.readIO" (argNDirect 1) . mkForeign $
    \(v :: STM.TVar Val) -> STM.readTVarIO v

  declareForeign Tracked "TVar.swap" (argNDirect 2) . mkForeign $
    \(v, c :: Val) -> unsafeSTMToIO $ STM.swapTVar v c

  declareForeign Tracked "STM.retry" unitDirect . mkForeign $
    \() -> unsafeSTMToIO STM.retry :: IO Val

  declareForeign Tracked "Promise.new" unitDirect . mkForeign $
    \() -> newPromise @Val

  -- the only exceptions from Promise.read are async and shouldn't be caught
  declareForeign Tracked "Promise.read" (argNDirect 1) . mkForeign $
    \(p :: Promise Val) -> readPromise p

  declareForeign Tracked "Promise.tryRead" argToMaybe . mkForeign $
    \(p :: Promise Val) -> tryReadPromise p

  declareForeign Tracked "Promise.write" (argNToBool 2) . mkForeign $
    \(p :: Promise Val, a :: Val) -> writePromise p a

  declareForeign Tracked "Tls.newClient.impl.v3" arg2ToEF . mkForeignTls $
    \( config :: TLS.ClientParams,
       socket :: SYS.Socket
       ) -> TLS.contextNew socket config

  declareForeign Tracked "Tls.newServer.impl.v3" arg2ToEF . mkForeignTls $
    \( config :: TLS.ServerParams,
       socket :: SYS.Socket
       ) -> TLS.contextNew socket config

  declareForeign Tracked "Tls.handshake.impl.v3" argToEF0 . mkForeignTls $
    \(tls :: TLS.Context) -> TLS.handshake tls

  declareForeign Tracked "Tls.send.impl.v3" arg2ToEF0 . mkForeignTls $
    \( tls :: TLS.Context,
       bytes :: Bytes.Bytes
       ) -> TLS.sendData tls (Bytes.toLazyByteString bytes)

  let wrapFailure t = Failure Ty.tlsFailureRef (Util.Text.pack t) unitValue
      decoded :: Bytes.Bytes -> Either String PEM
      decoded bytes = case pemParseLBS $ Bytes.toLazyByteString bytes of
        Right (pem : _) -> Right pem
        Right [] -> Left "no PEM found"
        Left l -> Left l
      asCert :: PEM -> Either String X.SignedCertificate
      asCert pem = X.decodeSignedCertificate $ pemContent pem
   in declareForeign Tracked "Tls.decodeCert.impl.v3" argToEF . mkForeignTlsE $
        \(bytes :: Bytes.Bytes) -> pure $ mapLeft wrapFailure $ (decoded >=> asCert) bytes

  declareForeign Tracked "Tls.encodeCert" (argNDirect 1) . mkForeign $
    \(cert :: X.SignedCertificate) -> pure $ Bytes.fromArray $ X.encodeSignedObject cert

  declareForeign Tracked "Tls.decodePrivateKey" (argNDirect 1) . mkForeign $
    \(bytes :: Bytes.Bytes) -> pure $ X.readKeyFileFromMemory $ L.toStrict $ Bytes.toLazyByteString bytes

  declareForeign Tracked "Tls.encodePrivateKey" (argNDirect 1) . mkForeign $
    \(privateKey :: X.PrivKey) -> pure $ Util.Text.toUtf8 $ Util.Text.pack $ show privateKey

  declareForeign Tracked "Tls.receive.impl.v3" argToEF . mkForeignTls $
    \(tls :: TLS.Context) -> do
      bs <- TLS.recvData tls
      pure $ Bytes.fromArray bs

  declareForeign Tracked "Tls.terminate.impl.v3" argToEF0 . mkForeignTls $
    \(tls :: TLS.Context) -> TLS.bye tls

  declareForeign Untracked "Code.validateLinks" argToExnE
    . mkForeign
    $ \(lsgs0 :: [(Referent, Code)]) -> do
      let f (msg, rs) =
            Failure Ty.miscFailureRef (Util.Text.fromText msg) rs
      pure . first f $ checkGroupHashes lsgs0
  declareForeign Untracked "Code.dependencies" (argNDirect 1)
    . mkForeign
    $ \(CodeRep sg _) ->
      pure $ Wrap Ty.termLinkRef . Ref <$> groupTermLinks sg
  declareForeign Untracked "Code.serialize" (argNDirect 1)
    . mkForeign
    $ \(co :: Code) ->
      pure . Bytes.fromArray $ serializeCode builtinForeignNames co
  declareForeign Untracked "Code.deserialize" argToEither
    . mkForeign
    $ pure . deserializeCode . Bytes.toArray
  declareForeign Untracked "Code.display" (argNDirect 2) . mkForeign $
    \(nm, (CodeRep sg _)) ->
      pure $ prettyGroup @Symbol (Util.Text.unpack nm) sg ""
  declareForeign Untracked "Value.dependencies" (argNDirect 1)
    . mkForeign
    $ pure . fmap (Wrap Ty.termLinkRef . Ref) . valueTermLinks
  declareForeign Untracked "Value.serialize" (argNDirect 1)
    . mkForeign
    $ pure . Bytes.fromArray . serializeValue
  declareForeign Untracked "Value.deserialize" argToEither
    . mkForeign
    $ pure . deserializeValue . Bytes.toArray
  -- Hashing functions
  let declareHashAlgorithm :: forall alg. (Hash.HashAlgorithm alg) => Data.Text.Text -> alg -> FDecl Symbol ()
      declareHashAlgorithm txt alg = do
        let algoRef = Builtin ("crypto.HashAlgorithm." <> txt)
        declareForeign Untracked ("crypto.HashAlgorithm." <> txt) direct . mkForeign $ \() ->
          pure (HashAlgorithm algoRef alg)

  declareHashAlgorithm "Sha3_512" Hash.SHA3_512
  declareHashAlgorithm "Sha3_256" Hash.SHA3_256
  declareHashAlgorithm "Sha2_512" Hash.SHA512
  declareHashAlgorithm "Sha2_256" Hash.SHA256
  declareHashAlgorithm "Sha1" Hash.SHA1
  declareHashAlgorithm "Blake2b_512" Hash.Blake2b_512
  declareHashAlgorithm "Blake2b_256" Hash.Blake2b_256
  declareHashAlgorithm "Blake2s_256" Hash.Blake2s_256
  declareHashAlgorithm "Md5" Hash.MD5

  declareForeign Untracked "crypto.hashBytes" (argNDirect 2) . mkForeign $
    \(HashAlgorithm _ alg, b :: Bytes.Bytes) ->
      let ctx = Hash.hashInitWith alg
       in pure . Bytes.fromArray . Hash.hashFinalize $ Hash.hashUpdates ctx (Bytes.byteStringChunks b)

  declareForeign Untracked "crypto.hmacBytes" (argNDirect 3)
    . mkForeign
    $ \(HashAlgorithm _ alg, key :: Bytes.Bytes, msg :: Bytes.Bytes) ->
      let out = u alg $ HMAC.hmac (Bytes.toArray @BA.Bytes key) (Bytes.toArray @BA.Bytes msg)
          u :: a -> HMAC.HMAC a -> HMAC.HMAC a
          u _ h = h -- to help typechecker along
       in pure $ Bytes.fromArray out

  declareForeign Untracked "crypto.hash" crypto'hash . mkForeign $
    \(HashAlgorithm _ alg, x) ->
      let hashlazy ::
            (Hash.HashAlgorithm a) =>
            a ->
            L.ByteString ->
            Hash.Digest a
          hashlazy _ l = Hash.hashlazy l
       in pure . Bytes.fromArray . hashlazy alg $ serializeValueForHash x

  declareForeign Untracked "crypto.hmac" crypto'hmac . mkForeign $
    \(HashAlgorithm _ alg, key, x) ->
      let hmac ::
            (Hash.HashAlgorithm a) => a -> L.ByteString -> HMAC.HMAC a
          hmac _ s =
            HMAC.finalize
              . HMAC.updates
                (HMAC.initialize $ Bytes.toArray @BA.Bytes key)
              $ L.toChunks s
       in pure . Bytes.fromArray . hmac alg $ serializeValueForHash x

  declareForeign Untracked "crypto.Ed25519.sign.impl" arg3ToEF
    . mkForeign
    $ pure . signEd25519Wrapper

  declareForeign Untracked "crypto.Ed25519.verify.impl" arg3ToEFBool
    . mkForeign
    $ pure . verifyEd25519Wrapper

  declareForeign Untracked "crypto.Rsa.sign.impl" arg2ToEF
    . mkForeign
    $ pure . signRsaWrapper

  declareForeign Untracked "crypto.Rsa.verify.impl" arg3ToEFBool
    . mkForeign
    $ pure . verifyRsaWrapper

  let catchAll :: (MonadCatch m, MonadIO m, NFData a) => m a -> m (Either Util.Text.Text a)
      catchAll e = do
        e <- Exception.tryAnyDeep e
        pure $ case e of
          Left se -> Left (Util.Text.pack (show se))
          Right a -> Right a

  declareForeign Untracked "Universal.murmurHash" murmur'hash . mkForeign $
    pure . asWord64 . hash64 . serializeValueForHash

  declareForeign Tracked "IO.randomBytes" (argNDirect 1) . mkForeign $
    \n -> Bytes.fromArray <$> getRandomBytes @IO @ByteString n

  declareForeign Untracked "Bytes.zlib.compress" (argNDirect 1) . mkForeign $ pure . Bytes.zlibCompress
  declareForeign Untracked "Bytes.gzip.compress" (argNDirect 1) . mkForeign $ pure . Bytes.gzipCompress
  declareForeign Untracked "Bytes.zlib.decompress" argToEither . mkForeign $ \bs ->
    catchAll (pure (Bytes.zlibDecompress bs))
  declareForeign Untracked "Bytes.gzip.decompress" argToEither . mkForeign $ \bs ->
    catchAll (pure (Bytes.gzipDecompress bs))

  declareForeign Untracked "Bytes.toBase16" (argNDirect 1) . mkForeign $ pure . Bytes.toBase16
  declareForeign Untracked "Bytes.toBase32" (argNDirect 1) . mkForeign $ pure . Bytes.toBase32
  declareForeign Untracked "Bytes.toBase64" (argNDirect 1) . mkForeign $ pure . Bytes.toBase64
  declareForeign Untracked "Bytes.toBase64UrlUnpadded" (argNDirect 1) . mkForeign $ pure . Bytes.toBase64UrlUnpadded

  declareForeign Untracked "Bytes.fromBase16" argToEither . mkForeign $
    pure . mapLeft Util.Text.fromText . Bytes.fromBase16
  declareForeign Untracked "Bytes.fromBase32" argToEither . mkForeign $
    pure . mapLeft Util.Text.fromText . Bytes.fromBase32
  declareForeign Untracked "Bytes.fromBase64" argToEither . mkForeign $
    pure . mapLeft Util.Text.fromText . Bytes.fromBase64
  declareForeign Untracked "Bytes.fromBase64UrlUnpadded" argToEither . mkForeign $
    pure . mapLeft Util.Text.fromText . Bytes.fromBase64UrlUnpadded

  declareForeign Untracked "Bytes.decodeNat64be" argToMaybeNTup . mkForeign $ pure . Bytes.decodeNat64be
  declareForeign Untracked "Bytes.decodeNat64le" argToMaybeNTup . mkForeign $ pure . Bytes.decodeNat64le
  declareForeign Untracked "Bytes.decodeNat32be" argToMaybeNTup . mkForeign $ pure . Bytes.decodeNat32be
  declareForeign Untracked "Bytes.decodeNat32le" argToMaybeNTup . mkForeign $ pure . Bytes.decodeNat32le
  declareForeign Untracked "Bytes.decodeNat16be" argToMaybeNTup . mkForeign $ pure . Bytes.decodeNat16be
  declareForeign Untracked "Bytes.decodeNat16le" argToMaybeNTup . mkForeign $ pure . Bytes.decodeNat16le

  declareForeign Untracked "Bytes.encodeNat64be" (argNDirect 1) . mkForeign $ pure . Bytes.encodeNat64be
  declareForeign Untracked "Bytes.encodeNat64le" (argNDirect 1) . mkForeign $ pure . Bytes.encodeNat64le
  declareForeign Untracked "Bytes.encodeNat32be" (argNDirect 1) . mkForeign $ pure . Bytes.encodeNat32be
  declareForeign Untracked "Bytes.encodeNat32le" (argNDirect 1) . mkForeign $ pure . Bytes.encodeNat32le
  declareForeign Untracked "Bytes.encodeNat16be" (argNDirect 1) . mkForeign $ pure . Bytes.encodeNat16be
  declareForeign Untracked "Bytes.encodeNat16le" (argNDirect 1) . mkForeign $ pure . Bytes.encodeNat16le

  declareForeign Untracked "MutableArray.copyTo!" arg5ToExnUnit
    . mkForeign
    $ \(dst, doff, src, soff, l) ->
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

  declareForeign Untracked "MutableByteArray.copyTo!" arg5ToExnUnit
    . mkForeign
    $ \(dst, doff, src, soff, l) ->
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

  declareForeign Untracked "ImmutableArray.copyTo!" arg5ToExnUnit
    . mkForeign
    $ \(dst, doff, src, soff, l) ->
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

  declareForeign Untracked "ImmutableArray.size" (argNDirect 1) . mkForeign $
    pure . fromIntegral @Int @Word64 . PA.sizeofArray @Val
  declareForeign Untracked "MutableArray.size" (argNDirect 1) . mkForeign $
    pure . fromIntegral @Int @Word64 . PA.sizeofMutableArray @PA.RealWorld @Val
  declareForeign Untracked "ImmutableByteArray.size" (argNDirect 1) . mkForeign $
    pure . fromIntegral @Int @Word64 . PA.sizeofByteArray
  declareForeign Untracked "MutableByteArray.size" (argNDirect 1) . mkForeign $
    pure . fromIntegral @Int @Word64 . PA.sizeofMutableByteArray @PA.RealWorld

  declareForeign Untracked "ImmutableByteArray.copyTo!" arg5ToExnUnit
    . mkForeign
    $ \(dst, doff, src, soff, l) ->
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

  declareForeign Untracked "MutableArray.read" arg2ToExn
    . mkForeign
    $ checkedRead "MutableArray.read"
  declareForeign Untracked "MutableByteArray.read8" arg2ToExn
    . mkForeign
    $ checkedRead8 "MutableByteArray.read8"
  declareForeign Untracked "MutableByteArray.read16be" arg2ToExn
    . mkForeign
    $ checkedRead16 "MutableByteArray.read16be"
  declareForeign Untracked "MutableByteArray.read24be" arg2ToExn
    . mkForeign
    $ checkedRead24 "MutableByteArray.read24be"
  declareForeign Untracked "MutableByteArray.read32be" arg2ToExn
    . mkForeign
    $ checkedRead32 "MutableByteArray.read32be"
  declareForeign Untracked "MutableByteArray.read40be" arg2ToExn
    . mkForeign
    $ checkedRead40 "MutableByteArray.read40be"
  declareForeign Untracked "MutableByteArray.read64be" arg2ToExn
    . mkForeign
    $ checkedRead64 "MutableByteArray.read64be"

  declareForeign Untracked "MutableArray.write" arg3ToExnUnit
    . mkForeign
    $ checkedWrite "MutableArray.write"
  declareForeign Untracked "MutableByteArray.write8" arg3ToExnUnit
    . mkForeign
    $ checkedWrite8 "MutableByteArray.write8"
  declareForeign Untracked "MutableByteArray.write16be" arg3ToExnUnit
    . mkForeign
    $ checkedWrite16 "MutableByteArray.write16be"
  declareForeign Untracked "MutableByteArray.write32be" arg3ToExnUnit
    . mkForeign
    $ checkedWrite32 "MutableByteArray.write32be"
  declareForeign Untracked "MutableByteArray.write64be" arg3ToExnUnit
    . mkForeign
    $ checkedWrite64 "MutableByteArray.write64be"

  declareForeign Untracked "ImmutableArray.read" arg2ToExn
    . mkForeign
    $ checkedIndex "ImmutableArray.read"
  declareForeign Untracked "ImmutableByteArray.read8" arg2ToExn
    . mkForeign
    $ checkedIndex8 "ImmutableByteArray.read8"
  declareForeign Untracked "ImmutableByteArray.read16be" arg2ToExn
    . mkForeign
    $ checkedIndex16 "ImmutableByteArray.read16be"
  declareForeign Untracked "ImmutableByteArray.read24be" arg2ToExn
    . mkForeign
    $ checkedIndex24 "ImmutableByteArray.read24be"
  declareForeign Untracked "ImmutableByteArray.read32be" arg2ToExn
    . mkForeign
    $ checkedIndex32 "ImmutableByteArray.read32be"
  declareForeign Untracked "ImmutableByteArray.read40be" arg2ToExn
    . mkForeign
    $ checkedIndex40 "ImmutableByteArray.read40be"
  declareForeign Untracked "ImmutableByteArray.read64be" arg2ToExn
    . mkForeign
    $ checkedIndex64 "ImmutableByteArray.read64be"

  declareForeign Untracked "MutableByteArray.freeze!" (argNDirect 1) . mkForeign $
    PA.unsafeFreezeByteArray
  declareForeign Untracked "MutableArray.freeze!" (argNDirect 1) . mkForeign $
    PA.unsafeFreezeArray @IO @Val

  declareForeign Untracked "MutableByteArray.freeze" arg3ToExn . mkForeign $
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

  declareForeign Untracked "MutableArray.freeze" arg3ToExn . mkForeign $
    \(src :: PA.MutableArray PA.RealWorld Val, off, len) ->
      if len == 0
        then fmap Right . PA.unsafeFreezeArray =<< PA.newArray 0 emptyVal
        else
          checkBounds
            "MutableArray.freeze"
            (PA.sizeofMutableArray src)
            (off + len - 1)
            $ Right <$> PA.freezeArray src (fromIntegral off) (fromIntegral len)

  declareForeign Untracked "MutableByteArray.length" (argNDirect 1) . mkForeign $
    pure . PA.sizeofMutableByteArray @PA.RealWorld

  declareForeign Untracked "ImmutableByteArray.length" (argNDirect 1) . mkForeign $
    pure . PA.sizeofByteArray

  declareForeign Tracked "IO.array" (argNDirect 1) . mkForeign $
    \n -> PA.newArray n emptyVal
  declareForeign Tracked "IO.arrayOf" (argNDirect 2) . mkForeign $
    \(v :: Val, n) -> PA.newArray n v
  declareForeign Tracked "IO.bytearray" (argNDirect 1) . mkForeign $ PA.newByteArray
  declareForeign Tracked "IO.bytearrayOf" (argNDirect 2)
    . mkForeign
    $ \(init, sz) -> do
      arr <- PA.newByteArray sz
      PA.fillByteArray arr 0 sz init
      pure arr

  declareForeign Untracked "Scope.array" (argNDirect 1) . mkForeign $
    \n -> PA.newArray n emptyVal
  declareForeign Untracked "Scope.arrayOf" (argNDirect 2) . mkForeign $
    \(v :: Val, n) -> PA.newArray n v
  declareForeign Untracked "Scope.bytearray" (argNDirect 1) . mkForeign $ PA.newByteArray
  declareForeign Untracked "Scope.bytearrayOf" (argNDirect 2)
    . mkForeign
    $ \(init, sz) -> do
      arr <- PA.newByteArray sz
      PA.fillByteArray arr 0 sz init
      pure arr

  declareForeign Untracked "Text.patterns.literal" (argNDirect 1) . mkForeign $
    \txt -> evaluate . TPat.cpattern $ TPat.Literal txt
  declareForeign Untracked "Text.patterns.digit" direct . mkForeign $
    let v = TPat.cpattern (TPat.Char (TPat.CharRange '0' '9')) in \() -> pure v
  declareForeign Untracked "Text.patterns.letter" direct . mkForeign $
    let v = TPat.cpattern (TPat.Char (TPat.CharClass TPat.Letter)) in \() -> pure v
  declareForeign Untracked "Text.patterns.space" direct . mkForeign $
    let v = TPat.cpattern (TPat.Char (TPat.CharClass TPat.Whitespace)) in \() -> pure v
  declareForeign Untracked "Text.patterns.punctuation" direct . mkForeign $
    let v = TPat.cpattern (TPat.Char (TPat.CharClass TPat.Punctuation)) in \() -> pure v
  declareForeign Untracked "Text.patterns.anyChar" direct . mkForeign $
    let v = TPat.cpattern (TPat.Char TPat.Any) in \() -> pure v
  declareForeign Untracked "Text.patterns.eof" direct . mkForeign $
    let v = TPat.cpattern TPat.Eof in \() -> pure v
  declareForeign Untracked "Text.patterns.charRange" (argNDirect 2) . mkForeign $
    \(beg, end) -> evaluate . TPat.cpattern . TPat.Char $ TPat.CharRange beg end
  declareForeign Untracked "Text.patterns.notCharRange" (argNDirect 2) . mkForeign $
    \(beg, end) -> evaluate . TPat.cpattern . TPat.Char . TPat.Not $ TPat.CharRange beg end
  declareForeign Untracked "Text.patterns.charIn" (argNDirect 1) . mkForeign $ \ccs -> do
    cs <- for ccs $ \case
      CharVal c -> pure c
      _ -> die "Text.patterns.charIn: non-character closure"
    evaluate . TPat.cpattern . TPat.Char $ TPat.CharSet cs
  declareForeign Untracked "Text.patterns.notCharIn" (argNDirect 1) . mkForeign $ \ccs -> do
    cs <- for ccs $ \case
      CharVal c -> pure c
      _ -> die "Text.patterns.notCharIn: non-character closure"
    evaluate . TPat.cpattern . TPat.Char . TPat.Not $ TPat.CharSet cs
  declareForeign Untracked "Pattern.many" (argNDirect 1) . mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.Many False p
  declareForeign Untracked "Pattern.many.corrected" (argNDirect 1) . mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.Many True p
  declareForeign Untracked "Pattern.capture" (argNDirect 1) . mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.Capture p
  declareForeign Untracked "Pattern.captureAs" (argNDirect 2) . mkForeign $
    \(t, (TPat.CP p _)) -> evaluate . TPat.cpattern $ TPat.CaptureAs t p
  declareForeign Untracked "Pattern.join" (argNDirect 1) . mkForeign $ \ps ->
    evaluate . TPat.cpattern . TPat.Join $ map (\(TPat.CP p _) -> p) ps
  declareForeign Untracked "Pattern.or" (argNDirect 2) . mkForeign $
    \(TPat.CP l _, TPat.CP r _) -> evaluate . TPat.cpattern $ TPat.Or l r
  declareForeign Untracked "Pattern.replicate" (argNDirect 3) . mkForeign $
    \(m0 :: Word64, n0 :: Word64, TPat.CP p _) ->
      let m = fromIntegral m0; n = fromIntegral n0
       in evaluate . TPat.cpattern $ TPat.Replicate m n p

  declareForeign Untracked "Pattern.run" arg2ToMaybeTup . mkForeign $
    \(TPat.CP _ matcher, input :: Text) -> pure $ matcher input

  declareForeign Untracked "Pattern.isMatch" (argNToBool 2) . mkForeign $
    \(TPat.CP _ matcher, input :: Text) -> pure . isJust $ matcher input

  declareForeign Untracked "Char.Class.any" direct . mkForeign $ \() -> pure TPat.Any
  declareForeign Untracked "Char.Class.not" (argNDirect 1) . mkForeign $ pure . TPat.Not
  declareForeign Untracked "Char.Class.and" (argNDirect 2) . mkForeign $ \(a, b) -> pure $ TPat.Intersect a b
  declareForeign Untracked "Char.Class.or" (argNDirect 2) . mkForeign $ \(a, b) -> pure $ TPat.Union a b
  declareForeign Untracked "Char.Class.range" (argNDirect 2) . mkForeign $ \(a, b) -> pure $ TPat.CharRange a b
  declareForeign Untracked "Char.Class.anyOf" (argNDirect 1) . mkForeign $ \ccs -> do
    cs <- for ccs $ \case
      CharVal c -> pure c
      _ -> die "Text.patterns.charIn: non-character closure"
    evaluate $ TPat.CharSet cs
  declareForeign Untracked "Char.Class.alphanumeric" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.AlphaNum)
  declareForeign Untracked "Char.Class.upper" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.Upper)
  declareForeign Untracked "Char.Class.lower" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.Lower)
  declareForeign Untracked "Char.Class.whitespace" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.Whitespace)
  declareForeign Untracked "Char.Class.control" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.Control)
  declareForeign Untracked "Char.Class.printable" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.Printable)
  declareForeign Untracked "Char.Class.mark" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.MarkChar)
  declareForeign Untracked "Char.Class.number" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.Number)
  declareForeign Untracked "Char.Class.punctuation" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.Punctuation)
  declareForeign Untracked "Char.Class.symbol" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.Symbol)
  declareForeign Untracked "Char.Class.separator" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.Separator)
  declareForeign Untracked "Char.Class.letter" direct . mkForeign $ \() -> pure (TPat.CharClass TPat.Letter)
  declareForeign Untracked "Char.Class.is" (argNToBool 2) . mkForeign $ \(cl, c) -> evaluate $ TPat.charPatternPred cl c
  declareForeign Untracked "Text.patterns.char" (argNDirect 1) . mkForeign $ \c ->
    let v = TPat.cpattern (TPat.Char c) in pure v

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

checkBounds :: Text -> Int -> Word64 -> IO (Either Failure b) -> IO (Either Failure b)
checkBounds name l w act
  | w < fromIntegral l = act
  | otherwise = pure $ Left err
  where
    msg = name <> ": array index out of bounds"
    err = Failure Ty.arrayFailureRef msg (natValue w)

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
    err = Failure Ty.arrayFailureRef msg (natValue off)

    bsz = fromIntegral isz
    w = off + esz

hostPreference :: Maybe Util.Text.Text -> SYS.HostPreference
hostPreference Nothing = SYS.HostAny
hostPreference (Just host) = SYS.Host $ Util.Text.unpack host

signEd25519Wrapper ::
  (Bytes.Bytes, Bytes.Bytes, Bytes.Bytes) -> Either Failure Bytes.Bytes
signEd25519Wrapper (secret0, public0, msg0) = case validated of
  CryptoFailed err ->
    Left (Failure Ty.cryptoFailureRef (errMsg err) unitValue)
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
    Left $ Failure Ty.cryptoFailureRef (errMsg err) unitValue
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
    Left (Failure Ty.cryptoFailureRef err unitValue)
  Right secret ->
    case RSA.sign Nothing (Just Hash.SHA256) secret msg of
      Left err -> Left (Failure Ty.cryptoFailureRef (Rsa.rsaErrorToText err) unitValue)
      Right signature -> Right $ Bytes.fromByteString signature
  where
    msg = Bytes.toArray msg0 :: ByteString
    validated = Rsa.parseRsaPrivateKey (Bytes.toArray secret0 :: ByteString)

verifyRsaWrapper ::
  (Bytes.Bytes, Bytes.Bytes, Bytes.Bytes) -> Either Failure Bool
verifyRsaWrapper (public0, msg0, sig0) = case validated of
  Left err ->
    Left $ Failure Ty.cryptoFailureRef err unitValue
  Right public ->
    Right $ RSA.verify (Just Hash.SHA256) public msg sig
  where
    msg = Bytes.toArray msg0 :: ByteString
    sig = Bytes.toArray sig0 :: ByteString
    validated = Rsa.parseRsaPublicKey (Bytes.toArray public0 :: ByteString)

foreignDeclResults ::
  Bool -> (Word64, [(Data.Text.Text, (Sandbox, SuperNormal Symbol))], EnumMap Word64 (Data.Text.Text, ForeignFunc))
foreignDeclResults sanitize =
  execState (runReaderT declareForeigns sanitize) (0, [], mempty)

foreignWrappers :: [(Data.Text.Text, (Sandbox, SuperNormal Symbol))]
foreignWrappers | (_, l, _) <- foreignDeclResults False = reverse l

numberedTermLookup :: EnumMap Word64 (SuperNormal Symbol)
numberedTermLookup =
  mapFromList . zip [1 ..] . Map.elems . fmap snd $ builtinLookup

builtinTermNumbering :: Map Reference Word64
builtinTermNumbering =
  Map.fromList (zip (Map.keys $ builtinLookup) [1 ..])

builtinTermBackref :: EnumMap Word64 Reference
builtinTermBackref =
  mapFromList . zip [1 ..] . Map.keys $ builtinLookup

builtinForeigns :: EnumMap Word64 ForeignFunc
builtinForeigns | (_, _, m) <- foreignDeclResults False = snd <$> m

sandboxedForeigns :: EnumMap Word64 ForeignFunc
sandboxedForeigns | (_, _, m) <- foreignDeclResults True = snd <$> m

builtinForeignNames :: EnumMap Word64 Data.Text.Text
builtinForeignNames | (_, _, m) <- foreignDeclResults False = fst <$> m

-- Bootstrapping for sandbox check. The eventual map will be one with
-- associations `r -> s` where `s` is all the 'sensitive' base
-- functions that `r` calls.
baseSandboxInfo :: Map Reference (Set Reference)
baseSandboxInfo =
  Map.fromList $
    [ (r, Set.singleton r)
      | (r, (sb, _)) <- Map.toList builtinLookup,
        sb == Tracked
    ]

builtinArities :: Map Reference Int
builtinArities =
  Map.fromList $
    [(r, arity s) | (r, (_, s)) <- Map.toList builtinLookup]

builtinInlineInfo :: Map Reference (Int, ANormal Symbol)
builtinInlineInfo =
  ANF.buildInlineMap $ fmap (Rec [] . snd) builtinLookup

unsafeSTMToIO :: STM.STM a -> IO a
unsafeSTMToIO (STM.STM m) = IO m
