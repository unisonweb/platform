
module Unison.Runtime.Machine.Primops where

import Control.Exception
import Control.Concurrent.STM as STM
import Data.Atomics qualified as Atomic
import Data.Bits
import Data.Functor.Classes (Eq1 (..), Ord1 (..))
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Word
import Unison.Builtin.Decls qualified as Ty
import Unison.Prelude hiding (Text)
import Unison.Reference (Reference)
import Unison.Referent (Referent, pattern Ref, toShortHash)
import Unison.Runtime.ANF (PackedTag (..), maskTags, Value, Code, codeGroup)
import Unison.Runtime.Array as PA
import Unison.Runtime.Foreign
import Unison.Runtime.Foreign.Function
import Unison.Runtime.Machine.Types
import Unison.Runtime.MCode
import Unison.Runtime.Stack
import Unison.Runtime.TypeTags qualified as Ty
import Unison.ShortHash qualified as SH
import Unison.Type qualified as Rf
import Unison.Util.Bytes (Bytes)
import Unison.Util.Bytes qualified as By
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Text as UText

prim1wrap ::
  ForeignConvention x =>
  (Stack -> x -> IO ()) ->
  Stack -> Int -> IO Stack
prim1wrap f stk i = do
  x <- readAtIndex stk i
  stk <- bump stk
  stk <$ f stk x
{-# inline prim1wrap #-}

prim1 :: CCache -> Stack -> Prim1 -> Int -> IO Stack
prim1 !_env !stk DECI !i = prim1wrap deci stk i
prim1 !_env !stk DECN !i = prim1wrap decn stk i
prim1 !_env !stk INCI !i = prim1wrap inci stk i
prim1 !_env !stk INCN !i = prim1wrap incn stk i
prim1 !_env !stk TRNC !i = prim1wrap trnc stk i
prim1 !_env !stk NEGI !i = prim1wrap negi stk i
prim1 !_env !stk SGNI !i = prim1wrap sgni stk i
prim1 !_env !stk ABSF !i = prim1wrap absf stk i
prim1 !_env !stk CEIL !i = prim1wrap ceil stk i
prim1 !_env !stk FLOR !i = prim1wrap flor stk i
prim1 !_env !stk TRNF !i = prim1wrap trnf stk i
prim1 !_env !stk RNDF !i = prim1wrap rndf stk i
prim1 !_env !stk EXPF !i = prim1wrap expf stk i
prim1 !_env !stk LOGF !i = prim1wrap logf stk i
prim1 !_env !stk SQRT !i = prim1wrap sqtf stk i
prim1 !_env !stk COSF !i = prim1wrap cosf stk i
prim1 !_env !stk SINF !i = prim1wrap sinf stk i
prim1 !_env !stk TANF !i = prim1wrap tanf stk i
prim1 !_env !stk COSH !i = prim1wrap cshf stk i
prim1 !_env !stk SINH !i = prim1wrap snhf stk i
prim1 !_env !stk TANH !i = prim1wrap tnhf stk i
prim1 !_env !stk ACOS !i = prim1wrap acsf stk i
prim1 !_env !stk ASIN !i = prim1wrap asnf stk i
prim1 !_env !stk ATAN !i = prim1wrap atnf stk i
prim1 !_env !stk ASNH !i = prim1wrap asnh stk i
prim1 !_env !stk ACSH !i = prim1wrap acsh stk i
prim1 !_env !stk ATNH !i = prim1wrap atnh stk i
prim1 !_env !stk ITOF !i = prim1wrap itof stk i
prim1 !_env !stk NTOF !i = prim1wrap ntof stk i
prim1 !_env !stk LZRO !i = prim1wrap lzro stk i
prim1 !_env !stk TZRO !i = prim1wrap tzro stk i
prim1 !_env !stk POPC !i = prim1wrap popc stk i
prim1 !_env !stk COMN !i = prim1wrap comn stk i
prim1 !_env !stk COMI !i = prim1wrap comi stk i
prim1 !_env !stk NOTB !i = prim1wrap notb stk i
prim1 !_env !stk SIZT i = prim1wrap sizt stk i
prim1 !_env !stk SIZS i = prim1wrap sizs stk i
prim1 !_env !stk ITOT i = prim1wrap itot stk i
prim1 !_env !stk NTOT i = prim1wrap ntot stk i
prim1 !_env !stk FTOT i = prim1wrap ftot stk i
prim1 !_env !stk USNC i = prim1wrap usnc stk i
prim1 !_env !stk UCNS i = prim1wrap ucns stk i
prim1 !_env !stk TTOI i = prim1wrap ttoi stk i
prim1 !_env !stk TTON i = prim1wrap tton stk i
prim1 !_env !stk TTOF i = prim1wrap ttof stk i
prim1 !_env !stk VWLS i = prim1wrap vwls stk i
prim1 !_env !stk VWRS i = prim1wrap vwrs stk i
prim1 !_env !stk PAKT i = prim1wrap pakt stk i
prim1 !_env !stk UPKT i = prim1wrap upkt stk i
prim1 !_env !stk PAKB i = prim1wrap pakb stk i
prim1 !_env !stk UPKB i = prim1wrap upkb stk i
prim1 !_env !stk SIZB i = prim1wrap sizb stk i
prim1 !_env !stk FLTB i = prim1wrap fltb stk i
prim1 !_env !stk REFR i = prim1wrap refr stk i
prim1 !_env !stk REFN i = prim1wrap refn stk i
prim1 !env !stk RRFC i = prim1wrap (rrfc env) stk i
prim1 !_env !stk TIKR i = prim1wrap tikr stk i

prim1 !env !stk MISS i = prim1wrap (miss env) stk i
prim1 !env !stk SDBL i = prim1wrap (sdbl env) stk i
prim1 !env !stk LKUP i = prim1wrap (lkup env) stk i
prim1 !env !stk CVLD i = prim1wrap (cvld env) stk i
prim1 !_env !stk TLTT i = prim1wrap tltt stk i
prim1 !env !stk DBTX i = prim1wrap (dbtx env) stk i

-- handled elsewhere
prim1 !_env !stk CACH _ = pure stk
prim1 !_env !stk LOAD _ = pure stk
prim1 !_env !stk VALU _ = pure stk
{-# inline prim1 #-}

-- Wrap an implementation to act on an index on two indices
prim2wrap2 ::
  ForeignConvention x =>
  ForeignConvention y =>
  (Stack -> x -> y -> IO ()) ->
  Stack -> Int -> Int -> IO Stack
prim2wrap2 f stk i j = do
  x <- readAtIndex stk i
  y <- readAtIndex stk j
  stk <- bump stk
  stk <$ f stk x y
{-# inline prim2wrap2 #-}

-- Primops applied to two stack indices
primxx :: CCache -> Stack -> Prim2 -> Int -> Int -> IO Stack
primxx _env stk ADDI i j = prim2wrap2 addi stk i j
primxx _env stk SUBI i j = prim2wrap2 subi stk i j
primxx _env stk MULI i j = prim2wrap2 muli stk i j
primxx _env stk DIVI i j = prim2wrap2 divi stk i j
primxx _env stk MODI i j = prim2wrap2 modi stk i j
primxx _env stk EQLI i j = prim2wrap2 eqli stk i j
primxx _env stk NEQI i j = prim2wrap2 neqi stk i j
primxx _env stk LEQI i j = prim2wrap2 leqi stk i j
primxx _env stk LESI i j = prim2wrap2 lesi stk i j
primxx _env stk ANDI i j = prim2wrap2 andi stk i j
primxx _env stk IORI i j = prim2wrap2 iori stk i j
primxx _env stk XORI i j = prim2wrap2 xori stk i j
primxx _env stk SHLI i j = prim2wrap2 shli stk i j
primxx _env stk SHRI i j = prim2wrap2 shri stk i j
primxx _env stk POWI i j = prim2wrap2 powi stk i j

primxx _env stk ADDN i j = prim2wrap2 addn stk i j
primxx _env stk SUBN i j = prim2wrap2 subn stk i j
primxx _env stk MULN i j = prim2wrap2 muln stk i j
primxx _env stk DIVN i j = prim2wrap2 divn stk i j
primxx _env stk MODN i j = prim2wrap2 modn stk i j
primxx _env stk SHLN i j = prim2wrap2 shln stk i j
primxx _env stk SHRN i j = prim2wrap2 shrn stk i j
primxx _env stk POWN i j = prim2wrap2 pown stk i j
primxx _env stk EQLN i j = prim2wrap2 eqln stk i j
primxx _env stk NEQN i j = prim2wrap2 neqn stk i j
primxx _env stk LEQN i j = prim2wrap2 leqn stk i j
primxx _env stk LESN i j = prim2wrap2 lesn stk i j
primxx _env stk ANDN i j = prim2wrap2 andn stk i j
primxx _env stk IORN i j = prim2wrap2 iorn stk i j
primxx _env stk XORN i j = prim2wrap2 xorn stk i j
primxx _env stk DRPN i j = prim2wrap2 drpn stk i j

primxx _env stk EQLF i j = prim2wrap2 eqlf stk i j
primxx _env stk NEQF i j = prim2wrap2 neqf stk i j
primxx _env stk LEQF i j = prim2wrap2 leqf stk i j
primxx _env stk LESF i j = prim2wrap2 lesf stk i j
primxx _env stk ADDF i j = prim2wrap2 addf stk i j
primxx _env stk SUBF i j = prim2wrap2 subf stk i j
primxx _env stk MULF i j = prim2wrap2 mulf stk i j
primxx _env stk DIVF i j = prim2wrap2 divf stk i j
primxx _env stk ATN2 i j = prim2wrap2 atn2 stk i j
primxx _env stk POWF i j = prim2wrap2 powf stk i j
primxx _env stk LOGB i j = prim2wrap2 logb stk i j
primxx _env stk MAXF i j = prim2wrap2 maxf stk i j
primxx _env stk MINF i j = prim2wrap2 minf stk i j
primxx _env stk DRPT i j = prim2wrap2 drpt stk i j
primxx _env stk TAKT i j = prim2wrap2 takt stk i j
primxx _env stk CATT i j = prim2wrap2 catt stk i j
primxx _env stk IXOT i j = prim2wrap2 ixot stk i j
primxx _env stk EQLT i j = prim2wrap2 eqlt stk i j
primxx _env stk LEQT i j = prim2wrap2 leqt stk i j
primxx _env stk LEST i j = prim2wrap2 lest stk i j
primxx _env stk EQLU i j = prim2wrap2 eqlu stk i j
primxx _env stk CMPU i j = prim2wrap2 cmpu stk i j
primxx _env stk LEQU i j = prim2wrap2 lequ stk i j
primxx _env stk LESU i j = prim2wrap2 lesu stk i j
primxx _env stk DRPS i j = prim2wrap2 drps stk i j
primxx _env stk TAKS i j = prim2wrap2 taks stk i j
primxx _env stk CONS i j = prim2wrap2 cons stk i j
primxx _env stk SNOC i j = prim2wrap2 snoc stk i j
primxx _env stk IDXS i j = prim2wrap2 idxs stk i j
primxx _env stk SPLL i j = prim2wrap2 spll stk i j
primxx _env stk SPLR i j = prim2wrap2 splr stk i j
primxx _env stk CATS i j = prim2wrap2 cats stk i j
primxx _env stk TAKB i j = prim2wrap2 takb stk i j
primxx _env stk DRPB i j = prim2wrap2 drpb stk i j
primxx _env stk IDXB i j = prim2wrap2 idxb stk i j
primxx _env stk CATB i j = prim2wrap2 catb stk i j
primxx _env stk IXOB i j = prim2wrap2 ixob stk i j
primxx _env stk REFW i j = prim2wrap2 refw stk i j
primxx _env stk CAST i j = prim2wrap2 cast stk i j
primxx _env stk ANDB i j = prim2wrap2 andb stk i j
primxx _env stk IORB i j = prim2wrap2 iorb stk i j

primxx env stk SDBV i j = prim2wrap2 (sdbv env) stk i j
primxx env stk SDBX i j = prim2wrap2 (sdbx env) stk i j

-- handled elsewhere
primxx _env stk THRO _ _ = pure stk
primxx _env stk TRCE _ _ = pure stk
{-# inline primxx #-}

termLinkVal :: Referent -> Val
termLinkVal = BoxedVal . Foreign . Wrap Rf.termLinkRef

typeLinkVal :: Reference -> Val
typeLinkVal = BoxedVal . Foreign . Wrap Rf.typeLinkRef

-- Evaluation and writeback portion of primops.
--
-- These are defined so that they can be common, and just referenced
-- in the various permutations of their application to literals or
-- variables.
--
-- The bodies always accept a stack that has already been bumped to
-- the appropriate location for their return result.
deci :: Stack -> Int -> IO ()
deci stk m = pokeI stk (m - 1)

decn :: Stack -> Word64 -> IO ()
decn stk m = pokeN stk (m - 1)

inci :: Stack -> Int -> IO ()
inci stk m = pokeI stk (m + 1)

incn :: Stack -> Word64 -> IO ()
incn stk m = pokeN stk (m + 1)

trnc :: Stack -> Int -> IO ()
trnc stk v = unsafePokeIasN stk (max 0 v)

negi :: Stack -> Int -> IO ()
negi stk m = pokeI stk (-m)

sgni :: Stack -> Int -> IO ()
sgni stk m = pokeI stk (signum m)

absf :: Stack -> Double -> IO ()
absf stk d = pokeD stk (abs d)

ceil :: Stack -> Double -> IO ()
ceil stk d = pokeI stk (ceiling d)

flor :: Stack -> Double -> IO ()
flor stk d = pokeI stk (floor d)

trnf :: Stack -> Double -> IO ()
trnf stk d = pokeI stk (truncate d)

rndf :: Stack -> Double -> IO ()
rndf stk d = pokeI stk (round d)

expf :: Stack -> Double -> IO ()
expf stk d = pokeD stk (exp d)

logf :: Stack -> Double -> IO ()
logf stk d = pokeD stk (log d)

sqtf :: Stack -> Double -> IO ()
sqtf stk d = pokeD stk (sqrt d)

cosf :: Stack -> Double -> IO ()
cosf stk d = pokeD stk (cos d)

sinf :: Stack -> Double -> IO ()
sinf stk d = pokeD stk (sin d)

tanf :: Stack -> Double -> IO ()
tanf stk d = pokeD stk (tan d)

cshf :: Stack -> Double -> IO ()
cshf stk d = pokeD stk (cosh d)

snhf :: Stack -> Double -> IO ()
snhf stk d = pokeD stk (sinh d)

tnhf :: Stack -> Double -> IO ()
tnhf stk d = pokeD stk (tanh d)

acsf :: Stack -> Double -> IO ()
acsf stk d = pokeD stk (acos d)

asnf :: Stack -> Double -> IO ()
asnf stk d = pokeD stk (asin d)

atnf :: Stack -> Double -> IO ()
atnf stk d = pokeD stk (atan d)

asnh :: Stack -> Double -> IO ()
asnh stk d = pokeD stk (asinh d)

acsh :: Stack -> Double -> IO ()
acsh stk d = pokeD stk (acosh d)

atnh :: Stack -> Double -> IO ()
atnh stk d = pokeD stk (atanh d)

itof :: Stack -> Int -> IO ()
itof stk n = pokeD stk (fromIntegral n)

ntof :: Stack -> Word64 -> IO ()
ntof stk n = pokeD stk (fromIntegral n)

lzro :: Stack -> Word64 -> IO ()
lzro stk n = unsafePokeIasN stk (countLeadingZeros n)

tzro :: Stack -> Word64 -> IO ()
tzro stk n = unsafePokeIasN stk (countTrailingZeros n)

popc :: Stack -> Word64 -> IO ()
popc stk n = unsafePokeIasN stk (popCount n)

comn :: Stack -> Word64 -> IO ()
comn stk n = pokeN stk (complement n)

comi :: Stack -> Int -> IO ()
comi stk n = pokeI stk (complement n)

notb :: Stack -> Bool -> IO ()
notb stk b = pokeBool stk (not b)

sizt :: Stack -> Text -> IO ()
sizt stk t = unsafePokeIasN stk $ UText.size t

sizs :: Stack -> USeq -> IO ()
sizs stk s = unsafePokeIasN stk $ Sq.length s

itot :: Stack -> Int -> IO ()
itot stk n = pokeBi stk . UText.pack $ show n

ntot :: Stack -> Word64 -> IO ()
ntot stk n = pokeBi stk . UText.pack $ show n

ftot :: Stack -> Double -> IO ()
ftot stk f = pokeBi stk . UText.pack $ show f

usnc :: Stack -> Text -> IO ()
usnc stk t = writeBack stk (UText.unsnoc t)
{-# inline usnc #-}

ucns :: Stack -> Text -> IO ()
ucns stk t = writeBack stk (UText.uncons t)
{-# inline ucns #-}

readIntegral :: (Bounded n, Integral n) => String -> Maybe n
readIntegral s = clamp =<< readMaybe s
{-# inline readIntegral #-}

clamp :: forall n. (Bounded n, Integral n) => Integer -> Maybe n
clamp i
  | minb <= i, i <= maxb = Just $ fromInteger i
  | otherwise = Nothing
  where
    minb = fromIntegral (minBound :: n)
    maxb = fromIntegral (maxBound :: n)
{-# inline clamp #-}

ttoi :: Stack -> Text -> IO ()
ttoi stk t = writeBack stk (readi $ UText.unpack t)
  where
    readi :: String -> Maybe Int
    readi ('+' : s) = readIntegral s
    readi s = readIntegral s
{-# inline ttoi #-}

tton :: Stack -> Text -> IO ()
tton stk t = writeBack stk (readIntegral @Word64 $ UText.unpack t)
{-# inline tton #-}

ttof :: Stack -> Text -> IO ()
ttof stk t = writeBack stk (readd $ UText.unpack t)
  where
    readd :: String -> Maybe Double
    readd = readMaybe

vwls :: Stack -> USeq -> IO ()
vwls stk s = writeBack stk result
  where
    result = case s of
      Sq.Empty -> SeqViewEmpty
      x Sq.:<| xs -> SeqViewElem x xs
{-# inline vwls #-}

vwrs :: Stack -> USeq -> IO ()
vwrs stk s = writeBack stk result
  where
    result = case s of
      Sq.Empty -> SeqViewEmpty
      xs Sq.:|> x -> SeqViewElem xs x
{-# inline vwrs #-}

pakt :: Stack -> USeq -> IO ()
pakt stk s = pokeBi stk . UText.pack . toList $ val2char <$> s
  where
    val2char :: Val -> Char
    val2char (CharVal c) = c
    val2char c = error $ "pack text: non-character closure: " ++ show c

upkt :: Stack -> Text -> IO ()
upkt stk t =
  pokeS stk
    . Sq.fromList
    . fmap CharVal
    . UText.unpack
    $ t

pakb :: Stack -> USeq -> IO ()
pakb stk s = pokeBi stk . By.fromWord8s . fmap val2w8 $ toList s
  where
    -- TODO: Should we have a tag for bytes specifically?
    val2w8 :: Val -> Word8
    val2w8 (NatVal n) = toEnum . fromEnum $ n
    val2w8 c = error $ "pack bytes: non-natural closure: " ++ show c

upkb :: Stack -> Bytes -> IO ()
upkb stk b =
  pokeS stk . Sq.fromList . fmap (NatVal . toEnum @Word64 . fromEnum @Word8) $
    By.toWord8s b

sizb :: Stack -> Bytes -> IO ()
sizb stk b = unsafePokeIasN stk $ By.size b

fltb :: Stack -> Bytes -> IO ()
fltb stk b = pokeBi stk $ By.flatten b

-- The docs for IORef state that IORef operations can be observed
-- out of order ([1]) but actually GHC does emit the appropriate
-- load and store barriers nowadays ([2], [3]).
--
-- [1] https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-IORef.html#g:2
-- [2] https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs#L286
-- [3] https://github.com/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs#L298
refr :: Stack -> IORef Val -> IO ()
refr stk ref = IORef.readIORef ref >>= poke stk

refn :: Stack -> Val -> IO ()
refn stk v = do
  -- Note that the CAS machinery is extremely fussy w/r to whether things are forced because it
  -- uses unsafe pointer equality. The only way we've gotten it to work as expected is with liberal
  -- forcing of the values and tickets.
  !v <- evaluate v
  ref <- IORef.newIORef v
  pokeBi stk ref

rrfc :: CCache -> Stack -> IORef Val -> IO ()
rrfc env stk ref
  | sandboxed env = die "attempted to use sandboxed operation: Ref.readForCAS"
  | otherwise = do
      ticket <- Atomic.readForCAS ref
      pokeBi stk ticket

tikr :: Stack -> Atomic.Ticket Val -> IO ()
tikr stk t = poke stk (Atomic.peekTicket t)

miss :: CCache -> Stack -> Referent -> IO ()
miss env stk tl
  | sandboxed env = die "attempted to use sandboxed operation: isMissing"
  | otherwise = case tl of
      Ref link -> do
        m <- readTVarIO (intermed env)
        pokeBool stk (link `M.member` m)
      _ -> die "exec:prim1:MISS: expected Ref"
{-# inline miss #-}

sdbl :: CCache -> Stack -> Referent -> IO ()
sdbl env stk tl = writeBack stk =<< sandboxList env tl
{-# inline sdbl #-}

sandboxList :: CCache -> Referent -> IO [Reference]
sandboxList cc (Ref r) = do
  sands <- readTVarIO $ sandbox cc
  pure . maybe [] S.toList $ M.lookup r sands
sandboxList _ _ = pure []

lkup :: CCache -> Stack -> Referent -> IO ()
lkup env stk tl
  | sandboxed env = die "attempted to use sandboxed operation: lookup"
  | otherwise = writeBack stk =<< lookupCode env tl
{-# inline lkup #-}

cvld :: CCache -> Stack -> [(Referent, Code)] -> IO ()
cvld env stk news
  | sandboxed env = die "attempted to use sandboxed operation: validate"
  | otherwise =
      traverse extract news >>= codeValidate env >>= writeBack stk
  where
    extract (Ref r, code) = pure (r, codeGroup code)
    extract _ = die "Prim1:CVLD: Con reference"
{-# inline cvld #-}

tltt :: Stack -> Referent -> IO ()
tltt stk r =
  pokeBi stk . UText.fromText . SH.toText $ toShortHash r
{-# inline tltt #-}

dbtx :: CCache -> Stack -> Val -> IO ()
dbtx env stk val
  | sandboxed env =
      die "attempted to use sandboxed operation: Debug.toText"
  | otherwise = writeBack stk traced
  where
    traced = case tracer env False val of
      NoTrace -> Nothing
      MsgTrace _ _ tx -> Just . Left $ UText.pack tx
      SimpleTrace tx -> Just . Right $ UText.pack tx
{-# inline dbtx #-}

addi :: Stack -> Int -> Int -> IO ()
addi stk m n = pokeI stk (m + n)
{-# inline addi #-}

subi :: Stack -> Int -> Int -> IO ()
subi stk m n = pokeI stk (m - n)
{-# inline subi #-}

muli :: Stack -> Int -> Int -> IO ()
muli stk m n = pokeI stk (m * n)
{-# inline muli #-}

divi :: Stack -> Int -> Int -> IO ()
divi stk m n = pokeI stk (m `div` n)
{-# inline divi #-}

modi :: Stack -> Int -> Int -> IO ()
modi stk m n = pokeI stk (m `mod` n)
{-# inline modi #-}

eqli :: Stack -> Int -> Int -> IO ()
eqli stk m n = pokeBool stk (m == n)
{-# inline eqli #-}

neqi :: Stack -> Int -> Int -> IO ()
neqi stk m n = pokeBool stk (m /= n)
{-# inline neqi #-}

leqi :: Stack -> Int -> Int -> IO ()
leqi stk m n = pokeBool stk (m <= n)
{-# inline leqi #-}

lesi :: Stack -> Int -> Int -> IO ()
lesi stk m n = pokeBool stk (m < n)
{-# inline lesi #-}

andi :: Stack -> Int -> Int -> IO ()
andi stk m n = pokeI stk (m .&. n)
{-# inline andi #-}

iori :: Stack -> Int -> Int -> IO ()
iori stk m n = pokeI stk (m .|. n)
{-# inline iori #-}

xori :: Stack -> Int -> Int -> IO ()
xori stk m n = pokeI stk (m `xor` n)
{-# inline xori #-}

shli :: Stack -> Int -> Int -> IO ()
shli stk m n = pokeI stk (m `shiftL` n)
{-# inline shli #-}

shri :: Stack -> Int -> Int -> IO ()
shri stk m n = pokeI stk (m `shiftR` n)
{-# inline shri #-}

powi :: Stack -> Int -> Word64 -> IO ()
powi stk m n = pokeI stk (m ^ n)
{-# inline powi #-}

addn :: Stack -> Word64 -> Word64 -> IO ()
addn stk m n = pokeN stk (m + n)
{-# inline addn #-}

subn :: Stack -> Word64 -> Word64 -> IO ()
subn stk m n = pokeI stk . fromIntegral $ m - n
{-# inline subn #-}

muln :: Stack -> Word64 -> Word64 -> IO ()
muln stk m n = pokeN stk (m * n)
{-# inline muln #-}

divn :: Stack -> Word64 -> Word64 -> IO ()
divn stk m n = pokeN stk (m `div` n)
{-# inline divn #-}

modn :: Stack -> Word64 -> Word64 -> IO ()
modn stk m n = pokeN stk (m `mod` n)
{-# inline modn #-}

shln :: Stack -> Word64 -> Int -> IO ()
shln stk m n = pokeN stk (m `shiftL` n)
{-# inline shln #-}

shrn :: Stack -> Word64 -> Int -> IO ()
shrn stk m n = pokeN stk (m `shiftR` n)
{-# inline shrn #-}

pown :: Stack -> Word64 -> Word64 -> IO ()
pown stk m n = pokeN stk (m ^ n)
{-# inline pown #-}

eqln :: Stack -> Word64 -> Word64 -> IO ()
eqln stk m n = pokeBool stk (m == n)
{-# inline eqln #-}

neqn :: Stack -> Word64 -> Word64 -> IO ()
neqn stk m n = pokeBool stk (m /= n)
{-# inline neqn #-}

leqn :: Stack -> Word64 -> Word64 -> IO ()
leqn stk m n = pokeBool stk (m <= n)
{-# inline leqn #-}

lesn :: Stack -> Word64 -> Word64 -> IO ()
lesn stk m n = pokeBool stk (m < n)
{-# inline lesn #-}

andn :: Stack -> Word64 -> Word64 -> IO ()
andn stk m n = pokeN stk (m .&. n)
{-# inline andn #-}

iorn :: Stack -> Word64 -> Word64 -> IO ()
iorn stk m n = pokeN stk (m .|. n)
{-# inline iorn #-}

xorn :: Stack -> Word64 -> Word64 -> IO ()
xorn stk m n = pokeN stk (m `xor` n)
{-# inline xorn #-}

drpn :: Stack -> Word64 -> Word64 -> IO ()
drpn stk m n = pokeN stk $ if n >= m then 0 else m - n
{-# inline drpn #-}

eqlf :: Stack -> Double -> Double -> IO ()
eqlf stk x y = pokeBool stk (x == y)
{-# inline eqlf #-}

neqf :: Stack -> Double -> Double -> IO ()
neqf stk x y = pokeBool stk (x /= y)
{-# inline neqf #-}

leqf :: Stack -> Double -> Double -> IO ()
leqf stk x y = pokeBool stk (x <= y)
{-# inline leqf #-}

lesf :: Stack -> Double -> Double -> IO ()
lesf stk x y = pokeBool stk (x < y)
{-# inline lesf #-}

addf :: Stack -> Double -> Double -> IO ()
addf stk x y = pokeD stk (x + y)
{-# inline addf #-}

subf :: Stack -> Double -> Double -> IO ()
subf stk x y = pokeD stk (x - y)
{-# inline subf #-}

mulf :: Stack -> Double -> Double -> IO ()
mulf stk x y = pokeD stk (x * y)
{-# inline mulf #-}

divf :: Stack -> Double -> Double -> IO ()
divf stk x y = pokeD stk (x / y)
{-# inline divf #-}

atn2 :: Stack -> Double -> Double -> IO ()
atn2 stk x y = pokeD stk (atan2 x y)
{-# inline atn2 #-}

powf :: Stack -> Double -> Double -> IO ()
powf stk x y = pokeD stk (x ** y)
{-# inline powf #-}

logb :: Stack -> Double -> Double -> IO ()
logb stk x y = pokeD stk (logBase x y)
{-# inline logb #-}

maxf :: Stack -> Double -> Double -> IO ()
maxf stk x y = pokeD stk (max x y)
{-# inline maxf #-}

minf :: Stack -> Double -> Double -> IO ()
minf stk x y = pokeD stk (min x y)
{-# inline minf #-}

drpt :: Stack -> Int -> Text -> IO ()
drpt stk n t0 = pokeBi stk t
  where
    -- Note; if n < 0, the Nat argument was greater than the maximum
    -- signed integer. As an approximation, just return the empty
    -- string, as a string larger than this would require an absurd
    -- amount of memory.
    t | n < 0 = UText.empty
      | otherwise = UText.drop n t0

takt :: Stack -> Int -> Text -> IO ()
takt stk n t0 = pokeBi stk t
  where
    -- Note: if n < 0, the Nat argument was greater than the maximum
    -- signed integer. As an approximation, we just return the
    -- original string, because it's unlikely such a large string
    -- exists.
    t | n < 0 = t0
      | otherwise = UText.take n t0

catt :: Stack -> Text -> Text -> IO ()
catt stk x y = pokeBi stk (x <> y :: UText.Text)
{-# inline catt #-}

ixot :: Stack -> Text -> Text -> IO ()
ixot stk x y = writeBack stk $ UText.indexOf x y
{-# inline ixot #-}

eqlt :: Stack -> Text -> Text -> IO ()
eqlt stk x y = pokeBool stk $ x == y
{-# inline eqlt #-}

leqt :: Stack -> Text -> Text -> IO ()
leqt stk x y = pokeBool stk $ x <= y
{-# inline leqt #-}

lest :: Stack -> Text -> Text -> IO ()
lest stk x y = pokeBool stk $ x < y
{-# inline lest #-}

eqlu :: Stack -> Val -> Val -> IO ()
eqlu stk x y = pokeBool stk $ universalEq (==) x y
{-# inline eqlu #-}

cmpu :: Stack -> Val -> Val -> IO ()
cmpu stk x y = pokeI stk . pred . fromEnum $ universalCompare compare x y
{-# inline cmpu #-}

lequ :: Stack -> Val -> Val -> IO ()
lequ stk x y = pokeBool stk $ universalCompare compare x y /= GT
{-# inline lequ #-}

lesu :: Stack -> Val -> Val -> IO ()
lesu stk x y = pokeBool stk $ universalCompare compare x y == LT
{-# inline lesu #-}

-- Note: if n < 0, then the Nat argument was larger than the largest
-- signed integer. Seq actually doesn't handle this well, despite it
-- being possible to build (lazy) sequences this large. So,
-- approximate by yielding the empty sequence.
drps :: Stack -> Int -> USeq -> IO ()
drps stk n s = pokeS stk $ if n < 0 then Sq.empty else Sq.drop n s
{-# inline drps #-}

taks :: Stack -> Int -> USeq -> IO ()
taks stk n s = pokeS stk $ if n < 0 then s else Sq.take n s
{-# inline taks #-}

cons :: Stack -> Val -> USeq -> IO ()
cons stk x s = pokeS stk $ x Sq.<| s
{-# inline cons #-}

snoc :: Stack -> USeq -> Val -> IO ()
snoc stk s x = pokeS stk $ s Sq.|> x
{-# inline snoc #-}

idxs :: Stack -> Int -> USeq -> IO ()
idxs stk n s = writeBack stk $ Sq.lookup n s
{-# inline idxs #-}

data SeqView a b = SeqViewEmpty | SeqViewElem a b

decodeSeqView ::
  ForeignConvention a =>
  ForeignConvention b =>
  Closure -> IO (SeqView a b)
decodeSeqView (Enum _ t)
  | t == Ty.seqViewEmptyTag = pure SeqViewEmpty
decodeSeqView (Data2 _ t x y)
  | t == Ty.seqViewElemTag = SeqViewElem <$> decodeVal x <*> decodeVal y
decodeSeqView v = foreignConventionError "Either" (BoxedVal v)

seqViewE :: Closure
seqViewE = Enum Ty.seqViewRef Ty.seqViewEmptyTag

encodeSeqView ::
  ForeignConvention a =>
  ForeignConvention b =>
  SeqView a b -> Closure
encodeSeqView SeqViewEmpty = seqViewE
encodeSeqView (SeqViewElem x y) =
  Data2 Ty.seqViewRef Ty.seqViewElemTag (encodeVal x) (encodeVal y)
{-# inline encodeSeqView #-}

instance ( ForeignConvention a
         , ForeignConvention b
         ) => ForeignConvention (SeqView a b) where
  readAtIndex stk i = decodeSeqView =<< bpeekOff stk i

  decodeVal (BoxedVal c) = decodeSeqView c
  decodeVal v = foreignConventionError "SeqView" v

  readsAt stk (VArg1 i) = readAtIndex stk i
  readsAt _ args = readsAtError "one argument" args

  encodeVal = BoxedVal . encodeSeqView

  writeBack stk v = bpoke stk $ encodeSeqView v
  {-# inline writeBack #-}

spll :: Stack -> Int -> USeq -> IO ()
spll stk n s = writeBack stk result
  where
    result | Sq.length s < n = SeqViewEmpty
           | (l, r) <- Sq.splitAt n s = SeqViewElem l r
{-# inline spll #-}

splr :: Stack -> Int -> USeq -> IO ()
splr stk n s = writeBack stk result
  where
    result | Sq.length s < n = SeqViewEmpty
           | (l, r) <- Sq.splitAt (Sq.length s - n) s = SeqViewElem l r
{-# inline splr #-}

cats :: Stack -> USeq -> USeq -> IO ()
cats stk x y = pokeS stk $ x Sq.>< y
{-# inline cats #-}

-- If n < 0, the Nat argument was larger than the maximum signed
-- integer. Building a value this large would reuire an absurd
-- amount of memory, so just assume n is larger.
takb :: Stack -> Int -> Bytes -> IO ()
takb stk n b = pokeBi stk $ if n < 0 then b else By.take n b
{-# inline takb #-}

-- See above for n < 0
drpb :: Stack -> Int -> Bytes -> IO ()
drpb stk n b = pokeBi stk $ if n < 0 then By.empty else By.drop n b
{-# inline drpb #-}

idxb :: Stack -> Int -> Bytes -> IO ()
idxb stk n b = writeBack stk $ By.at n b
{-# inline idxb #-}

catb :: Stack -> Bytes -> Bytes -> IO ()
catb stk l r = writeBack stk $ l <> r
{-# inline catb #-}

ixob :: Stack -> Bytes -> Bytes -> IO ()
ixob stk l r = writeBack stk $ By.indexOf l r
{-# inline ixob #-}

refw :: Stack -> IORef Val -> Val -> IO ()
refw stk ref v = IORef.writeIORef ref v *> bpoke stk unitClosure
{-# inline refw #-}

cast :: Stack -> Int -> Int -> IO ()
cast stk n tag = poke stk $ UnboxedVal n (unboxedTypeTagFromInt tag)
{-# inline cast #-}

andb :: Stack -> Bool -> Bool -> IO ()
andb stk x y = pokeBool stk $ x && y
{-# inline andb #-}

iorb :: Stack -> Bool -> Bool -> IO ()
iorb stk x y = pokeBool stk $ x || y
{-# inline iorb #-}

sdbv :: CCache -> Stack -> [Referent] -> Value -> IO ()
sdbv env stk allowed0 v
  | sandboxed env =
      die "attempted to use sandboxed operation: Value.validateSandboxed"
  | otherwise = checkValueSandboxing env allowed v >>= writeBack stk
  where
    allowed = allowed0 >>= \case (Ref r) -> [r] ; _ -> []
{-# inline sdbv #-}

sdbx :: CCache -> Stack -> [Referent] -> Closure -> IO ()
sdbx env stk allowed0 c = checkSandboxing env allowed c >>= pokeBool stk
  where
    allowed = allowed0 >>= \case (Ref r) -> [r] ; _ -> []
{-# inline sdbx #-}

-- Universal comparison functions

closureNum :: Closure -> Int
closureNum PAp {} = 0
closureNum DataC {} = 1
closureNum Captured {} = 2
closureNum Foreign {} = 3
closureNum UnboxedTypeTag {} = 4
closureNum BlackHole {} = 5

universalEq ::
  (Foreign -> Foreign -> Bool) ->
  Val ->
  Val ->
  Bool
universalEq frn = eqVal
  where
    eql :: (a -> b -> Bool) -> [a] -> [b] -> Bool
    eql cm l r = length l == length r && and (zipWith cm l r)
    eqVal :: Val -> Val -> Bool
    eqVal (UnboxedVal v1 t1) (UnboxedVal v2 t2) = matchUnboxedTypes t1 t2 && v1 == v2
    eqVal (BoxedVal x) (BoxedVal y) = eqc x y
    eqVal _ _ = False
    eqc :: Closure -> Closure -> Bool
    eqc (DataC _ ct1 [w1]) (DataC _ ct2 [w2]) =
      matchTags ct1 ct2 && eqVal w1 w2
    eqc (DataC _ ct1 vs1) (DataC _ ct2 vs2) =
      ct1 == ct2
        && eqValList vs1 vs2
    eqc (PApV cix1 _ segs1) (PApV cix2 _ segs2) =
      cix1 == cix2
        && eqValList segs1 segs2
    eqc (CapV k1 a1 vs1) (CapV k2 a2 vs2) =
      eqK k1 k2
        && a1 == a2
        && eqValList vs1 vs2
    eqc (Foreign fl) (Foreign fr)
      | Just al <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fl,
        Just ar <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fr =
          arrayEq eqVal al ar
      | Just sl <- maybeUnwrapForeign @(Seq Val) Rf.listRef fl,
        Just sr <- maybeUnwrapForeign @(Seq Val) Rf.listRef fr =
          length sl == length sr && and (Sq.zipWith eqVal sl sr)
      | otherwise = frn fl fr
    eqc c d = closureNum c == closureNum d

    eqValList :: [Val] -> [Val] -> Bool
    eqValList vs1 vs2 = eql eqVal vs1 vs2

    eqK :: K -> K -> Bool
    eqK KE KE = True
    eqK (CB cb) (CB cb') = cb == cb'
    eqK (Mark a ps m k) (Mark a' ps' m' k') =
      a == a' && ps == ps' && liftEq eqVal m m' && eqK k k'
    eqK (Push f a ci _ _sect k) (Push f' a' ci' _ _sect' k') =
      f == f' && a == a' && ci == ci' && eqK k k'
    eqK _ _ = False

-- IEEE floating point layout is such that comparison as integers
-- somewhat works. Positive floating values map to positive integers
-- and negatives map to negatives. The corner cases are:
--
--   1. If both numbers are negative, ordering is flipped.
--   2. There is both +0 and -0, with -0 being represented as the
--      minimum signed integer.
--   3. NaN does weird things.
--
-- So, the strategy here is to compare normally if one argument is
-- positive, since positive numbers compare normally to others.
-- Otherwise, the sign bit is cleared and the numbers are compared
-- backwards. Clearing the sign bit maps -0 to +0 and maps a negative
-- number to its absolute value (including infinities). The multiple
-- NaN values are just handled according to bit patterns, rather than
-- IEEE specified behavior.
--
-- Transitivity is somewhat non-obvious for this implementation.
--
--   if i <= j and j <= k
--     if j > 0 then k > 0, so all 3 comparisons use `compare`
--     if k > 0 then k > i, since i <= j <= 0
--     if all 3 are <= 0, all 3 comparisons use the alternate
--       comparison, which is transitive via `compare`
compareAsFloat :: Int -> Int -> Ordering
compareAsFloat i j
  | i > 0 || j > 0 = compare i j
  | otherwise = compare (clear j) (clear i)
  where
    clear k = clearBit k 64

universalCompare ::
  (Foreign -> Foreign -> Ordering) ->
  Val ->
  Val ->
  Ordering
universalCompare frn = cmpVal False
  where
    cmpVal :: Bool -> Val -> Val -> Ordering
    cmpVal tyEq = \cases
      (BoxedVal c1) (BoxedVal c2) -> cmpc tyEq c1 c2
      (UnboxedVal {}) (BoxedVal {}) -> LT
      (BoxedVal {}) (UnboxedVal {}) -> GT
      (NatVal i) (NatVal j) -> compare i j
      (UnboxedVal v1 t1) (UnboxedVal v2 t2) -> cmpUnboxed tyEq (t1, v1) (t2, v2)
    cmpl :: (a -> b -> Ordering) -> [a] -> [b] -> Ordering
    cmpl cm l r =
      compare (length l) (length r) <> fold (zipWith cm l r)
    cmpc :: Bool -> Closure -> Closure -> Ordering
    cmpc tyEq = \cases
      (DataC rf1 ct1 vs1) (DataC rf2 ct2 vs2) ->
        (if tyEq && ct1 /= ct2 then compare rf1 rf2 else EQ)
          <> compare (maskTags ct1) (maskTags ct2)
          -- when comparing corresponding `Any` values, which have
          -- existentials inside check that type references match
          <> cmpValList (tyEq || rf1 == Rf.anyRef) vs1 vs2
      (PApV cix1 _ segs1) (PApV cix2 _ segs2) ->
        compare cix1 cix2
          <> cmpValList tyEq segs1 segs2
      (CapV k1 a1 vs1) (CapV k2 a2 vs2) ->
        cmpK tyEq k1 k2
          <> compare a1 a2
          <> cmpValList True vs1 vs2
      (Foreign fl) (Foreign fr)
        | Just sl <- maybeUnwrapForeign @(Seq Val) Rf.listRef fl,
          Just sr <- maybeUnwrapForeign @(Seq Val) Rf.listRef fr ->
            fold (Sq.zipWith (cmpVal tyEq) sl sr)
              <> compare (length sl) (length sr)
        | Just al <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fl,
          Just ar <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fr ->
            arrayCmp (cmpVal tyEq) al ar
        | otherwise -> frn fl fr
      (UnboxedTypeTag t1) (UnboxedTypeTag t2) -> compare t1 t2
      (BlackHole) (BlackHole) -> EQ
      c d -> comparing closureNum c d

    cmpUnboxed :: Bool -> (UnboxedTypeTag, Int) -> (UnboxedTypeTag, Int) -> Ordering
    cmpUnboxed tyEq = \cases
      -- Need to cast to Nat or else maxNat == -1 and it flips comparisons of large Nats.
      -- TODO: Investigate whether bit-twiddling is faster than using Haskell's fromIntegral.
      (IntTag, n1) (IntTag, n2) -> compare n1 n2
      (NatTag, n1) (NatTag, n2) -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (NatTag, n1) (IntTag, n2)
        | n2 < 0 -> GT
        | otherwise -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (IntTag, n1) (NatTag, n2)
        | n1 < 0 -> LT
        | otherwise -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (FloatTag, n1) (FloatTag, n2) -> compareAsFloat n1 n2
      (t1, v1) (t2, v2) ->
        Monoid.whenM tyEq (compare t1 t2)
          <> compare v1 v2

    cmpValList :: Bool -> [Val] -> [Val] -> Ordering
    cmpValList tyEq vs1 vs2 = cmpl (cmpVal tyEq) vs1 vs2

    cmpK :: Bool -> K -> K -> Ordering
    cmpK tyEq = \cases
      KE KE -> EQ
      (CB cb) (CB cb') -> compare cb cb'
      (Mark a ps m k) (Mark a' ps' m' k') ->
        compare a a'
          <> compare ps ps'
          <> liftCompare (cmpVal tyEq) m m'
          <> cmpK tyEq k k'
      (Push f a ci _ _sect k) (Push f' a' ci' _ _sect' k') ->
        compare f f'
          <> compare a a'
          <> compare ci ci'
          <> cmpK tyEq k k'
      KE _ -> LT
      _ KE -> GT
      (CB {}) _ -> LT
      _ (CB {}) -> GT
      (Mark {}) _ -> LT
      _ (Mark {}) -> GT

arrayCmp ::
  (a -> a -> Ordering) ->
  PA.Array a ->
  PA.Array a ->
  Ordering
arrayCmp cmpVal l r =
  comparing PA.sizeofArray l r <> go (PA.sizeofArray l - 1)
  where
    go i
      | i < 0 = EQ
      | otherwise = cmpVal (PA.indexArray l i) (PA.indexArray r i) <> go (i - 1)

arrayEq :: (a -> a -> Bool) -> PA.Array a -> PA.Array a -> Bool
arrayEq eqc l r
  | PA.sizeofArray l /= PA.sizeofArray r = False
  | otherwise = go (PA.sizeofArray l - 1)
  where
    go i
      | i < 0 = True
      | otherwise = eqc (PA.indexArray l i) (PA.indexArray r i) && go (i - 1)

-- serialization doesn't necessarily preserve Int tags, so be
-- more accepting for those.
matchTags :: PackedTag -> PackedTag -> Bool
matchTags ct1 ct2 =
  ct1 == ct2
    || (ct1 == Ty.intTag && ct2 == Ty.natTag)
    || (ct1 == Ty.natTag && ct2 == Ty.intTag)

-- serialization doesn't necessarily preserve Int tags, so be
-- more accepting for those.
matchUnboxedTypes :: UnboxedTypeTag -> UnboxedTypeTag -> Bool
matchUnboxedTypes ct1 ct2 =
  ct1 == ct2
    || (ct1 == IntTag && ct2 == NatTag)
    || (ct1 == NatTag && ct2 == IntTag)

