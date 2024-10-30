{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Stack
  ( K (..),
    GClosure (..),
    Closure
      ( ..,
        DataC,
        PApV,
        CapV,
        PAp,
        Enum,
        DataU1,
        DataU2,
        DataB1,
        DataB2,
        DataUB,
        DataBU,
        DataG,
        Captured,
        Foreign,
        BlackHole
      ),
    IxClosure,
    Callback (..),
    Augment (..),
    Dump (..),
    Stack (..),
    Off,
    SZ,
    FP,
    Seg,
    USeg,
    BSeg,
    traceK,
    frameDataSize,
    marshalToForeign,
    unull,
    bnull,
    nullSeg,
    peekD,
    peekOffD,
    pokeD,
    pokeOffD,
    peekN,
    peekOffN,
    pokeN,
    pokeOffN,
    peekBi,
    peekOffBi,
    pokeBi,
    pokeOffBi,
    peekOffS,
    pokeS,
    pokeOffS,
    frameView,
    scount,
    closureTermRefs,
    dumpAP,
    dumpFP,
    alloc,
    peek,
    upeek,
    bpeek,
    peekOff,
    upeekOff,
    bpeekOff,
    bpoke,
    bpokeOff,
    upoke,
    upokeOff,
    bump,
    bumpn,
    grab,
    ensure,
    duplicate,
    discardFrame,
    saveFrame,
    saveArgs,
    restoreFrame,
    prepareArgs,
    acceptArgs,
    frameArgs,
    augSeg,
    dumpSeg,
    adjustArgs,
    fsize,
    asize,
  )
where

import Control.Monad.Primitive
import Data.Primitive (sizeOf)
import Data.Word
import GHC.Exts as L (IsList (..))
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Runtime.Array
import Unison.Runtime.Foreign
import Unison.Runtime.MCode
import Unison.Type qualified as Ty
import Unison.Util.EnumContainers as EC
import Prelude hiding (words)

newtype Callback = Hook (Stack -> IO ())

instance Eq Callback where _ == _ = True

instance Ord Callback where compare _ _ = EQ

-- Evaluation stack
data K
  = KE
  | -- callback hook
    CB Callback
  | -- mark continuation with a prompt
    Mark
      !Int -- pending args
      !(EnumSet Word64)
      !(EnumMap Word64 Closure)
      !K
  | -- save information about a frame for later resumption
    Push
      !Int -- frame size
      !Int -- pending args
      !CombIx -- resumption section reference
      !Int -- stack guard
      !(RSection Closure) -- resumption section
      !K

instance Eq K where
  KE == KE = True
  (CB cb) == (CB cb') = cb == cb'
  (Mark a ps m k) == (Mark a' ps' m' k') =
    a == a' && ps == ps' && m == m' && k == k'
  (Push f a ci _ _sect k) == (Push f' a' ci' _ _sect' k') =
    f == f' && a == a' && ci == ci' && k == k'
  _ == _ = False

instance Ord K where
  compare KE KE = EQ
  compare (CB cb) (CB cb') = compare cb cb'
  compare (Mark a ps m k) (Mark a' ps' m' k') =
    compare (a, ps, m, k) (a', ps', m', k')
  compare (Push f a ci _ _sect k) (Push f' a' ci' _ _sect' k') =
    compare (f, a, ci, k) (f', a', ci', k')
  compare KE _ = LT
  compare _ KE = GT
  compare (CB {}) _ = LT
  compare _ (CB {}) = GT
  compare (Mark {}) _ = LT
  compare _ (Mark {}) = GT

newtype Closure = Closure {unClosure :: (GClosure (RComb Closure))}
  deriving stock (Show, Eq, Ord)

type IxClosure = GClosure CombIx

data GClosure comb
  = GPAp
      !CombIx
      {-# UNPACK #-} !(GCombInfo comb)
      {-# UNPACK #-} !Seg -- args
  | GEnum !Reference !Word64
  | GDataU1 !Reference !Word64 {- <- packed type tag -} !Int
  | GDataU2 !Reference !Word64 {- <- packed type tag -} !Int !Int
  | GDataB1 !Reference !Word64 {- <- packed type tag -} !(GClosure comb)
  | GDataB2 !Reference !Word64 {- <- packed type tag -} !(GClosure comb) !(GClosure comb)
  | GDataUB !Reference !Word64 {- <- packed type tag -} !Int !(GClosure comb)
  | GDataBU !Reference !Word64 {- <- packed type tag -} !(GClosure comb) !Int
  | GDataG !Reference !Word64 {- <- packed type tag -} {-# UNPACK #-} !Seg
  | -- code cont, arg size, u/b data stacks
    GCaptured !K !Int {-# UNPACK #-} !Seg
  | GForeign !Foreign
  | GBlackHole
  deriving stock (Show, Functor, Foldable, Traversable)

instance Eq (GClosure comb) where
  -- This is safe because the embedded CombIx will break disputes
  a == b = (a $> ()) == (b $> ())

instance Ord (GClosure comb) where
  compare a b = compare (a $> ()) (b $> ())

pattern PAp cix comb seg = Closure (GPAp cix comb seg)

pattern Enum r t = Closure (GEnum r t)

pattern DataU1 r t i = Closure (GDataU1 r t i)

pattern DataU2 r t i j = Closure (GDataU2 r t i j)

pattern DataB1 r t x <- Closure (GDataB1 r t (Closure -> x))
  where
    DataB1 r t x = Closure (GDataB1 r t (unClosure x))

pattern DataB2 r t x y <- Closure (GDataB2 r t (Closure -> x) (Closure -> y))
  where
    DataB2 r t x y = Closure (GDataB2 r t (unClosure x) (unClosure y))

pattern DataUB r t i y <- Closure (GDataUB r t i (Closure -> y))
  where
    DataUB r t i y = Closure (GDataUB r t i (unClosure y))

pattern DataBU r t y i <- Closure (GDataBU r t (Closure -> y) i)
  where
    DataBU r t y i = Closure (GDataBU r t (unClosure y) i)

pattern DataG r t seg = Closure (GDataG r t seg)

pattern Captured k a seg = Closure (GCaptured k a seg)

pattern Foreign x = Closure (GForeign x)

pattern BlackHole = Closure GBlackHole

traceK :: Reference -> K -> [(Reference, Int)]
traceK begin = dedup (begin, 1)
  where
    dedup p (Mark _ _ _ k) = dedup p k
    dedup p@(cur, n) (Push _ _ (CIx r _ _) _ _ k)
      | cur == r = dedup (cur, 1 + n) k
      | otherwise = p : dedup (r, 1) k
    dedup p _ = [p]

splitData :: Closure -> Maybe (Reference, Word64, SegList)
splitData = \case
  (Enum r t) -> Just (r, t, [])
  (DataU1 r t i) -> Just (r, t, [Left i])
  (DataU2 r t i j) -> Just (r, t, [Left i, Left j])
  (DataB1 r t x) -> Just (r, t, [Right x])
  (DataB2 r t x y) -> Just (r, t, [Right x, Right y])
  (DataUB r t u b) -> Just (r, t, [Left u, Right b])
  (DataBU r t b u) -> Just (r, t, [Right b, Left u])
  (DataG r t seg) -> Just (r, t, segToList seg)
  _ -> Nothing

-- | Converts an unboxed segment to a list of integers for a more interchangeable
-- representation. The segments are stored in backwards order, so this reverses
-- the contents.
ints :: ByteArray -> [Int]
ints ba = fmap (indexByteArray ba) [n - 1, n - 2 .. 0]
  where
    n = sizeofByteArray ba `div` intSize

-- | Converts a list of integers representing an unboxed segment back into the
-- appropriate segment. Segments are stored backwards in the runtime, so this
-- reverses the list.
useg :: [Int] -> USeg
useg ws = case L.fromList $ reverse ws of
  PrimArray ba -> ByteArray ba

-- | Converts a boxed segment to a list of closures. The segments are stored
-- backwards, so this reverses the contents.
bsegToList :: BSeg -> [Closure]
bsegToList = reverse . L.toList

-- | Converts a list of closures back to a boxed segment. Segments are stored
-- backwards, so this reverses the contents.
bseg :: [Closure] -> BSeg
bseg = L.fromList . reverse

formData :: Reference -> Word64 -> SegList -> Closure
formData r t [] = Enum r t
formData r t [Left i] = DataU1 r t i
formData r t [Left i, Left j] = DataU2 r t i j
formData r t [Right x] = DataB1 r t x
formData r t [Right x, Right y] = DataB2 r t x y
formData r t [Left u, Right b] = DataUB r t u b
formData r t [Right b, Left u] = DataBU r t b u
formData r t segList = DataG r t (segFromList segList)

frameDataSize :: K -> Int
frameDataSize = go 0
  where
    go sz KE = sz
    go sz (CB _) = sz
    go sz (Mark a _ _ k) = go (sz + a) k
    go sz (Push f a _ _ _ k) =
      go (sz + f + a) k

pattern DataC :: Reference -> Word64 -> SegList -> Closure
pattern DataC rf ct segs <-
  (splitData -> Just (rf, ct, segs))
  where
    DataC rf ct segs = formData rf ct segs

type SegList = [Either Int Closure]

pattern PApV :: CombIx -> RCombInfo Closure -> SegList -> Closure
pattern PApV cix rcomb segs <-
  PAp cix rcomb (segToList -> segs)
  where
    PApV cix rcomb segs = PAp cix rcomb (segFromList segs)

pattern CapV :: K -> Int -> SegList -> Closure
pattern CapV k a segs <- Captured k a (segToList -> segs)
  where
    CapV k a segList = Captured k a (segFromList segList)

-- | Converts from the efficient stack form of a segment to the list representation. Segments are stored backwards,
-- so this reverses the contents
segToList :: Seg -> SegList
segToList (u, b) =
  zipWith combine (ints u) (bsegToList b)
  where
    combine i c = case c of
      BlackHole -> Left i
      _ -> Right c

-- | Converts from the list representation of a segment to the efficient stack form. Segments are stored backwards,
-- so this reverses the contents.
segFromList :: SegList -> Seg
segFromList xs = (useg u, bseg b)
  where
    u =
      xs <&> \case
        Left i -> i
        Right _ -> 0
    b =
      xs <&> \case
        Left _ -> BlackHole
        Right c -> c

{-# COMPLETE DataC, PAp, Captured, Foreign, BlackHole #-}

{-# COMPLETE DataC, PApV, Captured, Foreign, BlackHole #-}

{-# COMPLETE DataC, PApV, CapV, Foreign, BlackHole #-}

marshalToForeign :: (HasCallStack) => Closure -> Foreign
marshalToForeign (Foreign x) = x
marshalToForeign c =
  error $ "marshalToForeign: unhandled closure: " ++ show c

type Off = Int

type SZ = Int

type FP = Int

type UA = MutableByteArray (PrimState IO)

type BA = MutableArray (PrimState IO) Closure

intSize :: Int
intSize = sizeOf (0 :: Int)

words :: Int -> Int
words n = n `div` intSize

bytes :: Int -> Int
bytes n = n * intSize

type Arrs = (UA, BA)

argOnto :: Arrs -> Off -> Arrs -> Off -> Args' -> IO Int
argOnto (srcUstk, srcBstk) srcSp (dstUstk, dstBstk) dstSp args = do
  -- Both new cp's should be the same, so we can just return one.
  _cp <- uargOnto srcUstk srcSp dstUstk dstSp args
  cp <- bargOnto srcBstk srcSp dstBstk dstSp args
  pure cp

-- The Caller must ensure that when setting the unboxed stack, the equivalent
-- boxed stack is zeroed out to BlackHole where necessary.
uargOnto :: UA -> Off -> UA -> Off -> Args' -> IO Int
uargOnto stk sp cop cp0 (Arg1 i) = do
  (x :: Int) <- readByteArray stk (sp - i)
  writeByteArray cop cp x
  pure cp
  where
    cp = cp0 + 1
uargOnto stk sp cop cp0 (Arg2 i j) = do
  (x :: Int) <- readByteArray stk (sp - i)
  (y :: Int) <- readByteArray stk (sp - j)
  writeByteArray cop cp x
  writeByteArray cop (cp - 1) y
  pure cp
  where
    cp = cp0 + 2
uargOnto stk sp cop cp0 (ArgN v) = do
  buf <-
    if overwrite
      then newByteArray $ bytes sz
      else pure cop
  let loop i
        | i < 0 = return ()
        | otherwise = do
            (x :: Int) <- readByteArray stk (sp - indexPrimArray v i)
            writeByteArray buf (boff - i) x
            loop $ i - 1
  loop $ sz - 1
  when overwrite $
    copyMutableByteArray cop (bytes $ cp + 1) buf 0 (bytes sz)
  pure cp
  where
    cp = cp0 + sz
    sz = sizeofPrimArray v
    overwrite = sameMutableByteArray stk cop
    boff | overwrite = sz - 1 | otherwise = cp0 + sz
uargOnto stk sp cop cp0 (ArgR i l) = do
  moveByteArray cop cbp stk sbp (bytes l)
  pure $ cp0 + l
  where
    cbp = bytes $ cp0 + 1
    sbp = bytes $ sp - i - l + 1

bargOnto :: BA -> Off -> BA -> Off -> Args' -> IO Int
bargOnto stk sp cop cp0 (Arg1 i) = do
  x <- readArray stk (sp - i)
  writeArray cop cp x
  pure cp
  where
    cp = cp0 + 1
bargOnto stk sp cop cp0 (Arg2 i j) = do
  x <- readArray stk (sp - i)
  y <- readArray stk (sp - j)
  writeArray cop cp x
  writeArray cop (cp - 1) y
  pure cp
  where
    cp = cp0 + 2
bargOnto stk sp cop cp0 (ArgN v) = do
  buf <-
    if overwrite
      then newArray sz $ BlackHole
      else pure cop
  let loop i
        | i < 0 = return ()
        | otherwise = do
            x <- readArray stk $ sp - indexPrimArray v i
            writeArray buf (boff - i) x
            loop $ i - 1
  loop $ sz - 1

  when overwrite $
    copyMutableArray cop (cp0 + 1) buf 0 sz
  pure cp
  where
    cp = cp0 + sz
    sz = sizeofPrimArray v
    overwrite = stk == cop
    boff | overwrite = sz - 1 | otherwise = cp0 + sz
bargOnto stk sp cop cp0 (ArgR i l) = do
  copyMutableArray cop (cp0 + 1) stk (sp - i - l + 1) l
  pure $ cp0 + l

data Dump = A | F Int Int | S

dumpAP :: Int -> Int -> Int -> Dump -> Int
dumpAP _ fp sz d@(F _ a) = dumpFP fp sz d - a
dumpAP ap _ _ _ = ap

dumpFP :: Int -> Int -> Dump -> Int
dumpFP fp _ S = fp
dumpFP fp sz A = fp + sz
dumpFP fp sz (F n _) = fp + sz - n

-- closure augmentation mode
-- instruction, kontinuation, call
data Augment = I | K | C

data Stack = Stack
  { ap :: !Int, -- arg pointer
    fp :: !Int, -- frame pointer
    sp :: !Int, -- stack pointer
    ustk :: {-# UNPACK #-} !(MutableByteArray (PrimState IO)),
    bstk :: {-# UNPACK #-} !(MutableArray (PrimState IO) Closure)
  }

instance Show Stack where
  show (Stack ap fp sp _ _) =
    "Stack " ++ show ap ++ " " ++ show fp ++ " " ++ show sp

type UElem = Int

type USeg = ByteArray

type BElem = Closure

type BSeg = Array Closure

type Elem = (UElem, BElem)

type Seg = (USeg, BSeg)

alloc :: IO Stack
alloc = do
  ustk <- newByteArray 4096
  bstk <- newArray 512 BlackHole
  pure $ Stack {ap = -1, fp = -1, sp = -1, ustk, bstk}
{-# INLINE alloc #-}

peek :: Stack -> IO Elem
peek stk = do
  u <- upeek stk
  b <- bpeek stk
  pure (u, b)
{-# INLINE peek #-}

bpeek :: Stack -> IO BElem
bpeek (Stack _ _ sp _ bstk) = readArray bstk sp
{-# INLINE bpeek #-}

upeek :: Stack -> IO UElem
upeek (Stack _ _ sp ustk _) = readByteArray ustk sp
{-# INLINE upeek #-}

peekOff :: Stack -> Off -> IO Elem
peekOff stk i = do
  u <- upeekOff stk i
  b <- bpeekOff stk i
  pure (u, b)
{-# INLINE peekOff #-}

bpeekOff :: Stack -> Off -> IO BElem
bpeekOff (Stack _ _ sp _ bstk) i = readArray bstk (sp - i)
{-# INLINE bpeekOff #-}

upeekOff :: Stack -> Off -> IO UElem
upeekOff (Stack _ _ sp ustk _) i = readByteArray ustk (sp - i)
{-# INLINE upeekOff #-}

-- | Store an unboxed value and null out the boxed stack at that location, both so we know there's no value there,
-- and so garbage collection can clean up any value that was referenced there.
upoke :: Stack -> UElem -> IO ()
upoke stk@(Stack _ _ sp ustk _) u = do
  bpoke stk BlackHole
  writeByteArray ustk sp u
{-# INLINE upoke #-}

-- | Store a boxed value.
-- We don't bother nulling out the unboxed stack,
-- it's extra work and there's nothing to garbage collect.
bpoke :: Stack -> BElem -> IO ()
bpoke (Stack _ _ sp _ bstk) b = writeArray bstk sp b
{-# INLINE bpoke #-}

upokeOff :: Stack -> Off -> UElem -> IO ()
upokeOff stk i u = do
  bpokeOff stk i BlackHole
  writeByteArray (ustk stk) (sp stk - i) u
{-# INLINE upokeOff #-}

bpokeOff :: Stack -> Off -> BElem -> IO ()
bpokeOff (Stack _ _ sp _ bstk) i b = writeArray bstk (sp - i) b
{-# INLINE bpokeOff #-}

-- | Eats up arguments
grab :: Stack -> SZ -> IO (Seg, Stack)
grab (Stack _ fp sp ustk bstk) sze = do
  uSeg <- ugrab
  bSeg <- bgrab
  pure $ ((uSeg, bSeg), Stack (fp - sze) (fp - sze) (sp - sze) ustk bstk)
  where
    ugrab = do
      mut <- newByteArray bsz
      copyMutableByteArray mut 0 ustk (bfp - bsz) bsz
      seg <- unsafeFreezeByteArray mut
      moveByteArray ustk (bfp - bsz) ustk bfp fsz
      pure seg
      where
        bsz = bytes sze
        bfp = bytes $ fp + 1
        fsz = bytes $ sp - fp
    bgrab = do
      seg <- unsafeFreezeArray =<< cloneMutableArray bstk (fp + 1 - sze) sze
      copyMutableArray bstk (fp + 1 - sze) bstk (fp + 1) fsz
      pure seg
      where
        fsz = sp - fp
{-# INLINE grab #-}

ensure :: Stack -> SZ -> IO Stack
ensure stk@(Stack ap fp sp ustk bstk) sze
  | sze <= 0 = pure stk
  | sp + sze + 1 < bsz = pure stk
  | otherwise = do
      bstk' <- newArray (bsz + bext) BlackHole
      copyMutableArray bstk' 0 bstk 0 (sp + 1)
      ustk' <- resizeMutableByteArray ustk (usz + uext)
      pure $ Stack ap fp sp ustk' bstk'
  where
    usz = sizeofMutableByteArray ustk
    bsz = sizeofMutableArray bstk
    bext
      | sze > 1280 = sze + 512
      | otherwise = 1280
    uext
      | bytes sze > 10240 = bytes sze + 4096
      | otherwise = 10240
{-# INLINE ensure #-}

bump :: Stack -> IO Stack
bump (Stack ap fp sp ustk bstk) = pure $ Stack ap fp (sp + 1) ustk bstk
{-# INLINE bump #-}

bumpn :: Stack -> SZ -> IO Stack
bumpn (Stack ap fp sp ustk bstk) n = pure $ Stack ap fp (sp + n) ustk bstk
{-# INLINE bumpn #-}

duplicate :: Stack -> IO Stack
duplicate (Stack ap fp sp ustk bstk) = do
  ustk' <- dupUStk
  bstk' <- dupBStk
  pure $ Stack ap fp sp ustk' bstk'
  where
    dupUStk = do
      let sz = sizeofMutableByteArray ustk
      b <- newByteArray sz
      copyMutableByteArray b 0 ustk 0 sz
      pure b
    dupBStk = do
      cloneMutableArray bstk 0 (sizeofMutableArray bstk)
{-# INLINE duplicate #-}

discardFrame :: Stack -> IO Stack
discardFrame (Stack ap fp _ ustk bstk) = pure $ Stack ap fp fp ustk bstk
{-# INLINE discardFrame #-}

saveFrame :: Stack -> IO (Stack, SZ, SZ)
saveFrame (Stack ap fp sp ustk bstk) = pure (Stack sp sp sp ustk bstk, sp - fp, fp - ap)
{-# INLINE saveFrame #-}

saveArgs :: Stack -> IO (Stack, SZ)
saveArgs (Stack ap fp sp ustk bstk) = pure (Stack fp fp sp ustk bstk, fp - ap)
{-# INLINE saveArgs #-}

restoreFrame :: Stack -> SZ -> SZ -> IO Stack
restoreFrame (Stack _ fp0 sp ustk bstk) fsz asz = pure $ Stack ap fp sp ustk bstk
  where
    fp = fp0 - fsz
    ap = fp - asz
{-# INLINE restoreFrame #-}

prepareArgs :: Stack -> Args' -> IO Stack
prepareArgs (Stack ap fp sp ustk bstk) = \case
  ArgR i l
    | fp + l + i == sp ->
        pure $ Stack ap (sp - i) (sp - i) ustk bstk
  args -> do
    sp <- argOnto (ustk, bstk) sp (ustk, bstk) fp args
    pure $ Stack ap sp sp ustk bstk
{-# INLINE prepareArgs #-}

acceptArgs :: Stack -> Int -> IO Stack
acceptArgs (Stack ap fp sp ustk bstk) n = pure $ Stack ap (fp - n) sp ustk bstk
{-# INLINE acceptArgs #-}

frameArgs :: Stack -> IO Stack
frameArgs (Stack ap _ sp ustk bstk) = pure $ Stack ap ap sp ustk bstk
{-# INLINE frameArgs #-}

augSeg :: Augment -> Stack -> Seg -> Maybe Args' -> IO Seg
augSeg mode (Stack ap fp sp ustk bstk) (useg, bseg) margs = do
  useg' <- unboxedSeg
  bseg' <- boxedSeg
  pure (useg', bseg')
  where
    bpsz
      | I <- mode = 0
      | otherwise = fp - ap
    unboxedSeg = do
      cop <- newByteArray $ ssz + upsz + asz
      copyByteArray cop soff useg 0 ssz
      copyMutableByteArray cop 0 ustk (bytes $ ap + 1) upsz
      for_ margs $ uargOnto ustk sp cop (words poff + bpsz - 1)
      unsafeFreezeByteArray cop
      where
        ssz = sizeofByteArray useg
        (poff, soff)
          | K <- mode = (ssz, 0)
          | otherwise = (0, upsz + asz)
        upsz = bytes bpsz
        asz = case margs of
          Nothing -> bytes 0
          Just (Arg1 _) -> bytes 1
          Just (Arg2 _ _) -> bytes 2
          Just (ArgN v) -> bytes $ sizeofPrimArray v
          Just (ArgR _ l) -> bytes l
    boxedSeg = do
      cop <- newArray (ssz + bpsz + asz) BlackHole
      copyArray cop soff bseg 0 ssz
      copyMutableArray cop poff bstk (ap + 1) bpsz
      for_ margs $ bargOnto bstk sp cop (poff + bpsz - 1)
      unsafeFreezeArray cop
      where
        ssz = sizeofArray bseg
        (poff, soff)
          | K <- mode = (ssz, 0)
          | otherwise = (0, bpsz + asz)
        asz = case margs of
          Nothing -> 0
          Just (Arg1 _) -> 1
          Just (Arg2 _ _) -> 2
          Just (ArgN v) -> sizeofPrimArray v
          Just (ArgR _ l) -> l
{-# INLINE augSeg #-}

dumpSeg :: Stack -> Seg -> Dump -> IO Stack
dumpSeg (Stack ap fp sp ustk bstk) (useg, bseg) mode = do
  dumpUSeg
  dumpBSeg
  pure $ Stack ap' fp' sp' ustk bstk
  where
    sz = sizeofArray bseg
    sp' = sp + sz
    fp' = dumpFP fp sz mode
    ap' = dumpAP ap fp sz mode
    dumpUSeg = do
      let ssz = sizeofByteArray useg
      let bsp = bytes $ sp + 1
      copyByteArray ustk bsp useg 0 ssz
    dumpBSeg = do
      copyArray bstk (sp + 1) bseg 0 sz
{-# INLINE dumpSeg #-}

adjustArgs :: Stack -> SZ -> IO Stack
adjustArgs (Stack ap fp sp ustk bstk) sz = pure $ Stack (ap - sz) fp sp ustk bstk
{-# INLINE adjustArgs #-}

fsize :: Stack -> SZ
fsize (Stack _ fp sp _ _) = sp - fp
{-# INLINE fsize #-}

asize :: Stack -> SZ
asize (Stack ap fp _ _ _) = fp - ap
{-# INLINE asize #-}

peekN :: Stack -> IO Word64
peekN (Stack _ _ sp ustk _) = readByteArray ustk sp
{-# INLINE peekN #-}

peekD :: Stack -> IO Double
peekD (Stack _ _ sp ustk _) = readByteArray ustk sp
{-# INLINE peekD #-}

peekOffN :: Stack -> Int -> IO Word64
peekOffN (Stack _ _ sp ustk _) i = readByteArray ustk (sp - i)
{-# INLINE peekOffN #-}

peekOffD :: Stack -> Int -> IO Double
peekOffD (Stack _ _ sp ustk _) i = readByteArray ustk (sp - i)
{-# INLINE peekOffD #-}

pokeN :: Stack -> Word64 -> IO ()
pokeN stk@(Stack _ _ sp ustk _) n = do
  bpoke stk BlackHole
  writeByteArray ustk sp n
{-# INLINE pokeN #-}

pokeD :: Stack -> Double -> IO ()
pokeD stk@(Stack _ _ sp ustk _) d = do
  bpoke stk BlackHole
  writeByteArray ustk sp d
{-# INLINE pokeD #-}

pokeOffN :: Stack -> Int -> Word64 -> IO ()
pokeOffN stk@(Stack _ _ sp ustk _) i n = do
  bpokeOff stk i BlackHole
  writeByteArray ustk (sp - i) n
{-# INLINE pokeOffN #-}

pokeOffD :: Stack -> Int -> Double -> IO ()
pokeOffD stk@(Stack _ _ sp ustk _) i d = do
  bpokeOff stk i BlackHole
  writeByteArray ustk (sp - i) d
{-# INLINE pokeOffD #-}

pokeBi :: (BuiltinForeign b) => Stack -> b -> IO ()
pokeBi stk x = bpoke stk (Foreign $ wrapBuiltin x)
{-# INLINE pokeBi #-}

pokeOffBi :: (BuiltinForeign b) => Stack -> Int -> b -> IO ()
pokeOffBi stk i x = bpokeOff stk i (Foreign $ wrapBuiltin x)
{-# INLINE pokeOffBi #-}

peekBi :: (BuiltinForeign b) => Stack -> IO b
peekBi stk = unwrapForeign . marshalToForeign <$> bpeek stk
{-# INLINE peekBi #-}

peekOffBi :: (BuiltinForeign b) => Stack -> Int -> IO b
peekOffBi stk i = unwrapForeign . marshalToForeign <$> bpeekOff stk i
{-# INLINE peekOffBi #-}

peekOffS :: Stack -> Int -> IO (Seq Closure)
peekOffS stk i =
  unwrapForeign . marshalToForeign <$> bpeekOff stk i
{-# INLINE peekOffS #-}

pokeS :: Stack -> Seq Closure -> IO ()
pokeS stk s = bpoke stk (Foreign $ Wrap Ty.listRef s)
{-# INLINE pokeS #-}

pokeOffS :: Stack -> Int -> Seq Closure -> IO ()
pokeOffS stk i s = bpokeOff stk i (Foreign $ Wrap Ty.listRef s)
{-# INLINE pokeOffS #-}

unull :: USeg
unull = byteArrayFromListN 0 ([] :: [Int])

bnull :: BSeg
bnull = fromListN 0 []

nullSeg :: Seg
nullSeg = (unull, bnull)

instance Show K where
  show k = "[" ++ go "" k
    where
      go _ KE = "]"
      go _ (CB _) = "]"
      go com (Push f a ci _g _rsect k) =
        com ++ show (f, a, ci) ++ go "," k
      go com (Mark a ps _ k) =
        com ++ "M " ++ show a ++ " " ++ show ps ++ go "," k

frameView :: Stack -> IO ()
frameView stk = putStr "|" >> gof False 0
  where
    fsz = fsize stk
    asz = asize stk
    gof delim n
      | n >= fsz = putStr "|" >> goa False 0
      | otherwise = do
          when delim $ putStr ","
          putStr . show =<< peekOff stk n
          gof True (n + 1)
    goa delim n
      | n >= asz = putStrLn "|.."
      | otherwise = do
          when delim $ putStr ","
          putStr . show =<< peekOff stk (fsz + n)
          goa True (n + 1)

scount :: Seg -> Int
scount (_, bseg) = bscount bseg
  where
    bscount :: BSeg -> Int
    bscount seg = sizeofArray seg

closureTermRefs :: (Monoid m) => (Reference -> m) -> (Closure -> m)
closureTermRefs f = \case
  PAp (CIx r _ _) _ (_useg, bseg) ->
    f r <> foldMap (closureTermRefs f) bseg
  (DataB1 _ _ c) -> closureTermRefs f c
  (DataB2 _ _ c1 c2) ->
    closureTermRefs f c1 <> closureTermRefs f c2
  (DataUB _ _ _ c) ->
    closureTermRefs f c
  (Captured k _ (_useg, bseg)) ->
    contTermRefs f k <> foldMap (closureTermRefs f) bseg
  (Foreign fo)
    | Just (cs :: Seq Closure) <- maybeUnwrapForeign Ty.listRef fo ->
        foldMap (closureTermRefs f) cs
  _ -> mempty

contTermRefs :: (Monoid m) => (Reference -> m) -> K -> m
contTermRefs f (Mark _ _ m k) =
  foldMap (closureTermRefs f) m <> contTermRefs f k
contTermRefs f (Push _ _ (CIx r _ _) _ _ k) =
  f r <> contTermRefs f k
contTermRefs _ _ = mempty
