{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

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
        Data1,
        Data2,
        DataG,
        Captured,
        Foreign,
        BlackHole,
        UnboxedTypeTag
      ),
    UnboxedTypeTag (..),
    unboxedTypeTagToInt,
    unboxedTypeTagFromInt,
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
    SegList,
    Val
      ( ..,
        CharVal,
        NatVal,
        DoubleVal,
        IntVal,
        BoolVal,
        UnboxedVal,
        BoxedVal
      ),
    emptyVal,
    falseVal,
    trueVal,
    boxedVal,
    USeq,
    traceK,
    frameDataSize,
    marshalToForeign,
    unull,
    bnull,
    nullSeg,
    peekD,
    peekOffD,
    peekC,
    peekOffC,
    poke,
    pokeD,
    pokeOffD,
    pokeC,
    pokeOffC,
    pokeBool,
    pokeTag,
    peekTag,
    peekTagOff,
    peekI,
    peekOffI,
    peekN,
    peekOffN,
    pokeN,
    pokeOffN,
    pokeI,
    pokeOffI,
    pokeByte,
    peekBi,
    peekOffBi,
    pokeBi,
    pokeOffBi,
    peekBool,
    peekOffBool,
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
    pokeOff,
    upokeT,
    upokeOffT,
    unsafePokeIasN,
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

    -- * Unboxed type tags
    natTypeTag,
    intTypeTag,
    charTypeTag,
    floatTypeTag,
  )
where

import Control.Monad.Primitive
import Data.Char qualified as Char
import Data.IORef (IORef)
import Data.Kind (Constraint)
import Data.Primitive (sizeOf)
import Data.Primitive.ByteArray qualified as BA
import Data.Tagged (Tagged (..))
import Data.Word
import GHC.Exts as L (IsList (..))
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Runtime.ANF (PackedTag)
import Unison.Runtime.Array
import Unison.Runtime.Foreign
import Unison.Runtime.MCode
import Unison.Runtime.TypeTags qualified as TT
import Unison.Type qualified as Ty
import Unison.Util.EnumContainers as EC
import Prelude hiding (words)

{- ORMOLU_DISABLE -}
#ifdef STACK_CHECK
type DebugCallStack = (HasCallStack :: Constraint)

unboxedSentinel :: Int
unboxedSentinel = -99

boxedSentinel :: Closure
boxedSentinel = (Closure GUnboxedSentinel)

assertBumped :: HasCallStack => Stack -> Off -> IO ()
assertBumped (Stack _ _ sp ustk bstk) i = do
  u <- readByteArray ustk (sp - i)
  b :: BVal <- readArray bstk (sp - i)
  when (u /= unboxedSentinel || not (isBoxedSentinel b)) do
            error $ "Expected stack slot to have been bumped, but it was:" <> show (Val u b)
  where
    isBoxedSentinel :: Closure -> Bool
    isBoxedSentinel (Closure GUnboxedSentinel) = True
    isBoxedSentinel _ = False

assertUnboxed :: HasCallStack => Stack -> Off -> IO ()
assertUnboxed (Stack _ _ sp ustk bstk) i = do
  (u :: Int) <- readByteArray ustk (sp - i)
  b <- readArray bstk (sp - i)
  case b of
    UnboxedTypeTag _ -> pure ()
    _ -> error $ "Expected stack val to be unboxed, but it was:" <> show (Val u b)

pokeSentinelOff :: Stack -> Off -> IO ()
pokeSentinelOff (Stack _ _ sp ustk bstk) off = do
  writeByteArray ustk (sp - off) unboxedSentinel
  writeArray bstk (sp - off) boxedSentinel
#else
-- Don't track callstacks in production, it's expensive
type DebugCallStack = (() :: Constraint)
#endif
{- ORMOLU_ENABLE -}

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
      !(EnumMap Word64 Val)
      !K
  | -- save information about a frame for later resumption
    Push
      !Int -- frame size
      !Int -- pending args
      !CombIx -- resumption section reference
      !Int -- stack guard
      !(RSection Val) -- resumption section
      !K

newtype Closure = Closure {unClosure :: (GClosure (RComb Val))}
  deriving stock (Show)

-- | Implementation for Unison sequences.
type USeq = Seq Val

type IxClosure = GClosure CombIx

-- Don't re-order these, the ord instance affects Universal.compare
data UnboxedTypeTag
  = CharTag
  | FloatTag
  | IntTag
  | NatTag
  deriving stock (Show, Eq, Ord)

unboxedTypeTagToInt :: UnboxedTypeTag -> Int
unboxedTypeTagToInt = \case
  CharTag -> 0
  FloatTag -> 1
  IntTag -> 2
  NatTag -> 3

unboxedTypeTagFromInt :: (HasCallStack) => Int -> UnboxedTypeTag
unboxedTypeTagFromInt = \case
  0 -> CharTag
  1 -> FloatTag
  2 -> IntTag
  3 -> NatTag
  _ -> error "intToUnboxedTypeTag: invalid tag"

{- ORMOLU_DISABLE -}
data GClosure comb
  = GPAp
      !CombIx
      {-# UNPACK #-} !(GCombInfo comb)
      {-# UNPACK #-} !Seg -- args
  | GEnum !Reference !PackedTag
  | GData1 !Reference !PackedTag !Val
  | GData2 !Reference !PackedTag !Val !Val
  | GDataG !Reference !PackedTag {-# UNPACK #-} !Seg
  | -- code cont, arg size, u/b data stacks
    GCaptured !K !Int {-# UNPACK #-} !Seg
  | GForeign !Foreign
  | -- The type tag for the value in the corresponding unboxed stack slot.
    -- We should consider adding separate constructors for common builtin type tags.
    --  GHC will optimize nullary constructors into singletons.
    GUnboxedTypeTag !UnboxedTypeTag
  | GBlackHole
#ifdef STACK_CHECK
  | GUnboxedSentinel
#endif
  deriving stock (Show, Functor, Foldable, Traversable)
{- ORMOLU_ENABLE -}

pattern PAp :: CombIx -> GCombInfo (RComb Val) -> Seg -> Closure
pattern PAp cix comb seg = Closure (GPAp cix comb seg)

pattern Enum :: Reference -> PackedTag -> Closure
pattern Enum r t = Closure (GEnum r t)

pattern Data1 r t i = Closure (GData1 r t i)

pattern Data2 r t i j = Closure (GData2 r t i j)

pattern DataG r t seg = Closure (GDataG r t seg)

pattern Captured k a seg = Closure (GCaptured k a seg)

pattern Foreign x = Closure (GForeign x)

pattern BlackHole = Closure GBlackHole

pattern UnboxedTypeTag t <- Closure (GUnboxedTypeTag t)
  where
    UnboxedTypeTag t = case t of
      CharTag -> charTypeTag
      FloatTag -> floatTypeTag
      IntTag -> intTypeTag
      NatTag -> natTypeTag

{-# COMPLETE PAp, Enum, Data1, Data2, DataG, Captured, Foreign, UnboxedTypeTag, BlackHole #-}

{-# COMPLETE DataC, PAp, Captured, Foreign, BlackHole, UnboxedTypeTag #-}

{-# COMPLETE DataC, PApV, Captured, Foreign, BlackHole, UnboxedTypeTag #-}

{-# COMPLETE DataC, PApV, CapV, Foreign, BlackHole, UnboxedTypeTag #-}

-- We can avoid allocating a closure for common type tags on each poke by having shared top-level closures for them.
natTypeTag :: Closure
natTypeTag = (Closure (GUnboxedTypeTag NatTag))
{-# NOINLINE natTypeTag #-}

intTypeTag :: Closure
intTypeTag = (Closure (GUnboxedTypeTag IntTag))
{-# NOINLINE intTypeTag #-}

charTypeTag :: Closure
charTypeTag = (Closure (GUnboxedTypeTag CharTag))
{-# NOINLINE charTypeTag #-}

floatTypeTag :: Closure
floatTypeTag = (Closure (GUnboxedTypeTag FloatTag))
{-# NOINLINE floatTypeTag #-}

traceK :: Reference -> K -> [(Reference, Int)]
traceK begin = dedup (begin, 1)
  where
    dedup p (Mark _ _ _ k) = dedup p k
    dedup p@(cur, n) (Push _ _ (CIx r _ _) _ _ k)
      | cur == r = dedup (cur, 1 + n) k
      | otherwise = p : dedup (r, 1) k
    dedup p _ = [p]

splitData :: Closure -> Maybe (Reference, PackedTag, SegList)
splitData = \case
  (Enum r t) -> Just (r, t, [])
  (Data1 r t u) -> Just (r, t, [u])
  (Data2 r t i j) -> Just (r, t, [i, j])
  (DataG r t seg) -> Just (r, t, segToList seg)
  _ -> Nothing

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

formData :: Reference -> PackedTag -> SegList -> Closure
formData r t [] = Enum r t
formData r t [v1] = Data1 r t v1
formData r t [v1, v2] = Data2 r t v1 v2
formData r t segList = DataG r t (segFromList segList)

frameDataSize :: K -> Int
frameDataSize = go 0
  where
    go sz KE = sz
    go sz (CB _) = sz
    go sz (Mark a _ _ k) = go (sz + a) k
    go sz (Push f a _ _ _ k) =
      go (sz + f + a) k

pattern DataC :: Reference -> PackedTag -> SegList -> Closure
pattern DataC rf ct segs <-
  (splitData -> Just (rf, ct, segs))
  where
    DataC rf ct segs = formData rf ct segs

matchCharVal :: Val -> Maybe Char
matchCharVal = \case
  (UnboxedVal u CharTag) -> Just (Char.chr u)
  _ -> Nothing

pattern CharVal :: Char -> Val
pattern CharVal c <- (matchCharVal -> Just c)
  where
    CharVal c = Val (Char.ord c) charTypeTag

matchNatVal :: Val -> Maybe Word64
matchNatVal = \case
  (UnboxedVal u NatTag) -> Just (fromIntegral u)
  _ -> Nothing

pattern NatVal :: Word64 -> Val
pattern NatVal n <- (matchNatVal -> Just n)
  where
    NatVal n = Val (fromIntegral n) natTypeTag

matchDoubleVal :: Val -> Maybe Double
matchDoubleVal = \case
  (UnboxedVal u FloatTag) -> Just (intToDouble u)
  _ -> Nothing

pattern DoubleVal :: Double -> Val
pattern DoubleVal d <- (matchDoubleVal -> Just d)
  where
    DoubleVal d = Val (doubleToInt d) floatTypeTag

matchIntVal :: Val -> Maybe Int
matchIntVal = \case
  (UnboxedVal u IntTag) -> Just u
  _ -> Nothing

pattern IntVal :: Int -> Val
pattern IntVal i <- (matchIntVal -> Just i)
  where
    IntVal i = Val i intTypeTag

matchBoolVal :: Val -> Maybe Bool
matchBoolVal = \case
  (BoxedVal (Enum r t)) | r == Ty.booleanRef -> Just (t == TT.falseTag)
  _ -> Nothing

pattern BoolVal :: Bool -> Val
pattern BoolVal b <- (matchBoolVal -> Just b)
  where
    BoolVal b = if b then trueVal else falseVal

-- Define singletons we can use for the bools to prevent allocation where possible.
falseVal :: Val
falseVal = BoxedVal (Enum Ty.booleanRef TT.falseTag)
{-# NOINLINE falseVal #-}

trueVal :: Val
trueVal = BoxedVal (Enum Ty.booleanRef TT.trueTag)
{-# NOINLINE trueVal #-}

doubleToInt :: Double -> Int
doubleToInt d = indexByteArray (BA.byteArrayFromList [d]) 0
{-# INLINE doubleToInt #-}

intToDouble :: Int -> Double
intToDouble w = indexByteArray (BA.byteArrayFromList [w]) 0
{-# INLINE intToDouble #-}

type SegList = [Val]

pattern PApV :: CombIx -> RCombInfo Val -> SegList -> Closure
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
  zipWith Val (ints u) (bsegToList b)

-- | Converts an unboxed segment to a list of integers for a more interchangeable
-- representation. The segments are stored in backwards order, so this reverses
-- the contents.
ints :: ByteArray -> [Int]
ints ba = fmap (indexByteArray ba) [n - 1, n - 2 .. 0]
  where
    n = sizeofByteArray ba `div` 8

-- | Converts from the list representation of a segment to the efficient stack form. Segments are stored backwards,
-- so this reverses the contents.
segFromList :: SegList -> Seg
segFromList xs =
  xs
    & foldMap
      ( \(Val unboxed boxed) -> ([unboxed], [boxed])
      )
    & \(us, bs) -> (useg us, bseg bs)

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
    copyMutableByteArray cop (bytes $ cp0 + 1) buf 0 (bytes sz)
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

type UVal = Int

-- | A runtime value, which is either a boxed or unboxed value, but we may not know which.
data Val = Val {getUnboxedVal :: !UVal, getBoxedVal :: !BVal}
  -- The Eq instance for Val is deliberately omitted because you need to take into account the fact that if a Val is boxed, the
  -- unboxed side is garbage and should not be compared.
  -- See universalEq.
  deriving (Show)

instance BuiltinForeign (IORef Val) where foreignRef = Tagged Ty.refRef

-- | A nulled out value you can use when filling empty arrays, etc.
emptyVal :: Val
emptyVal = Val (-1) BlackHole

pattern UnboxedVal :: Int -> UnboxedTypeTag -> Val
pattern UnboxedVal v t = (Val v (UnboxedTypeTag t))

valToBoxed :: Val -> Maybe Closure
valToBoxed UnboxedVal {} = Nothing
valToBoxed (Val _ b) = Just b

-- | Matches a Val which is known to be boxed, and returns the closure portion.
pattern BoxedVal :: Closure -> Val
pattern BoxedVal b <- (valToBoxed -> Just b)
  where
    BoxedVal b = Val (-1) b

{-# COMPLETE UnboxedVal, BoxedVal #-}

-- | Lift a boxed val into an Val
boxedVal :: BVal -> Val
boxedVal = Val 0

type USeg = ByteArray

type BVal = Closure

type BSeg = Array Closure

type Seg = (USeg, BSeg)

alloc :: IO Stack
alloc = do
  ustk <- newByteArray 4096
  bstk <- newArray 512 BlackHole
  pure $ Stack {ap = -1, fp = -1, sp = -1, ustk, bstk}
{-# INLINE alloc #-}

{- ORMOLU_DISABLE -}
peek :: DebugCallStack => Stack -> IO Val
peek stk@(Stack _ _ sp ustk _) = do
  -- Can't use upeek here because in stack-check mode it will assert that the stack slot is unboxed.
  u <- readByteArray ustk sp
  b <- bpeek stk
  pure (Val u b)
{-# INLINE peek #-}

peekI :: DebugCallStack => Stack -> IO Int
peekI _stk@(Stack _ _ sp ustk _) = do
#ifdef STACK_CHECK
  assertUnboxed _stk 0
#endif
  readByteArray ustk sp
{-# INLINE peekI #-}

peekOffI :: DebugCallStack => Stack -> Off -> IO Int
peekOffI _stk@(Stack _ _ sp ustk _) i = do
#ifdef STACK_CHECK
  assertUnboxed _stk i
#endif
  readByteArray ustk (sp - i)
{-# INLINE peekOffI #-}

bpeek :: DebugCallStack => Stack -> IO BVal
bpeek (Stack _ _ sp _ bstk) = readArray bstk sp
{-# INLINE bpeek #-}

upeek :: DebugCallStack => Stack -> IO UVal
upeek _stk@(Stack _ _ sp ustk _) = do
#ifdef STACK_CHECK
  assertUnboxed _stk 0
#endif
  readByteArray ustk sp
{-# INLINE upeek #-}

peekOff :: DebugCallStack => Stack -> Off -> IO Val
peekOff stk@(Stack _ _ sp ustk _) i = do
  -- Can't use upeekOff here because in stack-check mode it will assert that the stack slot is unboxed.
  u <- readByteArray ustk (sp - i)
  b <- bpeekOff stk i
  pure $ Val u b
{-# INLINE peekOff #-}

bpeekOff :: DebugCallStack => Stack -> Off -> IO BVal
bpeekOff (Stack _ _ sp _ bstk) i = readArray bstk (sp - i)
{-# INLINE bpeekOff #-}

upeekOff :: DebugCallStack => Stack -> Off -> IO UVal
upeekOff _stk@(Stack _ _ sp ustk _) i = do
#ifdef STACK_CHECK
  assertUnboxed _stk i
#endif
  readByteArray ustk (sp - i)
{-# INLINE upeekOff #-}

upokeT :: DebugCallStack => Stack -> UVal -> BVal -> IO ()
upokeT !stk@(Stack _ _ sp ustk _) !u !t = do
  bpoke stk t
  writeByteArray ustk sp u
{-# INLINE upokeT #-}

poke :: DebugCallStack => Stack -> Val -> IO ()
poke _stk@(Stack _ _ sp ustk bstk) (Val u b) = do
#ifdef STACK_CHECK
  assertBumped _stk 0
#endif
  writeByteArray ustk sp u
  writeArray bstk sp b
{-# INLINE poke #-}

-- | Sometimes we get back an int from a foreign call which we want to use as a Nat.
-- If we know it's positive and smaller than 2^63 then we can safely store the Int directly as a Nat without
-- checks.
unsafePokeIasN :: DebugCallStack => Stack -> Int -> IO ()
unsafePokeIasN stk n = do
  upokeT stk n natTypeTag
{-# INLINE unsafePokeIasN #-}

-- | Store an unboxed tag to later match on.
-- Often used to indicate the constructor of a data type that's been unpacked onto the stack,
-- or some tag we're about to branch on.
pokeTag :: DebugCallStack => Stack -> Int -> IO ()
pokeTag =
  -- For now we just use ints, but maybe should have a separate type for tags so we can detect if we're leaking them.
  pokeI
{-# INLINE pokeTag #-}

peekTag :: DebugCallStack => Stack -> IO Int
peekTag = peekI
{-# INLINE peekTag #-}

peekTagOff :: DebugCallStack => Stack -> Off -> IO Int
peekTagOff = peekOffI
{-# INLINE peekTagOff #-}

pokeBool :: DebugCallStack => Stack -> Bool -> IO ()
pokeBool stk b =
  poke stk $ if b then trueVal else falseVal
{-# INLINE pokeBool #-}

-- | Store a boxed value.
-- We don't bother nulling out the unboxed stack,
-- it's extra work and there's nothing to garbage collect.
bpoke :: DebugCallStack => Stack -> BVal -> IO ()
bpoke _stk@(Stack _ _ sp _ bstk) b = do
#ifdef STACK_CHECK
  assertBumped _stk 0
#endif
  writeArray bstk sp b
{-# INLINE bpoke #-}

pokeOff :: DebugCallStack => Stack -> Off -> Val -> IO ()
pokeOff stk i (Val u t) = do
  bpokeOff stk i t
  writeByteArray (ustk stk) (sp stk - i) u
{-# INLINE pokeOff #-}

upokeOffT :: DebugCallStack => Stack -> Off -> UVal -> BVal -> IO ()
upokeOffT stk i u t = do
  bpokeOff stk i t
  writeByteArray (ustk stk) (sp stk - i) u
{-# INLINE upokeOffT #-}

bpokeOff :: DebugCallStack => Stack -> Off -> BVal -> IO ()
bpokeOff _stk@(Stack _ _ sp _ bstk) i b = do
#ifdef STACK_CHECK
  assertBumped _stk i
#endif
  writeArray bstk (sp - i) b
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
bump (Stack ap fp sp ustk bstk) = do
  let stk' = Stack ap fp (sp + 1) ustk bstk
#ifdef STACK_CHECK
  pokeSentinelOff stk' 0
#endif
  pure stk'
{-# INLINE bump #-}

bumpn :: Stack -> SZ -> IO Stack
bumpn (Stack ap fp sp ustk bstk) n = do
  let stk' = Stack ap fp (sp + n) ustk bstk
#ifdef STACK_CHECK
  for_ [0..n-1] $ \i ->
    pokeSentinelOff stk' i
#endif
  pure stk'
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
peekN _stk@(Stack _ _ sp ustk _) = do
#ifdef STACK_CHECK
  assertUnboxed _stk 0
#endif
  readByteArray ustk sp
{-# INLINE peekN #-}

peekD :: Stack -> IO Double
peekD _stk@(Stack _ _ sp ustk _) = do
#ifdef STACK_CHECK
  assertUnboxed _stk 0
#endif
  readByteArray ustk sp
{-# INLINE peekD #-}

peekC :: Stack -> IO Char
peekC stk = do
  Char.chr <$> peekI stk
{-# INLINE peekC #-}

peekOffN :: Stack -> Int -> IO Word64
peekOffN _stk@(Stack _ _ sp ustk _) i = do
#ifdef STACK_CHECK
  assertUnboxed _stk i
#endif
  readByteArray ustk (sp - i)
{-# INLINE peekOffN #-}

peekOffD :: Stack -> Int -> IO Double
peekOffD _stk@(Stack _ _ sp ustk _) i = do
#ifdef STACK_CHECK
  assertUnboxed _stk i
#endif
  readByteArray ustk (sp - i)
{-# INLINE peekOffD #-}

peekOffC :: Stack -> Int -> IO Char
peekOffC _stk@(Stack _ _ sp ustk _) i = do
#ifdef STACK_CHECK
  assertUnboxed _stk i
#endif
  Char.chr <$> readByteArray ustk (sp - i)
{-# INLINE peekOffC #-}

{- ORMOLU_ENABLE -}

pokeN :: Stack -> Word64 -> IO ()
pokeN stk@(Stack _ _ sp ustk _) n = do
  bpoke stk natTypeTag
  writeByteArray ustk sp n
{-# INLINE pokeN #-}

pokeD :: Stack -> Double -> IO ()
pokeD stk@(Stack _ _ sp ustk _) d = do
  bpoke stk floatTypeTag
  writeByteArray ustk sp d
{-# INLINE pokeD #-}

pokeC :: Stack -> Char -> IO ()
pokeC stk@(Stack _ _ sp ustk _) c = do
  bpoke stk charTypeTag
  writeByteArray ustk sp (Char.ord c)
{-# INLINE pokeC #-}

-- | Note: This is for poking an unboxed value that has the UNISON type 'int', not just any unboxed data.
pokeI :: Stack -> Int -> IO ()
pokeI stk@(Stack _ _ sp ustk _) i = do
  bpoke stk intTypeTag
  writeByteArray ustk sp i
{-# INLINE pokeI #-}

pokeByte :: Stack -> Word8 -> IO ()
pokeByte stk b = do
  -- NOTE: currently we just store bytes as Word64s, but we should have a separate type runtime type tag for them.
  pokeN stk (fromIntegral b)
{-# INLINE pokeByte #-}

pokeOffN :: Stack -> Int -> Word64 -> IO ()
pokeOffN stk@(Stack _ _ sp ustk _) i n = do
  bpokeOff stk i natTypeTag
  writeByteArray ustk (sp - i) n
{-# INLINE pokeOffN #-}

pokeOffD :: Stack -> Int -> Double -> IO ()
pokeOffD stk@(Stack _ _ sp ustk _) i d = do
  bpokeOff stk i floatTypeTag
  writeByteArray ustk (sp - i) d
{-# INLINE pokeOffD #-}

pokeOffI :: Stack -> Int -> Int -> IO ()
pokeOffI stk@(Stack _ _ sp ustk _) i n = do
  bpokeOff stk i intTypeTag
  writeByteArray ustk (sp - i) n
{-# INLINE pokeOffI #-}

pokeOffC :: Stack -> Int -> Char -> IO ()
pokeOffC stk i c = do
  upokeOffT stk i (Char.ord c) charTypeTag
{-# INLINE pokeOffC #-}

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

peekBool :: Stack -> IO Bool
peekBool stk = do
  b <- bpeek stk
  pure $ case b of
    Enum _ t -> t /= TT.falseTag
    _ -> error "peekBool: not a boolean"
{-# INLINE peekBool #-}

peekOffBool :: Stack -> Int -> IO Bool
peekOffBool stk i = do
  b <- bpeekOff stk i
  pure $ case b of
    Enum _ t -> t /= TT.falseTag
    _ -> error "peekBool: not a boolean"
{-# INLINE peekOffBool #-}

peekOffS :: Stack -> Int -> IO USeq
peekOffS stk i =
  unwrapForeign . marshalToForeign <$> bpeekOff stk i
{-# INLINE peekOffS #-}

pokeS :: Stack -> USeq -> IO ()
pokeS stk s = bpoke stk (Foreign $ Wrap Ty.listRef s)
{-# INLINE pokeS #-}

pokeOffS :: Stack -> Int -> USeq -> IO ()
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
  (DataC _ _ vs) ->
    vs & foldMap \case
      BoxedVal c -> closureTermRefs f c
      UnboxedVal {} -> mempty
  (Captured k _ (_useg, bseg)) ->
    contTermRefs f k <> foldMap (closureTermRefs f) bseg
  (Foreign fo)
    | Just (cs :: USeq) <- maybeUnwrapForeign Ty.listRef fo ->
        foldMap (\(Val _i clos) -> closureTermRefs f clos) cs
  _ -> mempty

contTermRefs :: (Monoid m) => (Reference -> m) -> K -> m
contTermRefs f (Mark _ _ m k) =
  ( m & foldMap \case
      BoxedVal clo -> closureTermRefs f clo
      _ -> mempty
  )
    <> contTermRefs f k
contTermRefs f (Push _ _ (CIx r _ _) _ _ k) =
  f r <> contTermRefs f k
contTermRefs _ _ = mempty
