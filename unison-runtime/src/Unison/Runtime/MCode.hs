{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime.MCode
  ( Args' (..),
    Args (..),
    RefNums (..),
    MLit (..),
    GInstr (..),
    Instr,
    RInstr,
    GSection (.., MatchT, MatchW),
    RSection,
    Section,
    GComb (.., Lam),
    GCombInfo (..),
    Comb,
    RComb (..),
    RCombInfo,
    GCombs,
    RCombs,
    CombIx (..),
    GRef (..),
    RRef,
    Ref,
    UPrim1 (..),
    UPrim2 (..),
    BPrim1 (..),
    BPrim2 (..),
    GBranch (..),
    Branch,
    RBranch,
    emitCombs,
    emitComb,
    resolveCombs,
    absurdCombs,
    emptyRNs,
    argsToLists,
    countArgs,
    combRef,
    combDeps,
    combTypes,
    prettyCombs,
    prettyComb,
  )
where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor, bimap, first)
import Data.Bitraversable (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Coerce
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Void (Void, absurd)
import Data.Word (Word16, Word64)
import GHC.Stack (HasCallStack)
import Unison.ABT.Normalized (pattern TAbss)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Runtime.ANF
  ( ANormal,
    Branched (..),
    CTag,
    Direction (..),
    Func (..),
    Mem (..),
    SuperGroup (..),
    SuperNormal (..),
    internalBug,
    packTags,
    pattern TApp,
    pattern TBLit,
    pattern TFOp,
    pattern TFrc,
    pattern THnd,
    pattern TLets,
    pattern TLit,
    pattern TMatch,
    pattern TName,
    pattern TPrm,
    pattern TShift,
    pattern TVar,
  )
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.Array
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Builtin.Types (builtinTypeNumbering)
import Unison.Util.EnumContainers as EC
import Unison.Util.Text (Text)
import Unison.Var (Var)

-- This outlines some of the ideas/features in this core
-- language, and how they may be used to implement features of
-- the surface language.

-----------------------
-- Delimited control --
-----------------------

-- There is native support for delimited control operations in
-- the core language. This means we can:
--   1. delimit a block of code with an integer tagged prompt,
--      which corresponds to pushing a frame onto the
--      continuation with said tag
--   2. capture a portion of the continuation up to a particular
--      tag frame and turn it into a value, which _removes_ the
--      tag frame from the continuation in the process
--   3. push such a captured value back onto the continuation

-- TBD: Since the captured continuations in _delimited_ control
-- are (in this case impure) functions, it may make sense to make
-- the representation of functions support these captured
-- continuations directly.

-- The obvious use case of this feature is effects and handlers.
-- Delimiting a block with a prompt is part of installing a
-- handler for said block at least naively. The other part is
-- establishing the code that should be executed for each
-- operation to be handled.

-- It's important (I believe) in #2 that the prompt be removed
-- from the continuation by a control effect. The captured
-- continuation not being automatically delimited corresponds to
-- a shallow handler's obligation to re-establish the handling of
-- a re-invoked computation if it wishes to do so. The delimiter
-- being removed from the capturing code's continuation
-- corresponds to a handler being allowed to yield effects from
-- the same siganture that it is handling.

-- In special cases, it should be possible to omit use of control
-- effects in handlers. At the least, if a handler case resumes
-- the computation in tail position, it should be unnecessary to
-- capture the continuation at all. If all cases act this way, we
-- don't need a delimiter, because we will never capture.

-- TBD: it may make more sense to have prompt pushing be part of
-- some other construct, due to A-normal forms of the code.

-----------------------------
-- Unboxed sum-of-products --
-----------------------------

-- It is not usually stated this way, but one of the core
-- features of the STG machine is that functions/closures can
-- return unboxed sum-of-products types. This is actually the way
-- _all_ data types work in STG. The discriminee of a case
-- statement must eventually return by pushing several values
-- onto the stack (the product part) and specifying which branch
-- to return to (the sum part).

-- The way heap allocated data is produced is that an
-- intermediate frame may be in the continuation that grabs this
-- information from the local storage and puts it into the heap.
-- If this frame were omitted, only the unboxed component would
-- be left. Also, in STG, the heap allocated data is just a means
-- of reconstructing its unboxed analogue. Evaluating a heap
-- allocated data type value just results in pushing its stored
-- fields back on the stack, and immediately returning the tag.

-- The portion of this with the heap allocation frame omitted
-- seems to be a natural match for the case analysis portion of
-- handlers. A naive implementation of an effect algebra is as
-- the data type of the polynomial functor generated by the
-- signature, and handling corresponds to case analysis. However,
-- in a real implementation, we don't want a heap allocated
-- representation of this algebra, because its purpose is control
-- flow. Each operation will be handled once as it occurs, and we
-- won't save work by remembering some reified representation of
-- which operations were used.

-- Since handlers in unison are written as functions, it seems to
-- make sense to define a calling convention for unboxed
-- sum-of-products as arguments. Variable numbers of stack
-- positions could be pushed for such arguments, with tags
-- specifying which case is being provided.

-- TBD: sum arguments to a function correspond to a product of
-- functions, so it's possible that the calling convention for
-- these functions should be similar to returning to a case,
-- where we push arguments and then select which of several
-- pieces of code to jump to. This view also seems relevant to
-- the optimized implementation of certain forms of handler,
-- where we want effects to just directly select some code to
-- execute based on state that has been threaded to that point.

-- One thing to note: it probably does not make sense to
-- completely divide returns into unboxed returns and allocation
-- frames. The reason this works in STG is laziness. Naming a
-- computation with `let` does not do any evaluation, but it does
-- allocate space for its (boxed) result. The only thing that
-- _does_ demand evaluation is case analysis. So, if a value with
-- sum type is being evaluated, we know it must be about to be
-- unpacked, and it makes little sense to pack it on the stack,
-- though we can build a closure version of it in the writeback
-- location established by `let`.

-- By contrast, in unison a `let` of a sum type evaluates it
-- immediately, even if no one is analyzing it. So we might waste
-- work rearranging the stack with the unpacked contents when we
-- only needed the closure version to begin with. Instead, we
-- gain the ability to make the unpacking operation use no stack,
-- because we know what we are unpacking must be a value. Turning
-- boxed function calls into unboxed versions thus seems like a
-- situational optimization, rather than a universal calling
-- convention.

-------------------------------
-- Delimited Dynamic Binding --
-------------------------------

-- There is a final component to the implementation of ability
-- handlers in this runtime system, and that is dynamically
-- scoped variables associated to each prompt. Each prompt
-- corresponds to an ability signature, and `reset` to a handler
-- for said signature, but we need storage space for the code
-- installed by said handler. It is possible to implement
-- dynamically scoped variables entirely with delimited
-- continuations, but it is more efficient to keep track of the
-- storage directly when manipulating the continuations.

-- The dynamic scoping---and how it interacts with
-- continuations---corresponds to the nested structure of
-- handlers. Installing a handler establishes a variable scope,
-- shadowing outer scopes for the same prompt. Shifting, however,
-- can exit these scopes dynamically. So, for instance, if we
-- have a structure like:

--    reset 0 $ ...
--      reset 1 $ ...
--        reset 0 $ ...
--          shift 1 <E>

-- We have nested scopes 0>1>0, with the second 0 shadowing the
-- first. However, when we shift to 1, the inner 0 scope is
-- captured into the continuation, and uses of the 0 ability in
-- <E> will be handled by the outer handler until it is shadowed
-- again (and the captured continuation will re-establish the
-- shadowing).

-- Mutation of the variables is possible, but mutation only
-- affects the current scope. Essentially, the dynamic scoping is
-- of mutable references, and when scope changes, we switch
-- between different references, and the mutation of each
-- reference does not affect the others. The purpose of the
-- mutation is to enable more efficient implementation of
-- certain recursive, 'deep' handlers, since those can operate
-- more like stateful code than control operators.

data Args'
  = Arg1 !Int
  | Arg2 !Int !Int
  | -- frame index of each argument to the function
    ArgN {-# UNPACK #-} !(PrimArray Int)
  | ArgR !Int !Int
  deriving (Show)

data Args
  = ZArgs
  | VArg1 !Int
  | VArg2 !Int !Int
  | VArgR !Int !Int
  | VArgN {-# UNPACK #-} !(PrimArray Int)
  | VArgV !Int
  deriving (Show, Eq, Ord)

argsToLists :: Args -> [Int]
argsToLists = \case
  ZArgs -> []
  VArg1 i -> [i]
  VArg2 i j -> [i, j]
  VArgR i l -> take l [i ..]
  VArgN us -> primArrayToList us
  VArgV _ -> internalBug "argsToLists: DArgV"

countArgs :: Args -> Int
countArgs ZArgs = 0
countArgs (VArg1 {}) = 1
countArgs (VArg2 {}) = 2
countArgs (VArgR _ l) = l
countArgs (VArgN us) = sizeofPrimArray us
countArgs (VArgV {}) = internalBug "countArgs: DArgV"

data UPrim1
  = -- integral
    DECI
  | INCI
  | NEGI
  | SGNI -- decrement,increment,negate,signum
  | LZRO
  | TZRO
  | COMN
  | POPC -- leading/trailingZeroes,complement
  -- floating
  | ABSF
  | EXPF
  | LOGF
  | SQRT -- abs,exp,log,sqrt
  | COSF
  | ACOS
  | COSH
  | ACSH -- cos,acos,cosh,acosh
  | SINF
  | ASIN
  | SINH
  | ASNH -- sin,asin,sinh,asinh
  | TANF
  | ATAN
  | TANH
  | ATNH -- tan,atan,tanh,atanh
  | ITOF
  | NTOF
  | CEIL
  | FLOR -- intToFloat,natToFloat,ceiling,floor
  | TRNF
  | RNDF -- truncate,round
  deriving (Show, Eq, Ord, Enum, Bounded)

data UPrim2
  = -- integral
    ADDI
  | SUBI
  | MULI
  | DIVI
  | MODI -- +,-,*,/,mod
  | DIVN
  | MODN
  | SHLI
  | SHRI
  | SHRN
  | POWI -- shiftl,shiftr,shiftr,pow
  | EQLI
  | LEQI
  | LEQN -- ==,<=,<=
  | ANDN
  | IORN
  | XORN -- and,or,xor
  -- floating
  | EQLF
  | LEQF -- ==,<=
  | ADDF
  | SUBF
  | MULF
  | DIVF
  | ATN2 -- +,-,*,/,atan2
  | POWF
  | LOGB
  | MAXF
  | MINF -- pow,low,max,min
  deriving (Show, Eq, Ord, Enum, Bounded)

data BPrim1
  = -- text
    SIZT
  | USNC
  | UCNS -- size,unsnoc,uncons
  | ITOT
  | NTOT
  | FTOT -- intToText,natToText,floatToText
  | TTOI
  | TTON
  | TTOF -- textToInt,textToNat,textToFloat
  | PAKT
  | UPKT -- pack,unpack
  -- sequence
  | VWLS
  | VWRS
  | SIZS -- viewl,viewr,size
  | PAKB
  | UPKB
  | SIZB -- pack,unpack,size
  | FLTB -- flatten
  -- code
  | MISS
  | CACH
  | LKUP
  | LOAD -- isMissing,cache_,lookup,load
  | CVLD -- validate
  | VALU
  | TLTT -- value, Term.Link.toText
  -- debug
  | DBTX -- debug text
  | SDBL -- sandbox link list
  deriving (Show, Eq, Ord, Enum, Bounded)

data BPrim2
  = -- universal
    EQLU
  | CMPU -- ==,compare
  -- text
  | DRPT
  | CATT
  | TAKT -- drop,append,take
  | IXOT -- indexof
  | EQLT
  | LEQT
  | LEST -- ==,<=,<
  -- sequence
  | DRPS
  | CATS
  | TAKS -- drop,append,take
  | CONS
  | SNOC
  | IDXS -- cons,snoc,index
  | SPLL
  | SPLR -- splitLeft,splitRight
  -- bytes
  | TAKB
  | DRPB
  | IDXB
  | CATB -- take,drop,index,append
  | IXOB -- indexof
  -- general
  | THRO -- throw
  | TRCE -- trace
  -- code
  | SDBX -- sandbox
  | SDBV -- sandbox Value
  deriving (Show, Eq, Ord, Enum, Bounded)

data MLit
  = MI !Int
  | MD !Double
  | MT !Text
  | MM !Referent
  | MY !Reference
  deriving (Show, Eq, Ord)

type Instr = GInstr CombIx

type RInstr clos = GInstr (RComb clos)

-- Instructions for manipulating the data stack in the main portion of
-- a block
data GInstr comb
  = -- 1-argument unboxed primitive operations
    UPrim1
      !UPrim1 -- primitive instruction
      !Int -- index of prim argument
  | -- 2-argument unboxed primitive operations
    UPrim2
      !UPrim2 -- primitive instruction
      !Int -- index of first prim argument
      !Int -- index of second prim argument
  | -- 1-argument primitive operations that may involve boxed values
    BPrim1
      !BPrim1
      !Int
  | -- 2-argument primitive operations that may involve boxed values
    BPrim2
      !BPrim2
      !Int
      !Int
  | -- Call out to a Haskell function. This is considerably slower
    -- for very simple operations, hence the primops.
    ForeignCall
      !Bool -- catch exceptions
      !Word64 -- FFI call
      !Args -- arguments
  | -- Set the value of a dynamic reference
    SetDyn
      !Word64 -- the prompt tag of the reference
      !Int -- the stack index of the closure to store
  | -- Capture the continuation up to a given marker.
    Capture !Word64 -- the prompt tag
  | -- This is essentially the opposite of `Call`. Pack a given
    -- statically known function into a closure with arguments.
    -- No stack is necessary, because no nested evaluation happens,
    -- so the instruction directly takes a follow-up.
    Name !(GRef comb) !Args
  | -- Dump some debugging information about the machine state to
    -- the screen.
    Info !String -- prefix for output
  | -- Pack a data type value into a closure and place it
    -- on the stack.
    Pack
      !Reference -- data type reference
      !Word64 -- tag
      !Args -- arguments to pack
  | -- Push a particular value onto the appropriate stack
    Lit !MLit -- value to push onto the stack
  | -- Push a particular value directly onto the boxed stack
    BLit !Reference !Word64 {- packed type tag for the ref -} !MLit
  | -- Print a value on the unboxed stack
    Print !Int -- index of the primitive value to print
  | -- Put a delimiter on the continuation
    Reset !(EnumSet Word64) -- prompt ids
  | -- Fork thread evaluating delayed computation on boxed stack
    Fork !Int
  | -- Atomic transaction evaluating delayed computation on boxed stack
    Atomically !Int
  | -- Build a sequence consisting of a variable number of arguments
    Seq !Args
  | -- Force a delayed expression, catching any runtime exceptions involved
    TryForce !Int
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

type Section = GSection CombIx

type RSection clos = GSection (RComb clos)

data GSection comb
  = -- Apply a function to arguments. This is the 'slow path', and
    -- handles applying functions from arbitrary sources. This
    -- requires checks to determine what exactly should happen.
    App
      !Bool -- skip argument check for known calling convention
      !(GRef comb) -- function to call
      !Args -- arguments
  | -- This is the 'fast path', for when we statically know we're
    -- making an exactly saturated call to a statically known
    -- function. This allows skipping various checks that can cost
    -- time in very tight loops. This also allows skipping the
    -- stack check if we know that the current stack allowance is
    -- sufficient for where we're jumping to.
    Call
      !Bool -- skip stack check
      !CombIx
      {- Lazy! Might be cyclic -} comb
      !Args -- arguments
  | -- Jump to a captured continuation value.
    Jump
      !Int -- index of captured continuation
      !Args -- arguments to send to continuation
  | -- Branch on the value in the unboxed data stack
    Match
      !Int -- index of unboxed item to match on
      !(GBranch comb) -- branches
  | -- Yield control to the current continuation, with arguments
    Yield !Args -- values to yield
  | -- Prefix an instruction onto a section
    Ins !(GInstr comb) !(GSection comb)
  | -- Sequence two sections. The second is pushed as a return
    -- point for the results of the first. Stack modifications in
    -- the first are lost on return to the second.
    --
    -- The stored CombIx is a combinator that contains the second
    -- section, which can be used to reconstruct structures that
    -- throw away the section, like serializable continuation values.
    -- Code generation will emit the section as its own combinator,
    -- but also include it directly here.
    Let
      !(GSection comb) -- binding
      !CombIx -- body section refrence
      !Int -- stack safety
      !(GSection comb) -- body code
  | -- Throw an exception with the given message
    Die String
  | -- Immediately stop a thread of interpretation. This is more of
    -- a debugging tool than a proper operation to target.
    Exit
  | -- Branch on a data type without dumping the tag onto the unboxed
    -- stack.
    DMatch
      !(Maybe Reference) -- expected data type
      !Int -- index of data item on boxed stack
      !(GBranch comb) -- branches
  | -- Branch on a numeric type without dumping it to the stack
    NMatch
      !(Maybe Reference) -- expected data type
      !Int -- index of data item on boxed stack
      !(GBranch comb) -- branches
  | -- Branch on a request representation without dumping the tag
    -- portion to the unboxed stack.
    RMatch
      !Int -- index of request item on the boxed stack
      !(GSection comb) -- pure case
      !(EnumMap Word64 (GBranch comb)) -- effect cases
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data CombIx
  = CIx
      !Reference -- top reference
      !Word64 -- top level
      !Word64 -- section
  deriving (Eq, Ord, Show)

combRef :: CombIx -> Reference
combRef (CIx r _ _) = r

data RefNums = RN
  { dnum :: Reference -> Word64,
    cnum :: Reference -> Word64
  }

emptyRNs :: RefNums
emptyRNs = RN mt mt
  where
    mt _ = internalBug "RefNums: empty"

type Comb = GComb Void CombIx

-- Actual information for a proper combinator. The GComb type is no
-- longer strictly a 'combinator.'
data GCombInfo comb
  = LamI
      !Int -- Number of arguments
      !Int -- Maximum needed frame size
      !(GSection comb) -- Entry
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

data GComb clos comb
  = Comb {-# UNPACK #-} !(GCombInfo comb)
  | -- A pre-evaluated comb, typically a pure top-level const
    CachedClosure !Word64 {- top level comb ix -} !clos
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

pattern Lam ::
  Int -> Int -> GSection comb -> GComb clos comb
pattern Lam a f sect = Comb (LamI a f sect)

-- it seems GHC can't figure this out itself
{-# COMPLETE CachedClosure, Lam #-}

instance Bifunctor GComb where
  bimap = bimapDefault

instance Bifoldable GComb where
  bifoldMap = bifoldMapDefault

instance Bitraversable GComb where
  bitraverse f _ (CachedClosure cix c) = CachedClosure cix <$> f c
  bitraverse _ f (Lam a fr s) = Lam a fr <$> traverse f s

type RCombs clos = GCombs clos (RComb clos)

-- | The fixed point of a GComb where all references to a Comb are themselves Combs.
newtype RComb clos = RComb {unRComb :: GComb clos (RComb clos)}

type RCombInfo clos = GCombInfo (RComb clos)

instance Show (RComb clos) where
  show _ = "<RCOMB>"

-- | Map of combinators, parameterized by comb reference type
type GCombs clos comb = EnumMap Word64 (GComb clos comb)

-- | A reference to a combinator, parameterized by comb
type Ref = GRef CombIx

type RRef clos = GRef (RComb clos)

data GRef comb
  = Stk !Int -- stack reference to a closure
  | Env !CombIx {- Lazy! Might be cyclic -} comb
  | Dyn !Word64 -- dynamic scope reference to a closure
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Branch = GBranch CombIx

type RBranch clos = GBranch (RComb clos)

data GBranch comb
  = -- if tag == n then t else f
    Test1
      !Word64
      !(GSection comb)
      !(GSection comb)
  | Test2
      !Word64
      !(GSection comb) -- if tag == m then ...
      !Word64
      !(GSection comb) -- else if tag == n then ...
      !(GSection comb) -- else ...
  | TestW
      !(GSection comb)
      !(EnumMap Word64 (GSection comb))
  | TestT
      !(GSection comb)
      !(M.Map Text (GSection comb))
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

-- Convenience patterns for matches used in the algorithms below.
pattern MatchW :: Int -> (GSection comb) -> EnumMap Word64 (GSection comb) -> (GSection comb)
pattern MatchW i d cs = Match i (TestW d cs)

pattern MatchT :: Int -> (GSection comb) -> M.Map Text (GSection comb) -> (GSection comb)
pattern MatchT i d cs = Match i (TestT d cs)

pattern NMatchW ::
  Maybe Reference -> Int -> (GSection comb) -> EnumMap Word64 (GSection comb) -> (GSection comb)
pattern NMatchW r i d cs = NMatch r i (TestW d cs)

-- Representation of the variable context available in the current
-- frame. This tracks tags that have been dumped to the stack for
-- proper indexing. The `Block` constructor is used to mark when we
-- go into the first portion of a `Let`, to track the size of that
-- sub-frame.
data Ctx v
  = ECtx
  | Block (Ctx v)
  | Tag (Ctx v)
  | Var v Mem (Ctx v)
  deriving (Show)

-- Represents the context formed by the top-level let rec around a
-- set of definitions. Previous steps have normalized the term to
-- only contain a single recursive binding group. The variables in
-- this binding group are resolved to numbered combinators rather
-- than stack positions.
type RCtx v = M.Map v Word64

-- Add a sequence of variables and corresponding calling conventions
-- to the context.
ctx :: [v] -> [Mem] -> Ctx v
ctx vs cs = pushCtx (zip vs cs) ECtx

-- Look up a variable in the context, getting its position on the
-- relevant stack and its calling convention if it is there.
ctxResolve :: (Var v) => Ctx v -> v -> Maybe (Int, Mem)
ctxResolve ctx v = walk 0 ctx
  where
    walk _ ECtx = Nothing
    walk i (Block ctx) = walk i ctx
    walk i (Tag ctx) = walk (i + 1) ctx
    walk i (Var x m ctx)
      | v == x = Just (i, m)
      | otherwise = walk (i + 1) ctx

-- Add a sequence of variables and calling conventions to the context.
pushCtx :: [(v, Mem)] -> Ctx v -> Ctx v
pushCtx new old = foldr (uncurry Var) old new

-- Concatenate two contexts
catCtx :: Ctx v -> Ctx v -> Ctx v
catCtx ECtx r = r
catCtx (Tag l) r = Tag $ catCtx l r
catCtx (Block l) r = Block $ catCtx l r
catCtx (Var v m l) r = Var v m $ catCtx l r

-- Split the context after a particular variable
breakAfter :: (Eq v) => (v -> Bool) -> Ctx v -> (Ctx v, Ctx v)
breakAfter _ ECtx = (ECtx, ECtx)
breakAfter p (Tag vs) = first Tag $ breakAfter p vs
breakAfter p (Block vs) = first Block $ breakAfter p vs
breakAfter p (Var v m vs) = (Var v m lvs, rvs)
  where
    (lvs, rvs)
      | p v = (ECtx, vs)
      | otherwise = breakAfter p vs

-- Modify the context to contain the variables introduced by an
-- unboxed sum
sumCtx :: (Var v) => Ctx v -> v -> [(v, Mem)] -> Ctx v
sumCtx ctx v vcs
  | (lctx, rctx) <- breakAfter (== v) ctx =
      catCtx lctx $ pushCtx vcs rctx

-- Look up a variable in the top let rec context
rctxResolve :: (Var v) => RCtx v -> v -> Maybe Word64
rctxResolve ctx u = M.lookup u ctx

-- Compile a top-level definition group to a collection of combinators.
-- The provided word refers to the numbering for the overall group,
-- and intra-group calls are numbered locally, with 0 specifying
-- the global entry point.
emitCombs ::
  (Var v) =>
  RefNums ->
  Reference ->
  Word64 ->
  SuperGroup v ->
  EnumMap Word64 Comb
emitCombs rns grpr grpn (Rec grp ent) =
  emitComb rns grpr grpn rec (0, ent) <> aux
  where
    (rvs, cmbs) = unzip grp
    ixs = map (`shiftL` 16) [1 ..]
    rec = M.fromList $ zip rvs ixs
    aux = foldMap (emitComb rns grpr grpn rec) (zip ixs cmbs)

-- | lazily replace all references to combinators with the combinators themselves,
-- tying the knot recursively when necessary.
resolveCombs ::
  -- Existing in-scope combs that might be referenced
  Maybe (EnumMap Word64 (RCombs clos)) ->
  -- Combinators which need their knots tied.
  EnumMap Word64 (GCombs clos CombIx) ->
  EnumMap Word64 (RCombs clos)
resolveCombs mayExisting combs =
  -- Fixed point lookup;
  -- We make sure not to force resolved Combs or we'll loop forever.
  let ~resolved =
        combs
          <&> (fmap . fmap) \(CIx _ n i) ->
            let cmbs = case mayExisting >>= EC.lookup n of
                  Just cmbs -> cmbs
                  Nothing ->
                    case EC.lookup n resolved of
                      Just cmbs -> cmbs
                      Nothing -> error $ "unknown combinator `" ++ show n ++ "`."
             in case EC.lookup i cmbs of
                  Just cmb -> RComb cmb
                  Nothing ->
                    error $
                      "unknown section `"
                        ++ show i
                        ++ "` of combinator `"
                        ++ show n
                        ++ "`."
   in resolved

absurdCombs :: EnumMap Word64 (EnumMap Word64 (GComb Void cix)) -> EnumMap Word64 (GCombs any cix)
absurdCombs = fmap . fmap . first $ absurd

-- Type for aggregating the necessary stack frame size. First field is the
-- necessary size. The Applicative instance takes the
-- maximum, so that combining values from different branches
-- results in finding the maximum number of slots either side requires.
--
-- TODO: Now that we have a single stack, most of this counting can probably be simplified.
data Counted a = C !Int a
  deriving (Functor)

instance Applicative Counted where
  pure = C 0
  C s0 f <*> C s1 x = C (max s0 s1) (f x)

newtype Emit a
  = EM (Word64 -> (EC.EnumMap Word64 Comb, Counted a))
  deriving (Functor)

runEmit :: Word64 -> Emit a -> EC.EnumMap Word64 Comb
runEmit w (EM e) = fst $ e w

instance Applicative Emit where
  pure = EM . pure . pure . pure
  EM ef <*> EM ex = EM $ (liftA2 . liftA2) (<*>) ef ex

counted :: Counted a -> Emit a
counted = EM . pure . pure

onCount :: (Counted a -> Counted b) -> Emit a -> Emit b
onCount f (EM e) = EM $ fmap f <$> e

letIndex :: Word16 -> Word64 -> Word64
letIndex l c = c .|. fromIntegral l

record :: Ctx v -> Word16 -> Emit Section -> Emit (Word64, Comb)
record ctx l (EM es) = EM $ \c ->
  let (m, C sz s) = es c
      na = countCtx0 0 ctx
      n = letIndex l c
      comb = Lam na sz s
   in (EC.mapInsert n comb m, C sz (n, comb))

recordTop :: [v] -> Word16 -> Emit Section -> Emit ()
recordTop vs l (EM e) = EM $ \c ->
  let (m, C sz s) = e c
      na = length vs
      n = letIndex l c
   in (EC.mapInsert n (Lam na sz s) m, C sz ())

-- Counts the stack space used by a context and annotates a value
-- with it.
countCtx :: Ctx v -> a -> Emit a
countCtx ctx = counted . C i
  where
    i = countCtx0 0 ctx

countCtx0 :: Int -> Ctx v -> Int
countCtx0 !i (Var _ _ ctx) = countCtx0 (i + 1) ctx
countCtx0 i (Tag ctx) = countCtx0 (i + 1) ctx
countCtx0 i (Block ctx) = countCtx0 i ctx
countCtx0 i ECtx = i

emitComb ::
  (Var v) =>
  RefNums ->
  Reference ->
  Word64 ->
  RCtx v ->
  (Word64, SuperNormal v) ->
  EC.EnumMap Word64 Comb
emitComb rns grpr grpn rec (n, Lambda ccs (TAbss vs bd)) =
  runEmit n
    . recordTop vs 0
    $ emitSection rns grpr grpn rec (ctx vs ccs) bd

addCount :: Int -> Emit a -> Emit a
addCount i = onCount $ \(C sz x) -> C (sz + i) x

-- Emit a machine code section from an ANF term
emitSection ::
  (Var v) =>
  RefNums ->
  Reference ->
  Word64 ->
  RCtx v ->
  Ctx v ->
  ANormal v ->
  Emit Section
emitSection rns grpr grpn rec ctx (TLets d us ms bu bo) =
  emitLet rns grpr grpn rec d (zip us ms) ctx bu $
    emitSection rns grpr grpn rec ectx bo
  where
    ectx = pushCtx (zip us ms) ctx
emitSection rns grpr grpn rec ctx (TName u (Left f) args bo) =
  emitClosures grpr grpn rec ctx args $ \ctx as ->
    let cix = (CIx f (cnum rns f) 0)
     in Ins (Name (Env cix cix) as)
          <$> emitSection rns grpr grpn rec (Var u BX ctx) bo
emitSection rns grpr grpn rec ctx (TName u (Right v) args bo)
  | Just (i, BX) <- ctxResolve ctx v =
      emitClosures grpr grpn rec ctx args $ \ctx as ->
        Ins (Name (Stk i) as)
          <$> emitSection rns grpr grpn rec (Var u BX ctx) bo
  | Just n <- rctxResolve rec v =
      emitClosures grpr grpn rec ctx args $ \ctx as ->
        let cix = (CIx grpr grpn n)
         in Ins (Name (Env cix cix) as)
              <$> emitSection rns grpr grpn rec (Var u BX ctx) bo
  | otherwise = emitSectionVErr v
emitSection _ grpr grpn rec ctx (TVar v)
  | Just (i, _) <- ctxResolve ctx v = countCtx ctx . Yield $ VArg1 i
  | Just j <- rctxResolve rec v =
      let cix = (CIx grpr grpn j)
       in countCtx ctx $ App False (Env cix cix) $ ZArgs
  | otherwise = emitSectionVErr v
emitSection _ _ grpn _ ctx (TPrm p args) =
  -- 3 is a conservative estimate of how many extra stack slots
  -- a prim op will need for its results.
  addCount 3
    . countCtx ctx
    . Ins (emitPOp p $ emitArgs grpn ctx args)
    . Yield
    . VArgV
    $ countBlock ctx
emitSection _ _ grpn _ ctx (TFOp p args) =
  addCount 3
    . countCtx ctx
    . Ins (emitFOp p $ emitArgs grpn ctx args)
    . Yield
    . VArgV
    $ countBlock ctx
emitSection rns grpr grpn rec ctx (TApp f args) =
  emitClosures grpr grpn rec ctx args $ \ctx as ->
    countCtx ctx $ emitFunction rns grpr grpn rec ctx f as
emitSection _ _ _ _ ctx (TLit l) =
  c . countCtx ctx . Ins (emitLit l) . Yield $ VArg1 0
  where
    c
      | ANF.T {} <- l = addCount 1
      | ANF.LM {} <- l = addCount 1
      | ANF.LY {} <- l = addCount 1
      | otherwise = addCount 1
emitSection _ _ _ _ ctx (TBLit l) =
  addCount 1 . countCtx ctx . Ins (emitBLit l) . Yield $ VArg1 0
emitSection rns grpr grpn rec ctx (TMatch v bs)
  | Just (i, BX) <- ctxResolve ctx v,
    MatchData r cs df <- bs =
      DMatch (Just r) i
        <$> emitDataMatching r rns grpr grpn rec ctx cs df
  | Just (i, BX) <- ctxResolve ctx v,
    MatchRequest hs0 df <- bs,
    hs <- mapFromList $ first (dnum rns) <$> M.toList hs0 =
      uncurry (RMatch i)
        <$> emitRequestMatching rns grpr grpn rec ctx hs df
  | Just (i, UN) <- ctxResolve ctx v,
    MatchIntegral cs df <- bs =
      emitLitMatching
        MatchW
        "missing integral case"
        rns
        grpr
        grpn
        rec
        ctx
        i
        cs
        df
  | Just (i, BX) <- ctxResolve ctx v,
    MatchNumeric r cs df <- bs =
      emitLitMatching
        (NMatchW (Just r))
        "missing integral case"
        rns
        grpr
        grpn
        rec
        ctx
        i
        cs
        df
  | Just (i, BX) <- ctxResolve ctx v,
    MatchText cs df <- bs =
      emitLitMatching
        MatchT
        "missing text case"
        rns
        grpr
        grpn
        rec
        ctx
        i
        cs
        df
  | Just (i, UN) <- ctxResolve ctx v,
    MatchSum cs <- bs =
      emitSumMatching rns grpr grpn rec ctx v i cs
  | Just (_, cc) <- ctxResolve ctx v =
      internalBug $
        "emitSection: mismatched calling convention for match: "
          ++ matchCallingError cc bs
  | otherwise =
      internalBug $
        "emitSection: could not resolve match variable: " ++ show (ctx, v)
emitSection rns grpr grpn rec ctx (THnd rs h b)
  | Just (i, BX) <- ctxResolve ctx h =
      Ins (Reset (EC.setFromList ws))
        . flip (foldr (\r -> Ins (SetDyn r i))) ws
        <$> emitSection rns grpr grpn rec ctx b
  | otherwise = emitSectionVErr h
  where
    ws = dnum rns <$> rs
emitSection rns grpr grpn rec ctx (TShift r v e) =
  Ins (Capture $ dnum rns r)
    <$> emitSection rns grpr grpn rec (Var v BX ctx) e
emitSection _ _ _ _ ctx (TFrc v)
  | Just (i, BX) <- ctxResolve ctx v =
      countCtx ctx $ App False (Stk i) ZArgs
  | Just _ <- ctxResolve ctx v =
      internalBug $
        "emitSection: values to be forced must be boxed: " ++ show v
  | otherwise = emitSectionVErr v
emitSection _ _ _ _ _ tm =
  internalBug $ "emitSection: unhandled code: " ++ show tm

-- Emit the code for a function call
emitFunction ::
  (Var v) =>
  RefNums ->
  Reference ->
  Word64 -> -- self combinator number
  RCtx v -> -- recursive binding group
  Ctx v -> -- local context
  Func v ->
  Args ->
  Section
emitFunction _ grpr grpn rec ctx (FVar v) as
  | Just (i, BX) <- ctxResolve ctx v =
      App False (Stk i) as
  | Just j <- rctxResolve rec v =
      let cix = CIx grpr grpn j
       in App False (Env cix cix) as
  | otherwise = emitSectionVErr v
emitFunction rns _grpr _ _ _ (FComb r) as
  | otherwise -- slow path
    =
      let cix = CIx r n 0
       in App False (Env cix cix) as
  where
    n = cnum rns r
emitFunction rns _grpr _ _ _ (FCon r t) as =
  Ins (Pack r (packTags rt t) as)
    . Yield
    $ VArg1 0
  where
    rt = toEnum . fromIntegral $ dnum rns r
emitFunction rns _grpr _ _ _ (FReq r e) as =
  -- Currently implementing packed calling convention for abilities
  -- TODO ct is 16 bits, but a is 48 bits. This will be a problem if we have
  -- more than 2^16 types.
  Ins (Pack r (packTags rt e) as)
    . App True (Dyn a)
    $ VArg1 0
  where
    a = dnum rns r
    rt = toEnum . fromIntegral $ a
emitFunction _ _grpr _ _ ctx (FCont k) as
  | Just (i, BX) <- ctxResolve ctx k = Jump i as
  | Nothing <- ctxResolve ctx k = emitFunctionVErr k
  | otherwise = internalBug $ "emitFunction: continuations are boxed"
emitFunction _ _grpr _ _ _ (FPrim _) _ =
  internalBug "emitFunction: impossible"

countBlock :: Ctx v -> Int
countBlock = go 0
  where
    go !i (Var _ _ ctx) = go (i + 1) ctx
    go i (Tag ctx) = go (i + 1) ctx
    go i _ = i

matchCallingError :: Mem -> Branched v -> String
matchCallingError cc b = "(" ++ show cc ++ "," ++ brs ++ ")"
  where
    brs
      | MatchData _ _ _ <- b = "MatchData"
      | MatchEmpty <- b = "MatchEmpty"
      | MatchIntegral _ _ <- b = "MatchIntegral"
      | MatchNumeric _ _ _ <- b = "MatchNumeric"
      | MatchRequest _ _ <- b = "MatchRequest"
      | MatchSum _ <- b = "MatchSum"
      | MatchText _ _ <- b = "MatchText"

emitSectionVErr :: (Var v, HasCallStack) => v -> a
emitSectionVErr v =
  internalBug $
    "emitSection: could not resolve function variable: " ++ show v

emitFunctionVErr :: (Var v, HasCallStack) => v -> a
emitFunctionVErr v =
  internalBug $
    "emitFunction: could not resolve function variable: " ++ show v

-- Emit machine code for a let expression. Some expressions do not
-- require a machine code Let, which uses more complicated stack
-- manipulation.
emitLet ::
  (Var v) =>
  RefNums ->
  Reference ->
  Word64 ->
  RCtx v ->
  Direction Word16 ->
  [(v, Mem)] ->
  Ctx v ->
  ANormal v ->
  Emit Section ->
  Emit Section
emitLet _ _ _ _ _ _ _ (TLit l) =
  fmap (Ins $ emitLit l)
emitLet _ _ _ _ _ _ _ (TBLit l) =
  fmap (Ins $ emitBLit l)
-- emitLet rns grp _   _ _   ctx (TApp (FComb r) args)
--   -- We should be able to tell if we are making a saturated call
--   -- or not here. We aren't carrying the information here yet, though.
--   | False -- not saturated
--   = fmap (Ins . Name (Env n 0) $ emitArgs grp ctx args)
--   where
--   n = cnum rns r
emitLet rns _ grpn _ _ _ ctx (TApp (FCon r n) args) =
  fmap (Ins . Pack r (packTags rt n) $ emitArgs grpn ctx args)
  where
    rt = toEnum . fromIntegral $ dnum rns r
emitLet _ _ grpn _ _ _ ctx (TApp (FPrim p) args) =
  fmap (Ins . either emitPOp emitFOp p $ emitArgs grpn ctx args)
emitLet rns grpr grpn rec d vcs ctx bnd
  | Direct <- d =
      internalBug $ "unsupported compound direct let: " ++ show bnd
  | Indirect w <- d =
      \esect ->
        f
          <$> emitSection rns grpr grpn rec (Block ctx) bnd
          <*> record (pushCtx vcs ctx) w esect
  where
    f s (w, Lam _ f bd) =
      let cix = (CIx grpr grpn w)
       in Let s cix f bd

-- Translate from ANF prim ops to machine code operations. The
-- machine code operations are divided with respect to more detailed
-- information about expected number and types of arguments.
emitPOp :: ANF.POp -> Args -> Instr
-- Integral
emitPOp ANF.ADDI = emitP2 ADDI
emitPOp ANF.ADDN = emitP2 ADDI
emitPOp ANF.SUBI = emitP2 SUBI
emitPOp ANF.SUBN = emitP2 SUBI
emitPOp ANF.MULI = emitP2 MULI
emitPOp ANF.MULN = emitP2 MULI
emitPOp ANF.DIVI = emitP2 DIVI
emitPOp ANF.DIVN = emitP2 DIVN
emitPOp ANF.MODI = emitP2 MODI -- TODO: think about how these behave
emitPOp ANF.MODN = emitP2 MODN -- TODO: think about how these behave
emitPOp ANF.POWI = emitP2 POWI
emitPOp ANF.POWN = emitP2 POWI
emitPOp ANF.SHLI = emitP2 SHLI
emitPOp ANF.SHLN = emitP2 SHLI -- Note: left shift behaves uniformly
emitPOp ANF.SHRI = emitP2 SHRI
emitPOp ANF.SHRN = emitP2 SHRN
emitPOp ANF.LEQI = emitP2 LEQI
emitPOp ANF.LEQN = emitP2 LEQN
emitPOp ANF.EQLI = emitP2 EQLI
emitPOp ANF.EQLN = emitP2 EQLI
emitPOp ANF.SGNI = emitP1 SGNI
emitPOp ANF.NEGI = emitP1 NEGI
emitPOp ANF.INCI = emitP1 INCI
emitPOp ANF.INCN = emitP1 INCI
emitPOp ANF.DECI = emitP1 DECI
emitPOp ANF.DECN = emitP1 DECI
emitPOp ANF.TZRO = emitP1 TZRO
emitPOp ANF.LZRO = emitP1 LZRO
emitPOp ANF.POPC = emitP1 POPC
emitPOp ANF.ANDN = emitP2 ANDN
emitPOp ANF.IORN = emitP2 IORN
emitPOp ANF.XORN = emitP2 XORN
emitPOp ANF.COMN = emitP1 COMN
-- Float
emitPOp ANF.ADDF = emitP2 ADDF
emitPOp ANF.SUBF = emitP2 SUBF
emitPOp ANF.MULF = emitP2 MULF
emitPOp ANF.DIVF = emitP2 DIVF
emitPOp ANF.LEQF = emitP2 LEQF
emitPOp ANF.EQLF = emitP2 EQLF
emitPOp ANF.MINF = emitP2 MINF
emitPOp ANF.MAXF = emitP2 MAXF
emitPOp ANF.POWF = emitP2 POWF
emitPOp ANF.EXPF = emitP1 EXPF
emitPOp ANF.ABSF = emitP1 ABSF
emitPOp ANF.SQRT = emitP1 SQRT
emitPOp ANF.LOGF = emitP1 LOGF
emitPOp ANF.LOGB = emitP2 LOGB
emitPOp ANF.CEIL = emitP1 CEIL
emitPOp ANF.FLOR = emitP1 FLOR
emitPOp ANF.TRNF = emitP1 TRNF
emitPOp ANF.RNDF = emitP1 RNDF
emitPOp ANF.COSF = emitP1 COSF
emitPOp ANF.SINF = emitP1 SINF
emitPOp ANF.TANF = emitP1 TANF
emitPOp ANF.COSH = emitP1 COSH
emitPOp ANF.SINH = emitP1 SINH
emitPOp ANF.TANH = emitP1 TANH
emitPOp ANF.ACOS = emitP1 ACOS
emitPOp ANF.ATAN = emitP1 ATAN
emitPOp ANF.ASIN = emitP1 ASIN
emitPOp ANF.ACSH = emitP1 ACSH
emitPOp ANF.ASNH = emitP1 ASNH
emitPOp ANF.ATNH = emitP1 ATNH
emitPOp ANF.ATN2 = emitP2 ATN2
-- conversions
emitPOp ANF.ITOF = emitP1 ITOF
emitPOp ANF.NTOF = emitP1 NTOF
emitPOp ANF.ITOT = emitBP1 ITOT
emitPOp ANF.NTOT = emitBP1 NTOT
emitPOp ANF.FTOT = emitBP1 FTOT
emitPOp ANF.TTON = emitBP1 TTON
emitPOp ANF.TTOI = emitBP1 TTOI
emitPOp ANF.TTOF = emitBP1 TTOF
-- text
emitPOp ANF.CATT = emitBP2 CATT
emitPOp ANF.TAKT = emitBP2 TAKT
emitPOp ANF.DRPT = emitBP2 DRPT
emitPOp ANF.IXOT = emitBP2 IXOT
emitPOp ANF.SIZT = emitBP1 SIZT
emitPOp ANF.UCNS = emitBP1 UCNS
emitPOp ANF.USNC = emitBP1 USNC
emitPOp ANF.EQLT = emitBP2 EQLT
emitPOp ANF.LEQT = emitBP2 LEQT
emitPOp ANF.PAKT = emitBP1 PAKT
emitPOp ANF.UPKT = emitBP1 UPKT
-- sequence
emitPOp ANF.CATS = emitBP2 CATS
emitPOp ANF.TAKS = emitBP2 TAKS
emitPOp ANF.DRPS = emitBP2 DRPS
emitPOp ANF.SIZS = emitBP1 SIZS
emitPOp ANF.CONS = emitBP2 CONS
emitPOp ANF.SNOC = emitBP2 SNOC
emitPOp ANF.IDXS = emitBP2 IDXS
emitPOp ANF.VWLS = emitBP1 VWLS
emitPOp ANF.VWRS = emitBP1 VWRS
emitPOp ANF.SPLL = emitBP2 SPLL
emitPOp ANF.SPLR = emitBP2 SPLR
-- bytes
emitPOp ANF.PAKB = emitBP1 PAKB
emitPOp ANF.UPKB = emitBP1 UPKB
emitPOp ANF.TAKB = emitBP2 TAKB
emitPOp ANF.DRPB = emitBP2 DRPB
emitPOp ANF.IXOB = emitBP2 IXOB
emitPOp ANF.IDXB = emitBP2 IDXB
emitPOp ANF.SIZB = emitBP1 SIZB
emitPOp ANF.FLTB = emitBP1 FLTB
emitPOp ANF.CATB = emitBP2 CATB
-- universal comparison
emitPOp ANF.EQLU = emitBP2 EQLU
emitPOp ANF.CMPU = emitBP2 CMPU
-- code operations
emitPOp ANF.MISS = emitBP1 MISS
emitPOp ANF.CACH = emitBP1 CACH
emitPOp ANF.LKUP = emitBP1 LKUP
emitPOp ANF.TLTT = emitBP1 TLTT
emitPOp ANF.CVLD = emitBP1 CVLD
emitPOp ANF.LOAD = emitBP1 LOAD
emitPOp ANF.VALU = emitBP1 VALU
emitPOp ANF.SDBX = emitBP2 SDBX
emitPOp ANF.SDBL = emitBP1 SDBL
emitPOp ANF.SDBV = emitBP2 SDBV
-- error call
emitPOp ANF.EROR = emitBP2 THRO
emitPOp ANF.TRCE = emitBP2 TRCE
emitPOp ANF.DBTX = emitBP1 DBTX
-- non-prim translations
emitPOp ANF.BLDS = Seq
emitPOp ANF.FORK = \case
  VArg1 i -> Fork i
  _ -> internalBug "fork takes exactly one boxed argument"
emitPOp ANF.ATOM = \case
  VArg1 i -> Atomically i
  _ -> internalBug "atomically takes exactly one boxed argument"
emitPOp ANF.PRNT = \case
  VArg1 i -> Print i
  _ -> internalBug "print takes exactly one boxed argument"
emitPOp ANF.INFO = \case
  ZArgs -> Info "debug"
  _ -> internalBug "info takes no arguments"
emitPOp ANF.TFRC = \case
  VArg1 i -> TryForce i
  _ -> internalBug "tryEval takes exactly one boxed argument"

-- handled in emitSection because Die is not an instruction

-- Emit machine code for ANF IO operations. These are all translated
-- to 'foreing function' calls, but there is a special case for the
-- standard handle access function, because it does not yield an
-- explicit error.
emitFOp :: ANF.FOp -> Args -> Instr
emitFOp fop = ForeignCall True (fromIntegral $ fromEnum fop)

-- Helper functions for packing the variable argument representation
-- into the indexes stored in prim op instructions
emitP1 :: UPrim1 -> Args -> Instr
emitP1 p (VArg1 i) = UPrim1 p i
emitP1 p a =
  internalBug $
    "wrong number of args for unary unboxed primop: "
      ++ show (p, a)

emitP2 :: UPrim2 -> Args -> Instr
emitP2 p (VArg2 i j) = UPrim2 p i j
emitP2 p a =
  internalBug $
    "wrong number of args for binary unboxed primop: "
      ++ show (p, a)

emitBP1 :: BPrim1 -> Args -> Instr
emitBP1 p (VArg1 i) = BPrim1 p i
emitBP1 p a =
  internalBug $
    "wrong number of args for unary boxed primop: "
      ++ show (p, a)

emitBP2 :: BPrim2 -> Args -> Instr
emitBP2 p (VArg2 i j) = BPrim2 p i j
emitBP2 p a =
  internalBug $
    "wrong number of args for binary boxed primop: "
      ++ show (p, a)

emitDataMatching ::
  (Var v) =>
  Reference ->
  RefNums ->
  Reference ->
  Word64 ->
  RCtx v ->
  Ctx v ->
  EnumMap CTag ([Mem], ANormal v) ->
  Maybe (ANormal v) ->
  Emit Branch
emitDataMatching r rns grpr grpn rec ctx cs df =
  TestW <$> edf <*> traverse (emitCase rns grpr grpn rec ctx) (coerce cs)
  where
    -- Note: this is not really accurate. A default data case needs
    -- stack space corresponding to the actual data that shows up there.
    -- However, we currently don't use default cases for data.
    edf
      | Just co <- df = emitSection rns grpr grpn rec ctx co
      | otherwise = countCtx ctx $ Die ("missing data case for hash " <> show r)

-- Emits code corresponding to an unboxed sum match.
-- The match is against a tag on the stack, and cases introduce
-- variables to the middle of the context, because the fields were
-- already there, but it was unknown how many there were until
-- branching on the tag.
emitSumMatching ::
  (Var v) =>
  RefNums ->
  Reference ->
  Word64 ->
  RCtx v ->
  Ctx v ->
  v ->
  Int ->
  EnumMap Word64 ([Mem], ANormal v) ->
  Emit Section
emitSumMatching rns grpr grpn rec ctx v i cs =
  MatchW i edf <$> traverse (emitSumCase rns grpr grpn rec ctx v) cs
  where
    edf = Die "uncovered unboxed sum case"

emitRequestMatching ::
  (Var v) =>
  RefNums ->
  Reference ->
  Word64 ->
  RCtx v ->
  Ctx v ->
  EnumMap Word64 (EnumMap CTag ([Mem], ANormal v)) ->
  ANormal v ->
  Emit (Section, EnumMap Word64 Branch)
emitRequestMatching rns grpr grpn rec ctx hs df = (,) <$> pur <*> tops
  where
    pur = emitCase rns grpr grpn rec ctx ([BX], df)
    tops = traverse f (coerce hs)
    f cs = TestW edf <$> traverse (emitCase rns grpr grpn rec ctx) cs
    edf = Die "unhandled ability"

emitLitMatching ::
  (Var v) =>
  (Traversable f) =>
  (Int -> Section -> f Section -> Section) ->
  String ->
  RefNums ->
  Reference ->
  Word64 ->
  RCtx v ->
  Ctx v ->
  Int ->
  f (ANormal v) ->
  Maybe (ANormal v) ->
  Emit Section
emitLitMatching con err rns grpr grpn rec ctx i cs df =
  con i <$> edf <*> traverse (emitCase rns grpr grpn rec ctx . ([],)) cs
  where
    edf
      | Just co <- df = emitSection rns grpr grpn rec ctx co
      | otherwise = countCtx ctx $ Die err

emitCase ::
  (Var v) =>
  RefNums ->
  Reference ->
  Word64 ->
  RCtx v ->
  Ctx v ->
  ([Mem], ANormal v) ->
  Emit Section
emitCase rns grpr grpn rec ctx (ccs, TAbss vs bo) =
  emitSection rns grpr grpn rec (pushCtx (zip vs ccs) ctx) bo

emitSumCase ::
  (Var v) =>
  RefNums ->
  Reference ->
  Word64 ->
  RCtx v ->
  Ctx v ->
  v ->
  ([Mem], ANormal v) ->
  Emit Section
emitSumCase rns grpr grpn rec ctx v (ccs, TAbss vs bo) =
  emitSection rns grpr grpn rec (sumCtx ctx v $ zip vs ccs) bo

litToMLit :: ANF.Lit -> MLit
litToMLit (ANF.I i) = MI $ fromIntegral i
litToMLit (ANF.N n) = MI $ fromIntegral n
litToMLit (ANF.C c) = MI $ fromEnum c
litToMLit (ANF.F d) = MD d
litToMLit (ANF.T t) = MT t
litToMLit (ANF.LM r) = MM r
litToMLit (ANF.LY r) = MY r

emitLit :: ANF.Lit -> Instr
emitLit = Lit . litToMLit

doubleToInt :: Double -> Int
doubleToInt d = indexByteArray (byteArrayFromList [d]) 0

emitBLit :: ANF.Lit -> Instr
emitBLit l = case l of
  (ANF.F d) -> BLit lRef builtinTypeTag (MI $ doubleToInt d)
  _ -> BLit lRef builtinTypeTag (litToMLit l)
  where
    lRef = ANF.litRef l
    builtinTypeTag :: Word64
    builtinTypeTag =
      case M.lookup (ANF.litRef l) builtinTypeNumbering of
        Nothing -> error "emitBLit: unknown builtin type reference"
        Just n ->
          let rt = toEnum (fromIntegral n)
           in (packTags rt 0)

-- Emits some fix-up code for calling functions. Some of the
-- variables in scope come from the top-level let rec, but these
-- are definitions, not values on the stack. These definitions cannot
-- be passed directly as function arguments, and must have a
-- corresponding stack entry allocated first. So, this function inserts
-- these allocations and passes the appropriate context into the
-- provided continuation.
emitClosures ::
  (Var v) =>
  Reference ->
  Word64 ->
  RCtx v ->
  Ctx v ->
  [v] ->
  (Ctx v -> Args -> Emit Section) ->
  Emit Section
emitClosures grpr grpn rec ctx args k =
  allocate ctx args $ \ctx -> k ctx $ emitArgs grpn ctx args
  where
    allocate ctx [] k = k ctx
    allocate ctx (a : as) k
      | Just _ <- ctxResolve ctx a = allocate ctx as k
      | Just n <- rctxResolve rec a =
          let cix = (CIx grpr grpn n)
           in Ins (Name (Env cix cix) ZArgs) <$> allocate (Var a BX ctx) as k
      | otherwise =
          internalBug $ "emitClosures: unknown reference: " ++ show a

emitArgs :: (Var v) => Word64 -> Ctx v -> [v] -> Args
emitArgs grpn ctx args
  | Just l <- traverse (ctxResolve ctx) args = demuxArgs l
  | otherwise =
      internalBug $
        "emitArgs["
          ++ show grpn
          ++ "]: "
          ++ "could not resolve argument variables: "
          ++ show args

-- Turns a list of stack positions and calling conventions into the
-- argument format expected in the machine code.
demuxArgs :: [(Int, Mem)] -> Args
demuxArgs = \case
  [] -> ZArgs
  [(i, _)] -> VArg1 i
  [(i, _), (j, _)] -> VArg2 i j
  args -> VArgN $ PA.primArrayFromList (fst <$> args)

combDeps :: GComb clos comb -> [Word64]
combDeps (Lam _ _ s) = sectionDeps s
combDeps (CachedClosure {}) = []

combTypes :: GComb any comb -> [Word64]
combTypes (Lam _ _ s) = sectionTypes s
combTypes (CachedClosure {}) = []

sectionDeps :: GSection comb -> [Word64]
sectionDeps (App _ (Env (CIx _ w _) _) _) = [w]
sectionDeps (Call _ (CIx _ w _) _ _) = [w]
sectionDeps (Match _ br) = branchDeps br
sectionDeps (DMatch _ _ br) = branchDeps br
sectionDeps (RMatch _ pu br) =
  sectionDeps pu ++ foldMap branchDeps br
sectionDeps (NMatch _ _ br) = branchDeps br
sectionDeps (Ins i s)
  | Name (Env (CIx _ w _) _) _ <- i = w : sectionDeps s
  | otherwise = sectionDeps s
sectionDeps (Let s (CIx _ w _) _ b) =
  w : sectionDeps s ++ sectionDeps b
sectionDeps _ = []

sectionTypes :: GSection comb -> [Word64]
sectionTypes (Ins i s) = instrTypes i ++ sectionTypes s
sectionTypes (Let s _ _ b) = sectionTypes s ++ sectionTypes b
sectionTypes (Match _ br) = branchTypes br
sectionTypes (DMatch _ _ br) = branchTypes br
sectionTypes (NMatch _ _ br) = branchTypes br
sectionTypes (RMatch _ pu br) =
  sectionTypes pu ++ foldMap branchTypes br
sectionTypes _ = []

instrTypes :: GInstr comb -> [Word64]
instrTypes (Pack _ w _) = [w `shiftR` 16]
instrTypes (Reset ws) = setToList ws
instrTypes (Capture w) = [w]
instrTypes (SetDyn w _) = [w]
instrTypes _ = []

branchDeps :: GBranch comb -> [Word64]
branchDeps (Test1 _ s1 d) = sectionDeps s1 ++ sectionDeps d
branchDeps (Test2 _ s1 _ s2 d) =
  sectionDeps s1 ++ sectionDeps s2 ++ sectionDeps d
branchDeps (TestW d m) =
  sectionDeps d ++ foldMap sectionDeps m
branchDeps (TestT d m) =
  sectionDeps d ++ foldMap sectionDeps m

branchTypes :: GBranch comb -> [Word64]
branchTypes (Test1 _ s1 d) = sectionTypes s1 ++ sectionTypes d
branchTypes (Test2 _ s1 _ s2 d) =
  sectionTypes s1 ++ sectionTypes s2 ++ sectionTypes d
branchTypes (TestW d m) =
  sectionTypes d ++ foldMap sectionTypes m
branchTypes (TestT d m) =
  sectionTypes d ++ foldMap sectionTypes m

indent :: Int -> ShowS
indent ind = showString (replicate (ind * 2) ' ')

prettyCombs ::
  Word64 ->
  EnumMap Word64 Comb ->
  ShowS
prettyCombs w es =
  foldr
    (\(i, c) r -> prettyComb w i c . showString "\n" . r)
    id
    (mapToList es)

prettyComb :: (Show clos, Show comb) => Word64 -> Word64 -> GComb clos comb -> ShowS
prettyComb w i = \case
  (Lam a _ s) ->
    shows w
      . showString ":"
      . shows i
      . shows a
      . showString ":\n"
      . prettySection 2 s
  (CachedClosure a b) ->
    shows w
      . showString ":"
      . shows i
      . shows a
      . showString ":\n"
      . shows b

prettySection :: (Show comb) => Int -> GSection comb -> ShowS
prettySection ind sec =
  indent ind . case sec of
    App _ r as ->
      showString "App "
        . showsPrec 12 r
        . showString " "
        . prettyArgs as
    Call _ i _ as ->
      showString "Call " . shows i . showString " " . prettyArgs as
    Jump i as ->
      showString "Jump " . shows i . showString " " . prettyArgs as
    Match i bs ->
      showString "Match "
        . shows i
        . showString "\n"
        . prettyBranches (ind + 1) bs
    Yield as -> showString "Yield " . prettyArgs as
    Ins i nx ->
      prettyIns i . showString "\n" . prettySection ind nx
    Let s _ _ b ->
      showString "Let\n"
        . prettySection (ind + 2) s
        . showString "\n"
        . indent ind
        . prettySection ind b
    Die s -> showString $ "Die " ++ s
    Exit -> showString "Exit"
    DMatch _ i bs ->
      showString "DMatch "
        . shows i
        . showString "\n"
        . prettyBranches (ind + 1) bs
    NMatch _ i bs ->
      showString "NMatch "
        . shows i
        . showString "\n"
        . prettyBranches (ind + 1) bs
    RMatch i pu bs ->
      showString "RMatch "
        . shows i
        . showString "\nPUR ->\n"
        . prettySection (ind + 1) pu
        . foldr (\p r -> rqc p . r) id (mapToList bs)
      where
        rqc (i, e) =
          showString "\n"
            . shows i
            . showString " ->\n"
            . prettyBranches (ind + 1) e

prettyBranches :: (Show comb) => Int -> GBranch comb -> ShowS
prettyBranches ind bs =
  case bs of
    Test1 i e df -> pdf df . picase i e
    Test2 i ei j ej df -> pdf df . picase i ei . picase j ej
    TestW df m ->
      pdf df . foldr (\(i, e) r -> picase i e . r) id (mapToList m)
    TestT df m ->
      pdf df . foldr (\(i, e) r -> ptcase i e . r) id (M.toList m)
  where
    pdf e = indent ind . showString "DFLT ->\n" . prettySection (ind + 1) e
    ptcase t e =
      showString "\n"
        . indent ind
        . shows t
        . showString " ->\n"
        . prettySection (ind + 1) e
    picase i e =
      showString "\n"
        . indent ind
        . shows i
        . showString " ->\n"
        . prettySection (ind + 1) e

prettyIns :: (Show comb) => GInstr comb -> ShowS
prettyIns (Pack r i as) =
  showString "Pack "
    . showsPrec 10 r
    . (' ' :)
    . shows i
    . (' ' :)
    . prettyArgs as
prettyIns i = shows i

prettyArgs :: Args -> ShowS
prettyArgs v = shows v
