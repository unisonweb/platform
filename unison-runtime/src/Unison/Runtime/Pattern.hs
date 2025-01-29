{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Pattern
  ( DataSpec,
    splitPatterns,
    builtinDataSpec,
  )
where

import Control.Monad.State (State, evalState, modify, runState, state)
import Data.Containers.ListUtils (nubOrd)
import Data.List (transpose)
import Data.Map.Strict
  ( fromListWith,
    insertWith,
  )
import Data.Map.Strict qualified as Map
import Data.Set (member)
import Data.Set qualified as Set
import Unison.ABT
  ( absChain',
    renames,
    visitPure,
    pattern AbsN',
  )
import Unison.Builtin.Decls (builtinDataDecls, builtinEffectDecls)
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.DataDeclaration (declFields)
import Unison.Pattern
import Unison.Pattern qualified as P
import Unison.Prelude hiding (guard)
import Unison.Reference (Reference, Reference' (Builtin, DerivedId))
import Unison.Runtime.ANF (internalBug)
import Unison.Term hiding (Term, matchPattern)
import Unison.Term qualified as Tm
import Unison.Type qualified as Rf
import Unison.Var (Type (Pattern), Var, freshIn, freshenId, typed)

type Term v = Tm.Term v ()

-- Represents the number of fields of constructors of a data type/
-- ability in order of constructors
type Cons = [Int]

type NCons = [(Int, Int)]

-- Maps references to the constructor information for abilities (left)
-- and data types (right)
type DataSpec = Map Reference (Either Cons Cons)

data PType = PData Reference | PReq (Set Reference) | Unknown

instance Semigroup PType where
  Unknown <> r = r
  l <> Unknown = l
  t@(PData l) <> PData r
    | l == r = t
  PReq l <> PReq r = PReq (l <> r)
  _ <> _ = internalBug "inconsistent pattern matching types"

instance Monoid PType where
  mempty = Unknown

type Ctx v = Map v PType

-- Representation of a row in a pattern compilation matrix.
-- There is a list of patterns annotated with the variables they
-- are matching against, an optional guard, and the body of the
-- 'match' clause associated with this row.
data PatternRow v = PR
  { matches :: [Pattern v],
    guard :: Maybe (Term v),
    body :: Term v
  }
  deriving (Show)

-- This is the data and ability 'constructor' information for all
-- the things defined in Haskell source code.
builtinDataSpec :: DataSpec
builtinDataSpec = Map.fromList decls
  where
    decls =
      [ (DerivedId x, declFields $ Right y)
        | (_, x, y) <- builtinDataDecls
      ]
        ++ [ (DerivedId x, declFields $ Left y)
             | (_, x, y) <- builtinEffectDecls
           ]

findPattern :: Eq v => v -> PatternRow v -> Maybe (Pattern v)
findPattern v (PR ms _ _)
  | (_, p : _) <- break ((== v) . loc) ms = Just p
  | otherwise = Nothing

-- A pattern compilation matrix is just a list of rows. There is
-- no need for the rows to have uniform length; the variable
-- annotations on the patterns in the rows keep track of what
-- should be matched against when decomposing a matrix.
data PatternMatrix v = PM {_rows :: [PatternRow v]}
  deriving (Show)

usedVars :: (Ord v) => PatternMatrix v -> Set v
usedVars (PM rs) = foldMap usedR rs
  where
    usedR (PR ps g b) =
      foldMap usedP ps <> foldMap freeVars g <> freeVars b
    usedP = foldMap Set.singleton

-- Heuristics guide the pattern compilation. They inspect the
-- pattern matrix and (may) choose a variable to split on next.
-- The full strategy will consist of multiple heuristics composed
-- in series.
type Heuristic v = PatternMatrix v -> Maybe v

choose :: [Heuristic v] -> PatternMatrix v -> v
choose [] (PM (PR (p : _) _ _ : _)) = loc p
choose [] _ =
  internalBug "pattern matching: failed to choose a splitting"
choose (h : hs) m
  | Just i <- h m = i
  | otherwise = choose hs m

refutable :: P.Pattern a -> Bool
refutable (P.Unbound _) = False
refutable (P.Var _) = False
refutable _ = True

noMatches :: PatternRow v -> Bool
noMatches (PR ps _ _) = null ps

rowRefutable :: PatternRow v -> Bool
rowRefutable (PR ps g _) = isJust g || not (null ps)

firstRow :: ([P.Pattern v] -> Maybe v) -> Heuristic v
firstRow f (PM (r : _)) = f $ matches r
firstRow _ _ = Nothing

heuristics :: [Heuristic v]
heuristics = [firstRow $ fmap loc . listToMaybe]

extractVar :: (Var v) => P.Pattern v -> Maybe v
extractVar p
  | P.Unbound {} <- p = Nothing
  | otherwise = Just (loc p)

extractVars :: (Var v) => [P.Pattern v] -> [v]
extractVars = catMaybes . fmap extractVar

-- Splits a data type pattern, yielding its subpatterns. The provided
-- integers are the tag and number of fields for the constructor being
-- matched against. A constructor pattern thus only yields results if
-- it matches the tag (and number of subpatterns as a consistency
-- check), while variables can also match and know how many subpatterns
-- to yield.
--
-- The outer list indicates success of the match. It could be Maybe,
-- but elsewhere these results are added to a list, so it is more
-- convenient to yield a list here.
decomposePattern ::
  (Var v) =>
  Maybe Reference ->
  Int ->
  Int ->
  P.Pattern v ->
  [[P.Pattern v]]
decomposePattern (Just rf0) t _ (P.Boolean _ b)
  | rf0 == Rf.booleanRef,
    t == if b then 1 else 0 =
      [[]]
decomposePattern (Just rf0) t nfields p@(P.Constructor _ (ConstructorReference rf u) ps)
  | t == fromIntegral u,
    rf0 == rf =
      if length ps == nfields
        then [ps]
        else internalBug err
  where
    err =
      "decomposePattern: wrong number of constructor fields: "
        ++ show (nfields, p)
decomposePattern (Just rf0) t nfields p@(P.EffectBind _ (ConstructorReference rf u) ps pk)
  | t == fromIntegral u,
    rf0 == rf =
      if length ps + 1 == nfields
        then [ps ++ [pk]]
        else internalBug err
  where
    err =
      "decomposePattern: wrong number of ability fields: "
        ++ show (nfields, p)
decomposePattern _ t _ (P.EffectPure _ p)
  | t == -1 = [[p]]
decomposePattern _ _ nfields (P.Var _) =
  [replicate nfields (P.Unbound (typed Pattern))]
decomposePattern _ _ nfields (P.Unbound _) =
  [replicate nfields (P.Unbound (typed Pattern))]
decomposePattern _ _ _ (P.SequenceLiteral _ _) =
  internalBug "decomposePattern: sequence literal"
decomposePattern _ _ _ _ = []

matchBuiltin :: P.Pattern a -> Maybe (P.Pattern ())
matchBuiltin (P.Var _) = Just $ P.Unbound ()
matchBuiltin (P.Unbound _) = Just $ P.Unbound ()
matchBuiltin (P.Nat _ n) = Just $ P.Nat () n
matchBuiltin (P.Int _ n) = Just $ P.Int () n
matchBuiltin (P.Text _ t) = Just $ P.Text () t
matchBuiltin (P.Char _ c) = Just $ P.Char () c
matchBuiltin (P.Float _ d) = Just $ P.Float () d
matchBuiltin _ = Nothing

-- Represents the various cases that that may occur when performing
-- a sequence match. These fall into two groups:
--
--   E, C, S: empty, cons, snoc
--   L, R, DL, DR: split left/right, insufficient elements
--
-- These groups correspond to different sorts of matches we can compile
-- to. We can view the left/right end of a sequence, or attempt to
-- split a sequence at a specified offset from the left/right side.
data SeqMatch = E | C | S | L Int | R Int | DL Int | DR Int
  deriving (Eq, Ord, Show)

seqPSize :: P.Pattern v -> Maybe Int
seqPSize (P.SequenceLiteral _ l) = Just $ length l
seqPSize (P.SequenceOp _ _ Cons r) = (1 +) <$> seqPSize r
seqPSize (P.SequenceOp _ l Snoc _) = (1 +) <$> seqPSize l
seqPSize (P.SequenceOp _ l Concat r) = (+) <$> seqPSize l <*> seqPSize r
seqPSize _ = Nothing

-- Decides a sequence matching operation to perform based on a column
-- of patterns that match against it. Literals do not really force a
-- bias, so the decision of which side to view is postponed to
-- subsequent patterns if we encounter a literal first. A literal with
-- priority does block a splitting pattern, though.
decideSeqPat :: [P.Pattern v] -> [SeqMatch]
decideSeqPat = go False
  where
    go _ [] = [E, C]
    go _ (P.SequenceLiteral {} : ps) = go True ps
    go _ (P.SequenceOp _ _ Snoc _ : _) = [E, S]
    go _ (P.SequenceOp _ _ Cons _ : _) = [E, C]
    go guard (P.SequenceOp _ l Concat r : _)
      | guard = [E, C] -- prefer prior literals
      | Just n <- seqPSize l = [L n, DL n]
      | Just n <- seqPSize r = [R n, DR n]
    go b (P.Unbound _ : ps) = go b ps
    go b (P.Var _ : ps) = go b ps
    go _ (p : _) =
      internalBug $ "Cannot process sequence pattern: " ++ show p

-- Represents the possible correspondences between a sequence pattern
-- and a sequence matching compilation target. Unlike data matching,
-- where a pattern either matches or is disjoint from a tag, sequence
-- patterns can overlap in non-trivial ways where it would be difficult
-- to avoid re-testing the original list.
data SeqCover v
  = Cover [P.Pattern v]
  | Disjoint
  | Overlap

-- Determines how a pattern corresponds to a sequence matching
-- compilation target.
decomposeSeqP :: (Var v) => Set v -> SeqMatch -> P.Pattern v -> SeqCover v
decomposeSeqP _ E (P.SequenceLiteral _ []) = Cover []
decomposeSeqP _ E _ = Disjoint
decomposeSeqP _ C (P.SequenceOp _ l Cons r) = Cover [l, r]
decomposeSeqP _ C (P.SequenceOp _ _ Concat _) = Overlap
decomposeSeqP _ C (P.SequenceLiteral _ []) = Disjoint
decomposeSeqP avoid C (P.SequenceLiteral _ (p : ps)) =
  Cover [p, P.SequenceLiteral u ps]
  where
    u = freshIn avoid $ typed Pattern
decomposeSeqP _ S (P.SequenceOp _ l Snoc r) = Cover [l, r]
decomposeSeqP _ S (P.SequenceOp _ _ Concat _) = Overlap
decomposeSeqP _ S (P.SequenceLiteral _ []) = Disjoint
decomposeSeqP avoid S (P.SequenceLiteral _ ps) =
  Cover [P.SequenceLiteral u (init ps), last ps]
  where
    u = freshIn avoid $ typed Pattern
decomposeSeqP _ (L n) (P.SequenceOp _ l Concat r)
  | Just m <- seqPSize l,
    n == m =
      Cover [l, r]
  | otherwise = Overlap
decomposeSeqP avoid (L n) (P.SequenceLiteral _ ps)
  | length ps >= n,
    (pl, pr) <- splitAt n ps =
      Cover $ P.SequenceLiteral u <$> [pl, pr]
  | otherwise = Disjoint
  where
    u = freshIn avoid $ typed Pattern
decomposeSeqP _ (R n) (P.SequenceOp _ l Concat r)
  | Just m <- seqPSize r,
    n == m =
      Cover [l, r]
decomposeSeqP avoid (R n) (P.SequenceLiteral _ ps)
  | length ps >= n,
    (pl, pr) <- splitAt (length ps - n) ps =
      Cover $ P.SequenceLiteral u <$> [pl, pr]
  | otherwise = Disjoint
  where
    u = freshIn avoid $ typed Pattern
decomposeSeqP _ (DL n) (P.SequenceOp _ l Concat _)
  | Just m <- seqPSize l, n == m = Disjoint
decomposeSeqP _ (DL n) (P.SequenceLiteral _ ps)
  | length ps >= n = Disjoint
decomposeSeqP _ (DR n) (P.SequenceOp _ _ Concat r)
  | Just m <- seqPSize r, n == m = Disjoint
decomposeSeqP _ (DR n) (P.SequenceLiteral _ ps)
  | length ps >= n = Disjoint
decomposeSeqP _ _ _ = Overlap

-- Splits a pattern row with respect to matching a variable against a
-- data type constructor. If the row would match the specified
-- constructor, the subpatterns and resulting row are yielded. A list
-- is used as the result value to indicate success or failure to match,
-- because these results are accumulated into a larger list elsewhere.
splitRow ::
  (Var v) =>
  v ->
  Maybe Reference ->
  Int ->
  Int ->
  PatternRow v ->
  [([P.Pattern v], PatternRow v)]
splitRow v rf t nfields (PR (break ((== v) . loc) -> (pl, sp : pr)) g b) =
  decomposePattern rf t nfields sp
    <&> \subs -> (subs, PR (pl ++ filter refutable subs ++ pr) g b)
splitRow _ _ _ _ row = [([], row)]

-- Splits a row with respect to a variable, expecting that the
-- variable will be matched against a builtin pattern (non-data type,
-- non-request, non-sequence). In addition to returning the
-- subpatterns and new row, returns a version of the pattern that was
-- matched against the variable that may be collected to determine the
-- cases the built-in value is matched against.
splitRowBuiltin ::
  (Var v) =>
  v ->
  PatternRow v ->
  [(P.Pattern (), [([P.Pattern v], PatternRow v)])]
splitRowBuiltin v (PR (break ((== v) . loc) -> (pl, sp : pr)) g b)
  | Just p <- matchBuiltin sp = [(p, [([], PR (pl ++ pr) g b)])]
  | otherwise = []
splitRowBuiltin _ r = [(P.Unbound (), [([], r)])]

-- Splits a row with respect to a variable, expecting that the
-- variable will be matched against a sequence matching operation.
-- Yields the subpatterns and a new row to be used in subsequent
-- compilation. The outer list result is used to indicate success or
-- failure.
splitRowSeq ::
  (Var v) =>
  Set v ->
  v ->
  SeqMatch ->
  PatternRow v ->
  [([P.Pattern v], PatternRow v)]
splitRowSeq avoid0 v m r@(PR (break ((== v) . loc) -> (pl, sp : pr)) g b) =
  case decomposeSeqP avoid m sp of
    Cover sps ->
      [(sps, PR (pl ++ filter refutable sps ++ pr) g b)]
    Disjoint -> []
    Overlap -> [([], r)]
  where
    avoid = avoid0 <> maybe mempty freeVars g <> freeVars b
splitRowSeq _ _ _ r = [([], r)]

-- Renames the variables annotating the patterns in a row, for once a
-- canonical choice has been made.
renameRow :: (Var v) => Map v v -> PatternRow v -> PatternRow v
renameRow m (PR p0 g0 b0) = PR p g b
  where
    access k
      | Just v <- Map.lookup k m = v
      | otherwise = k
    p = (fmap . fmap) access p0
    g = renames m <$> g0
    b = renames m b0

-- Chooses a common set of variables for use when decomposing
-- patterns into multiple sub-patterns. It is too naive to simply use
-- the variables in the first row, because it may have been generated
-- by decomposing a variable or unbound pattern, which will make up
-- variables for subpatterns.
chooseVars :: (Var v) => [[P.Pattern v]] -> [v]
chooseVars [] = []
chooseVars ([] : rs) = chooseVars rs
chooseVars ((P.Unbound {} : _) : rs) = chooseVars rs
chooseVars (r : _) = extractVars r

-- Creates a pattern matrix from many rows with the subpatterns
-- introduced during the splitting that generated those rows. Also
-- yields an indication of the type of the variables that the
-- subpatterns match against, if possible.
buildMatrix ::
  (Var v) =>
  [([P.Pattern v], PatternRow v)] ->
  ([(v, PType)], PatternMatrix v)
buildMatrix [] = ([], PM [])
buildMatrix vrs = (zip cvs rs, PM $ fixRow <$> vrs)
  where
    rs = fmap determineType . transpose . fmap fst $ vrs
    cvs = chooseVars $ fst <$> vrs
    fixRow (extractVars -> rvs, pr) =
      renameRow (fromListWith const . zip rvs $ cvs) pr

-- Splits a pattern matrix on a given variable, expected to be matched
-- against builtin type patterns. Yields the cases covered and
-- corresponding matrices for those cases, with types for any new
-- variables (although currently builtin patterns do not introduce
-- variables).
splitMatrixBuiltin ::
  (Var v) =>
  v ->
  PatternMatrix v ->
  [(P.Pattern (), [(v, PType)], PatternMatrix v)]
splitMatrixBuiltin v (PM rs) =
  fmap (\(a, (b, c)) -> (a, b, c))
    . Map.toList
    . fmap buildMatrix
    . fromListWith (flip (++))
    . expandIrrefutable
    $ splitRowBuiltin v =<< rs

expandIrrefutable ::
  (Var v) =>
  [(P.Pattern (), [([P.Pattern v], PatternRow v)])] ->
  [(P.Pattern (), [([P.Pattern v], PatternRow v)])]
expandIrrefutable rss = concatMap expand rss
  where
    specific = filter refutable $ fst <$> rss
    expand tup@(p, rs)
      | not (refutable p) = fmap (,rs) specific ++ [tup]
    expand tup = [tup]

matchPattern :: [(v, PType)] -> SeqMatch -> P.Pattern ()
matchPattern vrs = \case
  E -> sz 0
  C -> P.SequenceOp () vr Cons vr
  S -> P.SequenceOp () vr Snoc vr
  L n -> P.SequenceOp () (sz n) Concat (P.Var ())
  R n -> P.SequenceOp () (P.Var ()) Concat (sz n)
  DL _ -> P.Unbound ()
  DR _ -> P.Unbound ()
  where
    vr | [] <- vrs = P.Unbound () | otherwise = P.Var ()
    sz n = P.SequenceLiteral () . replicate n $ P.Unbound ()

-- Splits a matrix at a given variable with respect to sequence
-- patterns. Yields the appropriate patterns for the covered cases,
-- variables introduced for each case with their types, and new
-- matricies for subsequent compilation.
splitMatrixSeq ::
  (Var v) =>
  Set v ->
  v ->
  PatternMatrix v ->
  [(P.Pattern (), [(v, PType)], PatternMatrix v)]
splitMatrixSeq avoid v (PM rs) =
  cases
  where
    ms = decideSeqPat $ take 1 . dropWhile ((/= v) . loc) . matches =<< rs
    hint m vrs
      | m `elem` [E, C, S] = vrs
      | otherwise = (fmap . fmap) (const $ PData Rf.listRef) vrs
    cases =
      ms <&> \m ->
        let frs = rs >>= splitRowSeq avoid v m
            (vrs, pm) = buildMatrix frs
         in (matchPattern vrs m, hint m vrs, pm)

-- Splits a matrix at a given variable with respect to a data type or
-- ability match. Yields a new matrix for each constructor, with
-- variables introduced and their types for each case.
splitMatrix ::
  (Var v) =>
  v ->
  Maybe Reference ->
  NCons ->
  PatternMatrix v ->
  [(Int, [(v, PType)], PatternMatrix v)]
splitMatrix v rf cons (PM rs) =
  fmap (\(a, (b, c)) -> (a, b, c)) . (fmap . fmap) buildMatrix $ mmap
  where
    mmap = fmap (\(t, fs) -> (t, splitRow v rf t fs =<< rs)) cons

-- Eliminates a variable from a matrix, keeping the rows that are
-- _not_ specific matches on that variable (so, would potentially
-- occur in a default case).
antiSplitMatrix ::
  (Var v) =>
  v ->
  PatternMatrix v ->
  PatternMatrix v
antiSplitMatrix v (PM rs) = PM (f =<< rs)
  where
    -- keep rows that do not have a refutable pattern for v
    f r = [ r | isNothing $ findPattern v r ]

-- Monad for pattern preparation. It is a state monad carrying a fresh
-- variable source, the list of variables bound the pattern being
-- prepared, and a variable renaming mapping.
type PPM v a = State (Word64, [v], Map v v) a

freshVar :: (Var v) => PPM v v
freshVar = state $ \(fw, vs, rn) ->
  let v = freshenId fw $ typed Pattern
   in (v, (fw + 1, vs, rn))

useVar :: PPM v v
useVar = state $ \case
  (avoid, v : vs, rn) -> (v, (avoid, vs, rn))
  _ -> error "useVar: Expected multiple vars"

renameTo :: (Var v) => v -> v -> PPM v ()
renameTo to from =
  modify $ \(avoid, vs, rn) ->
    ( avoid,
      vs,
      insertWith (internalBug "renameTo: duplicate rename") from to rn
    )

-- Tries to rewrite sequence patterns into a format that can be
-- matched most flexibly.
normalizeSeqP :: P.Pattern a -> P.Pattern a
normalizeSeqP (P.As a p) = P.As a (normalizeSeqP p)
normalizeSeqP (P.EffectPure a p) = P.EffectPure a $ normalizeSeqP p
normalizeSeqP (P.EffectBind a r ps k) =
  P.EffectBind a r (normalizeSeqP <$> ps) (normalizeSeqP k)
normalizeSeqP (P.Constructor a r ps) =
  P.Constructor a r $ normalizeSeqP <$> ps
normalizeSeqP (P.SequenceLiteral a ps) =
  P.SequenceLiteral a $ normalizeSeqP <$> ps
normalizeSeqP (P.SequenceOp a p0 op q0) =
  case (op, normalizeSeqP p0, normalizeSeqP q0) of
    (Cons, p, P.SequenceLiteral _ ps) ->
      P.SequenceLiteral a (p : ps)
    (Snoc, P.SequenceLiteral _ ps, p) ->
      P.SequenceLiteral a (ps ++ [p])
    (Concat, P.SequenceLiteral _ ps, P.SequenceLiteral _ qs) ->
      P.SequenceLiteral a (ps ++ qs)
    (Concat, P.SequenceLiteral _ ps, q) ->
      foldr (\p r -> P.SequenceOp a p Cons r) q ps
    (Concat, p, P.SequenceLiteral _ qs) ->
      foldl (\r q -> P.SequenceOp a r Snoc q) p qs
    (op, p, q) -> P.SequenceOp a p op q
normalizeSeqP p = p

-- Prepares a pattern for compilation, like `preparePattern`. This
-- function, however, is used when a candidate variable for a pattern
-- has already been chosen, as with an As pattern. This allows turning
-- redundant names (like with the pattern u@v) into renamings.
prepareAs :: (Var v) => P.Pattern a -> v -> PPM v (P.Pattern v)
prepareAs (P.Unbound _) u = pure $ P.Var u
prepareAs (P.As _ p) u = (useVar >>= renameTo u) *> prepareAs p u
prepareAs (P.Var _) u = P.Var u <$ (renameTo u =<< useVar)
prepareAs (P.Constructor _ r ps) u = do
  P.Constructor u r <$> traverse preparePattern ps
prepareAs (P.EffectPure _ p) u = do
  P.EffectPure u <$> preparePattern p
prepareAs (P.EffectBind _ r ps k) u = do
  P.EffectBind u r
    <$> traverse preparePattern ps
    <*> preparePattern k
prepareAs (P.SequenceLiteral _ ps) u = do
  P.SequenceLiteral u <$> traverse preparePattern ps
prepareAs (P.SequenceOp _ p op q) u = do
  flip (P.SequenceOp u) op
    <$> preparePattern p
    <*> preparePattern q
prepareAs p u = pure $ u <$ p

-- Prepares a pattern for compilation. This removes the existing
-- annotations and replaces them with a choice of variable that the
-- pattern is matching against. As patterns are eliminated and the
-- variables they bind are used as candidates for what that level of
-- the pattern matches against.
preparePattern :: (Var v) => P.Pattern a -> PPM v (P.Pattern v)
preparePattern p = prepareAs p =<< freshVar

buildPattern :: Bool -> ConstructorReference -> [v] -> Int -> P.Pattern ()
buildPattern effect r vs nfields
  | effect, [] <- vps = internalBug "too few patterns for effect bind"
  | effect = P.EffectBind () r (init vps) (last vps)
  | otherwise = P.Constructor () r vps
  where
    vps
      | length vs < nfields =
          replicate nfields $ P.Unbound ()
      | otherwise =
          P.Var () <$ vs

numberCons :: Cons -> NCons
numberCons = zip [0 ..]

lookupData :: Reference -> DataSpec -> Either String Cons
lookupData rf (Map.lookup rf -> Just econs) =
  case econs of
    Left _ -> Left $ "data type matching on ability: " ++ show rf
    Right cs -> Right cs
lookupData rf _ = Left $ "unknown data reference: " ++ show rf

lookupAbil :: Reference -> DataSpec -> Either String Cons
lookupAbil rf (Map.lookup rf -> Just econs) =
  case econs of
    Right _ -> Left $ "ability matching on data: " ++ show rf
    Left cs -> Right $ fmap (1 +) cs
lookupAbil rf _ = Left $ "unknown ability reference: " ++ show rf

compile :: (Var v) => DataSpec -> Ctx v -> PatternMatrix v -> Term v
compile _ _ (PM []) = apps' bu [text () "pattern match failure"]
  where
    bu = ref () (Builtin "bug")
compile spec ctx m@(PM (r : rs))
  | noMatches r =
      case guard r of
        Nothing -> body r
        Just g -> iff mempty g (body r) $ compile spec ctx (PM rs)
  | PData rf <- ty,
    rf == Rf.listRef =
      match () (var () v) $
        buildCaseBuiltin spec ctx
          <$> splitMatrixSeq (Map.keysSet ctx <> usedVars m) v m
  | PData rf <- ty,
    rf `member` builtinCase =
      match () (var () v) $
        buildCaseBuiltin spec ctx
          <$> splitMatrixBuiltin v m
  | PData rf <- ty =
      case lookupData rf spec of
        Right cons ->
          match () (var () v) $
            (buildCase spec rf False cons ctx
              <$> splitMatrix v (Just rf) ncons m)
              ++ buildDefaultCase spec False needDefault ctx dm
          where
            needDefault = length ncons < length cons
        Left err -> internalBug err
  | PReq rfs <- ty =
      match () (var () v) $
        [ buildCasePure spec ctx tup
          | tup <- splitMatrix v Nothing [(-1, 1)] m
        ]
          ++ [ buildCase spec rf True cons ctx tup
               | rf <- Set.toList rfs,
                 Right cons <- [lookupAbil rf spec],
                 tup <- splitMatrix v (Just rf) (numberCons cons) m
             ]
  | Unknown <- ty =
      internalBug "unknown pattern compilation type"
  where
    v = choose heuristics m
    ncons = relevantConstructors m v
    ty = Map.findWithDefault Unknown v ctx
    dm = antiSplitMatrix v m

-- Calculates the data constructors—with their arities—that should be
-- matched on when splitting a matrix on a given variable. This
-- includes
relevantConstructors :: Ord v => PatternMatrix v -> v -> [(Int, Int)]
relevantConstructors (PM rows) v = search [] rows
  where
    search acc (row : rows)
      | rowRefutable row = case findPattern v row of
          Just (P.Constructor _ (ConstructorReference _ t) sps) ->
            search ((fromIntegral t, length sps) : acc) rows
          Just (P.Boolean _ b) ->
            search ((if b then 1 else 0, 0) : acc) rows
          Just p ->
            internalBug $ "unexpected data pattern: " ++ show p
          -- if the pattern is not found, it must have been irrefutable,
          -- so contributes no relevant constructor.
          _ -> search acc rows
    -- irrefutable row, or no rows left
    search acc _ = nubOrd $ reverse acc

buildCaseBuiltin ::
  (Var v) =>
  DataSpec ->
  Ctx v ->
  (P.Pattern (), [(v, PType)], PatternMatrix v) ->
  MatchCase () (Term v)
buildCaseBuiltin spec ctx0 (p, vrs, m) =
  MatchCase p Nothing . absChain' vs $ compile spec ctx m
  where
    vs = ((),) . fst <$> vrs
    ctx = Map.fromList vrs <> ctx0

buildCasePure ::
  (Var v) =>
  DataSpec ->
  Ctx v ->
  (Int, [(v, PType)], PatternMatrix v) ->
  MatchCase () (Term v)
buildCasePure spec ctx0 (_, vts, m) =
  MatchCase pat Nothing . absChain' vs $ compile spec ctx m
  where
    vp
      | [] <- vts = P.Unbound ()
      | otherwise = P.Var ()
    pat = P.EffectPure () vp
    vs = ((),) . fst <$> vts
    ctx = Map.fromList vts <> ctx0

buildCase ::
  (Var v) =>
  DataSpec ->
  Reference ->
  Bool ->
  Cons ->
  Ctx v ->
  (Int, [(v, PType)], PatternMatrix v) ->
  MatchCase () (Term v)
buildCase spec r eff cons ctx0 (t, vts, m) =
  MatchCase pat Nothing . absChain' vs $ compile spec ctx m
  where
    pat = buildPattern eff (ConstructorReference r (fromIntegral t)) vs $ cons !! t
    vs = ((),) . fst <$> vts
    ctx = Map.fromList vts <> ctx0

buildDefaultCase ::
  (Var v) =>
  DataSpec ->
  Bool ->
  Bool ->
  Ctx v ->
  PatternMatrix v ->
  [MatchCase () (Term v)]
buildDefaultCase spec _eff needed ctx pm
  | needed = [MatchCase (Unbound ()) Nothing $ compile spec ctx pm]
  | otherwise = []

mkRow ::
  (Var v) =>
  v ->
  MatchCase a (Term v) ->
  State Word64 (PatternRow v)
mkRow sv (MatchCase (normalizeSeqP -> p0) g0 (AbsN' vs b)) =
  state $ \w -> case runState (prepareAs p0 sv) (w, vs, mempty) of
    (p, (w, [], rn)) ->
      (,w) $
        PR
          (filter refutable [p])
          (renames rn <$> g)
          (renames rn b)
    _ -> internalBug "mkRow: not all variables used"
  where
    g = case g0 of
      Just (AbsN' us g)
        | us == vs -> Just g
        | length us == length vs ->
            Just $ renames (Map.fromList (zip us vs)) g
        | otherwise ->
            internalBug "mkRow: guard variables do not match body"
      Nothing -> Nothing

initialize ::
  (Var v) =>
  PType ->
  Term v ->
  [MatchCase () (Term v)] ->
  (Maybe v, (v, PType), PatternMatrix v)
initialize r sc cs =
  ( lv,
    (sv, r),
    PM $ evalState (traverse (mkRow sv) cs) 1
  )
  where
    (lv, sv)
      | Var' v <- sc = (Nothing, v)
      | pv <- freshenId 0 $ typed Pattern =
          (Just pv, pv)

splitPatterns :: (Var v) => DataSpec -> Term v -> Term v
splitPatterns spec0 = visitPure $ \case
  Match' sc0 cs0
    | ty <- determineType $ p <$> cs0,
      (lv, scrut, pm) <- initialize ty sc cs,
      body <- compile spec (uncurry Map.singleton scrut) pm ->
        Just $ case lv of
          Just v -> let1 False [(((), v), sc)] body
          _ -> body
    where
      sc = splitPatterns spec sc0
      cs = fmap (splitPatterns spec) <$> cs0
  _ -> Nothing
  where
    p (MatchCase pp _ _) = pp
    spec = Map.insert Rf.booleanRef (Right [0, 0]) spec0

builtinCase :: Set Reference
builtinCase =
  Set.fromList
    [ Rf.intRef,
      Rf.natRef,
      Rf.floatRef,
      Rf.textRef,
      Rf.charRef
    ]

determineType :: (Show a) => [P.Pattern a] -> PType
determineType = foldMap f
  where
    f (P.As _ p) = f p
    f P.Int {} = PData Rf.intRef
    f P.Nat {} = PData Rf.natRef
    f P.Float {} = PData Rf.floatRef
    f P.Boolean {} = PData Rf.booleanRef
    f P.Text {} = PData Rf.textRef
    f P.Char {} = PData Rf.charRef
    f P.SequenceLiteral {} = PData Rf.listRef
    f P.SequenceOp {} = PData Rf.listRef
    f (P.Constructor _ r _) = PData (r ^. ConstructorReference.reference_)
    f (P.EffectBind _ r _ _) = PReq $ Set.singleton (r ^. ConstructorReference.reference_)
    f P.EffectPure {} = PReq mempty
    f _ = Unknown
