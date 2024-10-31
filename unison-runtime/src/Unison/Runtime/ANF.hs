{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.ANF
  ( minimizeCyclesOrCrash,
    pattern TVar,
    pattern TLit,
    pattern TBLit,
    pattern TApp,
    pattern TApv,
    pattern TCom,
    pattern TCon,
    pattern TKon,
    pattern TReq,
    pattern TPrm,
    pattern TFOp,
    pattern THnd,
    pattern TLet,
    pattern TLetD,
    pattern TFrc,
    pattern TLets,
    pattern TName,
    pattern TBind,
    pattern TBinds,
    pattern TShift,
    pattern TMatch,
    CompileExn (..),
    internalBug,
    Mem (..),
    Lit (..),
    Cacheability (..),
    Direction (..),
    SuperNormal (..),
    SuperGroup (..),
    POp (..),
    FOp,
    close,
    saturate,
    float,
    floatGroup,
    lamLift,
    lamLiftGroup,
    litRef,
    inlineAlias,
    addDefaultCases,
    ANormalF (.., AApv, ACom, ACon, AKon, AReq, APrm, AFOp),
    ANormal,
    RTag,
    CTag,
    Tag (..),
    GroupRef (..),
    Code (..),
    UBValue,
    ValList,
    Value (..),
    Cont (..),
    BLit (..),
    packTags,
    unpackTags,
    maskTags,
    ANFM,
    Branched (.., MatchDataCover),
    Func (..),
    SGEqv (..),
    equivocate,
    superNormalize,
    anfTerm,
    codeGroup,
    valueTermLinks,
    valueLinks,
    groupTermLinks,
    foldGroup,
    foldGroupLinks,
    overGroup,
    overGroupLinks,
    traverseGroup,
    traverseGroupLinks,
    normalLinks,
    prettyGroup,
    prettySuperNormal,
    prettyANF,
  )
where

import Control.Exception (throw)
import Control.Lens (foldMapOf, folded, snoc, unsnoc, _Right)
import Control.Monad.Reader (ReaderT (..), ask, local)
import Control.Monad.State (MonadState (..), State, gets, modify, runState)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Functor.Compose (Compose (..))
import Data.List hiding (and, or)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Data.Text
import GHC.Stack (CallStack, callStack)
import Unison.ABT qualified as ABT
import Unison.ABT.Normalized qualified as ABTN
import Unison.Blank (nameb)
import Unison.Builtin.Decls qualified as Ty
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.Hashing.V2.Convert (hashTermComponentsWithoutTypes)
import Unison.Pattern (SeqOp (..))
import Unison.Pattern qualified as P
import Unison.Prelude
import Unison.Reference (Id, Reference, Reference' (Builtin, DerivedId))
import Unison.Referent (Referent, pattern Con, pattern Ref)
import Unison.Runtime.Array qualified as PA
import Unison.Symbol (Symbol)
import Unison.Term hiding (List, Ref, Text, float, fresh, resolve)
import Unison.Type qualified as Ty
import Unison.Typechecker.Components (minimize')
import Unison.Util.Bytes (Bytes)
import Unison.Util.EnumContainers as EC
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Text qualified as Util.Text
import Unison.Var (Var, typed)
import Unison.Var qualified as Var
import Prelude hiding (abs, and, or, seq)
import Prelude qualified

-- For internal errors
data CompileExn = CE CallStack (Pretty.Pretty Pretty.ColorText)
  deriving (Show)

instance Exception CompileExn

internalBug :: (HasCallStack) => String -> a
internalBug = throw . CE callStack . Pretty.lit . fromString

closure :: (Var v) => Map v (Set v, Set v) -> Map v (Set v)
closure m0 = trace (snd <$> m0)
  where
    refs = fst <$> m0

    expand acc fvs rvs =
      fvs <> foldMap (\r -> Map.findWithDefault mempty r acc) rvs

    trace acc
      | acc == acc' = acc
      | otherwise = trace acc'
      where
        acc' = Map.intersectionWith (expand acc) acc refs

expandRec ::
  (Var v, Monoid a) =>
  Set v ->
  [(v, Term v a)] ->
  [(v, Term v a)]
expandRec keep vbs = mkSub <$> fvl
  where
    mkSub (v, fvs) = (v, apps' (var mempty v) (var mempty <$> fvs))

    fvl =
      Map.toList
        . fmap (Set.toList)
        . closure
        $ Set.partition (`Set.member` keep)
          . ABT.freeVars
          <$> Map.fromList vbs

expandSimple ::
  (Var v, Monoid a) =>
  Set v ->
  (v, Term v a) ->
  (v, Term v a)
expandSimple keep (v, bnd) = (v, apps' (var a v) evs)
  where
    a = ABT.annotation bnd
    fvs = ABT.freeVars bnd
    evs = map (var a) . Set.toList $ Set.difference fvs keep

abstract :: (Var v) => Set v -> Term v a -> Term v a
abstract keep bnd = lamWithoutBindingAnns a evs bnd
  where
    a = ABT.annotation bnd
    fvs = ABT.freeVars bnd
    evs = Set.toList $ Set.difference fvs keep

enclose ::
  (Var v, Monoid a) =>
  Set v ->
  (Set v -> Term v a -> Term v a) ->
  Term v a ->
  Maybe (Term v a)
enclose keep rec (LetRecNamedTop' top vbs bd) =
  Just $ letRec' top lvbs lbd
  where
    xpnd = expandRec keep' vbs
    keep' = Set.union keep . Set.fromList . map fst $ vbs
    lvbs =
      vbs
        <&> \(v, trm) ->
          (v, ABT.annotation trm, (rec keep' . abstract keep' . ABT.substs xpnd) trm)
    lbd = rec keep' . ABT.substs xpnd $ bd
-- will be lifted, so keep this variable
enclose keep rec (Let1NamedTop' top v b@(unAnn -> LamsNamed' vs bd) e) =
  Just . let1' top [(v, lamb)] . rec (Set.insert v keep) $
    ABT.subst v av e
  where
    (_, av) = expandSimple keep (v, b)
    keep' = Set.difference keep $ Set.fromList vs
    fvs = ABT.freeVars b
    evs = Set.toList $ Set.difference fvs keep
    a = ABT.annotation b
    lbody = rec keep' bd
    annotate tm
      | Ann' _ ty <- b = ann a tm ty
      | otherwise = tm
    lamb = lamWithoutBindingAnns a evs (annotate $ lamWithoutBindingAnns a vs lbody)
enclose keep rec t@(unLamsAnnot -> Just (vs0, mty, vs1, body)) =
  Just $ if null evs then lamb else apps' lamb $ map (var a) evs
  where
    -- remove shadowed variables
    keep' = Set.difference keep $ Set.fromList (vs0 ++ vs1)
    fvs = ABT.freeVars t
    evs = Set.toList $ Set.difference fvs keep
    a = ABT.annotation t
    lbody = rec keep' body
    annotate tm
      | Just ty <- mty = ann a tm ty
      | otherwise = tm
    lamb = lamWithoutBindingAnns a (evs ++ vs0) . annotate . lamWithoutBindingAnns a vs1 $ lbody
enclose keep rec t@(Handle' h body)
  | isStructured body =
      Just . handle (ABT.annotation t) (rec keep h) $ apps' lamb args
  where
    fvs = ABT.freeVars body
    evs = Set.toList $ Set.difference fvs keep
    a = ABT.annotation body
    lbody = rec keep body
    fv = Var.freshIn fvs $ typed Var.Eta
    args
      | null evs = [constructor a (ConstructorReference Ty.unitRef 0)]
      | otherwise = var a <$> evs
    lamb
      | null evs = lamWithoutBindingAnns a [fv] lbody
      | otherwise = lamWithoutBindingAnns a evs lbody
enclose keep rec t@(Match' s0 cs0) = Just $ match a s cs
  where
    a = ABT.annotation t
    s = rec keep s0
    cs = encloseCase a keep rec <$> cs0
enclose _ _ _ = Nothing

encloseCase ::
  (Var v, Monoid a) =>
  a ->
  Set v ->
  (Set v -> Term v a -> Term v a) ->
  MatchCase a (Term v a) ->
  MatchCase a (Term v a)
encloseCase a keep rec0 (MatchCase pats guard body) =
  MatchCase pats (rec <$> guard) (rec body)
  where
    rec (ABT.AbsN' vs bd) =
      ABT.absChain' ((,) a <$> vs) $
        rec0 (keep `Set.difference` Set.fromList vs) bd

newtype Prefix v x = Pfx (Map v [v]) deriving (Show)

instance Functor (Prefix v) where
  fmap _ (Pfx m) = Pfx m

instance (Ord v) => Applicative (Prefix v) where
  pure _ = Pfx Map.empty
  Pfx ml <*> Pfx mr = Pfx $ Map.unionWith common ml mr

common :: (Eq v) => [v] -> [v] -> [v]
common (u : us) (v : vs)
  | u == v = u : common us vs
common _ _ = []

splitPfx :: v -> [Term v a] -> (Prefix v x, [Term v a])
splitPfx v = first (Pfx . Map.singleton v) . split
  where
    split (Var' u : as) = first (u :) $ split as
    split rest = ([], rest)

-- Finds the common variable prefixes that function variables are
-- applied to, so that they can be reduced.
prefix :: (Ord v) => Term v a -> Prefix v (Term v a)
prefix = ABT.visit \case
  Apps' (Var' u) as -> case splitPfx u as of
    (pf, rest) -> Just $ traverse prefix rest *> pf
  Var' u -> Just . Pfx $ Map.singleton u []
  _ -> Nothing

appPfx :: (Ord v) => Prefix v a -> v -> [v] -> [v]
appPfx (Pfx m) v = maybe (const []) common $ Map.lookup v m

-- Rewrites a term by dropping the first n arguments to every
-- application of `v`. This just assumes such a thing makes sense, as
-- in `beta`, where we've calculated how many arguments to drop by
-- looking at every occurrence of `v`.
dropPrefix :: (Ord v) => (Semigroup a) => v -> Int -> Term v a -> Term v a
dropPrefix _ 0 = id
dropPrefix v n = ABT.visitPure rw
  where
    rw (Apps' f@(Var' u) as)
      | v == u = Just (apps' (var (ABT.annotation f) u) (drop n as))
    rw _ = Nothing

dropPrefixes ::
  (Ord v) => (Semigroup a) => Map v Int -> Term v a -> Term v a
dropPrefixes m = ABT.visitPure rw
  where
    rw (Apps' f@(Var' u) as)
      | Just n <- Map.lookup u m =
          Just (apps' (var (ABT.annotation f) u) (drop n as))
    rw _ = Nothing

-- Performs opposite transformations to those in enclose. Named after
-- the lambda case, which is beta reduction.
beta :: (Var v) => (Monoid a) => (Term v a -> Term v a) -> Term v a -> Maybe (Term v a)
beta rec (LetRecNamedTop' top (fmap (fmap rec) -> vbs) (rec -> bd)) =
  Just $ letRec' top lvbs lbd
  where
    -- Avoid completely reducing a lambda expression, because recursive
    -- lets must be guarded.
    args (v, LamsNamed' vs Ann' {}) = (v, vs)
    args (v, LamsNamed' vs _) = (v, init vs)
    args (v, _) = (v, [])

    Pfx m0 = traverse_ (prefix . snd) vbs *> prefix bd

    f ls rs = case common ls rs of
      [] -> Nothing
      vs -> Just vs

    m = Map.map length $ Map.differenceWith f (Map.fromList $ map args vbs) m0
    lvbs =
      vbs <&> \(v, b0) -> (v,ABT.annotation b0,) $ case b0 of
        LamsNamed' vs b
          | Just n <- Map.lookup v m ->
              lamWithoutBindingAnns (ABT.annotation b0) (drop n vs) (dropPrefixes m b)
        -- shouldn't happen
        b -> dropPrefixes m b

    lbd = dropPrefixes m bd
beta rec (Let1NamedTop' top v l@(LamsNamed' vs bd) (rec -> e))
  | n > 0 = Just $ let1' top [(v, lamb)] (dropPrefix v n e)
  | otherwise = Nothing
  where
    lamb = lamWithoutBindingAnns al (drop n vs) (bd)
    al = ABT.annotation l
    -- Calculate a maximum number of arguments to drop.
    -- Enclosing doesn't create let-bound lambdas, so we
    -- should never reduce a lambda to a non-lambda, as that
    -- could affect evaluation order.
    m
      | Ann' _ _ <- bd = length vs
      | otherwise = length vs - 1
    n = min m . length $ appPfx (prefix e) v vs
beta rec (Apps' l@(LamsNamed' vs body) as)
  | n <- matchVars 0 vs as,
    n > 0 =
      Just $ apps' (lamWithoutBindingAnns al (drop n vs) (rec body)) (drop n as)
  | otherwise = Nothing
  where
    al = ABT.annotation l
    matchVars !n (u : us) (Var' v : as) | u == v = matchVars (1 + n) us as
    matchVars n _ _ = n
beta _ _ = Nothing

isStructured :: (Var v) => Term v a -> Bool
isStructured (Var' _) = False
isStructured (Lam' _) = False
isStructured (Nat' _) = False
isStructured (Int' _) = False
isStructured (Float' _) = False
isStructured (Text' _) = False
isStructured (Char' _) = False
isStructured (Constructor' _) = False
isStructured (Apps' Constructor' {} args) = any isStructured args
isStructured (If' b t f) =
  isStructured b || isStructured t || isStructured f
isStructured (And' l r) = isStructured l || isStructured r
isStructured (Or' l r) = isStructured l || isStructured r
isStructured _ = True

close :: (Var v, Monoid a) => Set v -> Term v a -> Term v a
close keep tm = ABT.visitPure (enclose keep close) tm

-- Attempts to undo what was done in `close`. Useful for decompiling.
open :: (Var v, Monoid a) => Term v a -> Term v a
open x = ABT.visitPure (beta open) x

type FloatM v a r = State (Set v, [(v, Term v a)], [(v, Term v a)]) r

freshFloat :: (Var v) => Set v -> v -> v
freshFloat avoid (Var.freshIn avoid -> v0) =
  case Var.typeOf v0 of
    Var.User nm
      | v <- typed (Var.User $ nm <> w),
        v `Set.notMember` avoid ->
          v
      | otherwise ->
          freshFloat (Set.insert v0 avoid) v0
    _ -> v0
  where
    w = Data.Text.pack . show $ Var.freshId v0

groupFloater ::
  (Var v, Monoid a) =>
  (Term v a -> FloatM v a (Term v a)) ->
  [(v, Term v a)] ->
  FloatM v a (Map v v)
groupFloater rec vbs = do
  cvs <- gets (\(vs, _, _) -> vs)
  let shadows =
        [ (v, freshFloat cvs v)
          | (v, _) <- vbs,
            Set.member v cvs
        ]
      shadowMap = Map.fromList shadows
      rn v = Map.findWithDefault v v shadowMap
      shvs = Set.fromList $ map (rn . fst) vbs
  modify $ \(cvs, ctx, dcmp) -> (cvs <> shvs, ctx, dcmp)
  fvbs <- traverse (\(v, b) -> (,) (rn v) <$> rec' (ABT.renames shadowMap b)) vbs
  let dvbs = fmap (\(v, b) -> (rn v, deannotate b)) vbs
  modify $ \(vs, ctx, dcmp) -> (vs, ctx ++ fvbs, dcmp <> dvbs)
  pure shadowMap
  where
    rec' b
      | Just (vs0, mty, vs1, bd) <- unLamsAnnot b =
          lamWithoutBindingAnns a vs0 . maybe id (flip $ ann a) mty . lamWithoutBindingAnns a vs1 <$> rec bd
      where
        a = ABT.annotation b
    rec' b = rec b

letFloater ::
  (Var v, Monoid a) =>
  (Term v a -> FloatM v a (Term v a)) ->
  [(v, Term v a)] ->
  Term v a ->
  FloatM v a (Term v a)
letFloater rec vbs e = do
  shadowMap <- groupFloater rec vbs
  pure $ ABT.renames shadowMap e

lamFloater ::
  (Var v, Monoid a) =>
  Bool ->
  Term v a ->
  Maybe v ->
  a ->
  [v] ->
  Term v a ->
  FloatM v a v
lamFloater closed tm mv a vs bd =
  state $ \trip@(cvs, ctx, dcmp) -> case find p ctx of
    Just (v, _) -> (v, trip)
    Nothing ->
      let v = ABT.freshIn cvs $ fromMaybe (typed Var.Float) mv
       in ( v,
            ( Set.insert v cvs,
              ctx <> [(v, lamWithoutBindingAnns a vs bd)],
              floatDecomp closed v tm dcmp
            )
          )
  where
    tgt = unannotate (lamWithoutBindingAnns a vs bd)
    p (_, flam) = unannotate flam == tgt

floatDecomp ::
  Bool -> v -> Term v a -> [(v, Term v a)] -> [(v, Term v a)]
floatDecomp True v b dcmp = (v, b) : dcmp
floatDecomp False _ _ dcmp = dcmp

floater ::
  (Var v, Monoid a) =>
  Bool ->
  (Term v a -> FloatM v a (Term v a)) ->
  Term v a ->
  Maybe (FloatM v a (Term v a))
floater top rec tm0@(Ann' tm ty) =
  (fmap . fmap) (\tm -> ann a tm ty) (floater top rec tm)
  where
    a = ABT.annotation tm0
floater top rec (LetRecNamed' vbs e) =
  Just $
    letFloater rec vbs e >>= \case
      lm@(LamsNamed' vs bd) | top -> lamWithoutBindingAnns a vs <$> rec bd
        where
          a = ABT.annotation lm
      tm -> rec tm
floater _ rec (Let1Named' v b e)
  | Just (vs0, _, vs1, bd) <- unLamsAnnot b =
      Just $
        rec bd
          >>= lamFloater True b (Just v) a (vs0 ++ vs1)
          >>= \lv -> rec $ ABT.renames (Map.singleton v lv) e
  where
    a = ABT.annotation b
floater top rec tm@(LamsNamed' vs bd)
  | top = Just $ lamWithoutBindingAnns a vs <$> rec bd
  | otherwise = Just $ do
      bd <- rec bd
      lv <- lamFloater True tm Nothing a vs bd
      pure $ var a lv
  where
    a = ABT.annotation tm
floater _ _ _ = Nothing

postFloat ::
  (Var v) =>
  (Monoid a) =>
  Map v Reference ->
  (Set v, [(v, Term v a)], [(v, Term v a)]) ->
  ( [(v, Term v a)],
    [(v, Id)],
    [(Reference, Term v a)],
    [(Reference, Term v a)]
  )
postFloat orig (_, bs, dcmp) =
  ( subs,
    subvs,
    fmap (first DerivedId) tops,
    dcmp >>= \(v, tm) ->
      let stm = open $ ABT.substs dsubs tm
       in (subm Map.! v, stm) : [(r, stm) | Just r <- [Map.lookup v orig]]
  )
  where
    m =
      fmap (fmap deannotate)
        . hashTermComponentsWithoutTypes
        . Map.fromList
        $ bs
    trips = Map.toList m
    f (v, (id, tm)) = ((v, id), (v, idtm), (id, tm))
      where
        idtm = ref (ABT.annotation tm) (DerivedId id)
    (subvs, subs, tops) = unzip3 $ map f trips
    subm = fmap DerivedId (Map.fromList subvs)
    dsubs = Map.toList $ Map.map (ref mempty) orig <> Map.fromList subs

float ::
  (Var v) =>
  (Monoid a) =>
  Map v Reference ->
  Term v a ->
  (Term v a, Map Reference Reference, [(Reference, Term v a)], [(Reference, Term v a)])
float orig tm = case runState go0 (Set.empty, [], []) of
  (bd, st) -> case postFloat orig st of
    (subs, subvs, tops, dcmp) ->
      ( letRec' True [] . ABT.substs subs . deannotate $ bd,
        Map.fromList . mapMaybe f $ subvs,
        tops,
        dcmp
      )
  where
    f (v, i) = (,DerivedId i) <$> Map.lookup v orig
    go0 = fromMaybe (go tm) (floater True go tm)
    go = ABT.visit $ floater False go

floatGroup ::
  (Var v) =>
  (Monoid a) =>
  Map v Reference ->
  [(v, Term v a)] ->
  ([(v, Id)], [(Reference, Term v a)], [(Reference, Term v a)])
floatGroup orig grp = case runState go0 (Set.empty, [], []) of
  (_, st) -> case postFloat orig st of
    (_, subvs, tops, dcmp) -> (subvs, tops, dcmp)
  where
    go = ABT.visit $ floater False go
    go0 = groupFloater go grp

unAnn :: Term v a -> Term v a
unAnn (Ann' tm _) = tm
unAnn tm = tm

unLamsAnnot :: Term v a -> Maybe ([v], Maybe (Ty.Type v a), [v], Term v a)
unLamsAnnot tm0
  | null vs0, null vs1 = Nothing
  | otherwise = Just (vs0, mty, vs1, bd)
  where
    (vs0, bd0)
      | LamsNamed' vs bd <- tm0 = (vs, bd)
      | otherwise = ([], tm0)
    (mty, bd1)
      | Ann' bd ty <- bd0 = (Just ty, bd)
      | otherwise = (Nothing, bd0)
    (vs1, bd)
      | LamsNamed' vs bd <- bd1 = (vs, bd)
      | otherwise = ([], bd1)

deannotate :: (Var v) => Term v a -> Term v a
deannotate = ABT.visitPure $ \case
  Ann' c _ -> Just $ deannotate c
  _ -> Nothing

lamLift ::
  (Var v) =>
  (Monoid a) =>
  Map v Reference ->
  Term v a ->
  (Term v a, Map Reference Reference, [(Reference, Term v a)], [(Reference, Term v a)])
lamLift orig = float orig . close Set.empty

lamLiftGroup ::
  (Var v) =>
  (Monoid a) =>
  Map v Reference ->
  [(v, Term v a)] ->
  ([(v, Id)], [(Reference, Term v a)], [(Reference, Term v a)])
lamLiftGroup orig gr = floatGroup orig . (fmap . fmap) (close keep) $ gr
  where
    keep = Set.fromList $ map fst gr

saturate ::
  (Var v, Monoid a) =>
  Map ConstructorReference Int ->
  Term v a ->
  Term v a
saturate dat = ABT.visitPure $ \case
  Apps' f@(Constructor' r) args -> sat r f args
  Apps' f@(Request' r) args -> sat r f args
  f@(Constructor' r) -> sat r f []
  f@(Request' r) -> sat r f []
  _ -> Nothing
  where
    frsh avoid _ =
      let v = Var.freshIn avoid $ typed Var.Eta
       in (Set.insert v avoid, v)
    sat r f args = case Map.lookup r dat of
      Just n
        | m < n,
          vs <- snd $ mapAccumL frsh fvs [1 .. n - m],
          nargs <- var mempty <$> vs ->
            Just . lamWithoutBindingAnns mempty vs . apps' f $ args' ++ nargs
        | m > n,
          (sargs, eargs) <- splitAt n args',
          sv <- Var.freshIn fvs $ typed Var.Eta ->
            Just
              . let1' False [(sv, apps' f sargs)]
              $ apps' (var mempty sv) eargs
      _ -> Just (apps' f args')
      where
        m = length args
        fvs = foldMap freeVars args
        args' = saturate dat <$> args

addDefaultCases :: (Var v) => (Monoid a) => Text -> Term v a -> Term v a
addDefaultCases = ABT.visitPure . defaultCaseVisitor

defaultCaseVisitor ::
  (Var v) => (Monoid a) => Text -> Term v a -> Maybe (Term v a)
defaultCaseVisitor func m@(Match' scrut cases)
  | scrut <- addDefaultCases func scrut,
    cases <- fmap (addDefaultCases func) <$> cases =
      Just $ match a scrut (cases ++ [dflt])
  where
    a = ABT.annotation m
    v = Var.freshIn mempty $ typed Var.Blank
    txt = "pattern match failure in function `" <> func <> "`"
    msg = text a txt
    bu = ref a (Builtin "bug")
    dflt =
      MatchCase (P.Var a) Nothing
        . ABT.abs' a v
        $ apps bu [(a, Ty.tupleTerm [msg, var a v])]
defaultCaseVisitor _ _ = Nothing

inlineAlias :: (Var v) => (Monoid a) => Term v a -> Term v a
inlineAlias = ABT.visitPure $ \case
  Let1Named' v b@(Var' _) e -> Just . inlineAlias $ ABT.subst v b e
  _ -> Nothing

minimizeCyclesOrCrash :: (Var v) => Term v a -> Term v a
minimizeCyclesOrCrash t = case minimize' t of
  Right t -> t
  Left e ->
    internalBug $
      "tried to minimize let rec with duplicate definitions: "
        ++ show (fst <$> toList e)

data Mem = UN | BX deriving (Eq, Ord, Show, Enum)

-- Context entries with evaluation strategy
data CTE v s
  = ST (Direction Word16) [v] [Mem] s
  | LZ v (Either Reference v) [v]
  deriving (Show)

pattern ST1 :: Direction Word16 -> v -> Mem -> s -> CTE v s
pattern ST1 d v m s = ST d [v] [m] s

-- All variables, both bound and free occurring in a CTE. This is
-- useful for avoiding both free and bound variables when
-- freshening.
cteVars :: (Ord v) => Cte v -> Set v
cteVars (ST _ vs _ e) = Set.fromList vs `Set.union` ABTN.freeVars e
cteVars (LZ v r as) = Set.fromList (either (const id) (:) r $ v : as)

data ANormalF v e
  = ALet (Direction Word16) [Mem] e e
  | AName (Either Reference v) [v] e
  | ALit Lit
  | ABLit Lit -- direct boxed literal
  | AMatch v (Branched e)
  | AShift Reference e
  | AHnd [Reference] v e
  | AApp (Func v) [v]
  | AFrc v
  | AVar v
  deriving (Show, Eq)

-- Types representing components that will go into the runtime tag of
-- a data type value. RTags correspond to references, while CTags
-- correspond to constructors.
newtype RTag = RTag Word64
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (EC.EnumKey)

newtype CTag = CTag Word16
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (EC.EnumKey)

class Tag t where rawTag :: t -> Word64

instance Tag RTag where rawTag (RTag w) = w

instance Tag CTag where rawTag (CTag w) = fromIntegral w

packTags :: RTag -> CTag -> Word64
packTags (RTag rt) (CTag ct) = ri .|. ci
  where
    ri = rt `shiftL` 16
    ci = fromIntegral ct

unpackTags :: Word64 -> (RTag, CTag)
unpackTags w = (RTag $ w `shiftR` 16, CTag . fromIntegral $ w .&. 0xFFFF)

-- Masks a packed tag to extract just the constructor tag portion
maskTags :: Word64 -> Word64
maskTags w = w .&. 0xFFFF

ensureRTag :: (Ord n, Show n, Num n) => String -> n -> r -> r
ensureRTag s n x
  | n > 0xFFFFFFFFFFFF =
      internalBug $ s ++ "@RTag: too large: " ++ show n
  | otherwise = x

ensureCTag :: (Ord n, Show n, Num n) => String -> n -> r -> r
ensureCTag s n x
  | n > 0xFFFF =
      internalBug $ s ++ "@CTag: too large: " ++ show n
  | otherwise = x

instance Enum RTag where
  toEnum i = ensureRTag "toEnum" i . RTag $ toEnum i
  fromEnum (RTag w) = fromEnum w

instance Enum CTag where
  toEnum i = ensureCTag "toEnum" i . CTag $ toEnum i
  fromEnum (CTag w) = fromEnum w

instance Num RTag where
  fromInteger i = ensureRTag "fromInteger" i . RTag $ fromInteger i
  (+) = internalBug "RTag: +"
  (*) = internalBug "RTag: *"
  abs = internalBug "RTag: abs"
  signum = internalBug "RTag: signum"
  negate = internalBug "RTag: negate"

instance Num CTag where
  fromInteger i = ensureCTag "fromInteger" i . CTag $ fromInteger i
  (+) = internalBug "CTag: +"
  (*) = internalBug "CTag: *"
  abs = internalBug "CTag: abs"
  signum = internalBug "CTag: signum"
  negate = internalBug "CTag: negate"

instance Functor (ANormalF v) where
  fmap _ (AVar v) = AVar v
  fmap _ (ALit l) = ALit l
  fmap _ (ABLit l) = ABLit l
  fmap f (ALet d m bn bo) = ALet d m (f bn) (f bo)
  fmap f (AName n as bo) = AName n as $ f bo
  fmap f (AMatch v br) = AMatch v $ f <$> br
  fmap f (AHnd rs h e) = AHnd rs h $ f e
  fmap f (AShift i e) = AShift i $ f e
  fmap _ (AFrc v) = AFrc v
  fmap _ (AApp f args) = AApp f args

instance Bifunctor ANormalF where
  bimap f _ (AVar v) = AVar (f v)
  bimap _ _ (ALit l) = ALit l
  bimap _ _ (ABLit l) = ABLit l
  bimap _ g (ALet d m bn bo) = ALet d m (g bn) (g bo)
  bimap f g (AName n as bo) = AName (f <$> n) (f <$> as) $ g bo
  bimap f g (AMatch v br) = AMatch (f v) $ fmap g br
  bimap f g (AHnd rs v e) = AHnd rs (f v) $ g e
  bimap _ g (AShift i e) = AShift i $ g e
  bimap f _ (AFrc v) = AFrc (f v)
  bimap f _ (AApp fu args) = AApp (fmap f fu) $ fmap f args

instance Bifoldable ANormalF where
  bifoldMap f _ (AVar v) = f v
  bifoldMap _ _ (ALit _) = mempty
  bifoldMap _ _ (ABLit _) = mempty
  bifoldMap _ g (ALet _ _ b e) = g b <> g e
  bifoldMap f g (AName n as e) = foldMap f n <> foldMap f as <> g e
  bifoldMap f g (AMatch v br) = f v <> foldMap g br
  bifoldMap f g (AHnd _ h e) = f h <> g e
  bifoldMap _ g (AShift _ e) = g e
  bifoldMap f _ (AFrc v) = f v
  bifoldMap f _ (AApp func args) = foldMap f func <> foldMap f args

instance ABTN.Align ANormalF where
  align f _ (AVar u) (AVar v) = Just $ AVar <$> f u v
  align _ _ (ALit l) (ALit r)
    | l == r = Just $ pure (ALit l)
  align _ _ (ABLit l) (ABLit r)
    | l == r = Just $ pure (ABLit l)
  align _ g (ALet dl ccl bl el) (ALet dr ccr br er)
    | dl == dr,
      ccl == ccr =
        Just $ ALet dl ccl <$> g bl br <*> g el er
  align f g (AName hl asl el) (AName hr asr er)
    | length asl == length asr,
      Just hs <- alignEither f hl hr =
        Just $
          AName
            <$> hs
            <*> traverse (uncurry f) (zip asl asr)
            <*> g el er
  align f g (AMatch vl bsl) (AMatch vr bsr)
    | Just bss <- alignBranch g bsl bsr =
        Just $ AMatch <$> f vl vr <*> bss
  align f g (AHnd rl hl bl) (AHnd rr hr br)
    | rl == rr = Just $ AHnd rl <$> f hl hr <*> g bl br
  align _ g (AShift rl bl) (AShift rr br)
    | rl == rr = Just $ AShift rl <$> g bl br
  align f _ (AFrc u) (AFrc v) = Just $ AFrc <$> f u v
  align f _ (AApp hl asl) (AApp hr asr)
    | Just hs <- alignFunc f hl hr,
      length asl == length asr =
        Just $ AApp <$> hs <*> traverse (uncurry f) (zip asl asr)
  align _ _ _ _ = Nothing

alignEither ::
  (Applicative f) =>
  (l -> r -> f s) ->
  Either Reference l ->
  Either Reference r ->
  Maybe (f (Either Reference s))
alignEither _ (Left rl) (Left rr) | rl == rr = Just . pure $ Left rl
alignEither f (Right u) (Right v) = Just $ Right <$> f u v
alignEither _ _ _ = Nothing

alignMaybe ::
  (Applicative f) =>
  (l -> r -> f s) ->
  Maybe l ->
  Maybe r ->
  Maybe (f (Maybe s))
alignMaybe f (Just l) (Just r) = Just $ Just <$> f l r
alignMaybe _ Nothing Nothing = Just (pure Nothing)
alignMaybe _ _ _ = Nothing

alignFunc ::
  (Applicative f) =>
  (vl -> vr -> f vs) ->
  Func vl ->
  Func vr ->
  Maybe (f (Func vs))
alignFunc f (FVar u) (FVar v) = Just $ FVar <$> f u v
alignFunc _ (FComb rl) (FComb rr) | rl == rr = Just . pure $ FComb rl
alignFunc f (FCont u) (FCont v) = Just $ FCont <$> f u v
alignFunc _ (FCon rl tl) (FCon rr tr)
  | rl == rr, tl == tr = Just . pure $ FCon rl tl
alignFunc _ (FReq rl tl) (FReq rr tr)
  | rl == rr, tl == tr = Just . pure $ FReq rl tl
alignFunc _ (FPrim ol) (FPrim or)
  | ol == or = Just . pure $ FPrim ol
alignFunc _ _ _ = Nothing

alignBranch ::
  (Applicative f) =>
  (el -> er -> f es) ->
  Branched el ->
  Branched er ->
  Maybe (f (Branched es))
alignBranch _ MatchEmpty MatchEmpty = Just $ pure MatchEmpty
alignBranch f (MatchIntegral bl dl) (MatchIntegral br dr)
  | keysSet bl == keysSet br,
    Just ds <- alignMaybe f dl dr =
      Just $
        MatchIntegral
          <$> interverse f bl br
          <*> ds
alignBranch f (MatchText bl dl) (MatchText br dr)
  | Map.keysSet bl == Map.keysSet br,
    Just ds <- alignMaybe f dl dr =
      Just $
        MatchText
          <$> traverse id (Map.intersectionWith f bl br)
          <*> ds
alignBranch f (MatchRequest bl pl) (MatchRequest br pr)
  | Map.keysSet bl == Map.keysSet br,
    all p (Map.keysSet bl) =
      Just $
        MatchRequest
          <$> traverse id (Map.intersectionWith (interverse (alignCCs f)) bl br)
          <*> f pl pr
  where
    p r = keysSet hsl == keysSet hsr && all q (keys hsl)
      where
        hsl = bl Map.! r
        hsr = br Map.! r
        q t = fst (hsl ! t) == fst (hsr ! t)
alignBranch f (MatchData rfl bl dl) (MatchData rfr br dr)
  | rfl == rfr,
    keysSet bl == keysSet br,
    all (\t -> fst (bl ! t) == fst (br ! t)) (keys bl),
    Just ds <- alignMaybe f dl dr =
      Just $ MatchData rfl <$> interverse (alignCCs f) bl br <*> ds
alignBranch f (MatchSum bl) (MatchSum br)
  | keysSet bl == keysSet br,
    all (\w -> fst (bl ! w) == fst (br ! w)) (keys bl) =
      Just $ MatchSum <$> interverse (alignCCs f) bl br
alignBranch f (MatchNumeric rl bl dl) (MatchNumeric rr br dr)
  | rl == rr,
    keysSet bl == keysSet br,
    Just ds <- alignMaybe f dl dr =
      Just $
        MatchNumeric rl
          <$> interverse f bl br
          <*> ds
alignBranch _ _ _ = Nothing

alignCCs :: (Functor f) => (l -> r -> f s) -> (a, l) -> (a, r) -> f (a, s)
alignCCs f (ccs, l) (_, r) = (,) ccs <$> f l r

matchLit :: Term v a -> Maybe Lit
matchLit (Int' i) = Just $ I i
matchLit (Nat' n) = Just $ N n
matchLit (Float' f) = Just $ F f
matchLit (Text' t) = Just $ T (Util.Text.fromText t)
matchLit (Char' c) = Just $ C c
matchLit _ = Nothing

pattern TLet ::
  (ABT.Var v) =>
  Direction Word16 ->
  v ->
  Mem ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern TLet d v m bn bo = ABTN.TTm (ALet d [m] bn (ABTN.TAbs v bo))

pattern TLetD ::
  (ABT.Var v) =>
  v ->
  Mem ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern TLetD v m bn bo = ABTN.TTm (ALet Direct [m] bn (ABTN.TAbs v bo))

pattern TLets ::
  (ABT.Var v) =>
  Direction Word16 ->
  [v] ->
  [Mem] ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern TLets d vs ms bn bo = ABTN.TTm (ALet d ms bn (ABTN.TAbss vs bo))

pattern TName ::
  (ABT.Var v) =>
  v ->
  Either Reference v ->
  [v] ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern TName v f as bo = ABTN.TTm (AName f as (ABTN.TAbs v bo))

pattern Lit' :: Lit -> Term v a
pattern Lit' l <- (matchLit -> Just l)

pattern TLit ::
  (ABT.Var v) =>
  Lit ->
  ABTN.Term ANormalF v
pattern TLit l = ABTN.TTm (ALit l)

pattern TBLit ::
  (ABT.Var v) =>
  Lit ->
  ABTN.Term ANormalF v
pattern TBLit l = ABTN.TTm (ABLit l)

pattern TApp ::
  (ABT.Var v) =>
  Func v ->
  [v] ->
  ABTN.Term ANormalF v
pattern TApp f args = ABTN.TTm (AApp f args)

pattern AApv :: v -> [v] -> ANormalF v e
pattern AApv v args = AApp (FVar v) args

pattern TApv ::
  (ABT.Var v) =>
  v ->
  [v] ->
  ABTN.Term ANormalF v
pattern TApv v args = TApp (FVar v) args

pattern ACom :: Reference -> [v] -> ANormalF v e
pattern ACom r args = AApp (FComb r) args

pattern TCom ::
  (ABT.Var v) =>
  Reference ->
  [v] ->
  ABTN.Term ANormalF v
pattern TCom r args = TApp (FComb r) args

pattern ACon :: Reference -> CTag -> [v] -> ANormalF v e
pattern ACon r t args = AApp (FCon r t) args

pattern TCon ::
  (ABT.Var v) =>
  Reference ->
  CTag ->
  [v] ->
  ABTN.Term ANormalF v
pattern TCon r t args = TApp (FCon r t) args

pattern AKon :: v -> [v] -> ANormalF v e
pattern AKon v args = AApp (FCont v) args

pattern TKon ::
  (ABT.Var v) =>
  v ->
  [v] ->
  ABTN.Term ANormalF v
pattern TKon v args = TApp (FCont v) args

pattern AReq :: Reference -> CTag -> [v] -> ANormalF v e
pattern AReq r t args = AApp (FReq r t) args

pattern TReq ::
  (ABT.Var v) =>
  Reference ->
  CTag ->
  [v] ->
  ABTN.Term ANormalF v
pattern TReq r t args = TApp (FReq r t) args

pattern APrm :: POp -> [v] -> ANormalF v e
pattern APrm p args = AApp (FPrim (Left p)) args

pattern TPrm ::
  (ABT.Var v) =>
  POp ->
  [v] ->
  ABTN.Term ANormalF v
pattern TPrm p args = TApp (FPrim (Left p)) args

pattern AFOp :: FOp -> [v] -> ANormalF v e
pattern AFOp p args = AApp (FPrim (Right p)) args

pattern TFOp ::
  (ABT.Var v) =>
  FOp ->
  [v] ->
  ABTN.Term ANormalF v
pattern TFOp p args = TApp (FPrim (Right p)) args

pattern THnd ::
  (ABT.Var v) =>
  [Reference] ->
  v ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern THnd rs h b = ABTN.TTm (AHnd rs h b)

pattern TShift ::
  (ABT.Var v) =>
  Reference ->
  v ->
  ABTN.Term ANormalF v ->
  ABTN.Term ANormalF v
pattern TShift i v e = ABTN.TTm (AShift i (ABTN.TAbs v e))

pattern TMatch ::
  (ABT.Var v) =>
  v ->
  Branched (ABTN.Term ANormalF v) ->
  ABTN.Term ANormalF v
pattern TMatch v cs = ABTN.TTm (AMatch v cs)

pattern TFrc :: (ABT.Var v) => v -> ABTN.Term ANormalF v
pattern TFrc v = ABTN.TTm (AFrc v)

pattern TVar :: (ABT.Var v) => v -> ABTN.Term ANormalF v
pattern TVar v = ABTN.TTm (AVar v)

{-# COMPLETE TLet, TName, TVar, TApp, TFrc, TLit, THnd, TShift, TMatch #-}

{-# COMPLETE
  TLet,
  TName,
  TVar,
  TFrc,
  TApv,
  TCom,
  TCon,
  TKon,
  TReq,
  TPrm,
  TFOp,
  TLit,
  THnd,
  TShift,
  TMatch
  #-}

bind :: (Var v) => Cte v -> ANormal v -> ANormal v
bind (ST d us ms bu) = TLets d us ms bu
bind (LZ u f as) = TName u f as

unbind :: (Var v) => ANormal v -> Maybe (Cte v, ANormal v)
unbind (TLets d us ms bu bd) = Just (ST d us ms bu, bd)
unbind (TName u f as bd) = Just (LZ u f as, bd)
unbind _ = Nothing

unbinds :: (Var v) => ANormal v -> ([Cte v], ANormal v)
unbinds (TLets d us ms bu (unbinds -> (ctx, bd))) =
  (ST d us ms bu : ctx, bd)
unbinds (TName u f as (unbinds -> (ctx, bd))) = (LZ u f as : ctx, bd)
unbinds tm = ([], tm)

pattern TBind ::
  (Var v) =>
  Cte v ->
  ANormal v ->
  ANormal v
pattern TBind bn bd <-
  (unbind -> Just (bn, bd))
  where
    TBind bn bd = bind bn bd

pattern TBinds :: (Var v) => [Cte v] -> ANormal v -> ANormal v
pattern TBinds ctx bd <-
  (unbinds -> (ctx, bd))
  where
    TBinds ctx bd = foldr bind bd ctx

{-# COMPLETE TBinds #-}

data SeqEnd = SLeft | SRight
  deriving (Eq, Ord, Enum, Show)

-- Note: MatchNumeric is a new form for matching directly on boxed
-- numeric data. This leaves MatchIntegral around so that builtins can
-- continue to use it. But interchanged code can be free of unboxed
-- details.
data Branched e
  = MatchIntegral (EnumMap Word64 e) (Maybe e)
  | MatchText (Map.Map Util.Text.Text e) (Maybe e)
  | MatchRequest (Map Reference (EnumMap CTag ([Mem], e))) e
  | MatchEmpty
  | MatchData Reference (EnumMap CTag ([Mem], e)) (Maybe e)
  | MatchSum (EnumMap Word64 ([Mem], e))
  | MatchNumeric Reference (EnumMap Word64 e) (Maybe e)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- Data cases expected to cover all constructors
pattern MatchDataCover :: Reference -> EnumMap CTag ([Mem], e) -> Branched e
pattern MatchDataCover r m = MatchData r m Nothing

data BranchAccum v
  = AccumEmpty
  | AccumIntegral
      Reference
      (Maybe (ANormal v))
      (EnumMap Word64 (ANormal v))
  | AccumText
      (Maybe (ANormal v))
      (Map.Map Util.Text.Text (ANormal v))
  | AccumDefault (ANormal v)
  | AccumPure (ANormal v)
  | AccumRequest
      (Map Reference (EnumMap CTag ([Mem], ANormal v)))
      (Maybe (ANormal v))
  | AccumData
      Reference
      (Maybe (ANormal v))
      (EnumMap CTag ([Mem], ANormal v))
  | AccumSeqEmpty (ANormal v)
  | AccumSeqView
      SeqEnd
      (Maybe (ANormal v)) -- empty
      (ANormal v) -- cons/snoc
  | AccumSeqSplit
      SeqEnd
      Int -- split at
      (Maybe (ANormal v)) -- default
      (ANormal v) -- split

instance Semigroup (BranchAccum v) where
  AccumEmpty <> r = r
  l <> AccumEmpty = l
  AccumIntegral rl dl cl <> AccumIntegral rr dr cr
    | rl == rr = AccumIntegral rl (dl <|> dr) $ cl <> cr
  AccumText dl cl <> AccumText dr cr =
    AccumText (dl <|> dr) (cl <> cr)
  AccumData rl dl cl <> AccumData rr dr cr
    | rl == rr = AccumData rl (dl <|> dr) (cl <> cr)
  AccumDefault dl <> AccumIntegral r _ cr =
    AccumIntegral r (Just dl) cr
  AccumDefault dl <> AccumText _ cr =
    AccumText (Just dl) cr
  AccumDefault dl <> AccumData rr _ cr =
    AccumData rr (Just dl) cr
  AccumIntegral r dl cl <> AccumDefault dr =
    AccumIntegral r (dl <|> Just dr) cl
  AccumText dl cl <> AccumDefault dr =
    AccumText (dl <|> Just dr) cl
  AccumData rl dl cl <> AccumDefault dr =
    AccumData rl (dl <|> Just dr) cl
  l@(AccumPure _) <> AccumPure _ = l
  AccumPure dl <> AccumRequest hr _ = AccumRequest hr (Just dl)
  AccumRequest hl dl <> AccumPure dr =
    AccumRequest hl (dl <|> Just dr)
  AccumRequest hl dl <> AccumRequest hr dr =
    AccumRequest hm $ dl <|> dr
    where
      hm = Map.unionWith (<>) hl hr
  l@(AccumSeqEmpty _) <> AccumSeqEmpty _ = l
  AccumSeqEmpty eml <> AccumSeqView er _ cnr =
    AccumSeqView er (Just eml) cnr
  AccumSeqView el eml cnl <> AccumSeqEmpty emr =
    AccumSeqView el (eml <|> Just emr) cnl
  AccumSeqView el eml cnl <> AccumSeqView er emr _
    | el /= er =
        internalBug "AccumSeqView: trying to merge views of opposite ends"
    | otherwise = AccumSeqView el (eml <|> emr) cnl
  AccumSeqView _ _ _ <> AccumDefault _ =
    internalBug "seq views may not have defaults"
  AccumDefault _ <> AccumSeqView _ _ _ =
    internalBug "seq views may not have defaults"
  AccumSeqSplit el nl dl bl <> AccumSeqSplit er nr dr _
    | el /= er =
        internalBug
          "AccumSeqSplit: trying to merge splits at opposite ends"
    | nl /= nr =
        internalBug
          "AccumSeqSplit: trying to merge splits at different positions"
    | otherwise =
        AccumSeqSplit el nl (dl <|> dr) bl
  AccumDefault dl <> AccumSeqSplit er nr _ br =
    AccumSeqSplit er nr (Just dl) br
  AccumSeqSplit el nl dl bl <> AccumDefault dr =
    AccumSeqSplit el nl (dl <|> Just dr) bl
  _ <> _ = internalBug $ "cannot merge data cases for different types"

instance Monoid (BranchAccum e) where
  mempty = AccumEmpty

-- Foreign operation, indexed by words
type FOp = Word64

data Func v
  = -- variable
    FVar v
  | -- top-level combinator
    FComb !Reference
  | -- continuation jump
    FCont v
  | -- data constructor
    FCon !Reference !CTag
  | -- ability request
    FReq !Reference !CTag
  | -- prim op
    FPrim (Either POp FOp)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Lit
  = I Int64
  | N Word64
  | F Double
  | T Util.Text.Text
  | C Char
  | LM Referent
  | LY Reference
  deriving (Show, Eq)

litRef :: Lit -> Reference
litRef (I _) = Ty.intRef
litRef (N _) = Ty.natRef
litRef (F _) = Ty.floatRef
litRef (T _) = Ty.textRef
litRef (C _) = Ty.charRef
litRef (LM _) = Ty.termLinkRef
litRef (LY _) = Ty.typeLinkRef

-- Note: Enum/Bounded instances should only be used for things like
-- getting a list of all ops. Using auto-generated numberings for
-- serialization, for instance, could cause observable changes to
-- formats that we want to control and version.
data POp
  = -- Int
    ADDI
  | SUBI
  | MULI
  | DIVI -- +,-,*,/
  | SGNI
  | NEGI
  | MODI -- sgn,neg,mod
  | POWI
  | SHLI
  | SHRI -- pow,shiftl,shiftr
  | INCI
  | DECI
  | LEQI
  | EQLI -- inc,dec,<=,==
  -- Nat
  | ADDN
  | SUBN
  | MULN
  | DIVN -- +,-,*,/
  | MODN
  | TZRO
  | LZRO
  | POPC -- mod,trailing/leadingZeros,popCount
  | POWN
  | SHLN
  | SHRN -- pow,shiftl,shiftr
  | ANDN
  | IORN
  | XORN
  | COMN -- and,or,xor,complement
  | INCN
  | DECN
  | LEQN
  | EQLN -- inc,dec,<=,==
  -- Float
  | ADDF
  | SUBF
  | MULF
  | DIVF -- +,-,*,/
  | MINF
  | MAXF
  | LEQF
  | EQLF -- min,max,<=,==
  | POWF
  | EXPF
  | SQRT
  | LOGF -- pow,exp,sqrt,log
  | LOGB -- logBase
  | ABSF
  | CEIL
  | FLOR
  | TRNF -- abs,ceil,floor,truncate
  | RNDF -- round
  -- Trig
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
  | ATN2 -- atan2
  -- Text
  | CATT
  | TAKT
  | DRPT
  | SIZT -- ++,take,drop,size
  | IXOT -- indexOf
  | UCNS
  | USNC
  | EQLT
  | LEQT -- uncons,unsnoc,==,<=
  | PAKT
  | UPKT -- pack,unpack
  -- Sequence
  | CATS
  | TAKS
  | DRPS
  | SIZS -- ++,take,drop,size
  | CONS
  | SNOC
  | IDXS
  | BLDS -- cons,snoc,at,build
  | VWLS
  | VWRS
  | SPLL
  | SPLR -- viewl,viewr,splitl,splitr
  -- Bytes
  | PAKB
  | UPKB
  | TAKB
  | DRPB -- pack,unpack,take,drop
  | IXOB -- indexOf
  | IDXB
  | SIZB
  | FLTB
  | CATB -- index,size,flatten,append
  -- Conversion
  | ITOF
  | NTOF
  | ITOT
  | NTOT
  | TTOI
  | TTON
  | TTOF
  | FTOT
  | -- Concurrency
    FORK
  | -- Universal operations
    EQLU
  | CMPU
  | EROR
  | -- Code
    MISS
  | CACH
  | LKUP
  | LOAD -- isMissing,cache_,lookup,load
  | CVLD
  | SDBX -- validate, sandbox
  | VALU
  | TLTT -- value, Term.Link.toText
  -- Debug
  | PRNT
  | INFO
  | TRCE
  | DBTX
  | -- STM
    ATOM
  | TFRC -- try force
  | SDBL -- sandbox link list
  | SDBV -- sandbox check for Values
  deriving (Show, Eq, Ord, Enum, Bounded)

type ANormal = ABTN.Term ANormalF

type Cte v = CTE v (ANormal v)

type Ctx v = Directed () [Cte v]

data Direction a = Indirect a | Direct
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

directed :: (Foldable f) => f (Cte v) -> Directed () (f (Cte v))
directed x = (foldMap f x, x)
  where
    f (ST d _ _ _) = () <$ d
    f _ = Direct

instance (Semigroup a) => Semigroup (Direction a) where
  Indirect l <> Indirect r = Indirect $ l <> r
  Direct <> r = r
  l <> Direct = l

instance (Semigroup a) => Monoid (Direction a) where
  mempty = Direct

type Directed a = (,) (Direction a)

type DNormal v = Directed () (ANormal v)

-- Should be a completely closed term
data SuperNormal v = Lambda {conventions :: [Mem], bound :: ANormal v}
  deriving (Show, Eq)

data SuperGroup v = Rec
  { group :: [(v, SuperNormal v)],
    entry :: SuperNormal v
  }
  deriving (Show)

-- | Whether the evaluation of a given definition is cacheable or not.
-- i.e. it's a top-level pure value.
data Cacheability = Cacheable | Uncacheable
  deriving stock (Eq, Show)

instance (Var v) => Eq (SuperGroup v) where
  g0 == g1 | Left _ <- equivocate g0 g1 = False | otherwise = True

-- Failure modes for SuperGroup alpha equivalence test
data SGEqv v
  = -- mismatch number of definitions in group
    NumDefns (SuperGroup v) (SuperGroup v)
  | -- mismatched SuperNormal calling conventions
    DefnConventions (SuperNormal v) (SuperNormal v)
  | -- mismatched subterms in corresponding definition
    Subterms (ANormal v) (ANormal v)

-- Checks if two SuperGroups are equivalent up to renaming. The rest
-- of the structure must match on the nose. If the two groups are not
-- equivalent, an example of conflicting structure is returned.
equivocate ::
  (Var v) =>
  SuperGroup v ->
  SuperGroup v ->
  Either (SGEqv v) ()
equivocate g0@(Rec bs0 e0) g1@(Rec bs1 e1)
  | length bs0 == length bs1 =
      traverse_ eqvSN (zip ns0 ns1) *> eqvSN (e0, e1)
  | otherwise = Left $ NumDefns g0 g1
  where
    (vs0, ns0) = unzip bs0
    (vs1, ns1) = unzip bs1
    vm = Map.fromList (zip vs1 vs0)

    promote (Left (l, r)) = Left $ Subterms l r
    promote (Right v) = Right v

    eqvSN (Lambda ccs0 e0, Lambda ccs1 e1)
      | ccs0 == ccs1 = promote $ ABTN.alpha vm e0 e1
    eqvSN (n0, n1) = Left $ DefnConventions n0 n1

type ANFM v =
  ReaderT
    (Set v)
    (State (Word64, Word16, [(v, SuperNormal v)]))

type ANFD v = Compose (ANFM v) (Directed ())

data GroupRef = GR Reference Word64
  deriving (Show, Eq)

-- | A value which is either unboxed or boxed.
type UBValue = Either Word64 Value

-- | A list of either unboxed or boxed values.
-- Each slot is one of unboxed or boxed but not both.
type ValList = [UBValue]

data Value
  = Partial GroupRef ValList
  | Data Reference Word64 ValList
  | Cont ValList Cont
  | BLit BLit
  deriving (Show, Eq)

-- Since we can now track cacheability of supergroups, this type
-- pairs the two together. This is the type that should be used
-- as the representation of unison Code values rather than the
-- previous `SuperGroup Symbol`.
data Code = CodeRep (SuperGroup Symbol) Cacheability
  deriving (Show)

codeGroup :: Code -> SuperGroup Symbol
codeGroup (CodeRep sg _) = sg

instance Eq Code where
  CodeRep sg1 _ == CodeRep sg2 _ = sg1 == sg2

overGroup :: (SuperGroup Symbol -> SuperGroup Symbol) -> Code -> Code
overGroup f (CodeRep sg ch) = CodeRep (f sg) ch

foldGroup :: (Monoid m) => (SuperGroup Symbol -> m) -> Code -> m
foldGroup f (CodeRep sg _) = f sg

traverseGroup ::
  (Applicative f) =>
  (SuperGroup Symbol -> f (SuperGroup Symbol)) ->
  Code ->
  f Code
traverseGroup f (CodeRep sg ch) = flip CodeRep ch <$> f sg

data Cont
  = KE
  | Mark
      Word64 -- pending args
      [Reference]
      (Map Reference Value)
      Cont
  | Push
      Word64 -- Frame size
      Word64 -- Pending args
      GroupRef
      Cont
  deriving (Show, Eq)

data BLit
  = Text Util.Text.Text
  | List (Seq Value)
  | TmLink Referent
  | TyLink Reference
  | Bytes Bytes
  | Quote Value
  | Code Code
  | BArr PA.ByteArray
  | Pos Word64
  | Neg Word64
  | Char Char
  | Float Double
  | Arr (PA.Array Value)
  deriving (Show, Eq)

groupVars :: ANFM v (Set v)
groupVars = ask

bindLocal :: (Ord v) => [v] -> ANFM v r -> ANFM v r
bindLocal vs = local (Set.\\ Set.fromList vs)

freshANF :: (Var v) => Word64 -> v
freshANF fr = Var.freshenId fr $ typed Var.ANFBlank

fresh :: (Var v) => ANFM v v
fresh = state $ \(fr, bnd, cs) -> (freshANF fr, (fr + 1, bnd, cs))

contextualize :: (Var v) => DNormal v -> ANFM v (Ctx v, v)
contextualize (_, TVar cv) = do
  gvs <- groupVars
  if cv `Set.notMember` gvs
    then pure (pure [], cv)
    else do
      bv <- fresh
      d <- Indirect <$> binder
      pure (directed [ST1 d bv BX $ TApv cv []], bv)
contextualize (d0, tm) = do
  fv <- fresh
  d <- bindDirection d0
  pure ((d0, [ST1 d fv BX tm]), fv)

binder :: ANFM v Word16
binder = state $ \(fr, bnd, cs) -> (bnd, (fr, bnd + 1, cs))

bindDirection :: Direction a -> ANFM v (Direction Word16)
bindDirection = traverse (const binder)

record :: (Var v) => (v, SuperNormal v) -> ANFM v ()
record p = modify $ \(fr, bnd, to) -> (fr, bnd, p : to)

superNormalize :: (Var v) => Term v a -> SuperGroup v
superNormalize tm = Rec l c
  where
    (bs, e)
      | LetRecNamed' bs e <- tm = (bs, e)
      | otherwise = ([], tm)
    grp = Set.fromList $ fst <$> bs
    comp = traverse_ superBinding bs *> toSuperNormal e
    subc = runReaderT comp grp
    (c, (_, _, l)) = runState subc (0, 1, [])

superBinding :: (Var v) => (v, Term v a) -> ANFM v ()
superBinding (v, tm) = do
  nf <- toSuperNormal tm
  modify $ \(cvs, bnd, ctx) -> (cvs, bnd, (v, nf) : ctx)

toSuperNormal :: (Var v) => Term v a -> ANFM v (SuperNormal v)
toSuperNormal tm = do
  grp <- groupVars
  if not . Set.null . (Set.\\ grp) $ freeVars tm
    then internalBug $ "free variables in supercombinator: " ++ show tm
    else
      Lambda (BX <$ vs) . ABTN.TAbss vs . snd
        <$> bindLocal vs (anfTerm body)
  where
    (vs, body) = fromMaybe ([], tm) $ unLams' tm

anfTerm :: (Var v) => Term v a -> ANFM v (DNormal v)
anfTerm tm = f <$> anfBlock tm
  where
    -- f = uncurry (liftA2 TBinds)
    f ((_, []), dtm) = dtm
    f ((_, cx), (_, tm)) = (Indirect (), TBinds cx tm)

floatableCtx :: (Var v) => Ctx v -> Bool
floatableCtx = all p . snd
  where
    p (LZ _ _ _) = True
    p (ST _ _ _ tm) = q tm
    q (TLit _) = True
    q (TVar _) = True
    q (TCon _ _ _) = True
    q _ = False

anfHandled :: (Var v) => Term v a -> ANFM v (Ctx v, DNormal v)
anfHandled body =
  anfBlock body >>= \case
    (ctx, (_, t@TCon {})) ->
      fresh <&> \v ->
        (ctx <> pure [ST1 Direct v BX t], pure $ TVar v)
    (ctx, (_, t@(TLit l))) ->
      fresh <&> \v ->
        (ctx <> pure [ST1 Direct v cc t], pure $ TVar v)
      where
        cc = case l of T {} -> BX; LM {} -> BX; LY {} -> BX; _ -> UN
    p -> pure p

fls, tru :: (Var v) => ANormal v
fls = TCon Ty.booleanRef 0 []
tru = TCon Ty.booleanRef 1 []

-- Helper function for renaming a variable arising from a
--   let v = u
-- binding during ANF translation. Renames a variable in a
-- context, and returns an indication of whether the varible
-- was shadowed by one of the context bindings.
--
-- Note: this assumes that `u` is not bound by any of the context
-- entries, as no effort is made to rename them to avoid capturing
-- `u`.
renameCtx :: (Var v) => v -> v -> Ctx v -> (Ctx v, Bool)
renameCtx v u (d, ctx) | (ctx, b) <- renameCtes v u ctx = ((d, ctx), b)

-- As above, but without the Direction.
renameCtes :: (Var v) => v -> v -> [Cte v] -> ([Cte v], Bool)
renameCtes v u = rn []
  where
    swap w
      | w == v = u
      | otherwise = w

    rn acc [] = (reverse acc, False)
    rn acc (ST d vs ccs b : es)
      | any (== v) vs = (reverse acc ++ e : es, True)
      | otherwise = rn (e : acc) es
      where
        e = ST d vs ccs $ ABTN.rename v u b
    rn acc (LZ w f as : es)
      | w == v = (reverse acc ++ e : es, True)
      | otherwise = rn (e : acc) es
      where
        e = LZ w (swap <$> f) (swap <$> as)

-- Simultaneously renames variables in a list of context entries.
--
-- Assumes that the variables being renamed to are not bound by the
-- context entries, so that it is unnecessary to rename them.
renamesCtes :: (Var v) => Map v v -> [Cte v] -> [Cte v]
renamesCtes rn = map f
  where
    swap w
      | Just u <- Map.lookup w rn = u
      | otherwise = w

    f (ST d vs ccs b) = ST d vs ccs (ABTN.renames rn b)
    f (LZ v r as) = LZ v (second swap r) (map swap as)

-- Calculates the free variables occurring in a context. This
-- consists of the free variables in the expressions being bound,
-- but with previously bound variables subtracted.
freeVarsCtx :: (Ord v) => Ctx v -> Set v
freeVarsCtx = freeVarsCte . snd

freeVarsCte :: (Ord v) => [Cte v] -> Set v
freeVarsCte = foldr m Set.empty
  where
    m (ST _ vs _ bn) rest =
      ABTN.freeVars bn `Set.union` (rest Set.\\ Set.fromList vs)
    m (LZ v r as) rest =
      Set.fromList (either (const id) (:) r as)
        `Set.union` Set.delete v rest

-- Conditionally freshens a list of variables. The predicate
-- argument selects which variables to freshen, and the set is a set
-- of variables to avoid for freshness. The process ensures that the
-- result is mutually fresh, and returns a new set of variables to
-- avoid, which includes the freshened variables.
--
-- Presumably any variables selected by the predicate should be
-- included in the set, but the set may contain additional variables
-- to avoid, when freshening.
freshens :: (Var v) => (v -> Bool) -> Set v -> [v] -> (Set v, [v])
freshens p avoid0 vs =
  mapAccumL f (Set.union avoid0 (Set.fromList vs)) vs
  where
    f avoid v
      | p v, u <- Var.freshIn avoid v = (Set.insert u avoid, u)
      | otherwise = (avoid, v)

-- Freshens the variable bindings in a context to avoid a set of
-- variables. Returns the renaming necessary for anything that was
-- bound in the freshened context.
--
-- Note: this only freshens if it's necessary to avoid variables in
-- the _original_ set. We need to keep track of other variables to
-- avoid when making up new names for those, but it it isn't
-- necessary to freshen variables to remove shadowing _within_ the
-- context, since it is presumably already correctly formed.
freshenCtx :: (Var v) => Set v -> Ctx v -> (Map v v, Ctx v)
freshenCtx avoid0 (d, ctx) =
  case go lavoid Map.empty [] $ reverse ctx of
    (rn, ctx) -> (rn, (d, ctx))
  where
    -- precalculate all variable occurrences in the context to just
    -- completely avoid those as well.
    lavoid =
      foldl (flip $ Set.union . cteVars) avoid0 ctx

    go _ rns fresh [] = (rns, fresh)
    go avoid rns fresh (bn : bns) = case bn of
      LZ v r as
        | v `Set.member` avoid0,
          u <- Var.freshIn avoid v,
          (fresh, _) <- renameCtes v u fresh,
          avoid <- Set.insert u avoid,
          rns <- Map.alter (Just . fromMaybe u) v rns ->
            go avoid rns (LZ u r as : fresh) bns
      ST d vs ccs expr
        | (avoid, us) <- freshens (`Set.member` avoid0) avoid vs,
          rn <- Map.fromList (filter (uncurry (/=)) $ zip vs us),
          not (Map.null rn),
          fresh <- renamesCtes rn fresh,
          -- Note: rns union left-biased, so inner contexts take
          -- priority.
          rns <- Map.union rns rn ->
            go avoid rns (ST d us ccs expr : fresh) bns
      _ -> go avoid rns (bn : fresh) bns

anfBlock :: (Ord v, Var v) => Term v a -> ANFM v (Ctx v, DNormal v)
anfBlock (Var' v) = pure (mempty, pure $ TVar v)
anfBlock (If' c t f) = do
  (cctx, cc) <- anfBlock c
  (df, cf) <- anfTerm f
  (dt, ct) <- anfTerm t
  (cx, v) <- contextualize cc
  let cases =
        MatchData
          (Builtin $ Data.Text.pack "Boolean")
          (EC.mapSingleton 0 ([], cf))
          (Just ct)
  pure (cctx <> cx, (Indirect () <> df <> dt, TMatch v cases))
anfBlock (And' l r) = do
  (lctx, vl) <- anfArg l
  (d, tmr) <- anfTerm r
  let tree =
        TMatch vl . MatchDataCover Ty.booleanRef $
          mapFromList
            [ (0, ([], fls)),
              (1, ([], tmr))
            ]
  pure (lctx, (Indirect () <> d, tree))
anfBlock (Or' l r) = do
  (lctx, vl) <- anfArg l
  (d, tmr) <- anfTerm r
  let tree =
        TMatch vl . MatchDataCover Ty.booleanRef $
          mapFromList
            [ (1, ([], tru)),
              (0, ([], tmr))
            ]
  pure (lctx, (Indirect () <> d, tree))
anfBlock (Handle' h body) =
  anfArg h >>= \(hctx, vh) ->
    anfHandled body >>= \case
      (ctx, (_, TCom f as)) | floatableCtx ctx -> do
        v <- fresh
        pure
          ( hctx <> ctx <> pure [LZ v (Left f) as],
            (Indirect (), TApp (FVar vh) [v])
          )
      (ctx, (_, TApv f as)) | floatableCtx ctx -> do
        v <- fresh
        pure
          ( hctx <> ctx <> pure [LZ v (Right f) as],
            (Indirect (), TApp (FVar vh) [v])
          )
      (ctx, (_, TVar v)) | floatableCtx ctx -> do
        pure (hctx <> ctx, (Indirect (), TApp (FVar vh) [v]))
      p@(_, _) ->
        internalBug $ "handle body should be a simple call: " ++ show p
anfBlock (Match' scrut cas) = do
  (sctx, sc) <- anfBlock scrut
  (cx, v) <- contextualize sc
  (d, brn) <- anfCases v cas
  fmap (first ((Indirect () <> d) <>)) <$> case brn of
    AccumDefault (TBinds (directed -> dctx) df) -> do
      pure (sctx <> cx <> dctx, pure df)
    AccumRequest _ Nothing ->
      internalBug "anfBlock: AccumRequest without default"
    AccumPure (ABTN.TAbss us bd)
      | [u] <- us,
        TBinds (directed -> bx) bd <- bd ->
          case cx of
            (_, []) -> do
              d0 <- Indirect <$> binder
              pure (sctx <> pure [ST1 d0 u BX (TFrc v)] <> bx, pure bd)
            (d0, [ST1 d1 _ BX tm]) ->
              pure (sctx <> (d0, [ST1 d1 u BX tm]) <> bx, pure bd)
            _ -> internalBug "anfBlock|AccumPure: impossible"
      | otherwise -> internalBug "pure handler with too many variables"
    AccumRequest abr (Just df) -> do
      (r, vs) <- do
        r <- fresh
        v <- fresh
        gvs <- groupVars
        let hfb = ABTN.TAbs v . TMatch v $ MatchRequest abr df
            hfvs = Set.toList $ ABTN.freeVars hfb `Set.difference` gvs
        record (r, Lambda (BX <$ hfvs ++ [v]) . ABTN.TAbss hfvs $ hfb)
        pure (r, hfvs)
      hv <- fresh
      let (d, msc)
            | (d, [ST1 _ _ BX tm]) <- cx = (d, tm)
            | (_, [ST _ _ _ _]) <- cx =
                internalBug "anfBlock: impossible"
            | otherwise = (Indirect (), TFrc v)
      pure
        ( sctx <> pure [LZ hv (Right r) vs],
          (d, THnd (Map.keys abr) hv msc)
        )
    AccumText df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchText cs df)
    AccumIntegral r df cs ->
      pure (sctx <> cx, pure $ TMatch v $ MatchNumeric r cs df)
    AccumData r df cs ->
      pure (sctx <> cx, pure . TMatch v $ MatchData r cs df)
    AccumSeqEmpty _ ->
      internalBug "anfBlock: non-exhaustive AccumSeqEmpty"
    AccumSeqView en (Just em) bd -> do
      r <- fresh
      let op
            | SLeft <- en = Builtin "List.viewl"
            | otherwise = Builtin "List.viewr"
      b <- binder
      pure
        ( sctx
            <> cx
            <> (Indirect (), [ST1 (Indirect b) r BX (TCom op [v])]),
          pure . TMatch r $
            MatchDataCover
              Ty.seqViewRef
              ( EC.mapFromList
                  [ (fromIntegral Ty.seqViewEmpty, ([], em)),
                    (fromIntegral Ty.seqViewElem, ([BX, BX], bd))
                  ]
              )
        )
    AccumSeqView {} ->
      internalBug "anfBlock: non-exhaustive AccumSeqView"
    AccumSeqSplit en n mdf bd -> do
      i <- fresh
      r <- fresh
      s <- fresh
      b <- binder
      let split = ST1 (Indirect b) r BX (TCom op [i, v])
      pure
        ( sctx <> cx <> directed [lit i, split],
          pure . TMatch r . MatchDataCover Ty.seqViewRef $
            mapFromList
              [ (fromIntegral Ty.seqViewEmpty, ([], df s)),
                (fromIntegral Ty.seqViewElem, ([BX, BX], bd))
              ]
        )
      where
        op
          | SLeft <- en = Builtin "List.splitLeft"
          | otherwise = Builtin "List.splitRight"
        lit i = ST1 Direct i BX (TBLit . N $ fromIntegral n)
        df n =
          fromMaybe
            ( TLet Direct n BX (TLit (T "pattern match failure")) $
                TPrm EROR [n, v]
            )
            mdf
    AccumEmpty -> pure (sctx <> cx, pure $ TMatch v MatchEmpty)
anfBlock (Let1Named' v b e) =
  anfBlock b >>= \case
    (bctx, (Direct, TVar u)) -> do
      (ectx, ce) <- anfBlock e
      (brn, bctx) <- fixupBctx bctx ectx ce
      u <- pure $ Map.findWithDefault u u brn
      (ectx, shaded) <- pure $ renameCtx v u ectx
      ce <- pure $ if shaded then ce else ABTN.rename v u <$> ce
      pure (bctx <> ectx, ce)
    (bctx, (d0, cb)) -> bindLocal [v] $ do
      (ectx, ce) <- anfBlock e
      d <- bindDirection d0
      (brn, bctx) <- fixupBctx bctx ectx ce
      cb <- pure $ ABTN.renames brn cb
      let octx = bctx <> directed [ST1 d v BX cb] <> ectx
      pure (octx, ce)
  where
    fixupBctx bctx ectx (_, ce) =
      pure $ freshenCtx (Set.union ecfvs efvs) bctx
      where
        ecfvs = freeVarsCtx ectx
        efvs = ABTN.freeVars ce
anfBlock (Apps' (Blank' b) args) = do
  nm <- fresh
  (actx, cas) <- anfArgs args
  pure
    ( actx <> pure [ST1 Direct nm BX (TLit (T msg))],
      pure $ TPrm EROR (nm : cas)
    )
  where
    msg = Util.Text.pack . fromMaybe "blank expression" $ nameb b
anfBlock (Apps' f args) = do
  (fctx, (d, cf)) <- anfFunc f
  (actx, cas) <- anfArgs args
  pure (fctx <> actx, (d, TApp cf cas))
anfBlock (Constructor' (ConstructorReference r t)) =
  pure (mempty, pure $ TCon r (fromIntegral t) [])
anfBlock (Request' (ConstructorReference r t)) =
  pure (mempty, (Indirect (), TReq r (fromIntegral t) []))
anfBlock (Boolean' b) =
  pure (mempty, pure $ TCon Ty.booleanRef (if b then 1 else 0) [])
anfBlock (Lit' l@(T _)) =
  pure (mempty, pure $ TLit l)
anfBlock (Lit' l) =
  pure (mempty, pure $ TBLit l)
anfBlock (Ref' r) = pure (mempty, (Indirect (), TCom r []))
anfBlock (Blank' b) = do
  nm <- fresh
  ev <- fresh
  pure
    ( pure
        [ ST1 Direct nm BX (TLit (T name)),
          ST1 Direct ev BX (TLit (T $ Util.Text.pack msg))
        ],
      pure $ TPrm EROR [nm, ev]
    )
  where
    name = "blank expression"
    msg = fromMaybe "blank expression" $ nameb b
anfBlock (TermLink' r) = pure (mempty, pure . TLit $ LM r)
anfBlock (TypeLink' r) = pure (mempty, pure . TLit $ LY r)
anfBlock (List' as) = fmap (pure . TPrm BLDS) <$> anfArgs tms
  where
    tms = toList as
anfBlock t = internalBug $ "anf: unhandled term: " ++ show t

-- Note: this assumes that patterns have already been translated
-- to a state in which every case matches a single layer of data,
-- with no guards, and no variables ignored. This is not checked
-- completely.
anfInitCase ::
  (Var v) =>
  v ->
  MatchCase p (Term v a) ->
  ANFD v (BranchAccum v)
anfInitCase u (MatchCase p guard (ABT.AbsN' vs bd))
  | Just _ <- guard = internalBug "anfInitCase: unexpected guard"
  | P.Unbound _ <- p,
    [] <- vs =
      AccumDefault <$> anfBody bd
  | P.Var _ <- p,
    [v] <- vs =
      AccumDefault . ABTN.rename v u <$> anfBody bd
  | P.Var _ <- p =
      internalBug $ "vars: " ++ show (length vs)
  | P.Int _ (fromIntegral -> i) <- p =
      AccumIntegral Ty.intRef Nothing . EC.mapSingleton i <$> anfBody bd
  | P.Nat _ i <- p =
      AccumIntegral Ty.natRef Nothing . EC.mapSingleton i <$> anfBody bd
  | P.Char _ c <- p,
    w <- fromIntegral $ fromEnum c =
      AccumIntegral Ty.charRef Nothing . EC.mapSingleton w <$> anfBody bd
  | P.Boolean _ b <- p,
    t <- if b then 1 else 0 =
      AccumData Ty.booleanRef Nothing
        . EC.mapSingleton t
        . ([],)
        <$> anfBody bd
  | P.Text _ t <- p,
    [] <- vs =
      AccumText Nothing . Map.singleton (Util.Text.fromText t) <$> anfBody bd
  | P.Constructor _ (ConstructorReference r t) ps <- p = do
      (,)
        <$> expandBindings ps vs
        <*> anfBody bd
        <&> \(us, bd) ->
          AccumData r Nothing . EC.mapSingleton (fromIntegral t) . (BX <$ us,) $ ABTN.TAbss us bd
  | P.EffectPure _ q <- p =
      (,)
        <$> expandBindings [q] vs
        <*> anfBody bd
        <&> \(us, bd) -> AccumPure $ ABTN.TAbss us bd
  | P.EffectBind _ (ConstructorReference r t) ps pk <- p = do
      (,,)
        <$> expandBindings (snoc ps pk) vs
        <*> Compose (pure <$> fresh)
        <*> anfBody bd
        <&> \(exp, kf, bd) ->
          let (us, uk) =
                maybe (internalBug "anfInitCase: unsnoc impossible") id $
                  unsnoc exp
              jn = Builtin "jumpCont"
           in flip AccumRequest Nothing
                . Map.singleton r
                . EC.mapSingleton (fromIntegral t)
                . (BX <$ us,)
                . ABTN.TAbss us
                . TShift r kf
                $ TName uk (Left jn) [kf] bd
  | P.SequenceLiteral _ [] <- p =
      AccumSeqEmpty <$> anfBody bd
  | P.SequenceOp _ l op r <- p,
    Concat <- op,
    P.SequenceLiteral p ll <- l = do
      AccumSeqSplit SLeft (length ll) Nothing
        <$> (ABTN.TAbss <$> expandBindings [P.Var p, r] vs <*> anfBody bd)
  | P.SequenceOp _ l op r <- p,
    Concat <- op,
    P.SequenceLiteral p rl <- r =
      AccumSeqSplit SLeft (length rl) Nothing
        <$> (ABTN.TAbss <$> expandBindings [l, P.Var p] vs <*> anfBody bd)
  | P.SequenceOp _ l op r <- p,
    dir <- case op of Cons -> SLeft; _ -> SRight =
      AccumSeqView dir Nothing
        <$> (ABTN.TAbss <$> expandBindings [l, r] vs <*> anfBody bd)
  where
    anfBody tm = Compose . bindLocal vs $ anfTerm tm
anfInitCase _ (MatchCase p _ _) =
  internalBug $ "anfInitCase: unexpected pattern: " ++ show p

valueTermLinks :: Value -> [Reference]
valueTermLinks = Set.toList . valueLinks f
  where
    f False r = Set.singleton r
    f _ _ = Set.empty

valueLinks :: (Monoid a) => (Bool -> Reference -> a) -> Value -> a
valueLinks f (Partial (GR cr _) vs) =
  f False cr <> foldMapOf (folded . _Right) (valueLinks f) vs
valueLinks f (Data dr _ vs) =
  f True dr <> foldMapOf (folded . _Right) (valueLinks f) vs
valueLinks f (Cont vs k) =
  foldMapOf (folded . _Right) (valueLinks f) vs <> contLinks f k
valueLinks f (BLit l) = blitLinks f l

contLinks :: (Monoid a) => (Bool -> Reference -> a) -> Cont -> a
contLinks f (Push _ _ (GR cr _) k) =
  f False cr <> contLinks f k
contLinks f (Mark _ ps de k) =
  foldMap (f True) ps
    <> Map.foldMapWithKey (\k c -> f True k <> valueLinks f c) de
    <> contLinks f k
contLinks _ KE = mempty

blitLinks :: (Monoid a) => (Bool -> Reference -> a) -> BLit -> a
blitLinks f (List s) = foldMap (valueLinks f) s
blitLinks _ _ = mempty

groupTermLinks :: (Var v) => SuperGroup v -> [Reference]
groupTermLinks = Set.toList . foldGroupLinks f
  where
    f False r = Set.singleton r
    f _ _ = Set.empty

overGroupLinks ::
  (Var v) =>
  (Bool -> Reference -> Reference) ->
  SuperGroup v ->
  SuperGroup v
overGroupLinks f =
  runIdentity . traverseGroupLinks (\b -> Identity . f b)

traverseGroupLinks ::
  (Applicative f, Var v) =>
  (Bool -> Reference -> f Reference) ->
  SuperGroup v ->
  f (SuperGroup v)
traverseGroupLinks f (Rec bs e) =
  Rec <$> (traverse . traverse) (normalLinks f) bs <*> normalLinks f e

foldGroupLinks ::
  (Monoid r, Var v) =>
  (Bool -> Reference -> r) ->
  SuperGroup v ->
  r
foldGroupLinks f = getConst . traverseGroupLinks (\b -> Const . f b)

normalLinks ::
  (Applicative f, Var v) =>
  (Bool -> Reference -> f Reference) ->
  SuperNormal v ->
  f (SuperNormal v)
normalLinks f (Lambda ccs e) = Lambda ccs <$> anfLinks f e

anfLinks ::
  (Applicative f, Var v) =>
  (Bool -> Reference -> f Reference) ->
  ANormal v ->
  f (ANormal v)
anfLinks f (ABTN.Term _ (ABTN.Abs v e)) =
  ABTN.TAbs v <$> anfLinks f e
anfLinks f (ABTN.Term _ (ABTN.Tm e)) =
  ABTN.TTm <$> anfFLinks f (anfLinks f) e

anfFLinks ::
  (Applicative f) =>
  (Bool -> Reference -> f Reference) ->
  (e -> f e) ->
  ANormalF v e ->
  f (ANormalF v e)
anfFLinks _ g (ALet d ccs b e) = ALet d ccs <$> g b <*> g e
anfFLinks f g (AName er vs e) =
  flip AName vs <$> bitraverse (f False) pure er <*> g e
anfFLinks f g (AMatch v bs) =
  AMatch v <$> branchLinks (f True) g bs
anfFLinks f g (AShift r e) =
  AShift <$> f True r <*> g e
anfFLinks f g (AHnd rs v e) =
  flip AHnd v <$> traverse (f True) rs <*> g e
anfFLinks f _ (AApp fu vs) = flip AApp vs <$> funcLinks f fu
anfFLinks f _ (ALit l) = ALit <$> litLinks f l
anfFLinks _ _ v = pure v

litLinks ::
  (Applicative f) =>
  (Bool -> Reference -> f Reference) ->
  Lit ->
  f Lit
litLinks f (LY r) = LY <$> f True r
litLinks f (LM (Con (ConstructorReference r i) t)) =
  LM . flip Con t . flip ConstructorReference i <$> f True r
litLinks f (LM (Ref r)) = LM . Ref <$> f False r
litLinks _ v = pure v

branchLinks ::
  (Applicative f) =>
  (Reference -> f Reference) ->
  (e -> f e) ->
  Branched e ->
  f (Branched e)
branchLinks f g (MatchRequest m e) =
  MatchRequest . Map.fromList
    <$> traverse (bitraverse f $ (traverse . traverse) g) (Map.toList m)
    <*> g e
branchLinks f g (MatchData r m e) =
  MatchData <$> f r <*> (traverse . traverse) g m <*> traverse g e
branchLinks _ g (MatchText m e) =
  MatchText <$> traverse g m <*> traverse g e
branchLinks _ g (MatchIntegral m e) =
  MatchIntegral <$> traverse g m <*> traverse g e
branchLinks _ g (MatchNumeric r m e) =
  MatchNumeric r <$> traverse g m <*> traverse g e
branchLinks _ g (MatchSum m) =
  MatchSum <$> (traverse . traverse) g m
branchLinks _ _ MatchEmpty = pure MatchEmpty

funcLinks ::
  (Applicative f) =>
  (Bool -> Reference -> f Reference) ->
  Func v ->
  f (Func v)
funcLinks f (FComb r) = FComb <$> f False r
funcLinks f (FCon r t) = flip FCon t <$> f True r
funcLinks f (FReq r t) = flip FReq t <$> f True r
funcLinks _ ff = pure ff

expandBindings' ::
  (Var v) =>
  Word64 ->
  [P.Pattern p] ->
  [v] ->
  Either String (Word64, [v])
expandBindings' fr [] [] = Right (fr, [])
expandBindings' fr (P.Unbound _ : ps) vs =
  fmap (u :) <$> expandBindings' (fr + 1) ps vs
  where
    u = freshANF fr
expandBindings' fr (P.Var _ : ps) (v : vs) =
  fmap (v :) <$> expandBindings' fr ps vs
expandBindings' _ [] (_ : _) =
  Left "expandBindings': more bindings than expected"
expandBindings' _ (_ : _) [] =
  Left "expandBindings': more patterns than expected"
expandBindings' _ _ _ =
  Left $ "expandBindings': unexpected pattern"

expandBindings :: (Var v) => [P.Pattern p] -> [v] -> ANFD v [v]
expandBindings ps vs =
  Compose . state $ \(fr, bnd, co) -> case expandBindings' fr ps vs of
    Left err -> internalBug $ err ++ " " ++ show (ps, vs)
    Right (fr, l) -> (pure l, (fr, bnd, co))

anfCases ::
  (Var v) =>
  v ->
  [MatchCase p (Term v a)] ->
  ANFM v (Directed () (BranchAccum v))
anfCases u = getCompose . fmap fold . traverse (anfInitCase u)

anfFunc :: (Var v) => Term v a -> ANFM v (Ctx v, Directed () (Func v))
anfFunc (Var' v) = pure (mempty, (Indirect (), FVar v))
anfFunc (Ref' r) = pure (mempty, (Indirect (), FComb r))
anfFunc (Constructor' (ConstructorReference r t)) = pure (mempty, (Direct, FCon r $ fromIntegral t))
anfFunc (Request' (ConstructorReference r t)) = pure (mempty, (Indirect (), FReq r $ fromIntegral t))
anfFunc tm = do
  (fctx, ctm) <- anfBlock tm
  (cx, v) <- contextualize ctm
  pure (fctx <> cx, (Indirect (), FVar v))

anfArg :: (Var v) => Term v a -> ANFM v (Ctx v, v)
anfArg tm = do
  (ctx, ctm) <- anfBlock tm
  (cx, v) <- contextualize ctm
  pure (ctx <> cx, v)

anfArgs :: (Var v) => [Term v a] -> ANFM v (Ctx v, [v])
anfArgs tms = first fold . unzip <$> traverse anfArg tms

indent :: Int -> ShowS
indent ind = showString (replicate (ind * 2) ' ')

prettyGroup :: (Var v) => String -> SuperGroup v -> ShowS
prettyGroup s (Rec grp ent) =
  showString ("let rec[" ++ s ++ "]\n")
    . foldr f id grp
    . showString "entry"
    . prettySuperNormal 1 ent
  where
    f (v, sn) r =
      indent 1
        . pvar v
        . prettySuperNormal 2 sn
        . showString "\n"
        . r

pvar :: (Var v) => v -> ShowS
pvar v = showString . Data.Text.unpack $ Var.name v

prettyVars :: (Var v) => [v] -> ShowS
prettyVars =
  foldr (\v r -> showString " " . pvar v . r) id

prettyLVars :: (Var v) => [Mem] -> [v] -> ShowS
prettyLVars [] [] = showString " "
prettyLVars (c : cs) (v : vs) =
  showString " "
    . showParen True (pvar v . showString ":" . shows c)
    . prettyLVars cs vs
prettyLVars [] (_ : _) = internalBug "more variables than conventions"
prettyLVars (_ : _) [] = internalBug "more conventions than variables"

prettyRBind :: (Var v) => [v] -> ShowS
prettyRBind [] = showString "()"
prettyRBind [v] = pvar v
prettyRBind (v : vs) =
  showParen True $
    pvar v . foldr (\v r -> shows v . showString "," . r) id vs

prettySuperNormal :: (Var v) => Int -> SuperNormal v -> ShowS
prettySuperNormal ind (Lambda ccs (ABTN.TAbss vs tm)) =
  prettyLVars ccs vs
    . showString "="
    . prettyANF False (ind + 1) tm

reqSpace :: (Var v) => Bool -> ANormal v -> Bool
reqSpace _ TLets {} = True
reqSpace _ TName {} = True
reqSpace b _ = b

prettyANF :: (Var v) => Bool -> Int -> ANormal v -> ShowS
prettyANF m ind tm =
  prettySpace (reqSpace m tm) ind . case tm of
    TLets _ vs _ bn bo ->
      prettyRBind vs
        . showString " ="
        . prettyANF False (ind + 1) bn
        . prettyANF True ind bo
    TName v f vs bo ->
      prettyRBind [v]
        . showString " := "
        . prettyLZF f
        . prettyVars vs
        . prettyANF True ind bo
    TLit l -> shows l
    TFrc v -> showString "!" . pvar v
    TVar v -> pvar v
    TApp f vs -> prettyFunc f . prettyVars vs
    TMatch v bs ->
      showString "match "
        . pvar v
        . showString " with"
        . prettyBranches (ind + 1) bs
    TShift r v bo ->
      showString "shift["
        . shows r
        . showString "]"
        . prettyVars [v]
        . showString "."
        . prettyANF False (ind + 1) bo
    THnd rs v bo ->
      showString "handle"
        . prettyRefs rs
        . prettyANF False (ind + 1) bo
        . showString " with "
        . pvar v
    _ -> shows tm

prettySpace :: Bool -> Int -> ShowS
prettySpace False _ = showString " "
prettySpace True ind = showString "\n" . indent ind

prettyLZF :: (Var v) => Either Reference v -> ShowS
prettyLZF (Left w) = showString "ENV(" . shows w . showString ") "
prettyLZF (Right v) = pvar v . showString " "

prettyRefs :: [Reference] -> ShowS
prettyRefs [] = showString "{}"
prettyRefs (r : rs) =
  showString "{"
    . shows r
    . foldr (\t r -> shows t . showString "," . r) id rs
    . showString "}"

prettyFunc :: (Var v) => Func v -> ShowS
prettyFunc (FVar v) = pvar v . showString " "
prettyFunc (FCont v) = pvar v . showString " "
prettyFunc (FComb w) = showString "ENV(" . shows w . showString ")"
prettyFunc (FCon r t) =
  showString "CON("
    . shows r
    . showString ","
    . shows t
    . showString ")"
prettyFunc (FReq r t) =
  showString "REQ("
    . shows r
    . showString ","
    . shows t
    . showString ")"
prettyFunc (FPrim op) = either shows shows op . showString " "

prettyBranches :: (Var v) => Int -> Branched (ANormal v) -> ShowS
prettyBranches ind bs = case bs of
  MatchEmpty -> showString "{}"
  MatchIntegral bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (mapToList bs)
  MatchText bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (Map.toList bs)
  MatchData _ bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr
        (uncurry $ prettyCase ind . shows)
        id
        (mapToList $ snd <$> bs)
  MatchRequest bs df ->
    foldr
      ( \(r, m) s ->
          foldr
            (\(c, e) -> prettyCase ind (prettyReq r c) e)
            s
            (mapToList $ snd <$> m)
      )
      (prettyCase ind (prettyReq (0 :: Int) (0 :: Int)) df id)
      (Map.toList bs)
  MatchSum bs ->
    foldr
      (uncurry $ prettyCase ind . shows)
      id
      (mapToList $ snd <$> bs)
  MatchNumeric _ bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (mapToList bs)
      -- _ -> error "prettyBranches: todo"
  where
    -- prettyReq :: Reference -> CTag -> ShowS
    prettyReq r c =
      showString "REQ("
        . shows r
        . showString ","
        . shows c
        . showString ")"

prettyCase :: (Var v) => Int -> ShowS -> ANormal v -> ShowS -> ShowS
prettyCase ind sc (ABTN.TAbss vs e) r =
  showString "\n"
    . indent ind
    . sc
    . prettyVars vs
    . showString " ->"
    . prettyANF False (ind + 1) e
    . r
