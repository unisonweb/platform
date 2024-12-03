module Unison.PatternMatchCoverage.Desugar
  ( desugarMatch,
  )
where

import U.Core.ABT qualified as ABT
import Unison.Pattern
import Unison.Pattern qualified as Pattern
import Unison.PatternMatchCoverage.Class
import Unison.PatternMatchCoverage.GrdTree
import Unison.PatternMatchCoverage.PmGrd
import Unison.PatternMatchCoverage.PmLit qualified as PmLit
import Unison.Term (MatchCase (..), Term', app, var)
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Util.Recursion

-- | Desugar a match into a 'GrdTree'
desugarMatch ::
  forall loc vt v m.
  (Pmc vt v loc m) =>
  -- | scrutinee type
  Type vt loc ->
  -- | scrutinee variable
  v ->
  -- | match cases
  [MatchCase loc (Term' vt v loc)] ->
  m (GrdTree (PmGrd vt v loc) loc)
desugarMatch scrutineeType v0 cs0 = Fork <$> traverse desugarClause cs0
  where
    desugarClause :: MatchCase loc (Term' vt v loc) -> m (GrdTree (PmGrd vt v loc) loc)
    desugarClause MatchCase {matchPattern, matchGuard} =
      desugarPattern scrutineeType v0 matchPattern (finalK (Pattern.loc matchPattern) matchGuard) []

    finalK :: loc -> Maybe (Term' vt v loc) -> [v] -> m (GrdTree (PmGrd vt v loc) loc)
    finalK loc mterm vs = case mterm of
      Nothing -> pure (Leaf loc)
      Just grdExpr -> do
        let ann = ABT.annotation grdExpr
            expr = foldr (\a b -> app ann (var ann a) b) grdExpr vs
            typ = Type.boolean ann
        v <- fresh
        pure (Grd (PmLet v expr typ) (Grd (PmLit v (PmLit.Boolean True)) (Leaf loc)))

desugarPattern ::
  forall v vt loc m.
  (Pmc vt v loc m) =>
  Type vt loc ->
  v ->
  Pattern loc ->
  ([v] -> m (GrdTree (PmGrd vt v loc) loc)) ->
  [v] ->
  m (GrdTree (PmGrd vt v loc) loc)
desugarPattern typ v0 pat k vs = case pat of
  Unbound _ -> k vs
  Var _ -> k (v0 : vs)
  Boolean _ x -> Grd (PmLit v0 $ PmLit.Boolean x) <$> k vs
  Int _ x -> Grd (PmLit v0 $ PmLit.Int x) <$> k vs
  Nat _ x -> Grd (PmLit v0 $ PmLit.Nat x) <$> k vs
  Float _ x -> Grd (PmLit v0 $ PmLit.Float x) <$> k vs
  Text _ x -> Grd (PmLit v0 $ PmLit.Text x) <$> k vs
  Char _ x -> Grd (PmLit v0 $ PmLit.Char x) <$> k vs
  Constructor _loc consRef pats -> do
    contyps <- getConstructorVarTypes typ consRef
    patvars <- assignFreshPatternVars pats
    let c = PmCon v0 consRef convars
        convars :: [(v, Type vt loc)]
        convars = map (\(v, _, t) -> (v, t)) tpatvars
        tpatvars = zipWith (\(v, p) t -> (v, p, t)) patvars contyps
    rest <- foldr (\(v, pat, t) b -> desugarPattern t v pat b) k tpatvars vs
    pure (Grd c rest)
  As _ rest -> desugarPattern typ v0 rest k (v0 : vs)
  EffectPure _ resume -> do
    v <- fresh
    let rt = case typ of
          Type.Apps' (Type.Ref' r) [_et, ret] | r == Type.effectRef -> ret
          _ -> error "impossible: pattern EffectPure doesn't correspond to a scrutinee of type Request?"
    Grd (PmEffectPure v0 (v, rt)) <$> desugarPattern rt v resume k vs
  EffectBind _loc consRef pats _resume -> do
    contyps <- getConstructorVarTypes typ consRef
    patvars <- assignFreshPatternVars pats
    let c = PmEffect v0 consRef convars
        convars :: [(v, Type vt loc)]
        convars = map (\(v, _, t) -> (v, t)) tpatvars
        tpatvars = zipWith (\(v, p) t -> (v, p, t)) patvars contyps
    rest <- foldr (\(v, pat, t) b -> desugarPattern t v pat b) k tpatvars vs
    pure (Grd c rest)
  SequenceLiteral {} -> handleSequence typ v0 pat k vs
  SequenceOp {} -> handleSequence typ v0 pat k vs

handleSequence ::
  forall v vt loc m.
  (Pmc vt v loc m) =>
  Type vt loc ->
  v ->
  Pattern loc ->
  ([v] -> m (GrdTree (PmGrd vt v loc) loc)) ->
  [v] ->
  m (GrdTree (PmGrd vt v loc) loc)
handleSequence typ v pat k vs = do
  let listArg = case typ of
        Type.App' _list arg -> arg
        _ -> error "list type is not an application?"
  listToGrdTree typ listArg v (normalizeList pat) k vs

listToGrdTree ::
  forall v vt loc m.
  (Pmc vt v loc m) =>
  Type vt loc ->
  Type vt loc ->
  v ->
  NormalizedList loc ->
  ([v] -> m (GrdTree (PmGrd vt v loc) loc)) ->
  [v] ->
  m (GrdTree (PmGrd vt v loc) loc)
listToGrdTree _listTyp elemTyp listVar nl0 k0 vs0 =
  let (minLen, maxLen) = cata countMinListLen nl0 0
   in Grd (PmListInterval listVar minLen maxLen) <$> cata go nl0 0 0 k0 vs0
  where
    go pat consCount snocCount k vs = case pat of
      N'ConsF x xs -> do
        element <- fresh
        let grd = PmListHead listVar consCount element elemTyp
        let !consCount' = consCount + 1
        Grd grd <$> desugarPattern elemTyp element x (xs consCount' snocCount k) vs
      N'SnocF xs x -> do
        element <- fresh
        let grd = PmListTail listVar snocCount element elemTyp
        let !snocCount' = snocCount + 1
        Grd grd <$> xs consCount snocCount' (desugarPattern elemTyp element x k) vs
      N'NilF -> k vs
      N'VarF _ -> k (listVar : vs)
      N'UnboundF _ -> k vs

    countMinListLen :: Algebra (NormalizedListF loc) (Int -> (Int, Int))
    countMinListLen = \case
      N'ConsF _ b -> \acc -> b $! acc + 1
      N'SnocF b _ -> \acc -> b $! acc + 1
      N'NilF -> \ !n -> (n, n)
      N'VarF _ -> \ !n -> (n, maxBound)
      N'UnboundF _ -> \ !n -> (n, maxBound)

data NormalizedListF loc a
  = N'ConsF (Pattern loc) a
  | N'SnocF a (Pattern loc)
  | N'NilF
  | N'VarF loc
  | N'UnboundF loc
  deriving stock (Functor)

type NormalizedList loc = Fix (NormalizedListF loc)

pattern N'Cons :: Pattern loc -> Fix (NormalizedListF loc) -> Fix (NormalizedListF loc)
pattern N'Cons x xs = Fix (N'ConsF x xs)

pattern N'Snoc :: Fix (NormalizedListF loc) -> Pattern loc -> Fix (NormalizedListF loc)
pattern N'Snoc xs x = Fix (N'SnocF xs x)

pattern N'Nil :: Fix (NormalizedListF loc)
pattern N'Nil = Fix N'NilF

pattern N'Var :: loc -> Fix (NormalizedListF loc)
pattern N'Var x = Fix (N'VarF x)

pattern N'Unbound :: loc -> Fix (NormalizedListF loc)
pattern N'Unbound x = Fix (N'UnboundF x)

-- | strip out sequence literals and concats
normalizeList :: Pattern loc -> NormalizedList loc
normalizeList pat0 = case goCons pat0 of
  Left f -> f N'Nil
  Right x -> x
  where
    goCons :: Pattern loc -> Either (NormalizedList loc -> NormalizedList loc) (NormalizedList loc)
    goCons = \case
      SequenceLiteral _loc xs ->
        Left \nil -> foldr N'Cons nil xs
      SequenceOp _loc lhs op rhs -> case op of
        Cons ->
          case goCons rhs of
            Left f -> Left (N'Cons lhs . f)
            Right x -> Right (N'Cons lhs x)
        Snoc ->
          case goCons lhs of
            Left f -> Left (f . N'Cons rhs)
            Right x -> Right (N'Snoc x rhs)
        Concat ->
          case goCons lhs of
            Left f -> case goCons rhs of
              Left g -> Left (f . g)
              Right x -> Right (f x)
            Right x -> Right (goSnoc rhs x)
      Var loc -> Right (N'Var loc)
      Unbound loc -> Right (N'Unbound loc)
      -- as-patterns are not handled properly here, which is fine while we
      -- only have boolean guards, but this needs to be fixed if we
      -- introduce pattern guards
      As _loc pat -> goCons pat
      _ -> error "goCons: unexpected pattern"

    goSnoc :: Pattern loc -> NormalizedList loc -> NormalizedList loc
    goSnoc pat nlp = case pat of
      SequenceLiteral _loc xs ->
        foldl N'Snoc nlp xs
      SequenceOp _loc lhs op rhs -> case op of
        Cons ->
          goSnoc rhs (N'Snoc nlp lhs)
        Snoc ->
          N'Snoc (goSnoc rhs nlp) lhs
        Concat ->
          goSnoc rhs (goSnoc lhs nlp)
      As _loc pat -> goSnoc pat nlp
      _ -> error "goSnoc: unexpected pattern"

assignFreshPatternVars :: (Pmc vt v loc m) => [Pattern loc] -> m [(v, Pattern loc)]
assignFreshPatternVars pats = traverse (\p -> (,p) <$> fresh) pats
