-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html

module U.Core.ABT where

import Control.Lens (Lens', use, (.=))
import Control.Monad.State
import Data.Foldable qualified as Foldable
import Data.Set qualified as Set
import Debug.RecoverRTTI qualified as RTTI
import U.Core.ABT.Var (Var (freshIn))
import Unison.Debug qualified as Debug
import Unison.Prelude
import Unison.Util.Recursion
import Prelude hiding (abs, cycle)

data ABT f v r
  = Var v
  | Cycle r
  | Abs v r
  | Tm (f r)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

-- | At each level in the tree, we store the set of free variables and
-- a value of type `a`. Variables are of type `v`.
data Term f v a = Term {freeVars :: Set v, annotation :: a, out :: ABT f v (Term f v a)}
  deriving (Functor, Foldable, Generic, Traversable)

data Term' f v a x = Term' {freeVars' :: Set v, annotation' :: a, out' :: ABT f v x}
  deriving (Functor)

instance (Functor f) => Recursive (Term f v a) (Term' f v a) where
  embed (Term' vs a abt) = Term vs a abt
  project (Term vs a abt) = Term' vs a abt

instance (Foldable f, Functor f, forall a. (Eq a) => Eq (f a), Var v) => Eq (Term f v a) where
  -- alpha equivalence, works by renaming any aligned Abs ctors to use a common fresh variable
  t1 == t2 = go (out t1) (out t2)
    where
      go :: ABT f v (Term f v a) -> ABT f v (Term f v a) -> Bool
      go (Var v) (Var v2) | v == v2 = True
      go (Cycle t1) (Cycle t2) = t1 == t2
      go (Abs v1 body1) (Abs v2 body2) =
        if v1 == v2
          then body1 == body2
          else
            let v3 = freshInBoth body1 body2 v1
             in rename v1 v3 body1 == rename v2 v3 body2
      go (Tm f1) (Tm f2) = f1 == f2
      go _ _ = False

instance
  ( forall a. (Eq a) => Eq (f a),
    Foldable f,
    Functor f,
    forall a. (Ord a) => Ord (f a),
    Var v
  ) =>
  Ord (Term f v a)
  where
  -- alpha equivalence, works by renaming any aligned Abs ctors to use a common fresh variable
  t1 `compare` t2 = go (out t1) (out t2)
    where
      go :: ABT f v (Term f v a) -> ABT f v (Term f v a) -> Ordering
      go (Var v) (Var v2) = v `compare` v2
      go (Cycle t1) (Cycle t2) = t1 `compare` t2
      go (Abs v1 body1) (Abs v2 body2) =
        if v1 == v2
          then body1 `compare` body2
          else
            let v3 = freshInBoth body1 body2 v1
             in rename v1 v3 body1 `compare` rename v2 v3 body2
      go (Tm f1) (Tm f2) = compare f1 f2
      go t1 t2 = tag t1 `compare` tag t2
      tag (Var _) = 0 :: Word
      tag (Tm _) = 1
      tag (Abs _ _) = 2
      tag (Cycle _) = 3

instance (forall a. (Show a) => Show (f a), Show v) => Show (Term f v a) where
  showsPrec p (Term _ ann out) = case out of
    Var v -> showParen (p >= 9) $ \x -> showAnn ++ "Var " ++ show v ++ x
    Cycle body -> (\s -> showAnn ++ "Cycle " ++ s) . showsPrec p body
    Abs v body -> showParen True $ (\s -> showAnn ++ show v ++ s) . showString ". " . showsPrec p body
    Tm f -> (showAnn ++) . showsPrec p f
    where
      showAnn =
        if Debug.shouldDebug Debug.Annotations
          then "(" ++ RTTI.anythingToString ann ++ ")"
          else ""

amap :: (Functor f) => (a -> a') -> Term f v a -> Term f v a'
amap = fmap

vmap :: (Functor f, Foldable f, Ord v') => (v -> v') -> Term f v a -> Term f v' a
vmap f (Term _ a out) = case out of
  Var v -> var a (f v)
  Tm fa -> tm a (fmap (vmap f) fa)
  Cycle r -> cycle a (vmap f r)
  Abs v body -> abs a (f v) (vmap f body)

vmapM :: (Applicative m, Traversable f, Foldable f, Ord v2) => (v -> m v2) -> Term f v a -> m (Term f v2 a)
vmapM f (Term _ a out) = case out of
  Var v -> var a <$> f v
  Tm fa -> tm a <$> traverse (vmapM f) fa
  Cycle r -> cycle a <$> vmapM f r
  Abs v body -> abs a <$> f v <*> vmapM f body

transform ::
  (Ord v, Foldable g, Functor g) =>
  (forall a. f a -> g a) ->
  Term f v a ->
  Term g v a
transform f t = case out t of
  Var v -> var (annotation t) v
  Abs v body -> abs (annotation t) v (transform f body)
  Tm subterms -> tm (annotation t) (fmap (transform f) (f subterms))
  Cycle body -> cycle (annotation t) (transform f body)

transformM ::
  (Ord v, Monad m, Traversable g) =>
  (forall a. f a -> m (g a)) ->
  Term f v a ->
  m (Term g v a)
transformM f t = case out t of
  Var v -> pure $ var (annotation t) v
  Abs v body -> abs (annotation t) v <$> (transformM f body)
  Tm subterms -> tm (annotation t) <$> (traverse (transformM f) =<< f subterms)
  Cycle body -> cycle (annotation t) <$> (transformM f body)

abs :: (Ord v) => a -> v -> Term f v a -> Term f v a
abs a v body = Term (Set.delete v (freeVars body)) a (Abs v body)

var :: a -> v -> Term f v a
var a v = Term (Set.singleton v) a (Var v)

cycle :: a -> Term f v a -> Term f v a
cycle a t = Term (freeVars t) a (Cycle t)

tm :: (Foldable f, Ord v) => a -> f (Term f v a) -> Term f v a
tm a t = Term (Set.unions (fmap freeVars (Foldable.toList t))) a (Tm t)

-- * Traversals

-- | `visit f t` applies an effectful function to each subtree of
-- `t` and sequences the results. When `f` returns `Nothing`, `visit`
-- descends into the children of the current subtree. When `f` returns
-- `Just t2`, `visit` replaces the current subtree with `t2`. Thus:
-- `visit (const Nothing) t == pure t` and
-- `visit (const (Just (pure t2))) t == pure t2`
visit ::
  (Traversable f, Applicative g, Ord v) =>
  (Term f v a -> Maybe (g (Term f v a))) ->
  Term f v a ->
  g (Term f v a)
visit f t = flip fromMaybe (f t) $ case out t of
  Var _ -> pure t
  Cycle body -> cycle (annotation t) <$> visit f body
  Abs x e -> abs (annotation t) x <$> visit f e
  Tm body -> tm (annotation t) <$> traverse (visit f) body

-- | Apply an effectful function to an ABT tree top down, sequencing the results.
visit' ::
  (Traversable f, Monad g, Ord v) =>
  (f (Term f v a) -> g (f (Term f v a))) ->
  Term f v a ->
  g (Term f v a)
visit' f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle (annotation t) <$> visit' f body
  Abs x e -> abs (annotation t) x <$> visit' f e
  Tm body -> f body >>= (fmap (tm (annotation t)) . traverse (visit' f))

-- | Apply an effectful function to an ABT tree top down, sequencing the results.
visit_ ::
  (Traversable f, Applicative g, Ord v) =>
  (f (Term f v a) -> g ()) ->
  Term f v a ->
  g (Term f v a)
visit_ f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle (annotation t) <$> visit_ f body
  Abs x e -> abs (annotation t) x <$> visit_ f e
  Tm body -> f body *> (tm (annotation t) <$> traverse (visit_ f) body)

-- | `visit` specialized to the `Identity` effect.
visitPure ::
  (Traversable f, Ord v) =>
  (Term f v a -> Maybe (Term f v a)) ->
  Term f v a ->
  Term f v a
visitPure f = runIdentity . visit (fmap pure . f)

foreachSubterm ::
  (Traversable f, Applicative g) =>
  (Term f v a -> g b) ->
  Term f v a ->
  g [b]
foreachSubterm f e = case out e of
  Var _ -> pure <$> f e
  Cycle body -> (:) <$> f e <*> foreachSubterm f body
  Abs _ body -> (:) <$> f e <*> foreachSubterm f body
  Tm body ->
    (:)
      <$> f e
      <*> (join . Foldable.toList <$> traverse (foreachSubterm f) body)

subterms :: (Ord v, Traversable f) => Term f v a -> [Term f v a]
subterms t = runIdentity $ foreachSubterm pure t

-- * Patterns

pattern Var' :: v -> Term f v a
pattern Var' v <- Term _ _ (Var v)

pattern Cycle' :: [v] -> Term f v a -> Term f v a
pattern Cycle' vs t <- Term _ _ (Cycle (AbsN' vs t))

pattern AbsN' :: [v] -> Term f v a -> Term f v a
pattern AbsN' vs body <- (unabs -> (vs, body))

{-# COMPLETE AbsN' #-}

pattern Tm' :: f (Term f v a) -> Term f v a
pattern Tm' f <- Term _ _ (Tm f)

unabs :: Term f v a -> ([v], Term f v a)
unabs (Term _ _ (Abs hd body)) =
  let (tl, body') = unabs body in (hd : tl, body')
unabs t = ([], t)

-- | Produce a variable which is free in both terms
freshInBoth :: (Var v) => Term f v a -> Term f v a -> v -> v
freshInBoth t1 t2 = freshIn $ Set.union (freeVars t1) (freeVars t2)

substsInheritAnnotation ::
  (Foldable f, Functor f, Var v) =>
  [(v, Term f v b)] ->
  Term f v a ->
  Term f v a
substsInheritAnnotation replacements body =
  foldr (uncurry substInheritAnnotation) body (reverse replacements)

-- Like `subst`, but the annotation of the replacement is inherited from
-- the previous annotation at each replacement point.
substInheritAnnotation ::
  (Foldable f, Functor f, Var v) =>
  v ->
  Term f v b ->
  Term f v a ->
  Term f v a
substInheritAnnotation v r =
  subst' (\ann -> const ann <$> r) v (freeVars r)

-- Slightly generalized version of `subst`, the replacement action is handled
-- by the function `replace`, which is given the annotation `a` at the point
-- of replacement. `r` should be the set of free variables contained in the
-- term returned by `replace`. See `substInheritAnnotation` for an example usage.
subst' :: (Foldable f, Functor f, Var v) => (a -> Term f v a) -> v -> Set v -> Term f v a -> Term f v a
subst' replace v r t2@(Term fvs ann body)
  | Set.notMember v fvs = t2 -- subtrees not containing the var can be skipped
  | otherwise = case body of
      Var v'
        | v == v' -> replace ann -- var match; perform replacement
        | otherwise -> t2 -- var did not match one being substituted; ignore
      Cycle body -> cycle ann (subst' replace v r body)
      Abs x _ | x == v -> t2 -- x shadows v; ignore subtree
      Abs x e -> abs ann x' e'
        where
          x' = freshIn (fvs `Set.union` r) x
          -- rename x to something that cannot be captured by `r`
          e' =
            if x /= x'
              then subst' replace v r (rename x x' e)
              else subst' replace v r e
      Tm body -> tm ann (fmap (subst' replace v r) body)

-- | renames `old` to `new` in the given term, ignoring subtrees that bind `old`
rename :: (Foldable f, Functor f, Var v) => v -> v -> Term f v a -> Term f v a
rename old new t0@(Term fvs ann t) =
  if Set.notMember old fvs
    then t0
    else case t of
      Var v -> if v == old then var ann new else t0
      Cycle body -> cycle ann (rename old new body)
      Abs v body ->
        -- v shadows old, so skip this subtree
        if v == old
          then abs ann v body
          else -- the rename would capture new, freshen this Abs
          -- to make that no longer true, then proceed with
          -- renaming `old` to `new`

            if v == new
              then
                let v' = freshIn (Set.fromList [new, old] <> freeVars body) v
                 in abs ann v' (rename old new (rename v v' body))
              else -- nothing special, just rename inside body of Abs
                abs ann v (rename old new body)
      Tm v -> tm ann (fmap (rename old new) v)

allVars :: (Foldable f) => Term f v a -> [v]
allVars t = case out t of
  Var v -> [v]
  Cycle body -> allVars body
  Abs v body -> v : allVars body
  Tm v -> Foldable.toList v >>= allVars

-- | Freshens the given variable wrt. the set of used variables
-- tracked by state. Adds the result to the set of used variables.
freshenS :: (Var v, MonadState (Set v) m) => v -> m v
freshenS = freshenS' id

-- | A more general version of `freshenS` that uses a lens
-- to focus on used variables inside state.
freshenS' :: (Var v, MonadState s m) => Lens' s (Set v) -> v -> m v
freshenS' uvLens v = do
  usedVars <- use uvLens
  let v' = freshIn usedVars v
  uvLens .= Set.insert v' usedVars
  pure v'
