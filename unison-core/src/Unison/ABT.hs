-- Based on: http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.ABT where

import Unison.Prelude

import Control.Lens (Lens', use, (.=))
import Control.Monad.State (MonadState,evalState)
import Data.Functor.Identity (runIdentity)
import Data.List hiding (cycle)
import Data.Vector ((!))
import Prelude hiding (abs,cycle)
import Prelude.Extras (Eq1(..), Show1(..), Ord1(..))
import Unison.Hashable (Accumulate,Hashable1,hash1)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Unison.Hashable as Hashable
import qualified Unison.Util.Components as Components

data ABT f v r
  = Var v
  | Cycle r
  | Abs v r
  | Tm (f r) deriving (Functor, Foldable, Traversable)

-- | At each level in the tree, we store the set of free variables and
-- a value of type `a`. Variables are of type `v`.
data Term f v a = Term { freeVars :: Set v, annotation :: a, out :: ABT f v (Term f v a) }

-- | A class for variables.
--
--   * `Set.notMember (freshIn vs v) vs`:
--     `freshIn` returns a variable not used in the `Set`
class Ord v => Var v where
  freshIn :: Set v -> v -> v

data V v = Free v | Bound v deriving (Eq,Ord,Show,Functor)

unvar :: V v -> v
unvar (Free v) = v
unvar (Bound v) = v

instance Var v => Var (V v) where
  freshIn s v = freshIn (Set.map unvar s) <$> v

newtype Path s t a b m = Path { focus :: s -> Maybe (a, b -> Maybe t, m) }

here :: Monoid m => Path s t s t m
here = Path $ \s -> Just (s, Just, mempty)

instance Semigroup (Path s t a b m) where
  (<>) = mappend

instance Monoid (Path s t a b m) where
  mempty = Path (const Nothing)
  mappend (Path p1) (Path p2) = Path p3 where
    p3 s = p1 s <|> p2 s

type Path' f g m = forall a v . Var v => Path (Term f v a) (Term f (V v) a) (Term g v a) (Term g (V v) a) m

compose :: Monoid m => Path s t a b m -> Path a b a' b' m -> Path s t a' b' m
compose (Path p1) (Path p2) = Path p3 where
  p3 s = do
    (get1,set1,m1) <- p1 s
    (get2,set2,m2) <- p2 get1
    pure (get2, set2 >=> set1, m1 `mappend` m2)

at :: Path s t a b m -> s -> Maybe a
at p s = (\(a,_,_) -> a) <$> focus p s

modify' :: Path s t a b m -> (m -> a -> b) -> s -> Maybe t
modify' p f s = focus p s >>= \(get,set,m) -> set (f m get)

wrap :: (Functor f, Foldable f, Var v) => v -> Term f (V v) a -> (V v, Term f (V v) a)
wrap v t =
  if Set.member (Free v) (freeVars t)
  then let v' = fresh t (Bound v) in (v', rename (Bound v) v' t)
  else (Bound v, t)

wrap' :: (Functor f, Foldable f, Var v)
      => v -> Term f (V v) a -> (V v -> Term f (V v) a -> c) -> c
wrap' v t f = uncurry f (wrap v t)

-- | Return the list of all variables bound by this ABT
bound' :: Foldable f => Term f v a -> [v]
bound' t = case out t of
  Abs v t -> v : bound' t
  Cycle t -> bound' t
  Tm f -> Foldable.toList f >>= bound'
  _ -> []

annotateBound' :: (Ord v, Functor f, Foldable f) => Term f v a0 -> Term f v [v]
annotateBound' t = snd <$> annotateBound'' t

-- Annotate the tree with the set of bound variables at each node.
annotateBound :: (Ord v, Foldable f, Functor f) => Term f v a -> Term f v (a, Set v)
annotateBound = go Set.empty where
  go bound t = let a = (annotation t, bound) in case out t of
    Var v -> annotatedVar a v
    Cycle body -> cycle' a (go bound body)
    Abs x body -> abs' a x (go (Set.insert x bound) body)
    Tm body -> tm' a (go bound <$> body)

annotateBound'' :: (Ord v, Functor f, Foldable f) => Term f v a -> Term f v (a, [v])
annotateBound'' = go [] where
  go env t = let a = (annotation t, env) in case out t of
    Abs v body -> abs' a v (go (v : env) body)
    Cycle body -> cycle' a (go env body)
    Tm f -> tm' a (go env <$> f)
    Var v -> annotatedVar a v

-- | Return the set of all variables bound by this ABT
bound :: (Ord v, Foldable f) => Term f v a -> Set v
bound t = Set.fromList (bound' t)

-- | `True` if the term has no free variables, `False` otherwise
isClosed :: Term f v a -> Bool
isClosed t = Set.null (freeVars t)

-- | `True` if `v` is a member of the set of free variables of `t`
isFreeIn :: Ord v => v -> Term f v a -> Bool
isFreeIn v t = Set.member v (freeVars t)

-- | Replace the annotation with the given argument.
annotate :: a -> Term f v a -> Term f v a
annotate a (Term fvs _ out) = Term fvs a out

vmap :: (Functor f, Foldable f, Ord v2) => (v -> v2) -> Term f v a -> Term f v2 a
vmap f (Term _ a out) = case out of
  Var v -> annotatedVar a (f v)
  Tm fa -> tm' a (fmap (vmap f) fa)
  Cycle r -> cycle' a (vmap f r)
  Abs v body -> abs' a (f v) (vmap f body)

amap :: (Functor f, Foldable f, Ord v) => (a -> a2) -> Term f v a -> Term f v a2
amap = amap' . const

amap' :: (Functor f, Foldable f, Ord v) => (Term f v a -> a -> a2) -> Term f v a -> Term f v a2
amap' f t@(Term _ a out) = case out of
  Var v -> annotatedVar (f t a) v
  Tm fa -> tm' (f t a) (fmap (amap' f) fa)
  Cycle r -> cycle' (f t a) (amap' f r)
  Abs v body -> abs' (f t a) v (amap' f body)

-- | Modifies the annotations in this tree
instance Functor f => Functor (Term f v) where
  fmap f (Term fvs a sub) = Term fvs (f a) (fmap (fmap f) sub)

extraMap :: Functor g => (forall k . f k -> g k) -> Term f v a -> Term g v a
extraMap p (Term fvs a sub) = Term fvs a (go p sub) where
  go :: Functor g => (forall k . f k -> g k) -> ABT f v (Term f v a) -> ABT g v (Term g v a)
  go p = \case
    Var v -> Var v
    Cycle r -> Cycle (extraMap p r)
    Abs v r -> Abs v (extraMap p r)
    Tm x -> Tm (fmap (extraMap p) (p x))

pattern Var' v <- Term _ _ (Var v)
pattern Cycle' vs t <- Term _ _ (Cycle (AbsN' vs t))
-- pattern Abs' v body <- Term _ _ (Abs v body)
pattern Abs' subst <- (unabs1 -> Just subst)
pattern AbsN' vs body <- (unabs -> (vs, body))
pattern Tm' f <- Term _ _ (Tm f)
pattern CycleA' a avs t <- Term _ a (Cycle (AbsNA' avs t))
pattern AbsNA' avs body <- (unabsA -> (avs, body))
pattern Abs1NA' avs body <- (unabs1A -> Just (avs, body))

unabsA :: Term f v a -> ([(a,v)], Term f v a)
unabsA (Term _ a (Abs hd body)) =
  let (tl, body') = unabsA body in ((a,hd) : tl, body')
unabsA t = ([], t)

unabs1A :: Term f v a -> Maybe ([(a,v)], Term f v a)
unabs1A t = case unabsA t of
  ([], _) -> Nothing
  x -> Just x

var :: v -> Term f v ()
var = annotatedVar ()

annotatedVar :: a -> v -> Term f v a
annotatedVar a v = Term (Set.singleton v) a (Var v)

abs :: Ord v => v -> Term f v () -> Term f v ()
abs = abs' ()

abs' :: Ord v => a -> v -> Term f v a -> Term f v a
abs' a v body = Term (Set.delete v (freeVars body)) a (Abs v body)

absr :: (Functor f, Foldable f, Var v) => v -> Term f (V v) () -> Term f (V v) ()
absr = absr' ()

-- | Rebuild an `abs`, renaming `v` to avoid capturing any `Free v` in `body`.
absr' :: (Functor f, Foldable f, Var v) => a -> v -> Term f (V v) a -> Term f (V v) a
absr' a v body = wrap' v body $ \v body -> abs' a v body

absChain :: Ord v => [v] -> Term f v () -> Term f v ()
absChain vs t = foldr abs t vs

absCycle :: Ord v => [v] -> Term f v () -> Term f v ()
absCycle vs t = cycle $ absChain vs t

absChain' :: Ord v => [(a, v)] -> Term f v a -> Term f v a
absChain' vs t = foldr (\(a,v) t -> abs' a v t) t vs

tm :: (Foldable f, Ord v) => f (Term f v ()) -> Term f v ()
tm = tm' ()

tm' :: (Foldable f, Ord v) => a -> f (Term f v a) -> Term f v a
tm' a t =
  Term (Set.unions (fmap freeVars (Foldable.toList t))) a (Tm t)

cycle :: Term f v () -> Term f v ()
cycle = cycle' ()

cycle' :: a -> Term f v a -> Term f v a
cycle' a t = Term (freeVars t) a (Cycle t)

cycler' :: (Functor f, Foldable f, Var v) => a -> [v] -> Term f (V v) a -> Term f (V v) a
cycler' a vs t = cycle' a $ foldr (absr' a) t vs

cycler :: (Functor f, Foldable f, Var v) => [v] -> Term f (V v) () -> Term f (V v) ()
cycler = cycler' ()

into :: (Foldable f, Ord v) => ABT f v (Term f v ()) -> Term f v ()
into = into' ()

into' :: (Foldable f, Ord v) => a -> ABT f v (Term f v a) -> Term f v a
into' a abt = case abt of
  Var x -> annotatedVar a x
  Cycle t -> cycle' a t
  Abs v r -> abs' a v r
  Tm t -> tm' a t

-- | renames `old` to `new` in the given term, ignoring subtrees that bind `old`
rename :: (Foldable f, Functor f, Var v) => v -> v -> Term f v a -> Term f v a
rename old new t0@(Term _ ann t) = case t of
  Var v -> if v == old then annotatedVar ann new else t0
  Cycle body -> cycle' ann (rename old new body)
  Abs v body -> if v == old then abs' ann v body
                else abs' ann v (rename old new body)
  Tm v -> tm' ann (fmap (rename old new) v)

changeVars :: (Foldable f, Functor f, Var v) => Map v v -> Term f v a -> Term f v a
changeVars m t = case out t of
  Abs v body -> case Map.lookup v m of
    Nothing -> abs' (annotation t) v (changeVars m body)
    Just v' -> abs' (annotation t) v' (changeVars m body)
  Cycle body -> cycle' (annotation t) (changeVars m body)
  Var v -> case Map.lookup v m of
    Nothing -> t
    Just v -> annotatedVar (annotation t) v
  Tm v -> tm' (annotation t) (changeVars m <$> v)

-- | Produce a variable which is free in both terms
freshInBoth :: Var v => Term f v a -> Term f v a -> v -> v
freshInBoth t1 t2 = freshIn $ Set.union (freeVars t1) (freeVars t2)

fresh :: Var v => Term f v a -> v -> v
fresh t = freshIn (freeVars t)

freshEverywhere :: (Foldable f, Var v) => Term f v a -> v -> v
freshEverywhere t = freshIn . Set.fromList $ allVars t

allVars :: Foldable f => Term f v a -> [v]
allVars t = case out t of
  Var v -> [v]
  Cycle body -> allVars body
  Abs v body -> v : allVars body
  Tm v -> Foldable.toList v >>= allVars

orderedFreeVars :: (Foldable f, Ord v, Show v) => Term f v a -> [v]
orderedFreeVars t =
    filter (`elem` unordered) ordered
  where
    ordered = nub $ allVars t
    unordered = Set.toList $ freeVars t

freshes :: Var v => Term f v a -> [v] -> [v]
freshes = freshes' . freeVars

freshes' :: Var v => Set v -> [v] -> [v]
freshes' used vs = evalState (traverse freshenS vs) used

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

-- | `subst v e body` substitutes `e` for `v` in `body`, avoiding capture by
-- renaming abstractions in `body`
subst
  :: (Foldable f, Functor f, Var v)
  => v
  -> Term f v a
  -> Term f v a
  -> Term f v a
subst v r = subst' (const r) v (freeVars r)

-- Slightly generalized version of `subst`, the replacement action is handled
-- by the function `replace`, which is given the annotation `a` at the point
-- of replacement. `r` should be the set of free variables contained in the
-- term returned by `replace`. See `substInheritAnnotation` for an example usage.
subst' :: (Foldable f, Functor f, Var v) => (a -> Term f v a) -> v -> Set v -> Term f v a -> Term f v a
subst' replace v r t2@(Term fvs ann body)
  | Set.notMember v fvs = t2 -- subtrees not containing the var can be skipped
  | otherwise = case body of
    Var v' | v == v' -> replace ann -- var match; perform replacement
           | otherwise -> t2 -- var did not match one being substituted; ignore
    Cycle body -> cycle' ann (subst' replace v r body)
    Abs x _ | x == v -> t2 -- x shadows v; ignore subtree
    Abs x e -> abs' ann x' e'
      where x' = freshIn (fvs `Set.union` r) x
            -- rename x to something that cannot be captured by `r`
            e' = if x /= x' then subst' replace v r (rename x x' e)
                 else subst' replace v r e
    Tm body -> tm' ann (fmap (subst' replace v r) body)

-- Like `subst`, but the annotation of the replacement is inherited from
-- the previous annotation at each replacement point.
substInheritAnnotation :: (Foldable f, Functor f, Var v)
                       => v -> Term f v b -> Term f v a -> Term f v a
substInheritAnnotation v r =
  subst' (\ann -> const ann <$> r) v (freeVars r)

substsInheritAnnotation
  :: (Foldable f, Functor f, Var v)
  => [(v, Term f v b)]
  -> Term f v a
  -> Term f v a
substsInheritAnnotation replacements body =
  foldr (uncurry substInheritAnnotation) body (reverse replacements)

-- | `substs [(t1,v1), (t2,v2), ...] body` performs multiple simultaneous
-- substitutions, avoiding capture
substs
  :: (Foldable f, Functor f, Var v)
  => [(v, Term f v a)]
  -> Term f v a
  -> Term f v a
substs replacements body = foldr (uncurry subst) body (reverse replacements)

-- Count the number times the given variable appears free in the term
occurrences :: (Foldable f, Var v) => v -> Term f v a -> Int
occurrences v t | not (v `isFreeIn` t) = 0
occurrences v t = case out t of
  Var v2 -> if v == v2 then 1 else 0
  Cycle t -> occurrences v t
  Abs v2 t -> if v == v2 then 0 else occurrences v t
  Tm t -> foldl' (\s t -> s + occurrences v t) 0 $ Foldable.toList t

rebuildUp :: (Ord v, Foldable f, Functor f)
          => (f (Term f v a) -> f (Term f v a))
          -> Term f v a
          -> Term f v a
rebuildUp f (Term _ ann body) = case body of
  Var v -> annotatedVar ann v
  Cycle body -> cycle' ann (rebuildUp f body)
  Abs x e -> abs' ann x (rebuildUp f e)
  Tm body -> tm' ann (f $ fmap (rebuildUp f) body)

rebuildUp' :: (Ord v, Foldable f, Functor f)
          => (Term f v a -> Term f v a)
          -> Term f v a
          -> Term f v a
rebuildUp' f (Term _ ann body) = case body of
  Var v -> f (annotatedVar ann v)
  Cycle body -> f $ cycle' ann (rebuildUp' f body)
  Abs x e -> f $ abs' ann x (rebuildUp' f e)
  Tm body -> f $ tm' ann (fmap (rebuildUp' f) body)

freeVarOccurrences :: (Traversable f, Ord v) => Set v -> Term f v a -> [(v, a)]
freeVarOccurrences except t =
  [ (v, a) | (v,a) <- go $ annotateBound t, not (Set.member v except) ]
  where
  go e = case out e of
    Var v -> if Set.member v (snd $ annotation e)
             then []
             else [(v, fst $ annotation e)]
    Cycle body -> go body
    Abs _ body -> go body
    Tm body -> foldMap go body

foreachSubterm
  :: (Traversable f, Applicative g, Ord v)
  => (Term f v a -> g b)
  -> Term f v a
  -> g [b]
foreachSubterm f e = case out e of
  Var   _    -> pure <$> f e
  Cycle body -> (:) <$> f e <*> foreachSubterm f body
  Abs _ body -> (:) <$> f e <*> foreachSubterm f body
  Tm body ->
    (:)
      <$> f e
      <*> (join . Foldable.toList <$> traverse (foreachSubterm f) body)

subterms :: (Ord v, Traversable f) => Term f v a -> [Term f v a]
subterms t = runIdentity $ foreachSubterm pure t

-- | `visit f t` applies an effectful function to each subtree of
-- `t` and sequences the results. When `f` returns `Nothing`, `visit`
-- descends into the children of the current subtree. When `f` returns
-- `Just t2`, `visit` replaces the current subtree with `t2`. Thus:
-- `visit (const Nothing) t == pure t` and
-- `visit (const (Just (pure t2))) t == pure t2`
visit
  :: (Traversable f, Applicative g, Ord v)
  => (Term f v a -> Maybe (g (Term f v a)))
  -> Term f v a
  -> g (Term f v a)
visit f t = flip fromMaybe (f t) $ case out t of
  Var   _    -> pure t
  Cycle body -> cycle' (annotation t) <$> visit f body
  Abs x e    -> abs' (annotation t) x <$> visit f e
  Tm body    -> tm' (annotation t) <$> traverse (visit f) body

-- | Apply an effectful function to an ABT tree top down, sequencing the results.
visit' :: (Traversable f, Applicative g, Monad g, Ord v)
       => (f (Term f v a) -> g (f (Term f v a)))
       -> Term f v a
       -> g (Term f v a)
visit' f t = case out t of
  Var _ -> pure t
  Cycle body -> cycle' (annotation t) <$> visit' f body
  Abs x e -> abs' (annotation t) x <$> visit' f e
  Tm body -> f body >>= (fmap (tm' (annotation t)) . traverse (visit' f))

-- | `visit` specialized to the `Identity` effect.
visitPure :: (Traversable f, Ord v)
      => (Term f v a -> Maybe (Term f v a)) -> Term f v a -> Term f v a
visitPure f = runIdentity . visit (fmap pure . f)

rewriteDown :: (Traversable f, Ord v)
            => (Term f v a -> Term f v a)
            -> Term f v a
            -> Term f v a
rewriteDown f t = let t' = f t in case out t' of
  Var _ -> t'
  Cycle body -> cycle' (annotation t) (rewriteDown f body)
  Abs x e -> abs' (annotation t) x (rewriteDown f e)
  Tm body -> tm' (annotation t) (rewriteDown f `fmap` body)

data Subst f v a =
  Subst { freshen :: forall m v' . Monad m => (v -> m v') -> m v'
        , bind :: Term f v a -> Term f v a
        , bindInheritAnnotation :: forall b . Term f v b -> Term f v a
        , variable :: v }

unabs1 :: (Foldable f, Functor f, Var v) => Term f v a -> Maybe (Subst f v a)
unabs1 (Term _ _ (Abs v body)) = Just (Subst freshen bind bindInheritAnnotation v) where
  freshen f = f v
  bind x = subst v x body
  bindInheritAnnotation x = substInheritAnnotation v x body
unabs1 _ = Nothing

unabs :: Term f v a -> ([v], Term f v a)
unabs (Term _ _ (Abs hd body)) =
  let (tl, body') = unabs body in (hd : tl, body')
unabs t = ([], t)

reabs :: Ord v => [v] -> Term f v () -> Term f v ()
reabs vs t = foldr abs t vs

transform :: (Ord v, Foldable g, Functor f)
          => (forall a. f a -> g a) -> Term f v a -> Term g v a
transform f tm = case out tm of
  Var v -> annotatedVar (annotation tm) v
  Abs v body -> abs' (annotation tm) v (transform f body)
  Tm subterms ->
    let subterms' = fmap (transform f) subterms
    in tm' (annotation tm) (f subterms')
  Cycle body -> cycle' (annotation tm) (transform f body)

-- Rebuild the tree annotations upward, starting from the leaves,
-- using the Monoid to choose the annotation at intermediate nodes
reannotateUp :: (Ord v, Foldable f, Functor f, Monoid b)
  => (Term f v a -> b)
  -> Term f v a
  -> Term f v (a, b)
reannotateUp g t = case out t of
  Var v -> annotatedVar (annotation t, g t) v
  Cycle body ->
    let body' = reannotateUp g body
    in cycle' (annotation t, snd (annotation body')) body'
  Abs v body ->
    let body' = reannotateUp g body
    in abs' (annotation t, snd (annotation body')) v body'
  Tm body ->
    let
      body' = reannotateUp g <$> body
      ann = g t <> foldMap (snd . annotation) body'
    in tm' (annotation t, ann) body'

-- Find all subterms that match a predicate.  Prune the search for speed.
-- (Some patterns of pruning can cut the complexity of the search.)
data FindAction x = Found x | Prune | Continue deriving Show
find :: (Ord v, Foldable f, Functor f)
  => (Term f v a -> FindAction x)
  -> Term f v a
  -> [x]
find p t = case p t of
    Found x -> x : go
    Prune -> []
    Continue -> go
  where go = case out t of
          Var _ -> []
          Cycle body -> Unison.ABT.find p body
          Abs _ body -> Unison.ABT.find p body
          Tm body -> Foldable.concat (Unison.ABT.find p <$> body)

find' :: (Ord v, Foldable f, Functor f)
  => (Term f v a -> Bool)
  -> Term f v a
  -> [Term f v a]
find' p = Unison.ABT.find (\t -> if p t then Found t else Continue)

instance (Foldable f, Functor f, Eq1 f, Var v) => Eq (Term f v a) where
  -- alpha equivalence, works by renaming any aligned Abs ctors to use a common fresh variable
  t1 == t2 = go (out t1) (out t2) where
    go (Var v) (Var v2) | v == v2 = True
    go (Cycle t1) (Cycle t2) = t1 == t2
    go (Abs v1 body1) (Abs v2 body2) =
      if v1 == v2 then body1 == body2
      else let v3 = freshInBoth body1 body2 v1
           in rename v1 v3 body1 == rename v2 v3 body2
    go (Tm f1) (Tm f2) = f1 ==# f2
    go _ _ = False

instance (Foldable f, Functor f, Ord1 f, Var v) => Ord (Term f v a) where
  -- alpha equivalence, works by renaming any aligned Abs ctors to use a common fresh variable
  t1 `compare` t2 = go (out t1) (out t2) where
    go (Var v) (Var v2) = v `compare` v2
    go (Cycle t1) (Cycle t2) = t1 `compare` t2
    go (Abs v1 body1) (Abs v2 body2) =
      if v1 == v2 then body1 `compare` body2
      else let v3 = freshInBoth body1 body2 v1
           in rename v1 v3 body1 `compare` rename v2 v3 body2
    go (Tm f1) (Tm f2) = compare1 f1 f2
    go t1 t2 = tag t1 `compare` tag t2
    tag (Var _) = 0 :: Word
    tag (Tm _) = 1
    tag (Abs _ _) = 2
    tag (Cycle _) = 3

components :: Var v => [(v, Term f v a)] -> [[(v, Term f v a)]]
components = Components.components freeVars

-- Converts to strongly connected components while preserving the
-- order of definitions. Satisfies `join (orderedComponents bs) == bs`.
orderedComponents' :: Var v => [(v, Term f v a)] -> [[(v, Term f v a)]]
orderedComponents' tms = go [] Set.empty tms
  where
  go [] _ [] = []
  go [] deps (hd:rem) = go [hd] (deps <> freeVars (snd hd)) rem
  go cur deps rem = case findIndex isDep rem of
    Nothing -> reverse cur : let (hd,tl) = splitAt 1 rem
                             in go hd (depsFor hd) tl
    Just i  -> go (reverse newMembers ++ cur) deps' (drop (i+1) rem)
               where deps' = deps <> depsFor newMembers
                     newMembers = take (i+1) rem
    where
    depsFor = foldMap (freeVars . snd)
    isDep (v, _) = Set.member v deps

-- Like `orderedComponents'`, but further break up cycles and move
-- cyclic subcycles before other components in the same cycle.
-- Tweak suggested by @aryairani.
--
-- Example: given `[[x],[ping,r,s,pong]]`, where `ping` and `pong`
-- are mutually recursive but `r` and `s` are uninvolved, this produces:
-- `[[x], [ping,pong], [r], [s]]`.
orderedComponents :: Var v => [(v, Term f v a)] -> [[(v, Term f v a)]]
orderedComponents bs0 = tweak =<< orderedComponents' bs0 where
  tweak :: Var v => [(v,Term f v a)] -> [[(v,Term f v a)]]
  tweak bs@(_:_:_) = case takeWhile isCyclic (components bs) of
    [] -> [bs]
    cycles -> cycles <> orderedComponents rest
      where
      rest = [ (v,b) | (v,b) <- bs, Set.notMember v cycleVars ]
      cycleVars = Set.fromList (fst <$> join cycles)
  tweak bs = [bs] -- any cycle with < 2 bindings is left alone
  isCyclic [(v,b)] = Set.member v (freeVars b)
  isCyclic bs      = length bs > 1

-- Hash a strongly connected component and sort its definitions into a canonical order.
hashComponent ::
  (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Var v, Ord h, Accumulate h)
  => Map.Map v (Term f v a) -> (h, [(v, Term f v a)])
hashComponent byName = let
  ts = Map.toList byName
  embeds = [ (v, void (transform Embed t)) | (v,t) <- ts ]
  vs = fst <$> ts
  tms = [ (v, absCycle vs (tm $ Component (snd <$> embeds) (var v))) | v <- vs ]
  hashed  = [ ((v,t), hash t) | (v,t) <- tms ]
  sortedHashed = sortOn snd hashed
  overallHash = Hashable.accumulate (Hashable.Hashed . snd <$> sortedHashed)
  in (overallHash, [ (v, t) | ((v, _),_) <- sortedHashed, Just t <- [Map.lookup v byName] ])

-- Group the definitions into strongly connected components and hash
-- each component. Substitute the hash of each component into subsequent
-- components (using the `termFromHash` function). Requires that the
-- overall component has no free variables.
hashComponents
  :: (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Var v, Ord h, Accumulate h)
  => (h -> Word64 -> Word64 -> Term f v ())
  -> Map.Map v (Term f v a)
  -> [(h, [(v, Term f v a)])]
hashComponents termFromHash termsByName = let
  bound = Set.fromList (Map.keys termsByName)
  escapedVars = Set.unions (freeVars <$> Map.elems termsByName) `Set.difference` bound
  sccs = components (Map.toList termsByName)
  go _ [] = []
  go prevHashes (component : rest) = let
    sub = substsInheritAnnotation (Map.toList prevHashes)
    (h, sortedComponent) = hashComponent $ Map.fromList [ (v, sub t) | (v, t) <- component ]
    size = fromIntegral (length sortedComponent)
    curHashes = Map.fromList [ (v, termFromHash h i size) | ((v, _),i) <- sortedComponent `zip` [0..]]
    newHashes = prevHashes `Map.union` curHashes
    newHashesL = Map.toList newHashes
    sortedComponent' = [ (v, substsInheritAnnotation newHashesL t) | (v, t) <- sortedComponent ]
    in (h, sortedComponent') : go newHashes rest
  in if Set.null escapedVars then go Map.empty sccs
     else error $ "can't hashComponents if bindings have free variables:\n  "
               ++ show (map show (Set.toList escapedVars))
               ++ "\n  " ++ show (map show (Map.keys termsByName))

-- Implementation detail of hashComponent
data Component f a = Component [a] a | Embed (f a) deriving (Functor, Traversable, Foldable)

instance (Hashable1 f, Functor f) => Hashable1 (Component f) where
  hash1 hashCycle hash c = case c of
    Component as a -> let
      (hs, hash) = hashCycle as
      toks = Hashable.Hashed <$> hs
      in Hashable.accumulate $ (Hashable.Tag 1 : toks) ++ [Hashable.Hashed (hash a)]
    Embed fa -> Hashable.hash1 hashCycle hash fa

-- | We ignore annotations in the `Term`, as these should never affect the
-- meaning of the term.
hash :: forall f v a h . (Functor f, Hashable1 f, Eq v, Show v, Var v, Ord h, Accumulate h)
     => Term f v a -> h
hash = hash' [] where
  hash' :: [Either [v] v] -> Term f v a -> h
  hash' env (Term _ _ t) = case t of
    Var v -> maybe die hashInt ind
      where lookup (Left cycle) = v `elem` cycle
            lookup (Right v') = v == v'
            ind = findIndex lookup env
            hashInt :: Int -> h
            hashInt i = Hashable.accumulate [Hashable.Nat $ fromIntegral i]
            die = error $ "unknown var in environment: " ++ show v
                        ++ " environment = " ++ show env
    Cycle (AbsN' vs t) -> hash' (Left vs : env) t
    Cycle t -> hash' env t
    Abs v t -> hash' (Right v : env) t
    Tm t -> Hashable.hash1 (hashCycle env) (hash' env) t

  hashCycle :: [Either [v] v] -> [Term f v a] -> ([h], Term f v a -> h)
  hashCycle env@(Left cycle : envTl) ts | length cycle == length ts =
    let
      permute p xs = case Vector.fromList xs of xs -> map (xs !) p
      hashed = map (\(i,t) -> ((i,t), hash' env t)) (zip [0..] ts)
      pt = fst <$> sortOn snd hashed
      (p,ts') = unzip pt
    in case map Right (permute p cycle) ++ envTl of
      env -> (map (hash' env) ts', hash' env)
  hashCycle env ts = (map (hash' env) ts, hash' env)

-- | Use the `hash` function to efficiently remove duplicates from the list, preserving order.
distinct :: forall f v h a proxy . (Functor f, Hashable1 f, Eq v, Show v, Var v, Ord h, Accumulate h)
         => proxy h
         -> [Term f v a] -> [Term f v a]
distinct _ ts = fst <$> sortOn snd m
  where m = Map.elems (Map.fromList (hashes `zip` (ts `zip` [0 :: Int .. 1])))
        hashes = map hash ts :: [h]

-- | Use the `hash` function to remove elements from `t1s` that exist in `t2s`, preserving order.
subtract :: forall f v h a proxy . (Functor f, Hashable1 f, Eq v, Show v, Var v, Ord h, Accumulate h)
         => proxy h
         -> [Term f v a] -> [Term f v a] -> [Term f v a]
subtract _ t1s t2s =
  let skips = Set.fromList (map hash t2s :: [h])
  in filter (\t -> Set.notMember (hash t) skips) t1s

instance (Show1 f, Show v) => Show (Term f v a) where
  -- annotations not shown
  showsPrec p (Term _ _ out) = case out of
    Var v -> \x -> "Var " ++ show v ++ x
    Cycle body -> ("Cycle " ++) . showsPrec p body
    Abs v body -> showParen True $ (show v ++) . showString ". " . showsPrec p body
    Tm f -> showsPrec1 p f
