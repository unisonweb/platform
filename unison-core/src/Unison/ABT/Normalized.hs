{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.ABT.Normalized
  ( ABT (..),
    Term (.., TAbs, TTm, TAbss),
    Align (..),
    alpha,
    renames,
    rename,
    transform,
  )
where

import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable (toList)
-- import Data.Bitraversable

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Unison.ABT (Var (..))

-- ABTs with support for 'normalized' structure where only variables
-- may occur at some positions. This is accomplished by passing the
-- variable type to the base functor.
data ABT f v
  = Abs v (Term f v)
  | Tm (f v (Term f v))

data Term f v = Term
  { freeVars :: Set v,
    out :: ABT f v
  }

instance
  (forall a b. (Show a) => (Show b) => Show (f a b), Show v) =>
  Show (ABT f v)
  where
  showsPrec p a = showParen (p >= 9) $ case a of
    Abs v tm ->
      showString "Abs "
        . showsPrec 10 v
        . showString " "
        . showsPrec 10 tm
    Tm e -> showString "Tm " . showsPrec 10 e

instance
  (forall a b. (Show a) => (Show b) => Show (f a b), Show v) =>
  Show (Term f v)
  where
  showsPrec p (Term _ e) =
    showParen (p >= 9) $ showString "Term " . showsPrec 10 e

instance
  (forall a b. (Eq a) => (Eq b) => Eq (f a b), Bifunctor f, Bifoldable f, Var v) =>
  Eq (ABT f v)
  where
  Abs v1 e1 == Abs v2 e2
    | v1 == v2 = e1 == e2
    | otherwise = e1 == rename v2 v1 e2
  Tm e1 == Tm e2 = e1 == e2
  _ == _ = False

instance
  (forall a b. (Eq a) => (Eq b) => Eq (f a b), Bifunctor f, Bifoldable f, Var v) =>
  Eq (Term f v)
  where
  Term _ abt1 == Term _ abt2 = abt1 == abt2

pattern TAbs :: (Var v) => v -> Term f v -> Term f v
pattern TAbs u bd <-
  Term _ (Abs u bd)
  where
    TAbs u bd = Term (Set.delete u (freeVars bd)) (Abs u bd)

pattern TTm :: (Var v, Bifoldable f) => f v (Term f v) -> Term f v
pattern TTm bd <-
  Term _ (Tm bd)
  where
    TTm bd = Term (bifoldMap Set.singleton freeVars bd) (Tm bd)

{-# COMPLETE TAbs, TTm #-}

class (Bifoldable f, Bifunctor f) => Align f where
  align ::
    (Applicative g) =>
    (vl -> vr -> g vs) ->
    (el -> er -> g es) ->
    f vl el ->
    f vr er ->
    Maybe (g (f vs es))

alphaErr ::
  (Align f) => (Var v) => Map v v -> Term f v -> Term f v -> Either (Term f v, Term f v) a
alphaErr un tml tmr = Left (tml, renames0 count un tmr)
  where
    count = Map.fromListWith (+) . flip zip [1, 1 ..] $ toList un

-- Checks if two terms are equal up to a given variable renaming. The
-- renaming should map variables in the right hand term to the
-- equivalent variable in the left hand term.
alpha :: (Align f) => (Var v) => Map v v -> Term f v -> Term f v -> Either (Term f v, Term f v) ()
alpha un (TAbs u tml) (TAbs v tmr) =
  alpha (Map.insert v u (Map.filter (/= u) un)) tml tmr
alpha un tml@(TTm bdl) tmr@(TTm bdr)
  | Just sub <- align av (alpha un) bdl bdr = () <$ sub
  where
    av u v
      | maybe False (== u) (Map.lookup v un) = pure ()
      | otherwise = alphaErr un tml tmr
alpha un tml tmr = alphaErr un tml tmr

unabss :: (Var v) => Term f v -> ([v], Term f v)
unabss (TAbs v (unabss -> (vs, bd))) = (v : vs, bd)
unabss bd = ([], bd)

pattern TAbss :: (Var v) => [v] -> Term f v -> Term f v
pattern TAbss vs bd <-
  (unabss -> (vs, bd))
  where
    TAbss vs bd = foldr TAbs bd vs

{-# COMPLETE TAbss #-}

-- Simultaneous variable renaming implementation.
--
-- subvs0 counts the number of variables being renamed to a particular
-- variable
--
-- rnv0 is the variable renaming map.
renames0 ::
  (Var v, Ord v, Bifunctor f, Bifoldable f) =>
  Map v Int ->
  Map v v ->
  Term f v ->
  Term f v
renames0 subvs0 rnv0 tm = case tm of
  TAbs u body
    | not $ Map.null rnv' -> TAbs u' (renames0 subvs' rnv' body)
    where
      rnv' = Map.alter (const $ adjustment) u rnv
      -- if u is in the set of variables we're substituting in, it
      -- needs to be renamed to avoid capturing things.
      u'
        | u `Map.member` subvs = freshIn (fvs `Set.union` Map.keysSet subvs) u
        | otherwise = u

      -- if u needs to be renamed to avoid capturing subvs
      -- and u actually occurs in the body, then add it to
      -- the substitutions
      (adjustment, subvs')
        | u /= u' && u `Set.member` fvs = (Just u', Map.insertWith (+) u' 1 subvs)
        | otherwise = (Nothing, subvs)
  TTm body
    | not $ Map.null rnv ->
        TTm $ bimap (\u -> Map.findWithDefault u u rnv) (renames0 subvs rnv) body
  _ -> tm
  where
    fvs = freeVars tm

    -- throw out irrelevant renamings
    rnv = Map.restrictKeys rnv0 fvs

    -- decrement the variable usage counts for the renamings we threw away
    subvs = Map.foldl' decrement subvs0 $ Map.withoutKeys rnv0 fvs
    decrement sv v = Map.update drop v sv
    drop n
      | n <= 1 = Nothing
      | otherwise = Just (n - 1)

-- Simultaneous variable renaming.
renames ::
  (Var v, Ord v, Bifunctor f, Bifoldable f) =>
  Map v v ->
  Term f v ->
  Term f v
renames rnv tm = renames0 subvs rnv tm
  where
    subvs = Map.fromListWith (+) . fmap (,1) $ Map.elems rnv

rename ::
  (Var v, Ord v, Bifunctor f, Bifoldable f) =>
  v ->
  v ->
  Term f v ->
  Term f v
rename old new = renames0 (Map.singleton new 1) (Map.singleton old new)

transform ::
  (Var v, Bifunctor g, Bifoldable f, Bifoldable g) =>
  (forall a b. f a b -> g a b) ->
  Term f v ->
  Term g v
transform phi (TTm body) = TTm . second (transform phi) $ phi body
transform phi (TAbs u body) = TAbs u $ transform phi body
