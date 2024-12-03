{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Util.Recursion
  ( Algebra,
    Recursive (..),
    cataM,
    para,
    Fix (..),
    Cofree' (..),
  )
where

import Control.Arrow ((&&&))
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad ((<=<))

type Algebra f a = f a -> a

class Recursive t f | t -> f where
  cata :: (Algebra f a) -> t -> a
  default cata :: (Functor f) => (f a -> a) -> t -> a
  cata φ = φ . fmap (cata φ) . project
  project :: t -> f t
  default project :: (Functor f) => t -> f t
  project = cata (fmap embed)
  embed :: f t -> t
  {-# MINIMAL embed, (cata | project) #-}

cataM :: (Recursive t f, Traversable f, Monad m) => (f a -> m a) -> t -> m a
cataM φ = cata $ φ <=< sequenceA

para :: (Recursive t f, Functor f) => (f (t, a) -> a) -> t -> a
para φ = snd . cata (embed . fmap fst &&& φ)

newtype Fix f = Fix (f (Fix f))

deriving instance (forall a. (Show a) => Show (f a)) => Show (Fix f)

deriving instance (forall a. (Eq a) => Eq (f a)) => Eq (Fix f)

deriving instance (Eq (Fix f), forall a. (Ord a) => Ord (f a)) => Ord (Fix f)

instance (Functor f) => Recursive (Fix f) f where
  embed = Fix
  project (Fix f) = f

data Cofree' f a x = a :<< f x
  deriving (Foldable, Functor, Traversable)

-- |
--
--  __NB__: `Cofree` from “free” is lazy, so this instance is technically partial.
instance (Functor f) => Recursive (Cofree f a) (Cofree' f a) where
  embed (a :<< fco) = a :< fco
  project (a :< fco) = a :<< fco
