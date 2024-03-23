{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Util.Defns
  ( Defns (..),
    DefnsF,
    alignDefnsWith,
    defnsAreEmpty,
    mapDefns,
    unzipDefns,
    unzipDefnsWith,
    zipDefns,
    zipDefnsWith,
  )
where

import Data.Align (Semialign, alignWith)
import Data.Bifoldable (Bifoldable, bifoldMap, binull)
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.These (These)
import Unison.Prelude

-- | Definitions (terms and types) in a namespace.
data Defns terms types = Defns
  { terms :: terms,
    types :: types
  }
  deriving stock (Generic, Show)
  deriving (Monoid, Semigroup) via GenericSemigroupMonoid (Defns terms types)

instance Bifoldable Defns where
  bifoldMap f g (Defns x y) =
    f x <> g y

instance Bifunctor Defns where
  bimap f g (Defns x y) =
    Defns (f x) (g y)

instance Bitraversable Defns where
  bitraverse f g (Defns x y) =
    Defns <$> f x <*> g y

-- | A common shape of definitions - terms and types are stored in the same structure.
type DefnsF f terms types =
  Defns (f terms) (f types)

alignDefnsWith :: Semialign f => (These a b -> c) -> Defns (f a) (f b) -> f c
alignDefnsWith f defns =
  alignWith f defns.terms defns.types

defnsAreEmpty :: (Foldable f, Foldable g) => Defns (f a) (g b) -> Bool
defnsAreEmpty =
  binull

mapDefns :: (a -> b) -> Defns a a -> Defns b b
mapDefns f =
  bimap f f

unzipDefns :: Defns (tm1, tm2) (ty1, ty2) -> (Defns tm1 ty1, Defns tm2 ty2)
unzipDefns =
  unzipDefnsWith id id

unzipDefnsWith :: (tm1 -> (tm2, tm3)) -> (ty1 -> (ty2, ty3)) -> Defns tm1 ty1 -> (Defns tm2 ty2, Defns tm3 ty3)
unzipDefnsWith f g (Defns terms1 types1) =
  let (terms2, terms3) = f terms1
      (types2, types3) = g types1
   in (Defns terms2 types2, Defns terms3 types3)

zipDefns :: Defns tm1 ty1 -> Defns tm2 ty2 -> Defns (tm1, tm2) (ty1, ty2)
zipDefns =
  zipDefnsWith (,) (,)

zipDefnsWith :: (tm1 -> tm2 -> tm3) -> (ty1 -> ty2 -> ty3) -> Defns tm1 ty1 -> Defns tm2 ty2 -> Defns tm3 ty3
zipDefnsWith f g (Defns terms1 types1) (Defns terms2 types2) =
  Defns (f terms1 terms2) (g types1 types2)
