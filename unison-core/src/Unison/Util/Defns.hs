module Unison.Util.Defns
  ( Defns (..),
    DefnsF,
    DefnsF2,
    DefnsF3,
    DefnsF4,
    terms_,
    types_,
    alignDefnsWith,
    defnsAreEmpty,
    fromTerms,
    fromTypes,
    hoistDefnsF,
    mapDefns,
    unzipDefns,
    unzipDefnsWith,
    zipDefns,
    zipDefnsWith,
    zipDefnsWith3,
    zipDefnsWith4,
  )
where

import Control.Lens (Lens)
import Data.Align (Semialign, alignWith)
import Data.Bifoldable (Bifoldable, bifoldMap)
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.These (These)
import Unison.Prelude

-- | Definitions (terms and types) in a namespace.
data Defns terms types = Defns
  { terms :: terms,
    types :: types
  }
  deriving stock (Generic, Functor, Show, Eq, Ord)
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

terms_ :: Lens (Defns terms types) (Defns terms' types) terms terms'
terms_ f (Defns x y) = (\x' -> Defns x' y) <$> f x

types_ :: Lens (Defns terms types) (Defns terms types') types types'
types_ f (Defns x y) = (\y' -> Defns x y') <$> f y

-- | A common shape of definitions - terms and types are stored in the same structure.
type DefnsF f terms types =
  Defns (f terms) (f types)

type DefnsF2 f g terms types =
  Defns (f (g terms)) (f (g types))

type DefnsF3 f g h terms types =
  Defns (f (g (h terms))) (f (g (h types)))

type DefnsF4 f g h i terms types =
  Defns (f (g (h (i terms)))) (f (g (h (i types))))

alignDefnsWith :: (Semialign f) => (These a b -> c) -> Defns (f a) (f b) -> f c
alignDefnsWith f defns =
  alignWith f defns.terms defns.types

defnsAreEmpty :: (Foldable f, Foldable g) => Defns (f a) (g b) -> Bool
defnsAreEmpty defns =
  null defns.terms && null defns.types

fromTerms :: (Monoid types) => terms -> Defns terms types
fromTerms terms =
  Defns {terms, types = mempty}

fromTypes :: (Monoid terms) => types -> Defns terms types
fromTypes types =
  Defns {terms = mempty, types}

hoistDefnsF :: (forall x. f x -> g x) -> DefnsF f a b -> DefnsF g a b
hoistDefnsF f (Defns x y) =
  Defns (f x) (f y)

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

zipDefnsWith3 ::
  (tm1 -> tm2 -> tm3 -> tm4) ->
  (ty1 -> ty2 -> ty3 -> ty4) ->
  Defns tm1 ty1 ->
  Defns tm2 ty2 ->
  Defns tm3 ty3 ->
  Defns tm4 ty4
zipDefnsWith3 f g (Defns terms1 types1) (Defns terms2 types2) (Defns terms3 types3) =
  Defns (f terms1 terms2 terms3) (g types1 types2 types3)

zipDefnsWith4 ::
  (tm1 -> tm2 -> tm3 -> tm4 -> tm5) ->
  (ty1 -> ty2 -> ty3 -> ty4 -> ty5) ->
  Defns tm1 ty1 ->
  Defns tm2 ty2 ->
  Defns tm3 ty3 ->
  Defns tm4 ty4 ->
  Defns tm5 ty5
zipDefnsWith4 f g (Defns terms1 types1) (Defns terms2 types2) (Defns terms3 types3) (Defns terms4 types4) =
  Defns (f terms1 terms2 terms3 terms4) (g types1 types2 types3 types4)
