module Unison.Merge.ThreeWay
  ( ThreeWay (..),
    forgetLca,
    toTwoOrThreeWay,
  )
where

import Data.Semialign (Semialign (alignWith), Unzip (unzipWith), Zip (zipWith))
import Data.These (These (..))
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Prelude

data ThreeWay a = ThreeWay
  { lca :: !a,
    alice :: !a,
    bob :: !a
  }
  deriving stock (Foldable, Functor, Generic, Traversable)

instance Applicative ThreeWay where
  pure :: a -> ThreeWay a
  pure x =
    ThreeWay x x x

  (<*>) :: ThreeWay (a -> b) -> ThreeWay a -> ThreeWay b
  ThreeWay f g h <*> ThreeWay x y z =
    ThreeWay (f x) (g y) (h z)

instance Semialign ThreeWay where
  alignWith :: (These a b -> c) -> ThreeWay a -> ThreeWay b -> ThreeWay c
  alignWith f (ThreeWay a b c) (ThreeWay x y z) =
    ThreeWay (f (These a x)) (f (These b y)) (f (These c z))

instance Unzip ThreeWay where
  unzipWith :: (c -> (a, b)) -> ThreeWay c -> (ThreeWay a, ThreeWay b)
  unzipWith f (ThreeWay a b c) =
    let (i, x) = f a
        (j, y) = f b
        (k, z) = f c
     in (ThreeWay i j k, ThreeWay x y z)

instance Zip ThreeWay where
  zipWith :: (a -> b -> c) -> ThreeWay a -> ThreeWay b -> ThreeWay c
  zipWith f (ThreeWay a b c) (ThreeWay x y z) =
    ThreeWay (f a x) (f b y) (f c z)

forgetLca :: ThreeWay a -> TwoWay a
forgetLca ThreeWay {alice, bob} =
  TwoWay {alice, bob}

toTwoOrThreeWay :: ThreeWay a -> TwoOrThreeWay a
toTwoOrThreeWay ThreeWay {alice, bob, lca} =
  TwoOrThreeWay {alice, bob, lca = Just lca}
