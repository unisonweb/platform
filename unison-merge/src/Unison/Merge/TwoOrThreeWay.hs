module Unison.Merge.TwoOrThreeWay
  ( TwoOrThreeWay (..),
    toThreeWay,
  )
where

import Unison.Merge.Internal.Types (ThreeWay (..), TwoOrThreeWay (..))
import Unison.Prelude

toThreeWay :: a -> TwoOrThreeWay a -> ThreeWay a
toThreeWay x TwoOrThreeWay {alice, bob, lca} =
  ThreeWay {alice, bob, lca = fromMaybe x lca}
