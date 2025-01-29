module Unison.Merge.ThreeWay
  ( ThreeWay (..),
    forgetLca,
    toTwoOrThreeWay,
  )
where

import Unison.Merge.Internal.Types (ThreeWay (..))
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))

forgetLca :: ThreeWay a -> TwoWay a
forgetLca ThreeWay {alice, bob} =
  TwoWay {alice, bob}

toTwoOrThreeWay :: ThreeWay a -> TwoOrThreeWay a
toTwoOrThreeWay ThreeWay {alice, bob, lca} =
  TwoOrThreeWay {alice, bob, lca = Just lca}
