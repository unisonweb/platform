{-# LANGUAGE OverloadedStrings #-}

-- | Roundtrip tests for types used in sync.
module Unison.Test.Sync.Roundtrip (Unison.Test.Sync.Roundtrip.test) where

import Codec.Serialise qualified as Serialise
import EasyTest qualified as EasyTest
import Hedgehog hiding (Test, test)
import Unison.Prelude
import Unison.Server.Orphans ()
import Unison.Test.Sync.Gen qualified as Gen

test :: EasyTest.Test ()
test =
  void . EasyTest.scope "syncv2.roundtrip" $ do
    success <-
      EasyTest.io $
        checkParallel $
          Group
            "syncv2.roundtrip"
            [ ("termComponentRoundtrip", termComponentRoundtrip)
            ]
    EasyTest.expect success

termComponentRoundtrip :: Property
termComponentRoundtrip =
  property $ do
    te <- forAll $ Gen.genTempEntity
    (Serialise.deserialise . Serialise.serialise $ te) === te
