{-# LANGUAGE OverloadedStrings #-}

-- | Round trip tests for ANF serialization.
module Unison.Test.Runtime.ANF.Serialization (Unison.Test.Runtime.ANF.Serialization.test) where

import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Primitive.Array (Array)
import Data.Primitive.Array qualified as Array
import Data.Primitive.ByteArray (ByteArray)
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Primitive.Types (Prim)
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)
import EasyTest qualified as EasyTest
import Hedgehog hiding (Rec, Test, test)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Unison.Prelude
import Unison.Runtime.ANF
import Unison.Runtime.ANF.Serialize
import Unison.Test.Gen
import Unison.Util.Bytes qualified as Util.Bytes

test :: EasyTest.Test ()
test =
  void . EasyTest.scope "anf.serialization" $ do
    success <-
      EasyTest.io $
        checkParallel $
          Group
            "roundtrip"
            [ ("value", valueRoundtrip)
            ]
    EasyTest.expect success

genUBytes :: Gen Util.Bytes.Bytes
genUBytes = Util.Bytes.fromByteString <$> Gen.bytes (Range.linear 0 4)

genGroupRef :: Gen GroupRef
genGroupRef = GR <$> genReference <*> genSmallWord64

genUBValue :: Gen UBValue
genUBValue =
  Gen.choice
    [ -- Unboxed values are no longer valid in ANF serialization.
      -- Left <$> genSmallWord64,
      Right <$> genValue
    ]

genValList :: Gen ValList
genValList = Gen.list (Range.linear 0 4) genUBValue

genCont :: Gen Cont
genCont = do
  Gen.choice
    [ pure KE,
      Mark <$> genSmallWord64 <*> Gen.list (Range.linear 0 4) genReference <*> Gen.map (Range.linear 0 4) ((,) <$> genReference <*> genValue) <*> genCont,
      Push <$> genSmallWord64 <*> genSmallWord64 <*> genGroupRef <*> genCont
    ]

genArray :: Range Int -> Gen a -> Gen (Array a)
genArray range gen =
  Array.arrayFromList <$> Gen.list range gen

genByteArray :: (Prim p) => Gen p -> Gen ByteArray
genByteArray genP = do
  ByteArray.byteArrayFromList <$> Gen.list (Range.linear 0 20) genP

genBLit :: Gen BLit
genBLit =
  Gen.choice
    [ Text <$> genUText,
      List <$> Gen.seq (Range.linear 0 4) genValue,
      TmLink <$> genReferent,
      TyLink <$> genReference,
      Bytes <$> genUBytes,
      Quote <$> genValue,
      -- Code is not yet included, generating valid ANF terms is complex.
      -- , Code <$> genCode
      BArr <$> genByteArray genSmallWord64,
      Pos <$> genSmallWord64,
      Neg <$> genSmallWord64,
      Char <$> Gen.unicode,
      Float <$> Gen.double (Range.linearFrac 0 100),
      Arr <$> genArray (Range.linear 0 4) genValue
    ]

genValue :: Gen Value
genValue = Gen.sized \n -> do
  -- Limit amount of recursion to avoid infinitely deep values
  let gValList
        | n > 1 = Gen.small genValList
        | otherwise = pure []
  Gen.choice
    [ Partial <$> genGroupRef <*> gValList,
      Data <$> genReference <*> genSmallWord64 <*> gValList,
      Cont <$> gValList <*> genCont,
      BLit <$> genBLit
    ]

valueRoundtrip :: Property
valueRoundtrip =
  getPutRoundtrip getValue putValue genValue

getPutRoundtrip :: (Eq a, Show a) => (Version -> Get a) -> (Version -> a -> Put) -> Gen a -> Property
getPutRoundtrip get put builder =
  property $ do
    v <- forAll builder
    version <- forAll versionToTest
    let bytes = runPutS (put version v)
    runGetS (get version) bytes === Right v
  where
    versionToTest = do
      Gen.choice
        [ Transfer <$> Gen.enum 4 valueVersion,
          Hash <$> Gen.enum 4 valueVersion
        ]
