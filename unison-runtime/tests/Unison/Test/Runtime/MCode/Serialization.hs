{-# LANGUAGE OverloadedStrings #-}

-- | Round trip tests runtime serialization
module Unison.Test.Runtime.MCode.Serialization (Unison.Test.Runtime.MCode.Serialization.test) where

import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Primitive (Array, arrayFromList)
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)
import EasyTest qualified as EasyTest
import Hedgehog hiding (Rec, Test, test)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Unison.Prelude
import Unison.Runtime.Foreign.Function.Type (ForeignFunc)
import Unison.Runtime.Interface
import Unison.Runtime.MCode (Arg (..), Args (..), Prim1, Prim2, Branch, Comb, CombIx (..), GBranch (..), GComb (..), GCombInfo (..), GInstr (..), GRef (..), GSection (..), Instr, MLit (..), Ref, Section, UnboxedTypeTag (..))
import Unison.Runtime.Machine (Combs)
import Unison.Runtime.TypeTags (PackedTag (..))
import Unison.Test.Gen
import Unison.Util.EnumContainers (EnumMap, EnumSet)
import Unison.Util.EnumContainers qualified as EC

test :: EasyTest.Test ()
test =
  void . EasyTest.scope "mcode.serialization" $ do
    success <-
      EasyTest.io $
        checkParallel $
          Group
            "roundtrip"
            [ ("SCache", sCacheRoundtrip)
            ]
    EasyTest.expect success

genForeignCall :: Gen ForeignFunc
genForeignCall = Gen.enumBounded

genEnumMap :: (EC.EnumKey k) => Gen k -> Gen v -> Gen (EnumMap k v)
genEnumMap genK genV = EC.mapFromList <$> Gen.list (Range.linear 0 10) ((,) <$> genK <*> genV)

genEnumSet :: Gen Word64 -> Gen (EnumSet Word64)
genEnumSet gen = EC.setFromList <$> Gen.list (Range.linear 0 10) gen

genCombs :: Gen Combs
genCombs = genEnumMap genSmallWord64 genComb

genArray :: Gen a -> Gen (Array a)
genArray gen = arrayFromList <$> Gen.list (Range.linear 0 10) gen

genArg :: Gen Arg
genArg = Gen.choice [ MLit <$> genMLit, Ix <$> genSmallInt ]

genArgs :: Gen Args
genArgs =
  Gen.choice
    [ pure ZArgs,
      VArg1 <$> genArg,
      VArg2 <$> genArg <*> genArg,
      VArgR <$> genSmallInt <*> genSmallInt,
      VArgN <$> genArray genArg,
      VArgV <$> genSmallInt
    ]

genCombIx :: Gen CombIx
genCombIx =
  CIx
    <$> genReference
    <*> genSmallWord64
    <*> genSmallWord64

genGRef :: Gen Ref
genGRef =
  Gen.choice
    [ Stk <$> genSmallInt,
      -- For Env, we discard the comb when serializing and replace it with the CombIx anyways, so we do
      -- the same during generation to prevent false negatives in roundtrip tests.
      do
        cix <- genCombIx
        pure $ Env cix cix,
      Dyn <$> genSmallWord64
    ]

genBranch :: Gen Branch
genBranch =
  Gen.choice
    [ Test1 <$> genSmallWord64 <*> genSection <*> genSection,
      Test2 <$> genSmallWord64 <*> genSection <*> genSmallWord64 <*> genSection <*> genSection,
      TestW <$> genSection <*> genEnumMap genSmallWord64 genSection,
      TestT <$> genSection <*> Gen.map (Range.linear 0 10) ((,) <$> genUText <*> genSection)
    ]

genPrim1 :: Gen Prim1
genPrim1 = Gen.enumBounded

genPrim2 :: Gen Prim2
genPrim2 = Gen.enumBounded

genMLit :: Gen MLit
genMLit =
  Gen.choice
    [ MI <$> genSmallInt,
      MD <$> Gen.double (Range.linearFrac 0 100),
      MT <$> genUText,
      MM <$> genReferent,
      MY <$> genReference
    ]

genPackedTag :: Gen PackedTag
genPackedTag = PackedTag <$> genSmallWord64

genInstr :: Gen Instr
genInstr =
  Gen.choice
    [ Prim1 <$> genPrim1 <*> genSmallInt,
      PrimXX <$> genPrim2 <*> genSmallInt <*> genSmallInt,
      PrimIX <$> genPrim2 <*> genTypeTag <*> genSmallInt <*> genSmallInt,
      ForeignCall <$> Gen.bool <*> genForeignCall <*> genArgs,
      SetDyn <$> genSmallWord64 <*> genSmallInt,
      Capture <$> genSmallWord64,
      Name <$> genGRef <*> genArgs,
      Info <$> Gen.string (Range.linear 0 10) Gen.alphaNum,
      Pack <$> genReference <*> genPackedTag <*> genArgs,
      Lit <$> genMLit,
      Print <$> genSmallInt,
      Reset <$> genEnumSet genSmallWord64,
      Fork <$> genSmallInt,
      Atomically <$> genSmallInt,
      Seq <$> genArgs,
      TryForce <$> genSmallInt
    ]

genSection :: Gen Section
genSection = do
  Gen.recursive
    Gen.choice
    [ Yield <$> genArgs,
      Die <$> Gen.string (Range.linear 0 10) Gen.alphaNum,
      pure Exit
    ]
    [ App <$> Gen.bool <*> genGRef <*> genArgs,
      do
        b <- Gen.bool
        cix <- genCombIx
        args <- genArgs
        -- For Call, we discard the comb when serializing and replace it with the CombIx anyways, so we do
        -- the same during generation to prevent false negatives in roundtrip tests.
        pure $ Call b cix cix args,
      Match <$> genSmallInt <*> genBranch,
      Ins <$> genInstr <*> genSection,
      Let <$> genSection <*> genCombIx <*> genSmallInt <*> genSection,
      DMatch <$> Gen.maybe genReference <*> genSmallInt <*> genBranch,
      NMatch <$> Gen.maybe genReference <*> genSmallInt <*> genBranch,
      RMatch <$> genSmallInt <*> genSection <*> genEnumMap genSmallWord64 genBranch
    ]

genCombInfo :: Gen (GCombInfo CombIx)
genCombInfo =
  LamI
    <$> Gen.int (Range.linear 0 10)
    <*> Gen.int (Range.linear 0 10)
    <*> genSection

genComb :: Gen Comb
genComb =
  Gen.choice
    [ Comb <$> genCombInfo
    -- We omit cached closures from roundtrip tests since we don't currently serialize cached closure results
    -- CachedClosure
    ]

genTypeTag :: Gen UnboxedTypeTag
genTypeTag =
  Gen.choice
    [ pure CharTag
    , pure FloatTag
    , pure IntTag
    , pure NatTag
    ]

genStoredCache :: Gen StoredCache
genStoredCache =
  SCache
    <$> (genEnumMap genSmallWord64 genCombs)
    <*> (genEnumMap genSmallWord64 genReference)
    <*> (genEnumSet genSmallWord64)
    <*> (genEnumMap genSmallWord64 genReference)
    <*> genSmallWord64
    <*> genSmallWord64
    <*>
    -- We don't yet generate supergroups because generating valid ones is difficult.
    mempty
    <*> (Gen.map (Range.linear 0 10) ((,) <$> genReference <*> genSmallWord64))
    <*> (Gen.map (Range.linear 0 10) ((,) <$> genReference <*> genSmallWord64))
    <*> (Gen.map (Range.linear 0 10) ((,) <$> genReference <*> (Gen.set (Range.linear 0 10) genReference)))

sCacheRoundtrip :: Property
sCacheRoundtrip =
  getPutRoundtrip getStoredCache (putStoredCache) genStoredCache

getPutRoundtrip :: (Eq a, Show a) => Get a -> (a -> Put) -> Gen a -> Property
getPutRoundtrip get put builder =
  property $ do
    v <- forAll builder
    let bytes = runPutS (put v)
    runGetS get bytes === Right v
