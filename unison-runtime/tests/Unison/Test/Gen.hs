-- | Hedgehog generators for common unison types.
module Unison.Test.Gen where

import Data.Text qualified as Text
import Hedgehog hiding (Rec, Test, test)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Unison.ConstructorReference
import Unison.ConstructorType qualified as CT
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Util.Text qualified as Unison.Text

genSmallWord64 :: Gen Word64
genSmallWord64 = Gen.word64 (Range.linear 0 100)

genSmallInt :: Gen Int
genSmallInt = Gen.int (Range.linear 0 100)

genReference :: Gen Reference.Reference
genReference =
  Gen.choice
    [ Reference.ReferenceBuiltin <$> genSmallText,
      Reference.ReferenceDerived <$> genRefId
    ]
  where
    genRefId :: Gen (Reference.Id' Hash)
    genRefId = Reference.Id <$> genHash <*> genSmallWord64

-- This can generate invalid hashes, but that's not really an issue for testing serialization.
genHash :: Gen Hash
genHash = Hash.fromByteString <$> Gen.bytes (Range.singleton 32)

genReferent :: Gen Referent.Referent
genReferent =
  Gen.choice
    [ Referent.Ref <$> genReference,
      Referent.Con <$> genConstructorReference <*> genConstructorType
    ]
  where
    genConstructorType = Gen.choice [pure CT.Data, pure CT.Effect]
    genConstructorReference = ConstructorReference <$> genReference <*> genSmallWord64

genSmallText :: Gen Text
genSmallText = Gen.text (Range.linear 2 4) Gen.alphaNum

genUText :: Gen Unison.Text.Text
genUText = Unison.Text.pack . Text.unpack <$> genSmallText
