module Unison.Hashable
  ( accumulate',
    hash,
    Accumulate (..),
    Token (..),
  )
where

import Crypto.Hash qualified as CH
import Data.ByteArray qualified as BA
import Data.ByteString qualified as B
import Data.ByteString.Builder (doubleBE, int64BE, toLazyByteString, word64BE)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Prelude
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Relation3 (Relation3)
import Unison.Util.Relation3 qualified as Relation3
import Unison.Util.Relation4 (Relation4)
import Unison.Util.Relation4 qualified as Relation4

data Token h
  = Tag !Word8
  | Bytes !ByteString
  | Int !Int64
  | Text !Text
  | Double !Double
  | Hashed !h
  | Nat !Word64
  deriving stock (Show)

class Accumulate h where
  accumulate :: [Token h] -> h
  fromBytes :: ByteString -> h
  toBytes :: h -> ByteString

accumulateToken :: (Accumulate h, Hashable t) => t -> Token h
accumulateToken = Hashed . accumulate'

hash, accumulate' :: (Accumulate h, Hashable t) => t -> h
accumulate' = accumulate . tokens
hash = accumulate'

-- | NOTE: This typeclass is distinct from 'Unison.Hashing.V2.Hashable', which is the
-- content-based hashish class used for Unison types & terms.
--
-- This class however, is meant only to be used as a utility when hash-based identities are
-- useful in algorithms, the runtime, etc.
-- Consider carefully which class you want in each use-case.
class Hashable t where
  tokens :: (Accumulate h) => t -> [Token h]

instance (Hashable a) => Hashable [a] where
  tokens = map accumulateToken

instance (Hashable a, Hashable b) => Hashable (a, b) where
  tokens (a, b) = [accumulateToken a, accumulateToken b]

instance (Hashable a) => Hashable (Set.Set a) where
  tokens = tokens . Set.toList

instance (Hashable k, Hashable v) => Hashable (Map.Map k v) where
  tokens = tokens . Map.toList

instance (Hashable a, Hashable b) => Hashable (Relation a b) where
  tokens = tokens . Relation.toList

instance (Hashable d1, Hashable d2, Hashable d3) => Hashable (Relation3 d1 d2 d3) where
  tokens s = [accumulateToken $ Relation3.toNestedList s]

instance (Hashable d1, Hashable d2, Hashable d3, Hashable d4) => Hashable (Relation4 d1 d2 d3 d4) where
  tokens s = [accumulateToken $ Relation4.toNestedList s]

instance Hashable () where
  tokens _ = []

instance Hashable Double where
  tokens d = [Double d]

instance Hashable Text where
  tokens s = [Text s]

instance Hashable Char where
  tokens c = [Nat $ fromIntegral $ fromEnum c]

instance Hashable ByteString where
  tokens bs = [Bytes bs]

instance Hashable Word64 where
  tokens w = [Nat w]

instance Hashable Int64 where
  tokens w = [Int w]

instance Hashable Bool where
  tokens b = [Tag . fromIntegral $ fromEnum b]

instance Hashable Hash where
  tokens h = [Bytes (Hash.toByteString h)]

instance Accumulate Hash where
  accumulate = fromBytes . BA.convert . CH.hashFinalize . go CH.hashInit
    where
      go :: CH.Context CH.SHA3_512 -> [Token Hash] -> CH.Context CH.SHA3_512
      go acc tokens = CH.hashUpdates acc (tokens >>= toBS)
      toBS (Tag b) = [B.singleton b]
      toBS (Bytes bs) = [encodeLength $ B.length bs, bs]
      toBS (Int i) = BL.toChunks . toLazyByteString . int64BE $ i
      toBS (Nat i) = BL.toChunks . toLazyByteString . word64BE $ i
      toBS (Double d) = BL.toChunks . toLazyByteString . doubleBE $ d
      toBS (Text txt) =
        let tbytes = encodeUtf8 txt
         in [encodeLength (B.length tbytes), tbytes]
      toBS (Hashed h) = [Hash.toByteString h]
      encodeLength :: (Integral n) => n -> B.ByteString
      encodeLength = BL.toStrict . toLazyByteString . word64BE . fromIntegral
  fromBytes = Hash.fromByteString
  toBytes = Hash.toByteString
