module Unison.Hashing.V2.Tokenizable
  ( Tokenizable (..),
    Hashable1 (..),
    Token (..),
    hashTokenizable,
    accumulate,
    accumulateToken,
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

-- | The version of the current hashing function.
-- This should be incremented every time the hashing function is changed.
--
-- The reasoning is that, if a change to the hashing function changes the hashes for _some_
-- values, it should change it for _all_ values so that we don't have collisions between
-- different hashing function versions. If we don't do this, it's possible for the hashes of
-- simple types (like an Int for example) to keep the same hashes, which would lead to
-- collisions in the `hash` table, since each hash has a different hash version but the same
-- base32 representation.
hashingVersion :: Token
hashingVersion = Tag 2

data Token
  = Tag !Word8
  | Bytes !ByteString
  | Int !Int64
  | Text !Text
  | Double !Double
  | Hashed !Hash
  | Nat !Word64

accumulateToken :: (Tokenizable t) => t -> Token
accumulateToken = Hashed . hashTokenizable

-- | Tokenize then accumulate a type into a Hash.
hashTokenizable :: (Tokenizable t) => t -> Hash
hashTokenizable = accumulate . tokens

-- | Tokenizable converts a value into a set of hashing tokens which will later be accumulated
-- into a Hash. Be very careful when adding or altering instances of this typeclass, changing
-- the hash of a value is a major breaking change and requires a complete codebase migration.
--
-- If you simply want to provide a convenience instance for a type which wraps some Hashable
-- type, write an instance of 'Hashable' which calls through to the inner instance instead.
--
-- E.g. If I want to be able to hash a @TaggedBranch@ using its Branch0 hashable instance:
--
-- @@
-- data TaggedBranch = TaggedBranch String Branch
--
-- instance Hashable TaggedBranch where
--   hash (TaggedBranch _ b) = hash b
-- @@
class Tokenizable t where
  tokens :: t -> [Token]

instance (Tokenizable a) => Tokenizable [a] where
  tokens = map accumulateToken

instance (Tokenizable a, Tokenizable b) => Tokenizable (a, b) where
  tokens (a, b) = [accumulateToken a, accumulateToken b]

instance (Tokenizable a) => Tokenizable (Set.Set a) where
  tokens = tokens . Set.toList

instance (Tokenizable k, Tokenizable v) => Tokenizable (Map.Map k v) where
  tokens = tokens . Map.toList

instance Tokenizable Double where
  tokens d = [Double d]

instance Tokenizable Text where
  tokens s = [Text s]

instance Tokenizable Char where
  tokens c = [Nat $ fromIntegral $ fromEnum c]

instance Tokenizable Word64 where
  tokens w = [Nat w]

instance Tokenizable Int64 where
  tokens w = [Int w]

instance Tokenizable Bool where
  tokens b = [Tag . fromIntegral $ fromEnum b]

instance Tokenizable Hash where
  tokens h = [Bytes (Hash.toByteString h)]

accumulate :: [Token] -> Hash
accumulate = Hash.fromByteString . BA.convert . CH.hashFinalize . go CH.hashInit
  where
    go :: CH.Context CH.SHA3_512 -> [Token] -> CH.Context CH.SHA3_512
    go acc tokens = CH.hashUpdates acc (hashingVersion : tokens >>= toBS)
    toBS (Tag b) = [B.singleton b]
    toBS (Bytes bs) = [encodeLength $ B.length bs, bs]
    toBS (Int i) = [BL.toStrict . toLazyByteString . int64BE $ i]
    toBS (Nat i) = [BL.toStrict . toLazyByteString . word64BE $ i]
    toBS (Double d) = [BL.toStrict . toLazyByteString . doubleBE $ d]
    toBS (Text txt) =
      let tbytes = encodeUtf8 txt
       in [encodeLength (B.length tbytes), tbytes]
    toBS (Hashed h) = [Hash.toByteString h]
    encodeLength :: (Integral n) => n -> B.ByteString
    encodeLength = BL.toStrict . toLazyByteString . word64BE . fromIntegral

class Hashable1 f where
  -- | Produce a hash for an `f a`, given a hashing function for `a`.
  -- If there is a notion of order-independence in some aspect of a subterm
  -- of `f`, then the first argument (`hashUnordered :: [a] -> ([h], a -> h)`)
  -- should be used to impose an order, and then apply that order in further hashing.
  -- Otherwise the second argument (`hash :: a -> h`) should be used.
  --
  -- Example 1: A simple functor with no unordered components. Hashable1 instance
  --            just uses `hash`:
  --
  --   data T a = One a | Two a a deriving Functor
  --
  --   instance Hashable1 T where
  --     hash1 _ hash t = case t of
  --       One a -> accumulate [Tag 0, Hashed (hash a)]
  --       Two a a2 -> accumulate [Tag 1, Hashed (hash a), Hashed (hash a2)]
  --
  -- Example 2: A functor with unordered components. For hashing, we need to
  --            pick a canonical ordering of the unordered components, so we
  --            use `hashUnordered`:
  --
  --   data U a = U { unordered :: [a], uno :: a, dos :: a } deriving Functor
  --
  --   instance Hashable1 U where
  --     hash1 hashUnordered _ (U unordered uno dos) =
  --       let (hs, hash) = hashUnordered unordered
  --       in accumulate $ map Hashed hs ++ [Hashed (hash uno), Hashed (hash dos)]
  hash1 :: ([a] -> ([Hash], a -> Hash)) -> (a -> Hash) -> f a -> Hash
