{-# LANGUAGE DerivingVia #-}

module U.Codebase.Sqlite.LocalIds where

import Control.Lens
import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Bits (Bits)
import Data.Vector (Vector)
import Data.Word (Word64)
import U.Codebase.Sqlite.DbId

-- | A mapping between index ids that are local to an object and the ids in the database
data LocalIds' t h = LocalIds
  { textLookup :: Vector t,
    defnLookup :: Vector h
  }
  deriving stock (Functor, Show, Eq)

type LocalIds = LocalIds' TextId ObjectId

type WatchLocalIds = LocalIds' TextId HashId

-- | represents an index into a textLookup
newtype LocalTextId = LocalTextId Word64 deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Bits) via Word64

-- | represents an index into a defnLookup
--
-- In this context, "definition" means an object that is either a term component or a (type) decl component, not a
-- patch, namespace, or any other kind of object.
newtype LocalDefnId = LocalDefnId Word64 deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Bits) via Word64

-- | a local index to a hash, used when the corresponding object is allowed to be absent
newtype LocalHashId = LocalHashId Word64 deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Bits) via Word64

newtype LocalPatchObjectId = LocalPatchObjectId Word64 deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Bits) via Word64

newtype LocalBranchChildId = LocalBranchChildId Word64 deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Bits) via Word64

-- | causal hashes are treated differently from HashIds, which don't have dependencies
newtype LocalCausalHashId = LocalCausalHashId Word64 deriving (Eq, Ord, Show, Num, Real, Enum, Integral, Bits) via Word64

instance Bitraversable LocalIds' where
  bitraverse f g (LocalIds t d) = LocalIds <$> traverse f t <*> traverse g d

instance Bifoldable LocalIds' where
  bifoldMap f g (LocalIds t d) = foldMap f t <> foldMap g d

instance Bifunctor LocalIds' where
  bimap f g (LocalIds t d) = LocalIds (f <$> t) (g <$> d)

t_ :: Traversal (LocalIds' t h) (LocalIds' t' h) t t'
t_ f (LocalIds t d) = LocalIds <$> traverse f t <*> pure d

h_ :: Traversal (LocalIds' t h) (LocalIds' t h') h h'
h_ f (LocalIds t d) = LocalIds <$> pure t <*> traverse f d
