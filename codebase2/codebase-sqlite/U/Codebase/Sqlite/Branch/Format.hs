module U.Codebase.Sqlite.Branch.Format
  ( BranchFormat' (..),
    BranchFormat,
    HashBranchFormat,
    BranchLocalIds,
    BranchLocalIds' (..),
    HashBranchLocalIds,
    SyncBranchFormat,
    SyncBranchFormat' (..),
    LocalBranchBytes (..),
    localToDbBranch,
    localToDbDiff,
    localToHashBranch,
    localToBranch,
    -- dbToLocalDiff,
  )
where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import U.Codebase.HashTags
import U.Codebase.Sqlite.Branch.Diff (Diff, LocalDiff)
import U.Codebase.Sqlite.Branch.Diff qualified as Branch.Diff
import U.Codebase.Sqlite.Branch.Full (DbBranch, HashBranch, LocalBranch)
import U.Codebase.Sqlite.Branch.Full qualified as Branch.Full
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds
  ( LocalBranchChildId (..),
    LocalDefnId (..),
    LocalPatchObjectId (..),
    LocalTextId (..),
  )
import Unison.Prelude

-- | A 'BranchFormat' is a deserialized namespace object (@object.bytes@).
--
-- you can use the exact same `BranchLocalIds` when converting between `Full` and `Diff`
data
  BranchFormat'
    text
    defRef
    patchRef
    childRef
    branchRef
  = Full (BranchLocalIds' text defRef patchRef childRef) LocalBranch
  | Diff branchRef (BranchLocalIds' text defRef patchRef childRef) LocalDiff
  deriving (Show)

-- | The 'BranchFormat'' used to store a branch in Sqlite
type BranchFormat = BranchFormat' TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId) BranchObjectId

-- | A BranchFormat which uses Hashes and Text for all its references, no
-- Ids which are specific to a particular codebase.
type HashBranchFormat = BranchFormat' Text ComponentHash PatchHash (BranchHash, CausalHash)

-- = Full BranchLocalIds LocalBranch
-- \| Diff BranchObjectId BranchLocalIds LocalDiff

-- | A 'BranchLocalIds' is a mapping between local ids (local to this object) encoded as offsets, and actual database ids.
--
-- For example, a @branchTextLookup@ vector of @[50, 74]@ means "local id 0 corresponds to database text id 50, and
-- local id 1 corresponds to database text id 74".
type BranchLocalIds = BranchLocalIds' TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId)

type HashBranchLocalIds = BranchLocalIds' Text ComponentHash PatchHash (BranchHash, CausalHash)

-- temp_entity
--  branch #foo
--
-- temp_entity_missing_dependency
--   #foo depends on causal #bar
--   #foo depends on namespace #baz
--
-- 1. store causal #bar, go to flush dependencies like normal
-- 2. ... oh this case is different than the others - we don't want to delete that row

----
-- can't simply treat causal's value hash as a mandatory dependency because we can't be sure
-- that the causal doesn't already exist in the target codebase without the value.
-- it probably does exist together with the value (though we found cases in the past where it didn't
-- due to race conditions, but we fixed that and added transactions and it shouldn't happen again?),
-- but it's not enforced at the schema level.
-- to enforce it at the schema level, we'd have to do something like store namespace_object_id instead
-- of value_hash in causal, which would require a db migration for a thing we don't necessarily even want
-- long term.
-- so, we can't simply "require" the value hash as a dependency of the causals and expect things to work smoothly
-- without relying on prayer.
--
-- temp_entity
--  branch #foo
--  causal #bar

-- temp_entity_missing_dependency
--   #foo depends on causal #bar
--   #bar depends on namespace #baz
--

data BranchLocalIds' t d p c = LocalIds
  { branchTextLookup :: Vector t,
    branchDefnLookup :: Vector d,
    branchPatchLookup :: Vector p,
    branchChildLookup :: Vector c
  }
  deriving (Show, Eq)

-- | Bytes encoding a LocalBranch
newtype LocalBranchBytes = LocalBranchBytes ByteString
  deriving (Show, Eq, Ord)

data SyncBranchFormat' parent text defn patch child
  = SyncFull (BranchLocalIds' text defn patch child) LocalBranchBytes
  | SyncDiff parent (BranchLocalIds' text defn patch child) LocalBranchBytes
  deriving (Eq, Show)

type SyncBranchFormat = SyncBranchFormat' BranchObjectId TextId ObjectId PatchObjectId (BranchObjectId, CausalHashId)

localToBranch :: (Ord t, Ord d) => BranchLocalIds' t d p c -> LocalBranch -> (Branch.Full.Branch' t d p c)
localToBranch li =
  Branch.Full.quadmap (lookupBranchLocalText li) (lookupBranchLocalDefn li) (lookupBranchLocalPatch li) (lookupBranchLocalChild li)

localToDbBranch :: BranchLocalIds -> LocalBranch -> DbBranch
localToDbBranch = localToBranch

localToHashBranch :: HashBranchLocalIds -> LocalBranch -> HashBranch
localToHashBranch = localToBranch

localToDbDiff :: BranchLocalIds -> LocalDiff -> Diff
localToDbDiff li =
  Branch.Diff.quadmap (lookupBranchLocalText li) (lookupBranchLocalDefn li) (lookupBranchLocalPatch li) (lookupBranchLocalChild li)

lookupBranchLocalText :: BranchLocalIds' t d p c -> LocalTextId -> t
lookupBranchLocalText li (LocalTextId w) = branchTextLookup li Vector.! fromIntegral w

lookupBranchLocalDefn :: BranchLocalIds' t d p c -> LocalDefnId -> d
lookupBranchLocalDefn li (LocalDefnId w) = branchDefnLookup li Vector.! fromIntegral w

lookupBranchLocalPatch :: BranchLocalIds' t d p c -> LocalPatchObjectId -> p
lookupBranchLocalPatch li (LocalPatchObjectId w) = branchPatchLookup li Vector.! fromIntegral w

lookupBranchLocalChild :: BranchLocalIds' t d p c -> LocalBranchChildId -> c
lookupBranchLocalChild li (LocalBranchChildId w) = branchChildLookup li Vector.! fromIntegral w
