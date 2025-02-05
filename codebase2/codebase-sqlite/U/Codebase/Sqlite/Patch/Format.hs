module U.Codebase.Sqlite.Patch.Format
  ( PatchFormat (..),
    PatchLocalIds,
    PatchLocalIds' (..),
    HashPatchLocalIds,
    SyncPatchFormat,
    SyncPatchFormat' (..),
    applyPatchDiffs,
    localPatchToPatch,
    localPatchToPatch',
    localPatchDiffToPatchDiff,
    localPatchToHashPatch,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import U.Codebase.HashTags
import U.Codebase.Sqlite.DbId (HashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalDefnId (LocalDefnId), LocalHashId (LocalHashId), LocalTextId (LocalTextId))
import U.Codebase.Sqlite.Patch.Diff (LocalPatchDiff, PatchDiff, PatchDiff' (..))
import U.Codebase.Sqlite.Patch.Diff qualified as Patch.Diff
import U.Codebase.Sqlite.Patch.Full (HashPatch, LocalPatch, Patch, Patch' (..))
import U.Codebase.Sqlite.Patch.Full qualified as Patch.Full
import Unison.Prelude

data PatchFormat
  = Full PatchLocalIds LocalPatch
  | Diff PatchObjectId PatchLocalIds LocalPatchDiff

type PatchLocalIds = PatchLocalIds' TextId HashId ObjectId

-- | LocalIds type which can be used in hashing the Patch.
type HashPatchLocalIds = PatchLocalIds' Text ComponentHash ComponentHash

data PatchLocalIds' t h d = LocalIds
  { patchTextLookup :: Vector t,
    patchHashLookup :: Vector h,
    patchDefnLookup :: Vector d
  }
  deriving stock (Eq, Show)

type SyncPatchFormat = SyncPatchFormat' PatchObjectId TextId HashId ObjectId

data SyncPatchFormat' parent text hash defn
  = SyncFull (PatchLocalIds' text hash defn) ByteString
  | -- | p is the identity of the thing that the diff is relative to
    SyncDiff parent (PatchLocalIds' text hash defn) ByteString
  deriving stock (Eq, Show)

-- | Apply a list of patch diffs to a patch, left to right.
applyPatchDiffs :: Patch -> [PatchDiff] -> Patch
applyPatchDiffs =
  foldl' apply
  where
    apply :: Patch -> PatchDiff -> Patch
    apply (Patch termEdits typeEdits) (PatchDiff addedTermEdits addedTypeEdits removedTermEdits removedTypeEdits) =
      let !termEdits' = addRemove addedTermEdits removedTermEdits termEdits
          !typeEdits' = addRemove addedTypeEdits removedTypeEdits typeEdits
       in Patch
            { termEdits = termEdits',
              typeEdits = typeEdits'
            }

    addRemove :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Map a (Set b) -> Map a (Set b)
    addRemove add del src =
      Map.unionWith (<>) add (Map.differenceWith remove src del)

    remove :: (Ord b) => Set b -> Set b -> Maybe (Set b)
    remove src del =
      let diff = Set.difference src del
       in if Set.null diff then Nothing else Just diff

localToPatch' :: (Ord t, Ord h, Ord d) => PatchLocalIds' t h d -> (Patch' LocalTextId LocalHashId LocalDefnId) -> Patch' t h d
localToPatch' li =
  Patch.Full.trimap (lookupPatchLocalText li) (lookupPatchLocalHash li) (lookupPatchLocalDefn li)

-- | Generic version of `localPatchToPatch` that works with any `PatchLocalIds'`.
localPatchToPatch' ::
  (Ord t, Ord h, Ord d) =>
  PatchLocalIds' t h d ->
  Patch' LocalTextId LocalHashId LocalDefnId ->
  Patch' t h d
localPatchToPatch' li =
  Patch.Full.trimap (lookupPatchLocalText li) (lookupPatchLocalHash li) (lookupPatchLocalDefn li)

-- | Type specialized version of `localToPatch'`.
localPatchToPatch :: PatchLocalIds -> LocalPatch -> Patch
localPatchToPatch = localToPatch'

localPatchToHashPatch :: HashPatchLocalIds -> LocalPatch -> HashPatch
localPatchToHashPatch = localToPatch'

localPatchDiffToPatchDiff :: PatchLocalIds -> LocalPatchDiff -> PatchDiff
localPatchDiffToPatchDiff li =
  Patch.Diff.trimap
    (lookupPatchLocalText li)
    (lookupPatchLocalHash li)
    (lookupPatchLocalDefn li)

lookupPatchLocalText :: PatchLocalIds' t h d -> LocalTextId -> t
lookupPatchLocalText li (LocalTextId w) = patchTextLookup li Vector.! fromIntegral w

lookupPatchLocalHash :: PatchLocalIds' t h d -> LocalHashId -> h
lookupPatchLocalHash li (LocalHashId w) = patchHashLookup li Vector.! fromIntegral w

lookupPatchLocalDefn :: PatchLocalIds' t h d -> LocalDefnId -> d
lookupPatchLocalDefn li (LocalDefnId w) = patchDefnLookup li Vector.! fromIntegral w
