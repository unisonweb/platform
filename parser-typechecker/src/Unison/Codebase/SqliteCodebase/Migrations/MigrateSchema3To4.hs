{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema3To4 (migrateSchema3To4) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Generics.Product
import Data.Map qualified as Map
import Data.Semigroup
import Data.Set.Lens (setOf)
import U.Codebase.Sqlite.Branch.Format qualified as S.BranchFormat
import U.Codebase.Sqlite.Branch.Full qualified as DBBranch
import U.Codebase.Sqlite.DbId qualified as DB
import U.Codebase.Sqlite.LocalizeObject qualified as S.LocalizeObject
import U.Codebase.Sqlite.Operations qualified as Ops
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Serialization qualified as S
import U.Codebase.Sync qualified as Sync
import U.Util.Serialization qualified as S
import Unison.Codebase.SqliteCodebase.Migrations.Helpers (abortMigration)
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2.DbHelpers qualified as Helpers
import Unison.Debug qualified as Debug
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Prelude hiding (log)

data MigrationState = MigrationState
  { -- A mapping from a causal hash to the _corrected_ and _canonicalized_ branch hash and
    -- object.
    _canonicalBranchForCausalHashId :: Map DB.CausalHashId (DB.BranchHashId, DB.BranchObjectId),
    -- A mapping of branch hashes which were found to be correct and don't need to be
    -- re-hashed/re-canonicalized, it allows us to skip some redundant work.
    _validBranchHashIds :: Map DB.BranchHashId DB.BranchObjectId,
    _numMigrated :: Int
  }
  deriving (Generic)

canonicalBranchForCausalHashId :: Lens' MigrationState (Map DB.CausalHashId (DB.BranchHashId, DB.BranchObjectId))
canonicalBranchForCausalHashId =
  field @"_canonicalBranchForCausalHashId"

validBranchHashIds :: Lens' MigrationState (Map DB.BranchHashId DB.BranchObjectId)
validBranchHashIds =
  field @"_validBranchHashIds"

numMigrated :: Lens' MigrationState Int
numMigrated =
  field @"_numMigrated"

-- | There was a bug in previous versions of UCM which incorrectly used causal hashes as branch hashes.
-- This remained undetected because there was never a need for this hash to be verifiable,
-- and the hashes were still unique because the namespace hash was PART of the causal hash.
-- It did however result in many identical branches being stored multiple times under
-- different `primary_hash_id`s.
--
-- However, with the advent of Share and Sync, we now need to correctly verify these namespace
-- hashes.
--
-- This migration fixes the issue by re-hashing namespace objects where the value_hash_id of a
-- causal matches the self_hash_id.
-- Luckily this doesn't change any causal hashes.
--
-- However, due to the possibility of multiple identical objects stored under different
-- `primary_hash_id`s, we may now have multiple objects with the same `primary_hash_id`, which
-- our DB schema doesn't allow.
--
-- To address this, we keep exactly one 'canonical' object for each hash, then remap all
-- references to old objects into this canonical object instead. Unfortunately this requires
-- mapping over every branch object and traversing the child references.
--
-- It was also discovered that some developers had many branches which referenced objects
-- which weren't in their codebase. We're not yet sure how this happened, but it's unlikely
-- to be the case for most end users, and it turned out that these references were in causals
-- and branches which were unreachable from the root namespace. As a fix, this migration also
-- tracks every causal and branch which is reachable from the root namespace and deletes all
-- causals and namespaces which are unreachable. Note that this may orphan some definitions,
-- patches, etc. which were previously referenced in an 'unreachable' branch, but they were
-- already floating around in an unreachable state.
migrateSchema3To4 :: Sqlite.Transaction ()
migrateSchema3To4 = do
  Q.expectSchemaVersion 3
  rootCausalHashId <- expectNamespaceRoot
  totalCausals <- causalCount
  migrationState <- flip execStateT (MigrationState mempty mempty 0) $ Sync.sync migrationSync (migrationProgress totalCausals) [rootCausalHashId]
  let MigrationState {_canonicalBranchForCausalHashId = mapping} = migrationState
  let reachableCausalHashes = Map.keysSet mapping
  let reachableBranchObjIds = setOf (traversed . _2) mapping
  log $ "🛠  Cleaning up unreachable branches and causals..."
  dropUnreachableCausalsAndBranches reachableCausalHashes reachableBranchObjIds
  Q.setSchemaVersion 4
  where
    causalCount :: Sqlite.Transaction Int
    causalCount = do
      Sqlite.queryOneCol
        [Sqlite.sql|
          SELECT count(*) FROM causal;
        |]

expectNamespaceRoot :: Sqlite.Transaction DB.CausalHashId
expectNamespaceRoot =
  Sqlite.queryOneCol loadNamespaceRootSql

loadNamespaceRootSql :: Sqlite.Sql
loadNamespaceRootSql =
  [Sqlite.sql|
    SELECT causal_id
    FROM namespace_root
  |]

migrationProgress :: Int -> Sync.Progress (StateT MigrationState Sqlite.Transaction) DB.CausalHashId
migrationProgress totalCausals =
  Sync.Progress {Sync.need, Sync.done, Sync.error, Sync.allDone}
  where
    need e = lift $ debugLog $ "Need " <> show e
    done _ =
      do
        numDone <- numMigrated <+= 1
        lift $ Sqlite.unsafeIO $ putStr $ "\r🏗  " <> show numDone <> " / ~" <> show totalCausals <> " entities migrated. 🚧"
    error e = lift . log $ "Error " <> show e
    allDone = do
      -- In some corrupted codebases we don't necessarily process every causal, or there may
      -- be unreachable causals. We'll show the final number here just so everything looks
      -- good to users. It's okay since we'll process the other branches and clean them up in
      -- a batch step.
      lift $ Sqlite.unsafeIO $ putStrLn $ "\r🏗  " <> show totalCausals <> " / ~" <> show totalCausals <> " entities migrated. 🚧"
      lift . Sqlite.unsafeIO . putStrLn $ "Finished."

migrationSync :: Sync.Sync (StateT MigrationState Sqlite.Transaction) DB.CausalHashId
migrationSync =
  Sync.Sync \e -> do
    (runExceptT $ migrateCausal e) >>= \case
      Left syncResult -> pure syncResult
      Right _ -> pure Sync.Done

liftT :: Sqlite.Transaction a -> ExceptT (Sync.TrySyncResult DB.CausalHashId) (StateT MigrationState Sqlite.Transaction) a
liftT = lift . lift

dropUnreachableCausalsAndBranches :: Set DB.CausalHashId -> Set DB.BranchObjectId -> Sqlite.Transaction ()
dropUnreachableCausalsAndBranches reachableCausals reachableBranchObjs = do
  createReachabilityTables
  traverse_ insertReachableCausalSql reachableCausals
  traverse_ insertReachableBranchObjectSql reachableBranchObjs
  deleteUnreachableHashObjects
  deleteUnreachableBranchObjects
  deleteUnreachableCausalParents
  deleteUnreachableCausals
  where
    deleteUnreachableHashObjects =
      Sqlite.execute
        [Sqlite.sql|
        DELETE FROM hash_object AS ho
          WHERE
            NOT EXISTS (SELECT 1 FROM reachable_branch_objects AS ro WHERE ho.object_id = ro.object_id)
            -- Ensure hash objects we're deleting are for branch objects.
            AND EXISTS (SELECT 1 FROM object AS o WHERE o.id = ho.object_id AND type_id = 2)
      |]
    deleteUnreachableBranchObjects =
      Sqlite.execute
        [Sqlite.sql|
        DELETE FROM object AS o
          WHERE
            o.type_id = 2 -- Filter for only branches
            AND NOT EXISTS (SELECT 1 FROM reachable_branch_objects AS ro WHERE o.id = ro.object_id)
      |]
    deleteUnreachableCausals =
      Sqlite.execute
        [Sqlite.sql|
        DELETE FROM causal AS c
          WHERE NOT EXISTS (SELECT 1 FROM reachable_causals AS rc WHERE c.self_hash_id = rc.self_hash_id)
      |]
    deleteUnreachableCausalParents =
      Sqlite.execute
        [Sqlite.sql|
        DELETE FROM causal_parent AS cp
          WHERE
            -- We only need to check the children, because if it's impossible for a parent to be
            -- unreachable if the child is reachable. A.k.a. reachable(child) =implies> reachable(parent)
            NOT EXISTS (SELECT 1 FROM reachable_causals AS rc WHERE cp.causal_id = rc.self_hash_id)
      |]
    insertReachableCausalSql h =
      Sqlite.execute
        [Sqlite.sql|
          INSERT INTO reachable_causals (self_hash_id) VALUES (:h)
            ON CONFLICT DO NOTHING
        |]
    insertReachableBranchObjectSql o =
      Sqlite.execute
        [Sqlite.sql|
          INSERT INTO reachable_branch_objects (object_id) VALUES (:o)
            ON CONFLICT DO NOTHING
        |]
    createReachabilityTables = do
      Sqlite.execute
        [Sqlite.sql|
           CREATE TEMP TABLE IF NOT EXISTS reachable_branch_objects (
            object_id INTEGER PRIMARY KEY NOT NULL
           )
          |]
      Sqlite.execute
        [Sqlite.sql|
           CREATE TEMP TABLE IF NOT EXISTS reachable_causals (
            self_hash_id INTEGER PRIMARY KEY NOT NULL
           )
          |]

migrateCausal :: DB.CausalHashId -> ExceptT (Sync.TrySyncResult DB.CausalHashId) (StateT MigrationState Sqlite.Transaction) ()
migrateCausal causalHashId = do
  preuse (canonicalBranchForCausalHashId . ix causalHashId) >>= \case
    Just _ -> throwError Sync.PreviouslyDone
    Nothing -> do
      causalParents <- liftT $ Q.loadCausalParents causalHashId
      unmigratedParents <- flip filterM causalParents $ \parentHashId -> (uses canonicalBranchForCausalHashId (Map.notMember parentHashId))
      when (not . null $ unmigratedParents) $ throwError (Sync.Missing unmigratedParents)
      valueHashId <- liftT $ Q.expectCausalValueHashId causalHashId
      preuse (validBranchHashIds . ix valueHashId) >>= \case
        Nothing -> pure ()
        Just objId -> do
          canonicalBranchForCausalHashId . at causalHashId ?= (valueHashId, objId)
          throwError Sync.Done
      liftT (Q.loadBranchObjectIdByCausalHashId causalHashId) >>= \case
        Nothing -> do
          liftT . abortMigration $ "Missing object for child branch of causal: " <> show causalHashId
        Just branchObjId -> do
          rehashAndCanonicalizeNamespace causalHashId valueHashId branchObjId

rehashAndCanonicalizeNamespace :: DB.CausalHashId -> DB.BranchHashId -> DB.BranchObjectId -> ExceptT (Sync.TrySyncResult DB.CausalHashId) (StateT MigrationState Sqlite.Transaction) ()
rehashAndCanonicalizeNamespace causalHashId possiblyIncorrectNamespaceHashId objId = do
  dbBranch <- liftT $ Ops.expectDbBranch objId
  canonicalBranchForCausalMap <- use canonicalBranchForCausalHashId
  -- remap all of the object ID's of the child branches to the correct and canonical objects,
  -- get a list of any unmigrated children, and also track whether any re-mappings actually
  -- occurred, so we don't do extra work when nothing changed.
  let ((unmigratedChildren, Any changes), remappedBranch) =
        dbBranch
          & DBBranch.childrenHashes_ %%~ \(ids@(childBranchObjId, childCausalHashId)) -> do
            case Map.lookup childCausalHashId canonicalBranchForCausalMap of
              Nothing -> (([childCausalHashId], Any False), ids)
              Just (_, canonicalObjId) ->
                let changed = canonicalObjId /= childBranchObjId
                 in (([], Any changed), (canonicalObjId, childCausalHashId))
  when (not . null $ unmigratedChildren) $ throwError (Sync.Missing unmigratedChildren)
  when changes $ do
    liftT $ replaceBranch objId remappedBranch
  correctNamespaceHash <- liftT $ Helpers.dbBranchHash remappedBranch
  liftT . debugLog $ "Correct namespace hash: " <> show correctNamespaceHash
  correctNamespaceHashId <- liftT $ Q.saveBranchHash correctNamespaceHash

  when (correctNamespaceHashId == possiblyIncorrectNamespaceHashId) $ do
    -- If the existing hash for this namespace was already correct, we don't need to
    -- canonicalize the branch or worry about deleting/updating bad objects.
    -- We just record the mapping and move on.
    canonicalBranchForCausalHashId . at causalHashId ?= (correctNamespaceHashId, objId)
    validBranchHashIds . at possiblyIncorrectNamespaceHashId ?= objId
    throwError Sync.Done

  -- Update the value_hash_id on the causal to the correct hash for the branch
  liftT $ updateCausalValueHash correctNamespaceHashId possiblyIncorrectNamespaceHashId
  -- It's possible that an object already exists for this new hash
  mayCanonical <- getCanonicalObjectForHash correctNamespaceHashId
  liftT . debugLog $ "(objId, Canonical object ID):" <> show (objId, mayCanonical)
  liftT . debugLog $ "Updating causal value hash (from, to)" <> show (possiblyIncorrectNamespaceHashId, correctNamespaceHashId)
  canonicalObjId <- case mayCanonical of
    -- If there's an existing canonical object, record the mapping from this object id to
    -- that one.
    Just canonicalObjectId
      | canonicalObjectId /= objId -> do
          -- Found an existing but different object with this hash, so the current object is a duplicate and
          -- needs to be deleted.
          liftT . debugLog $ "Mapping objID: " <> show objId <> " to canonical: " <> show canonicalObjectId
          liftT . debugLog $ "Unilaterally deleting: " <> show objId
          -- Remove possible foreign-key references before deleting the objects themselves
          liftT $ deleteHashObjectsByObjectId objId
          liftT $ deleteObjectById objId
          pure canonicalObjectId
      | otherwise -> do
          -- This should be impossible.
          error $ "We proved that the new hash is different from the existing one, but somehow found the same object for each hash. Please report this as a bug." <> show (objId, canonicalObjectId)
    Nothing -> do
      -- There's no existing canonical object, this object BECOMES the canonical one by
      -- reassigning its primary hash.
      liftT . debugLog $ "Updating in place: " <> show objId
      liftT $ deleteHashObjectsByObjectId objId
      liftT $ updateHashIdForObject correctNamespaceHashId objId
      liftT $ Q.saveHashObject (DB.unBranchHashId correctNamespaceHashId) (DB.unBranchObjectId objId) 2
      pure objId
  -- Save the canonical branch info for the causal for use in remappings.
  canonicalBranchForCausalHashId . at causalHashId ?= (correctNamespaceHashId, canonicalObjId)
  where
    updateCausalValueHash :: DB.BranchHashId -> DB.BranchHashId -> Sqlite.Transaction ()
    updateCausalValueHash correctNamespaceHashId possiblyIncorrectNamespaceHashId =
      Sqlite.execute
        [Sqlite.sql|
          UPDATE causal
            SET value_hash_id = :correctNamespaceHashId
            WHERE value_hash_id = :possiblyIncorrectNamespaceHashId
        |]

    getCanonicalObjectForHash ::
      DB.BranchHashId ->
      ExceptT
        (Sync.TrySyncResult DB.CausalHashId)
        (StateT MigrationState Sqlite.Transaction)
        (Maybe DB.BranchObjectId)
    getCanonicalObjectForHash namespaceHashId =
      liftT $
        Sqlite.queryMaybeCol
          [Sqlite.sql|
            SELECT id
              FROM object
              WHERE primary_hash_id = :namespaceHashId
          |]

    updateHashIdForObject hashId objId =
      Sqlite.execute
        [Sqlite.sql|
          UPDATE object
            SET primary_hash_id = :hashId
            WHERE id = :objId
        |]

    -- Replace the bytes payload of a given branch in-place.
    -- This does NOT update the hash of the object.
    replaceBranch :: DB.BranchObjectId -> DBBranch.DbBranch -> Sqlite.Transaction ()
    replaceBranch objId branch = do
      let (localBranchIds, localBranch) = S.LocalizeObject.localizeBranch branch
      let bytes = S.putBytes S.putBranchFormat $ S.BranchFormat.Full localBranchIds localBranch
      Sqlite.execute
        [Sqlite.sql|
          UPDATE object
          SET bytes = :bytes
          WHERE id = :objId
        |]

    deleteHashObjectsByObjectId objId =
      Sqlite.execute
        [Sqlite.sql|
          DELETE FROM hash_object
            WHERE object_id = :objId
        |]

    deleteObjectById objId =
      Sqlite.execute
        [Sqlite.sql|
          DELETE FROM object
            WHERE id = :objId
        |]

log :: String -> Sqlite.Transaction ()
log = Sqlite.unsafeIO . putStrLn

debugLog :: String -> Sqlite.Transaction ()
debugLog = Debug.whenDebug Debug.Migration . Sqlite.unsafeIO . putStrLn
