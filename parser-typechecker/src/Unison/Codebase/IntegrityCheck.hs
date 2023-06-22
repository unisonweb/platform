{-# LANGUAGE QuasiQuotes #-}

-- | There are many invariants we expect to hold in our sqlite database and on codebase
-- objects which we can't maintain using database checks. This module performs checks for some
-- of these invariants, which can be useful to run after performing potentially dangerous
-- operations like migrations.
module Unison.Codebase.IntegrityCheck
  ( integrityCheckFullCodebase,
    integrityCheckAllBranches,
    integrityCheckAllCausals,
    prettyPrintIntegrityErrors,
    IntegrityResult (..),
  )
where

import Control.Lens
import Data.List.NonEmpty qualified as NEList
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Void
import Text.Pretty.Simple
import U.Codebase.HashTags (BranchHash (..))
import U.Codebase.Sqlite.Branch.Full qualified as DBBranch
import U.Codebase.Sqlite.DbId qualified as DB
import U.Codebase.Sqlite.Operations qualified as Ops
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2.DbHelpers qualified as Helpers
import Unison.Debug qualified as Debug
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Pretty qualified as P
import Prelude hiding (log)

debugLog :: TL.Text -> Sqlite.Transaction ()
debugLog msg = Debug.whenDebug Debug.Integrity $ logInfo msg

logInfo :: TL.Text -> Sqlite.Transaction ()
logInfo msg = Sqlite.unsafeIO $ TL.putStrLn msg

logError :: TL.Text -> Sqlite.Transaction ()
logError msg = logInfo $ "  ⚠️   " <> msg

data IntegrityError
  = DetectedObjectsWithoutCorrespondingHashObjects (NESet DB.ObjectId)
  | -- (causal hash, branch hash)
    DetectedCausalsWithoutCorrespondingBranchObjects (NESet (Hash, Hash))
  | DetectedCausalsWithCausalHashAsBranchHash (NESet Hash)
  | DetectedBranchErrors BranchHash (NESet BranchError)
  deriving stock (Show, Eq, Ord)

data BranchError
  = IncorrectHashForBranch BranchHash BranchHash
  | MismatchedObjectForChild Hash DB.BranchObjectId DB.BranchObjectId
  | MissingObjectForChildCausal Hash
  | MissingObject DB.BranchObjectId
  | MissingCausalForChild Hash
  | ChildCausalHashObjectIdMismatch Hash DB.BranchObjectId
  deriving stock (Show, Eq, Ord)

data IntegrityResult = IntegrityErrorDetected (NESet IntegrityError) | NoIntegrityErrors
  deriving stock (Show, Eq, Ord)

instance Semigroup IntegrityResult where
  IntegrityErrorDetected errA <> IntegrityErrorDetected errB = IntegrityErrorDetected (errA <> errB)
  NoIntegrityErrors <> IntegrityErrorDetected err = IntegrityErrorDetected err
  IntegrityErrorDetected err <> NoIntegrityErrors = IntegrityErrorDetected err
  NoIntegrityErrors <> NoIntegrityErrors = NoIntegrityErrors

instance Monoid IntegrityResult where
  mempty = NoIntegrityErrors

integrityCheckAllHashObjects :: Sqlite.Transaction IntegrityResult
integrityCheckAllHashObjects = do
  Sqlite.queryListCol @DB.ObjectId objectsWithoutHashObjectsSQL >>= \case
    (o : os) -> do
      let badObjects = NESet.fromList (o NEList.:| os)
      pure $ IntegrityErrorDetected (NESet.singleton $ DetectedObjectsWithoutCorrespondingHashObjects badObjects)
    [] -> do
      pure NoIntegrityErrors
  where
    objectsWithoutHashObjectsSQL =
      [Sqlite.sql|
        SELECT o.id
        FROM object AS o
        WHERE NOT EXISTS (
          SELECT 1
          FROM hash_object as ho
          WHERE ho.object_id = o.id
        )
      |]

-- | Performs a bevy of checks on causals.
integrityCheckAllCausals :: Sqlite.Transaction IntegrityResult
integrityCheckAllCausals = do
  branchObjIntegrity <-
    Sqlite.queryListRow @(DB.CausalHashId, DB.BranchHashId) causalsWithMissingBranchObjects >>= \case
      [] -> pure NoIntegrityErrors
      (c : cs) -> do
        badCausals <- for (c NEList.:| cs) $ \(causalHashId, branchHashId) -> do
          ch <- Q.expectHash (DB.unCausalHashId causalHashId)
          bh <- Q.expectHash (DB.unBranchHashId branchHashId)
          pure (ch, bh)
        logError $ "Detected " <> pShow (length badCausals) <> " causals with missing branch objects."
        debugLog . pShow $ badCausals
        pure $ IntegrityErrorDetected (NESet.singleton $ DetectedCausalsWithoutCorrespondingBranchObjects $ NESet.fromList badCausals)

  differingBranchHashIntegrity <-
    Sqlite.queryListCol @DB.HashId causalsWithMatchingValueHashAndSelfHash >>= \case
      [] -> pure NoIntegrityErrors
      (c : cs) -> do
        badCausalHashes <- for (c NEList.:| cs) Q.expectHash
        pure (IntegrityErrorDetected (NESet.singleton $ DetectedCausalsWithCausalHashAsBranchHash $ NESet.fromList badCausalHashes))
  pure (branchObjIntegrity <> differingBranchHashIntegrity)
  where
    causalsWithMissingBranchObjects :: Sqlite.Sql
    causalsWithMissingBranchObjects =
      [Sqlite.sql|
        SELECT c.self_hash_id, c.value_hash_id
          FROM causal c
          WHERE NOT EXISTS (SELECT 1 from object o WHERE o.primary_hash_id = c.value_hash_id);
      |]
    causalsWithMatchingValueHashAndSelfHash :: Sqlite.Sql
    causalsWithMatchingValueHashAndSelfHash =
      [Sqlite.sql|
        SELECT self_hash_id
          FROM causal
          WHERE self_hash_id = value_hash_id
      |]

-- | Performs a bevy of checks on branch objects and their relation to causals.
integrityCheckAllBranches :: Sqlite.Transaction IntegrityResult
integrityCheckAllBranches = do
  branchObjIds <- Sqlite.queryListCol allBranchObjectIdsSql
  flip foldMapM branchObjIds integrityCheckBranch
  where
    allBranchObjectIdsSql :: Sqlite.Sql
    allBranchObjectIdsSql =
      [Sqlite.sql|
        SELECT id FROM object WHERE type_id = 2;
      |]

    doesCausalExistForCausalHashId :: DB.CausalHashId -> Sqlite.Transaction Bool
    doesCausalExistForCausalHashId hashId =
      Sqlite.queryOneCol
        [Sqlite.sql|
          SELECT EXISTS (SELECT 1 FROM causal WHERE self_hash_id = :hashId)
        |]

    integrityCheckBranch :: DB.BranchObjectId -> Sqlite.Transaction IntegrityResult
    integrityCheckBranch objId = do
      dbBranch <- Ops.expectDbBranch objId
      expectedBranchHash <- Helpers.dbBranchHash dbBranch
      actualBranchHash <- BranchHash <$> Q.expectPrimaryHashByObjectId (DB.unBranchObjectId objId)
      branchHashCheck <- assertExpectedBranchHash expectedBranchHash actualBranchHash
      branchChildChecks <- flip foldMapM (toListOf DBBranch.childrenHashes_ dbBranch) $ \(childObjId, childCausalHashId) -> do
        let checks =
              [ assertBranchObjExists childObjId,
                assertCausalExists childCausalHashId,
                assertCausalValueMatchesObject childCausalHashId childObjId
              ]
        (fold <$> sequenceA checks)
      case NESet.nonEmptySet (branchHashCheck <> branchChildChecks) of
        Nothing -> pure NoIntegrityErrors
        Just errs -> pure . IntegrityErrorDetected . NESet.singleton $ DetectedBranchErrors actualBranchHash errs
      where
        assertExpectedBranchHash :: BranchHash -> BranchHash -> Sqlite.Transaction (Set BranchError)
        assertExpectedBranchHash expectedBranchHash actualBranchHash = do
          if (expectedBranchHash /= actualBranchHash)
            then do
              logError $ "Expected hash for namespace doesn't match actual hash for namespace: " <> pShow (expectedBranchHash, actualBranchHash)
              pure (Set.singleton $ IncorrectHashForBranch expectedBranchHash actualBranchHash)
            else do
              pure mempty

        assertBranchObjExists :: DB.BranchObjectId -> Sqlite.Transaction (Set BranchError)
        assertBranchObjExists branchObjId = do
          Q.loadNamespaceObject @Void (DB.unBranchObjectId branchObjId) (const $ Right ()) >>= \case
            Just _ -> pure mempty
            Nothing -> do
              logError $ "Expected namespace object for object ID: " <> pShow branchObjId
              pure (Set.singleton $ MissingObject branchObjId)
        assertCausalExists :: DB.CausalHashId -> Sqlite.Transaction (Set BranchError)
        assertCausalExists causalHashId = do
          doesCausalExistForCausalHashId causalHashId >>= \case
            True -> pure mempty
            False -> do
              ch <- Q.expectHash (DB.unCausalHashId causalHashId)
              logError $ "Expected causal for causal hash ID, but none was found: " <> pShow causalHashId
              pure (Set.singleton $ MissingCausalForChild ch)
        assertCausalValueMatchesObject ::
          DB.CausalHashId ->
          DB.BranchObjectId ->
          Sqlite.Transaction (Set BranchError)
        assertCausalValueMatchesObject causalHashId branchObjId = do
          -- Assert the object for the causal hash ID matches the given object Id.
          Q.loadBranchObjectIdByCausalHashId causalHashId >>= \case
            Nothing -> do
              ch <- Q.expectHash (DB.unCausalHashId causalHashId)
              logError $ "Expected branch object for causal hash ID: " <> pShow causalHashId
              pure (Set.singleton $ MissingObjectForChildCausal ch)
            Just foundBranchId
              | foundBranchId /= branchObjId -> do
                  logError $ "Expected child branch object to match canonical object ID for causal hash's namespace: " <> pShow (causalHashId, foundBranchId, branchObjId)
                  ch <- Q.expectHash (DB.unCausalHashId causalHashId)
                  pure (Set.singleton $ MismatchedObjectForChild ch branchObjId foundBranchId)
              | otherwise -> pure mempty

prettyPrintIntegrityErrors :: (Foldable f) => f IntegrityError -> P.Pretty P.ColorText
prettyPrintIntegrityErrors xs
  | null xs = mempty
  | otherwise =
      xs
        & toList
        & fmap
          ( \case
              DetectedObjectsWithoutCorrespondingHashObjects objs ->
                P.hang
                  "Detected objects without any corresponding hash_object. Object IDs:"
                  (P.commas (prettyObjectId <$> NESet.toList objs))
              DetectedCausalsWithoutCorrespondingBranchObjects hashes ->
                P.hang
                  "Detected causals without a corresponding branch object:\n"
                  ( P.column2Header
                      "Causal Hash"
                      "Branch Hash"
                      (toList hashes <&> bimap prettyHash prettyHash)
                  )
              DetectedCausalsWithCausalHashAsBranchHash ns ->
                P.hang
                  "Detected causals with the same causal hash as branch hash:"
                  (P.commas (prettyHash <$> toList ns))
              DetectedBranchErrors bh errs ->
                P.hang
                  ("Detected errors in branch: " <> prettyHash (unBranchHash bh))
                  (P.lines . fmap (<> "\n") . fmap prettyBranchError . toList $ errs)
          )
        & fmap (<> "\n")
        & P.lines
        & P.warnCallout
  where
    prettyHash :: Hash -> P.Pretty P.ColorText
    prettyHash h = P.blue . P.text $ ("#" <> Hash.toBase32HexText h)
    prettyBranchObjectId :: DB.BranchObjectId -> P.Pretty P.ColorText
    prettyBranchObjectId = prettyObjectId . DB.unBranchObjectId
    prettyObjectId :: DB.ObjectId -> P.Pretty P.ColorText
    prettyObjectId (DB.ObjectId n) = P.green (P.shown n)
    prettyBranchError :: BranchError -> P.Pretty P.ColorText
    prettyBranchError =
      P.wrap . \case
        IncorrectHashForBranch expected actual -> "The Branch hash for this branch is incorrect. Expected Hash: " <> prettyHash (unBranchHash expected) <> ", Actual Hash: " <> prettyHash (unBranchHash actual)
        MismatchedObjectForChild ha obj1 obj2 ->
          "The child with causal hash: " <> prettyHash ha <> " is mapped to object ID " <> prettyBranchObjectId obj1 <> " but should map to " <> prettyBranchObjectId obj2 <> "."
        MissingObjectForChildCausal ha ->
          "There's no corresponding branch object for the causal hash: " <> prettyHash ha
        MissingObject objId -> "Expected an object for the child reference to object id: " <> prettyBranchObjectId objId
        MissingCausalForChild ch -> "Expected a causal to exist for hash: " <> prettyHash ch
        ChildCausalHashObjectIdMismatch ch objId ->
          "Expected the object ID reference " <> prettyHash ch <> " to match the provided object ID: " <> prettyBranchObjectId objId

-- | Performs all available integrity checks.
integrityCheckFullCodebase :: Sqlite.Transaction IntegrityResult
integrityCheckFullCodebase = do
  fmap fold . sequenceA $
    [ integrityCheckAllHashObjects,
      integrityCheckAllBranches,
      integrityCheckAllCausals
    ]
