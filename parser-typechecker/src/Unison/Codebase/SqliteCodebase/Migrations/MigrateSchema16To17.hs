{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema16To17 (migrateSchema16To17) where

import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import U.Codebase.Branch.Type qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.Sqlite.DbId (CausalHashId, ProjectBranchId (..), ProjectId (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache qualified as BranchCache
import Unison.Codebase.SqliteCodebase.Operations qualified as CodebaseOps
import Unison.Codebase.SqliteCodebase.Operations qualified as Ops
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName), ProjectName (UnsafeProjectName))
import Unison.Debug qualified as Debug
import Unison.NameSegment (NameSegment)
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Unison.Sqlite.Connection qualified as Connection
import Unison.Syntax.NameSegment qualified as NameSegment
import UnliftIO qualified
import UnliftIO qualified as UnsafeIO

-- | This migration converts the codebase from having all projects in a single codebase root to having separate causal
-- roots for each project branch.
-- It:
--
-- * Adds the new project reflog table
-- * Adds the project-branch head as a causal-hash-id column on the project-branch table, and populates it from all the projects in the project root.
-- * Makes a new legacy project from the existing root branch (minus .__projects)
-- * Adds a new scratch/main project
-- * Adds a currentProjectPath table to replace the most-recent-path functionality.
--
-- It requires a Connection argument rather than working inside a Transaction because it needs to temporarily disable
-- foreign key checking, and the foreign_key pragma cannot be set within a transaction.
migrateSchema16To17 :: Sqlite.Connection -> IO ()
migrateSchema16To17 conn = withDisabledForeignKeys $ do
  Q.expectSchemaVersion 16
  Q.addProjectBranchReflogTable
  Debug.debugLogM Debug.Migration "Adding causal hashes to project branches table."
  addCausalHashesToProjectBranches
  Debug.debugLogM Debug.Migration "Making legacy project from loose code."
  makeLegacyProjectFromLooseCode
  Debug.debugLogM Debug.Migration "Adding scratch project"
  scratchMain <-
    Q.loadProjectBranchByNames scratchProjectName scratchBranchName >>= \case
      Just pb -> pure pb
      Nothing -> do
        (_, emptyCausalHashId) <- Codebase.emptyCausalHash
        (_proj, pb) <- Ops.insertProjectAndBranch scratchProjectName scratchBranchName emptyCausalHashId
        pure pb

  -- Try to set the recent project branch to what it was, default back to scratch if it doesn't exist or the user is in
  -- loose code.
  mayRecentProjectBranch <- runMaybeT $ do
    (projectId, branchId) <- MaybeT getMostRecentProjectBranchIds
    -- Make sure the project-branch still exists.
    _projBranch <- MaybeT $ Q.loadProjectBranch projectId branchId
    pure (projectId, branchId)

  Debug.debugLogM Debug.Migration "Adding current project path table"
  Q.addCurrentProjectPathTable
  Debug.debugLogM Debug.Migration "Setting current project path to scratch project"

  case mayRecentProjectBranch of
    Just (projectId, branchId) ->
      initializeCurrentProjectPath projectId branchId []
    Nothing -> initializeCurrentProjectPath scratchMain.projectId scratchMain.branchId []
  Debug.debugLogM Debug.Migration "Done migrating to version 17"
  Q.setSchemaVersion 17
  where
    scratchProjectName = UnsafeProjectName "scratch"
    scratchBranchName = UnsafeProjectBranchName "main"
    withDisabledForeignKeys :: Sqlite.Transaction r -> IO r
    withDisabledForeignKeys m = do
      let disable = Connection.execute conn [Sqlite.sql| PRAGMA foreign_keys=OFF |]
      let enable = Connection.execute conn [Sqlite.sql| PRAGMA foreign_keys=ON |]
      let action = Sqlite.runWriteTransaction conn \run -> run $ m
      UnsafeIO.bracket disable (const enable) (const action)
    initializeCurrentProjectPath :: ProjectId -> ProjectBranchId -> [NameSegment] -> Sqlite.Transaction ()
    initializeCurrentProjectPath projId branchId path = do
      Sqlite.execute
        [Sqlite.sql| DELETE FROM current_project_path |]
      Sqlite.execute
        [Sqlite.sql|
          INSERT INTO current_project_path(project_id, branch_id, path)
          VALUES (:projId, :branchId, :jsonPath)
        |]
      where
        jsonPath :: Text
        jsonPath =
          Text.Lazy.toStrict (Aeson.encodeToLazyText $ NameSegment.toUnescapedText <$> path)

data ForeignKeyFailureException
  = ForeignKeyFailureException
      -- We leave the data as raw as possible to ensure we can display it properly rather than get decoding errors while
      -- trying to display some other error.
      [[Sqlite.SQLData]]
  | MissingRootBranch
  deriving stock (Show)
  deriving anyclass (Exception)

addCausalHashesToProjectBranches :: Sqlite.Transaction ()
addCausalHashesToProjectBranches = do
  Debug.debugLogM Debug.Migration "Creating new_project_branch"
  -- Create the new version of the project_branch table with the causal_hash_id column.
  Sqlite.execute
    [Sqlite.sql|
CREATE TABLE new_project_branch (
  project_id uuid NOT NULL REFERENCES project (id),
  branch_id uuid NOT NULL,
  name text NOT NULL,
  causal_hash_id integer NOT NULL REFERENCES causal(self_hash_id),

  primary key (project_id, branch_id),

  unique (project_id, name)
)
without rowid;
|]
  rootCausalHashId <- expectNamespaceRoot
  rootCh <- Q.expectCausalHash rootCausalHashId
  projectsRoot <- Codebase.getShallowCausalAtPathFromRootHash rootCh (Path.singleton $ projectsNameSegment) >>= V2Causal.value
  ifor_ (V2Branch.children projectsRoot) \projectIdNS projectsCausal -> do
    projectId <- case projectIdNS of
      UUIDNameSegment projectIdUUID -> pure $ ProjectId projectIdUUID
      _ -> error $ "Invalid Project Id NameSegment:" <> show projectIdNS
    Debug.debugM Debug.Migration "Migrating project" projectId
    projectsBranch <- V2Causal.value projectsCausal
    case (Map.lookup branchesNameSegment $ V2Branch.children projectsBranch) of
      Nothing -> pure ()
      Just branchesCausal -> do
        branchesBranch <- V2Causal.value branchesCausal
        ifor_ (V2Branch.children branchesBranch) \branchIdNS projectBranchCausal -> void . runMaybeT $ do
          projectBranchId <- case branchIdNS of
            UUIDNameSegment branchIdUUID -> pure $ ProjectBranchId branchIdUUID
            _ -> error $ "Invalid Branch Id NameSegment:" <> show branchIdNS
          Debug.debugM Debug.Migration "Migrating project branch" projectBranchId
          let branchCausalHash = V2Causal.causalHash projectBranchCausal
          causalHashId <- lift $ Q.expectCausalHashIdByCausalHash branchCausalHash
          branchName <-
            MaybeT $
              Sqlite.queryMaybeCol @ProjectBranchName
                [Sqlite.sql|
                  SELECT project_branch.name
                  FROM project_branch
                  WHERE
                    project_branch.project_id = :projectId
                    AND project_branch.branch_id = :projectBranchId
                |]
          -- Insert the full project branch with HEAD into the new table
          lift $
            Sqlite.execute
              [Sqlite.sql|
              INSERT INTO new_project_branch (project_id, branch_id, name, causal_hash_id)
                VALUES (:projectId, :projectBranchId, :branchName, :causalHashId)
            |]

  Debug.debugLogM Debug.Migration "Deleting orphaned project branch data"
  -- Delete any project branch data that don't have a matching branch in the current root.
  -- This is to make sure any old or invalid project branches get cleared out and won't cause problems when we rewrite
  -- foreign key references.
  -- We have to do this manually since we had to disable foreign key checks to add the new column.
  Sqlite.execute
    [Sqlite.sql| DELETE FROM project_branch_parent AS pbp
      WHERE NOT EXISTS(SELECT 1 FROM new_project_branch npb WHERE npb.project_id = pbp.project_id AND npb.branch_id = pbp.branch_id)
    |]
  Debug.debugLogM Debug.Migration "Deleting orphaned remote mapping data"
  Sqlite.execute
    [Sqlite.sql| DELETE FROM project_branch_remote_mapping AS pbrp
      WHERE NOT EXISTS(SELECT 1 FROM new_project_branch npb WHERE npb.project_id = pbrp.local_project_id AND npb.branch_id = pbrp.local_branch_id)
    |]
  -- Delete any project branch rows that don't have a matching branch in the current root.
  Sqlite.execute
    [Sqlite.sql|
    DELETE FROM most_recent_branch AS mrb
      WHERE NOT EXISTS(SELECT 1 FROM new_project_branch npb WHERE npb.project_id = mrb.project_id AND npb.branch_id = mrb.branch_id)
    |]

  Debug.debugLogM Debug.Migration "Swapping old and new project branch tables"
  -- Drop the old project_branch table and rename the new one to take its place.
  Sqlite.execute [Sqlite.sql| DROP TABLE project_branch |]
  Sqlite.execute [Sqlite.sql| ALTER TABLE new_project_branch RENAME TO project_branch |]
  Debug.debugLogM Debug.Migration "Checking foreign keys"
  foreignKeyErrs <- Sqlite.queryListRow [Sqlite.sql| PRAGMA foreign_key_check |]
  when (not . null $ foreignKeyErrs) . Sqlite.unsafeIO . UnliftIO.throwIO $ ForeignKeyFailureException foreignKeyErrs

makeLegacyProjectFromLooseCode :: Sqlite.Transaction ()
makeLegacyProjectFromLooseCode = do
  rootChId <-
    Sqlite.queryOneCol @CausalHashId
      [Sqlite.sql|
    SELECT causal_id
    FROM namespace_root
  |]
  rootCh <- Q.expectCausalHash rootChId
  branchCache <- Sqlite.unsafeIO BranchCache.newBranchCache
  getDeclType <- Sqlite.unsafeIO $ CodebaseOps.makeCachedTransaction 2048 CodebaseOps.getDeclType
  rootBranch <-
    CodebaseOps.getBranchForHash branchCache getDeclType rootCh `whenNothingM` do
      Sqlite.unsafeIO . UnliftIO.throwIO $ MissingRootBranch
  -- Remove the hidden projects root if one existed.
  let rootWithoutProjects = rootBranch & over (Branch.head_ . Branch.children) (Map.delete projectsNameSegment)
  CodebaseOps.putBranch rootWithoutProjects
  let legacyBranchRootHash = Branch.headHash rootWithoutProjects
  legacyBranchRootHashId <- Q.expectCausalHashIdByCausalHash legacyBranchRootHash

  let findLegacyName :: Maybe Int -> Sqlite.Transaction ProjectName
      findLegacyName mayN = do
        let tryProjName = case mayN of
              Nothing -> UnsafeProjectName "legacy"
              Just n -> UnsafeProjectName $ "legacy" <> Text.pack (show n)
        Q.loadProjectBranchByNames tryProjName legacyBranchName >>= \case
          Nothing -> pure tryProjName
          Just _ -> findLegacyName . Just $ maybe 1 succ mayN
  legacyProjName <- findLegacyName Nothing
  void $ Ops.insertProjectAndBranch legacyProjName legacyBranchName legacyBranchRootHashId
  pure ()
  where
    legacyBranchName = UnsafeProjectBranchName "main"

expectNamespaceRoot :: Sqlite.Transaction CausalHashId
expectNamespaceRoot =
  Sqlite.queryOneCol loadNamespaceRootSql

loadNamespaceRootSql :: Sqlite.Sql
loadNamespaceRootSql =
  [Sqlite.sql|
    SELECT causal_id
    FROM namespace_root
  |]

pattern UUIDNameSegment :: UUID -> NameSegment
pattern UUIDNameSegment uuid <-
  ( NameSegment.toUnescapedText ->
      (Text.uncons -> Just ('_', UUID.fromText . Text.map (\c -> if c == '_' then '-' else c) -> Just uuid))
    )
  where
    UUIDNameSegment uuid =
      NameSegment (Text.cons '_' (Text.map (\c -> if c == '-' then '_' else c) (UUID.toText uuid)))

projectsNameSegment :: NameSegment
projectsNameSegment = NameSegment.unsafeParseText "__projects"

branchesNameSegment :: NameSegment
branchesNameSegment = NameSegment.unsafeParseText "branches"

expectMostRecentNamespace :: Sqlite.Transaction [NameSegment]
expectMostRecentNamespace =
  Sqlite.queryOneColCheck
    [Sqlite.sql|
      SELECT namespace
      FROM most_recent_namespace
    |]
    check
  where
    check :: Text -> Either Q.JsonParseFailure [NameSegment]
    check bytes =
      case Aeson.eitherDecodeStrict (Text.encodeUtf8 bytes) of
        Left failure -> Left (Q.JsonParseFailure {bytes, failure = Text.pack failure})
        Right namespace -> Right (map NameSegment namespace)

getMostRecentProjectBranchIds :: Sqlite.Transaction (Maybe (ProjectId, ProjectBranchId))
getMostRecentProjectBranchIds = do
  nameSegments <- expectMostRecentNamespace
  case nameSegments of
    (proj : UUIDNameSegment projectId : branches : UUIDNameSegment branchId : _)
      | proj == projectsNameSegment && branches == branchesNameSegment ->
          pure . Just $ (ProjectId projectId, ProjectBranchId branchId)
    _ -> pure Nothing
