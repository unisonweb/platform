-- | @project.clone@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectClone
  ( projectClone,
  )
where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.These (These (..))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId (ProjectBranchId (..), ProjectId (..), RemoteProjectBranchId (..), RemoteProjectId (..))
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (stepAt)
import Unison.Cli.ProjectUtils (loggeth, projectBranchPath)
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.HandleInput.Pull as HandleInput.Pull
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName, projectNameUserSlug)
import qualified Unison.Share.API.Hash as Share.API
import qualified Unison.Share.API.Projects as Share.API
import qualified Unison.Share.Sync as Share (downloadEntities)
import qualified Unison.Sqlite as Sqlite
import Unison.Sync.Common (hash32ToCausalHash)
import qualified Unison.Sync.Types as Share (RepoName (..))
import Witch (unsafeFrom)

-- | Clone a remote project or remote project branch.
projectClone :: These ProjectName ProjectBranchName -> Cli ()
projectClone = \case
  These projectName branchName ->
    cloneProjectAndBranch (ProjectAndBranch projectName branchName)
  This projectName -> cloneProject projectName
  That branchName -> cloneBranch branchName

-- Clone a project, defaulting to branch "main"
cloneProject :: ProjectName -> Cli ()
cloneProject projectName = do
  cloneProjectAndBranch
    ProjectAndBranch
      { project = projectName,
        branch = unsafeFrom @Text "main"
      }

-- Clone a branch from the remote project associated with the current project.
cloneBranch :: ProjectBranchName -> Cli ()
cloneBranch _remoteBranchName = do
  loggeth ["not implemented: project.clone /branch"]
  Cli.returnEarlyWithoutOutput

cloneProjectAndBranch :: ProjectAndBranch ProjectName ProjectBranchName -> Cli ()
cloneProjectAndBranch (ProjectAndBranch remoteProjectName remoteBranchName) = do
  -- TODO: allow user to override these with second argument
  let localProjectName = remoteProjectName
  let localBranchName = remoteBranchName

  -- Assert that this project name has a user slug
  remoteProjectUserSlug <-
    case projectNameUserSlug remoteProjectName of
      Just slug -> pure slug
      Nothing -> do
        loggeth ["can't clone project without user slug"]
        Cli.returnEarlyWithoutOutput

  -- Quick local check before hitting share to determine whether this project+branch already exists.
  let assertLocalProjectBranchDoesntExist :: Sqlite.Transaction (Either Output.Output (Maybe Queries.Project))
      assertLocalProjectBranchDoesntExist =
        Queries.loadProjectByName (into @Text localProjectName) >>= \case
          Nothing -> pure (Right Nothing)
          Just project ->
            Queries.projectBranchExistsByName (project ^. #projectId) (into @Text localBranchName) <&> \case
              False -> Right (Just project)
              True -> Left (Output.ProjectAndBranchNameAlreadyExists localProjectName localBranchName)
  void (Cli.runEitherTransaction assertLocalProjectBranchDoesntExist)

  -- Get the branch of the given project.
  remoteProjectBranch <- do
    project <-
      Share.getProjectByName remoteProjectName >>= \case
        Share.API.GetProjectResponseNotFound _ ->
          Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri remoteProjectName remoteBranchName)
        Share.API.GetProjectResponseUnauthorized (Share.API.Unauthorized message) ->
          Cli.returnEarly (Output.Unauthorized message)
        Share.API.GetProjectResponseSuccess project -> pure project
    let remoteProjectId = RemoteProjectId (project ^. #projectId)
    Share.getProjectBranchByName (ProjectAndBranch remoteProjectId remoteBranchName) >>= \case
      Share.API.GetProjectBranchResponseBranchNotFound _ ->
        Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri remoteProjectName remoteBranchName)
      Share.API.GetProjectBranchResponseProjectNotFound _ ->
        Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri remoteProjectName remoteBranchName)
      Share.API.GetProjectBranchResponseUnauthorized (Share.API.Unauthorized message) ->
        Cli.returnEarly (Output.Unauthorized message)
      Share.API.GetProjectBranchResponseSuccess projectBranch -> pure projectBranch

  -- Pull the remote branch's contents
  let remoteBranchHeadJwt = remoteProjectBranch ^. #branchHead
  Cli.with HandleInput.Pull.withEntitiesDownloadedProgressCallback \downloadedCallback -> do
    let download =
          Share.downloadEntities
            Share.hardCodedBaseUrl
            (Share.RepoName remoteProjectUserSlug)
            remoteBranchHeadJwt
            downloadedCallback
    download >>= \case
      Left err -> do
        loggeth ["download entities error: ", tShow err]
        Cli.returnEarlyWithoutOutput
      Right () -> pure ()

  localProjectAndBranch <-
    Cli.runEitherTransaction do
      -- Repeat the check from before, because (although it's highly unlikely) we could have a name conflict after
      -- downloading the remote branch
      assertLocalProjectBranchDoesntExist >>= \case
        Left err -> pure (Left err)
        Right maybeLocalProject -> do
          -- Create the local project (if necessary), and create the local branch
          localProjectId <-
            case maybeLocalProject of
              Nothing -> do
                localProjectId <- Sqlite.unsafeIO (ProjectId <$> UUID.nextRandom)
                Queries.insertProject localProjectId (into @Text localProjectName)
                pure localProjectId
              Just localProject -> pure (localProject ^. #projectId)
          localBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          Queries.insertProjectBranch localProjectId localBranchId (into @Text localBranchName)
          Queries.insertBranchRemoteMapping
            localProjectId
            localBranchId
            (RemoteProjectId (remoteProjectBranch ^. #projectId))
            Share.hardCodedUri
            (RemoteProjectBranchId (remoteProjectBranch ^. #branchId))
          pure (Right (ProjectAndBranch localProjectId localBranchId))

  -- Manipulate the root namespace and cd
  Cli.Env {codebase} <- ask
  let branchHead = hash32ToCausalHash (Share.API.hashJWTHash remoteBranchHeadJwt)
  theBranch <- liftIO (Codebase.expectBranchForHash codebase branchHead)
  let path = projectBranchPath localProjectAndBranch
  Cli.stepAt "project.clone" (Path.unabsolute path, const (Branch.head theBranch))
  Cli.cd path
