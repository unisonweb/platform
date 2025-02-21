{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains Share API calls related to projects, wrapped in the Cli monad.
--
-- Here, we also validate inputs from Share that the API itself does not. For example, in the API,
-- a project name is just a Text. But because our client requires a richer structure for project names, we try parsing
-- them into a ProjectName, and fail right away if parsing fails.
module Unison.Cli.Share.Projects
  ( -- * API types
    RemoteProject (..),
    RemoteProjectBranch (..),

    -- * API functions
    getProjectById,
    getProjectByName,
    getProjectByName',
    createProject,
    GetProjectBranchResponse (..),
    IncludeSquashedHead (..),
    getProjectBranchById,
    getProjectBranchByName,
    getProjectBranchByName',
    createProjectBranch,
    SetProjectBranchHeadResponse (..),
    setProjectBranchHead,

    -- * Temporary special hard-coded base url
    hardCodedBaseUrl,
    hardCodedUri,
  )
where

import Control.Monad.Reader (ask)
import Data.Proxy
import Network.HTTP.Client qualified as Http.Client
import Network.URI (URI)
import Network.URI qualified as URI
import Servant.API ((:<|>) (..), (:>))
import Servant.Client
import Servant.Client qualified as Servant
import U.Codebase.Sqlite.DbId (RemoteProjectBranchId (..), RemoteProjectId (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Auth.HTTPClient qualified as Auth
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.Share.Projects.Types (RemoteProject (..), RemoteProjectBranch (..))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Hash32 (Hash32)
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Share.API.Hash qualified as HashJWT
import Unison.Share.API.Projects qualified as Share.API
import Unison.Share.Codeserver (defaultCodeserver)
import Unison.Share.Types (codeserverBaseURL)
import Unison.Sync.Common qualified as Sync

-- | Get a project by id.
--
-- On success, update the `remote_project` table.
getProjectById :: RemoteProjectId -> Cli (Maybe RemoteProject)
getProjectById (RemoteProjectId projectId) = do
  response <- servantClientToCli (getProject0 (Just projectId) Nothing) & onLeftM servantClientError
  onGetProjectResponse response

-- | Get a project by name.
--
-- On success, update the `remote_project` table.
getProjectByName :: ProjectName -> Cli (Maybe RemoteProject)
getProjectByName projectName =
  getProjectByName' projectName & onLeftM servantClientError

-- | Variant of 'getProjectByName' that returns servant client errors.
getProjectByName' :: ProjectName -> Cli (Either Servant.ClientError (Maybe RemoteProject))
getProjectByName' projectName = do
  servantClientToCli (getProject0 Nothing (Just (into @Text projectName))) >>= \case
    Left err -> pure (Left err)
    Right response -> Right <$> onGetProjectResponse response

-- | Create a new project. Kinda weird: returns `Nothing` if the user handle part of the project doesn't exist.
--
-- On success, update the `remote_project` table.
createProject :: ProjectName -> Cli (Maybe RemoteProject)
createProject projectName = do
  let request = Share.API.CreateProjectRequest {projectName = into @Text projectName}
  servantClientToCli (createProject0 request) >>= \case
    Left err -> servantClientError err
    Right (Share.API.CreateProjectResponseNotFound {}) -> pure Nothing
    Right (Share.API.CreateProjectResponseUnauthorized x) -> unauthorized x
    Right (Share.API.CreateProjectResponseSuccess project) -> Just <$> onGotProject project

data GetProjectBranchResponse
  = GetProjectBranchResponseBranchNotFound
  | GetProjectBranchResponseProjectNotFound
  | GetProjectBranchResponseSuccess !RemoteProjectBranch

data IncludeSquashedHead
  = IncludeSquashedHead
  | NoSquashedHead
  deriving stock (Show, Eq)

-- | Get a project branch by id.
--
-- On success, update the `remote_project_branch` table.
getProjectBranchById :: IncludeSquashedHead -> ProjectAndBranch RemoteProjectId RemoteProjectBranchId -> Cli GetProjectBranchResponse
getProjectBranchById includeSquashed (ProjectAndBranch (RemoteProjectId projectId) (RemoteProjectBranchId branchId)) = do
  let squashed = includeSquashed == IncludeSquashedHead
  response <- servantClientToCli (getProjectBranch0 projectId (Just branchId) Nothing squashed) & onLeftM servantClientError
  onGetProjectBranchResponse response

-- | Get a project branch by name.
--
-- On success, update the `remote_project_branch` table.
getProjectBranchByName :: IncludeSquashedHead -> ProjectAndBranch RemoteProjectId ProjectBranchName -> Cli GetProjectBranchResponse
getProjectBranchByName includeSquashed (ProjectAndBranch (RemoteProjectId projectId) branchName) = do
  let squashed = includeSquashed == IncludeSquashedHead
  response <-
    servantClientToCli (getProjectBranch0 projectId Nothing (Just (into @Text branchName)) squashed)
      & onLeftM servantClientError
  onGetProjectBranchResponse response

-- | Variant of 'getProjectBranchByName' that returns servant client errors.
getProjectBranchByName' ::
  IncludeSquashedHead ->
  ProjectAndBranch RemoteProjectId ProjectBranchName ->
  Cli (Either Servant.ClientError GetProjectBranchResponse)
getProjectBranchByName' includeSquashed (ProjectAndBranch (RemoteProjectId projectId) branchName) = do
  let squashed = includeSquashed == IncludeSquashedHead
  servantClientToCli (getProjectBranch0 projectId Nothing (Just (into @Text branchName)) squashed) >>= \case
    Left err -> pure (Left err)
    Right response -> Right <$> onGetProjectBranchResponse response

-- | Create a new project branch.
--
-- On success, update the `remote_project_branch` table.
createProjectBranch :: Share.API.CreateProjectBranchRequest -> Cli (Maybe RemoteProjectBranch)
createProjectBranch request =
  servantClientToCli (createProjectBranch0 request) >>= \case
    Left err -> servantClientError err
    Right (Share.API.CreateProjectBranchResponseMissingCausalHash hash) -> bugRemoteMissingCausalHash hash
    Right (Share.API.CreateProjectBranchResponseNotFound {}) -> pure Nothing
    Right (Share.API.CreateProjectBranchResponseUnauthorized x) -> unauthorized x
    Right (Share.API.CreateProjectBranchResponseSuccess branch) -> Just <$> onGotProjectBranch branch

data SetProjectBranchHeadResponse
  = SetProjectBranchHeadResponseNotFound
  | -- | (expected, actual)
    SetProjectBranchHeadResponseExpectedCausalHashMismatch !Hash32 !Hash32
  | SetProjectBranchHeadResponsePublishedReleaseIsImmutable
  | SetProjectBranchHeadResponseDeprecatedReleaseIsImmutable
  | SetProjectBranchHeadResponseSuccess
  deriving stock (Eq, Show, Generic)

-- | Set a project branch head (can be a fast-forward or force-push).
setProjectBranchHead :: Share.API.SetProjectBranchHeadRequest -> Cli SetProjectBranchHeadResponse
setProjectBranchHead request =
  servantClientToCli (setProjectBranchHead0 request) >>= \case
    Left err -> servantClientError err
    Right (Share.API.SetProjectBranchHeadResponseUnauthorized x) -> unauthorized x
    Right (Share.API.SetProjectBranchHeadResponseNotFound _) -> pure SetProjectBranchHeadResponseNotFound
    Right (Share.API.SetProjectBranchHeadResponseMissingCausalHash hash) -> bugRemoteMissingCausalHash hash
    Right (Share.API.SetProjectBranchHeadResponseExpectedCausalHashMismatch expected actual) ->
      pure (SetProjectBranchHeadResponseExpectedCausalHashMismatch expected actual)
    Right (Share.API.SetProjectBranchHeadResponsePublishedReleaseIsImmutable) -> pure SetProjectBranchHeadResponsePublishedReleaseIsImmutable
    Right (Share.API.SetProjectBranchHeadResponseDeprecatedReleaseIsImmutable) -> pure SetProjectBranchHeadResponseDeprecatedReleaseIsImmutable
    Right (Share.API.SetProjectBranchHeadResponseSuccess) -> pure SetProjectBranchHeadResponseSuccess

------------------------------------------------------------------------------------------------------------------------
-- Database manipulation callbacks

onGetProjectResponse :: Share.API.GetProjectResponse -> Cli (Maybe RemoteProject)
onGetProjectResponse = \case
  -- FIXME should we mark remote project as deleted?
  Share.API.GetProjectResponseNotFound {} -> pure Nothing
  Share.API.GetProjectResponseUnauthorized x -> unauthorized x
  Share.API.GetProjectResponseSuccess project -> Just <$> onGotProject project

onGetProjectBranchResponse :: Share.API.GetProjectBranchResponse -> Cli GetProjectBranchResponse
onGetProjectBranchResponse = \case
  -- FIXME should we mark remote project/branch as deleted in these two cases?
  Share.API.GetProjectBranchResponseBranchNotFound {} -> pure GetProjectBranchResponseBranchNotFound
  Share.API.GetProjectBranchResponseProjectNotFound {} -> pure GetProjectBranchResponseProjectNotFound
  Share.API.GetProjectBranchResponseUnauthorized x -> unauthorized x
  Share.API.GetProjectBranchResponseSuccess branch -> GetProjectBranchResponseSuccess <$> onGotProjectBranch branch

onGotProject :: Share.API.Project -> Cli RemoteProject
onGotProject project = do
  let projectId = RemoteProjectId (project ^. #projectId)
  projectName <- validateProjectName (project ^. #projectName)
  let latestRelease = (project ^. #latestRelease) >>= eitherToMaybe . tryFrom @Text
  Cli.runTransaction (Queries.ensureRemoteProject projectId hardCodedUri projectName)
  pure RemoteProject {projectId, projectName, latestRelease}

onGotProjectBranch :: Share.API.ProjectBranch -> Cli RemoteProjectBranch
onGotProjectBranch branch = do
  let projectId = RemoteProjectId (branch ^. #projectId)
  let branchId = RemoteProjectBranchId (branch ^. #branchId)
  let causalHash = Sync.hash32ToCausalHash $ HashJWT.hashJWTHash (branch ^. #branchHead)
  projectName <- validateProjectName (branch ^. #projectName)
  branchName <- validateBranchName (branch ^. #branchName)
  Cli.runTransaction do
    causalHashId <- Queries.saveCausalHash causalHash
    Queries.ensureRemoteProjectBranch
      projectId
      hardCodedUri
      branchId
      branchName
      (Just causalHashId)
  pure
    RemoteProjectBranch
      { projectId,
        projectName,
        branchId,
        branchName,
        branchHead = branch ^. #branchHead,
        squashedBranchHead = branch ^. #squashedBranchHead
      }

validateProjectName :: Text -> Cli ProjectName
validateProjectName projectName =
  tryInto @ProjectName projectName & onLeft \_ ->
    Cli.returnEarly (Output.InvalidProjectName projectName)

validateBranchName :: Text -> Cli ProjectBranchName
validateBranchName branchName =
  tryInto @ProjectBranchName branchName & onLeft \_ ->
    Cli.returnEarly (Output.InvalidProjectBranchName branchName)

servantClientError :: Servant.ClientError -> Cli void
servantClientError =
  Cli.returnEarly . Output.ServantClientError

unauthorized :: Share.API.Unauthorized -> Cli void
unauthorized (Share.API.Unauthorized message) =
  Cli.returnEarly (Output.Unauthorized message)

bugRemoteMissingCausalHash :: Hash32 -> a
bugRemoteMissingCausalHash hash =
  error (reportBug "E796475" ("Create remote branch: causal hash missing: " ++ show hash))

------------------------------------------------------------------------------------------------------------------------
-- Low-level servant client generation and wrapping

-- For now, since there's no syntax for specifying an alternative share server in any of the UCM commands, we
-- just hard-code the default codeserver here.
hardCodedBaseUrl :: BaseUrl
hardCodedBaseUrl =
  codeserverBaseURL defaultCodeserver

-- Like hardCodedBaseUri using an isomorphic-ish type
hardCodedUri :: URI
hardCodedUri =
  case URI.parseURI (showBaseUrl hardCodedBaseUrl) of
    Nothing -> error ("BaseUrl is an invalid URI: " ++ showBaseUrl hardCodedBaseUrl)
    Just uri -> uri

servantClientToCli :: ClientM a -> Cli (Either Servant.ClientError a)
servantClientToCli action = do
  Cli.Env {authHTTPClient = Auth.AuthenticatedHttpClient httpManager} <- ask

  let clientEnv :: ClientEnv
      clientEnv =
        (mkClientEnv httpManager hardCodedBaseUrl)
          { Servant.makeClientRequest = \url request ->
              (Servant.defaultMakeClientRequest url request)
                <&> \req ->
                  req
                    { Http.Client.responseTimeout = Http.Client.responseTimeoutMicro (60 * 1000 * 1000 {- 60s -})
                    }
          }

  liftIO (runClientM action clientEnv)

getProject0 :: Maybe Text -> Maybe Text -> ClientM Share.API.GetProjectResponse
createProject0 :: Share.API.CreateProjectRequest -> ClientM Share.API.CreateProjectResponse
getProjectBranch0 :: Text -> Maybe Text -> Maybe Text -> Bool -> ClientM Share.API.GetProjectBranchResponse
createProjectBranch0 :: Share.API.CreateProjectBranchRequest -> ClientM Share.API.CreateProjectBranchResponse
setProjectBranchHead0 :: Share.API.SetProjectBranchHeadRequest -> ClientM Share.API.SetProjectBranchHeadResponse
( getProject0
    :<|> createProject0
    :<|> getProjectBranch0
    :<|> createProjectBranch0
    :<|> setProjectBranchHead0
  ) =
    client (Proxy :: Proxy ("ucm" :> "v1" :> "projects" :> Share.API.ProjectsAPI))
