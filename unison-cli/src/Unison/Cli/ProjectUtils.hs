-- | Project-related utilities.
module Unison.Cli.ProjectUtils
  ( -- * Project/path helpers
    getCurrentProject,
    expectCurrentProject,
    getCurrentProjectBranch,
    expectCurrentProjectBranch,
    projectPath,
    projectBranchesPath,
    projectBranchPath,
    projectBranchSegment,
    projectBranchPathPrism,

    -- * Name hydration
    hydrateNames,

    -- * Loading local project info
    expectProjectAndBranchByIds,
    expectProjectAndBranchByTheseNames,

    -- * Loading remote project info
    expectRemoteProjectByName,
    expectRemoteProjectBranchById,
    loadRemoteProjectBranchByName,
    expectRemoteProjectBranchByName,
    loadRemoteProjectBranchByNames,
    expectRemoteProjectBranchByNames,
    expectRemoteProjectBranchByTheseNames,
  )
where

import Control.Lens
import Data.Text qualified as Text
import Data.These (These (..))
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase.Editor.Output (Output (LocalProjectBranchDoesntExist))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Sqlite qualified as Sqlite
import Witch (unsafeFrom)

-- | Get the current project that a user is on.
getCurrentProject :: Cli (Maybe Sqlite.Project)
getCurrentProject = do
  path <- Cli.getCurrentPath
  case preview projectBranchPathPrism path of
    Nothing -> pure Nothing
    Just (ProjectAndBranch projectId _branchId, _restPath) ->
      Cli.runTransaction do
        project <- Queries.expectProject projectId
        pure (Just project)

-- | Like 'getCurrentProject', but fails with a message if the user is not on a project branch.
expectCurrentProject :: Cli Sqlite.Project
expectCurrentProject = do
  getCurrentProject & onNothingM (Cli.returnEarly Output.NotOnProjectBranch)

-- | Get the current project+branch+branch path that a user is on.
getCurrentProjectBranch :: Cli (Maybe (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch, Path.Path))
getCurrentProjectBranch = do
  path <- Cli.getCurrentPath
  case preview projectBranchPathPrism path of
    Nothing -> pure Nothing
    Just (ProjectAndBranch projectId branchId, restPath) ->
      Cli.runTransaction do
        project <- Queries.expectProject projectId
        branch <- Queries.expectProjectBranch projectId branchId
        pure (Just (ProjectAndBranch project branch, restPath))

-- | Like 'getCurrentProjectBranch', but fails with a message if the user is not on a project branch.
expectCurrentProjectBranch :: Cli (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch, Path.Path)
expectCurrentProjectBranch =
  getCurrentProjectBranch & onNothingM (Cli.returnEarly Output.NotOnProjectBranch)

-- We often accept a `These ProjectName ProjectBranchName` from the user, so they can leave off either a project or
-- branch name, which we infer. This helper "hydrates" such a type to a `(ProjectName, BranchName)`, using the following
-- defaults if a name is missing:
--
--   * The project at the current path
--   * The branch named "main"
hydrateNames :: These ProjectName ProjectBranchName -> Cli (ProjectAndBranch ProjectName ProjectBranchName)
hydrateNames = \case
  This projectName -> pure (ProjectAndBranch projectName (unsafeFrom @Text "main"))
  That branchName -> do
    (ProjectAndBranch project _branch, _restPath) <- expectCurrentProjectBranch
    pure (ProjectAndBranch (project ^. #name) branchName)
  These projectName branchName -> pure (ProjectAndBranch projectName branchName)

-- Expect a local project+branch by ids.
expectProjectAndBranchByIds ::
  ProjectAndBranch ProjectId ProjectBranchId ->
  Sqlite.Transaction (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
expectProjectAndBranchByIds (ProjectAndBranch projectId branchId) = do
  project <- Queries.expectProject projectId
  branch <- Queries.expectProjectBranch projectId branchId
  pure (ProjectAndBranch project branch)

-- Expect a local project branch by a "these names", using the following defaults if a name is missing:
--
--   * The project at the current path
--   * The branch named "main"
expectProjectAndBranchByTheseNames ::
  These ProjectName ProjectBranchName ->
  Cli (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
expectProjectAndBranchByTheseNames = \case
  This projectName -> expectProjectAndBranchByTheseNames (These projectName (unsafeFrom @Text "main"))
  That branchName -> do
    (ProjectAndBranch project _branch, _restPath) <- expectCurrentProjectBranch
    branch <-
      Cli.runTransaction (Queries.loadProjectBranchByName (project ^. #projectId) branchName) & onNothingM do
        Cli.returnEarly (LocalProjectBranchDoesntExist (ProjectAndBranch (project ^. #name) branchName))
    pure (ProjectAndBranch project branch)
  These projectName branchName -> do
    maybeProjectAndBranch <-
      Cli.runTransaction do
        runMaybeT do
          project <- MaybeT (Queries.loadProjectByName projectName)
          branch <- MaybeT (Queries.loadProjectBranchByName (project ^. #projectId) branchName)
          pure (ProjectAndBranch project branch)
    maybeProjectAndBranch & onNothing do
      Cli.returnEarly (LocalProjectBranchDoesntExist (ProjectAndBranch projectName branchName))

------------------------------------------------------------------------------------------------------------------------
-- Remote project utils

expectRemoteProjectByName :: ProjectName -> Cli Share.RemoteProject
expectRemoteProjectByName remoteProjectName = do
  Share.getProjectByName remoteProjectName & onNothingM do
    Cli.returnEarly (Output.RemoteProjectDoesntExist Share.hardCodedUri remoteProjectName)

expectRemoteProjectBranchById ::
  ProjectAndBranch (RemoteProjectId, ProjectName) (RemoteProjectBranchId, ProjectBranchName) ->
  Cli Share.RemoteProjectBranch
expectRemoteProjectBranchById projectAndBranch = do
  Share.getProjectBranchById projectAndBranchIds >>= \case
    Share.GetProjectBranchResponseBranchNotFound -> remoteProjectBranchDoesntExist projectAndBranchNames
    Share.GetProjectBranchResponseProjectNotFound -> remoteProjectBranchDoesntExist projectAndBranchNames
    Share.GetProjectBranchResponseSuccess branch -> pure branch
  where
    projectAndBranchIds = projectAndBranch & over #project fst & over #branch fst
    projectAndBranchNames = projectAndBranch & over #project snd & over #branch snd

loadRemoteProjectBranchByName ::
  ProjectAndBranch RemoteProjectId ProjectBranchName ->
  Cli (Maybe Share.RemoteProjectBranch)
loadRemoteProjectBranchByName projectAndBranch =
  Share.getProjectBranchByName projectAndBranch <&> \case
    Share.GetProjectBranchResponseBranchNotFound -> Nothing
    Share.GetProjectBranchResponseProjectNotFound -> Nothing
    Share.GetProjectBranchResponseSuccess branch -> Just branch

expectRemoteProjectBranchByName ::
  ProjectAndBranch (RemoteProjectId, ProjectName) ProjectBranchName ->
  Cli Share.RemoteProjectBranch
expectRemoteProjectBranchByName projectAndBranch =
  Share.getProjectBranchByName (projectAndBranch & over #project fst) >>= \case
    Share.GetProjectBranchResponseBranchNotFound -> doesntExist
    Share.GetProjectBranchResponseProjectNotFound -> doesntExist
    Share.GetProjectBranchResponseSuccess branch -> pure branch
  where
    doesntExist =
      remoteProjectBranchDoesntExist (projectAndBranch & over #project snd)

loadRemoteProjectBranchByNames ::
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli (Maybe Share.RemoteProjectBranch)
loadRemoteProjectBranchByNames (ProjectAndBranch projectName branchName) =
  runMaybeT do
    project <- MaybeT (Share.getProjectByName projectName)
    MaybeT (loadRemoteProjectBranchByName (ProjectAndBranch (project ^. #projectId) branchName))

expectRemoteProjectBranchByNames ::
  ProjectAndBranch ProjectName ProjectBranchName ->
  Cli Share.RemoteProjectBranch
expectRemoteProjectBranchByNames (ProjectAndBranch projectName branchName) = do
  project <- expectRemoteProjectByName projectName
  expectRemoteProjectBranchByName (ProjectAndBranch (project ^. #projectId, project ^. #projectName) branchName)

-- Expect a remote project branch by a "these names".
--
--   If both names are provided, use them.
--
--   If only a project name is provided, use branch name "main".
--
--   If only a branch name is provided, use the current branch's remote mapping (falling back to its parent, etc) to get
--   the project.
expectRemoteProjectBranchByTheseNames :: These ProjectName ProjectBranchName -> Cli Share.RemoteProjectBranch
expectRemoteProjectBranchByTheseNames = \case
  This remoteProjectName -> do
    remoteProject <- expectRemoteProjectByName remoteProjectName
    let remoteProjectId = remoteProject ^. #projectId
    let remoteBranchName = unsafeFrom @Text "main"
    expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
  That branchName -> do
    (ProjectAndBranch localProject localBranch, _restPath) <- expectCurrentProjectBranch
    let localProjectId = localProject ^. #projectId
    let localBranchId = localBranch ^. #branchId
    Cli.runTransaction (Queries.loadRemoteProjectBranch localProjectId Share.hardCodedUri localBranchId) >>= \case
      Just (remoteProjectId, _maybeProjectBranchId) -> do
        remoteProjectName <- Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
        expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, remoteProjectName) branchName)
      Nothing -> do
        Cli.returnEarly $
          Output.NoAssociatedRemoteProject
            Share.hardCodedUri
            (ProjectAndBranch (localProject ^. #name) (localBranch ^. #name))
  These projectName branchName -> do
    remoteProject <- expectRemoteProjectByName projectName
    let remoteProjectId = remoteProject ^. #projectId
    expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, projectName) branchName)

remoteProjectBranchDoesntExist :: ProjectAndBranch ProjectName ProjectBranchName -> Cli void
remoteProjectBranchDoesntExist projectAndBranch =
  Cli.returnEarly (Output.RemoteProjectBranchDoesntExist Share.hardCodedUri projectAndBranch)

------------------------------------------------------------------------------------------------------------------------

-- | Get the path that a project is stored at. Users aren't supposed to go here.
--
-- >>> projectPath "ABCD"
-- .__projects._ABCD
projectPath :: ProjectId -> Path.Absolute
projectPath projectId =
  review projectPathPrism projectId

-- | Get the path that a project's branches are stored at. Users aren't supposed to go here.
--
-- >>> projectBranchesPath "ABCD"
-- .__projects._ABCD.branches
projectBranchesPath :: ProjectId -> Path.Absolute
projectBranchesPath projectId =
  snoc (projectPath projectId) "branches"

-- | Get the path that a branch is stored at. Users aren't supposed to go here.
--
-- >>> projectBranchPath ProjectAndBranch { project = "ABCD", branch = "DEFG" }
-- .__projects._ABCD.branches._DEFG
projectBranchPath :: ProjectAndBranch ProjectId ProjectBranchId -> Path.Absolute
projectBranchPath projectAndBranch =
  review projectBranchPathPrism (projectAndBranch, Path.empty)

-- | Get the name segment that a branch is stored at.
--
-- >>> projectBranchSegment "DEFG"
-- "_DEFG"
projectBranchSegment :: ProjectBranchId -> NameSegment
projectBranchSegment (ProjectBranchId branchId) =
  UUIDNameSegment branchId

pattern UUIDNameSegment :: UUID -> NameSegment
pattern UUIDNameSegment uuid <-
  NameSegment (Text.uncons -> Just ('_', UUID.fromText . Text.map (\c -> if c == '_' then '-' else c) -> Just uuid))
  where
    UUIDNameSegment uuid = NameSegment (Text.cons '_' (Text.map (\c -> if c == '-' then '_' else c) (UUID.toText uuid)))

-- | The prism between paths like
--
-- @
-- .__projects._XX_XX
-- @
--
-- and the project id
--
-- @
-- XX-XX
-- @
projectPathPrism :: Prism' Path.Absolute ProjectId
projectPathPrism =
  prism' toPath toId
  where
    toPath :: ProjectId -> Path.Absolute
    toPath projectId =
      Path.Absolute $
        Path.fromList
          [ "__projects",
            UUIDNameSegment (unProjectId projectId)
          ]

    toId :: Path.Absolute -> Maybe ProjectId
    toId path =
      case Path.toList (Path.unabsolute path) of
        ["__projects", UUIDNameSegment projectId] -> Just (ProjectId projectId)
        _ -> Nothing

-- | The prism between paths like
--
-- @
-- .__projects._XX_XX.branches._YY_YY.foo.bar
-- @
--
-- and the @(project id, branch id, path)@ triple
--
-- @
-- (XX-XX, YY-YY, foo.bar)
-- @
projectBranchPathPrism :: Prism' Path.Absolute (ProjectAndBranch ProjectId ProjectBranchId, Path.Path)
projectBranchPathPrism =
  prism' toPath toIds
  where
    toPath :: (ProjectAndBranch ProjectId ProjectBranchId, Path.Path) -> Path.Absolute
    toPath (ProjectAndBranch {project = projectId, branch = branchId}, restPath) =
      Path.Absolute $
        Path.fromList
          ( [ "__projects",
              UUIDNameSegment (unProjectId projectId),
              "branches",
              UUIDNameSegment (unProjectBranchId branchId)
            ]
              ++ Path.toList restPath
          )

    toIds :: Path.Absolute -> Maybe (ProjectAndBranch ProjectId ProjectBranchId, Path.Path)
    toIds path =
      case Path.toList (Path.unabsolute path) of
        "__projects" : UUIDNameSegment projectId : "branches" : UUIDNameSegment branchId : restPath ->
          Just (ProjectAndBranch {project = ProjectId projectId, branch = ProjectBranchId branchId}, Path.fromList restPath)
        _ -> Nothing
