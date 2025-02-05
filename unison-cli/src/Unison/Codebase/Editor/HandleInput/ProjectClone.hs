-- | @clone@-related input handlers
module Unison.Codebase.Editor.HandleInput.ProjectClone
  ( handleClone,
  )
where

import Control.Lens (_2)
import Data.These (These (..))
import Data.UUID.V4 qualified as UUID
import U.Codebase.Sqlite.DbId (ProjectBranchId (..), ProjectId (..))
import U.Codebase.Sqlite.DbId qualified as Sqlite
import U.Codebase.Sqlite.Project qualified as Sqlite (Project (..))
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.DownloadUtils (SyncVersion (..), downloadProjectBranchFromShare)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli (getCurrentProjectAndBranch)
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectAndBranchNames (..), ProjectBranchName, ProjectName, projectNameUserSlug)
import Unison.Sqlite qualified as Sqlite
import Witch (unsafeFrom)

data LocalProjectKey
  = LocalProjectKey'Name ProjectName
  | LocalProjectKey'Project Sqlite.Project

data RemoteProjectKey
  = RemoteProjectKey'Id Sqlite.RemoteProjectId
  | RemoteProjectKey'Name ProjectName

-- | Clone a remote branch.
handleClone :: ProjectAndBranchNames -> Maybe ProjectAndBranchNames -> Cli ()
handleClone remoteNames0 maybeLocalNames0 = do
  currentProjectBranch <- Cli.getCurrentProjectAndBranch
  resolvedRemoteNames <- resolveRemoteNames Share.NoSquashedHead currentProjectBranch remoteNames0
  localNames1 <- resolveLocalNames currentProjectBranch resolvedRemoteNames maybeLocalNames0
  cloneInto localNames1 resolvedRemoteNames.branch

data ResolvedRemoteNames = ResolvedRemoteNames
  { branch :: Share.RemoteProjectBranch,
    from :: ResolvedRemoteNamesFrom
  }
  deriving stock (Generic)

data ResolvedRemoteNamesFrom
  = ResolvedRemoteNamesFrom'Branch
  | ResolvedRemoteNamesFrom'Project
  | ResolvedRemoteNamesFrom'ProjectAndBranch

-- Resolve remote names to an actual remote branch.
--
--   <project>/           ==>   abort if <project> doesn't have a user slug
--
--   /<branch>            ==>   abort if not in a project
--
--                              otherwise, abort if current branch doesn't have an associated remote project
--
--   <project>/<branch>   ==>   abort if <project> doesn't have a user slug
--
--   <thing>              ==>   if we're not in a project, then treat as if it was <thing>/
--
--                              otherwise, if <thing> doesn't have a user slug, treat it as /<thing>
--
--                              otherwise, if the current branch doesn't have an associated remote project, treat it as
--                              <thing>/
--
--                              otherwise, hit the server, and if <thing>/ xor /<thing> was valid (e.g. cloning the
--                              "@runar/topic" project-or-branch, where the "@runar/topic" branch does exist in the
--                              project in question, and the "@runar/topic" project does not exist), we'll do that,
--                              otherwise abort
resolveRemoteNames ::
  Share.IncludeSquashedHead ->
  (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch) ->
  ProjectAndBranchNames ->
  Cli ResolvedRemoteNames
resolveRemoteNames includeSquashed currentProjectAndBranch = \case
  ProjectAndBranchNames'Ambiguous remoteProjectName remoteBranchName -> do
    case projectNameUserSlug remoteProjectName of
      Nothing -> resolveB remoteBranchName
      Just _ ->
        Cli.runTransaction (loadAssociatedRemoteProjectId currentProjectAndBranch) >>= \case
          Nothing -> resolveP remoteProjectName
          Just remoteBranchProjectId -> do
            -- Fetching these in parallel would be an improvement
            maybeRemoteProject <- Share.getProjectByName remoteProjectName
            maybeRemoteBranch <-
              Share.getProjectBranchByName includeSquashed (ProjectAndBranch remoteBranchProjectId remoteBranchName) <&> \case
                Share.GetProjectBranchResponseBranchNotFound -> Nothing
                Share.GetProjectBranchResponseProjectNotFound -> Nothing
                Share.GetProjectBranchResponseSuccess remoteBranch -> Just remoteBranch
            case (maybeRemoteProject, maybeRemoteBranch) of
              (Just remoteProject, Nothing) -> do
                let remoteProjectId = remoteProject.projectId
                let remoteProjectName = remoteProject.projectName
                let remoteBranchName = unsafeFrom @Text "main"
                remoteBranch <-
                  ProjectUtils.expectRemoteProjectBranchByName
                    includeSquashed
                    (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
                pure
                  ResolvedRemoteNames
                    { branch = remoteBranch,
                      from = ResolvedRemoteNamesFrom'Project
                    }
              (Nothing, Just remoteBranch) ->
                pure
                  ResolvedRemoteNames
                    { branch = remoteBranch,
                      from = ResolvedRemoteNamesFrom'Branch
                    }
              -- Treat neither existing and both existing uniformly as "ambiguous input"
              -- Alternatively, if neither exist, we could instead say "although your input was ambiguous, disambuating
              -- wouldn't help, because we did enough work to know neither thing exists"
              _ -> do
                branchProjectName <-
                  Cli.runTransaction (Queries.expectRemoteProjectName remoteBranchProjectId Share.hardCodedUri)
                Cli.returnEarly $
                  Output.AmbiguousCloneRemote
                    remoteProjectName
                    (ProjectAndBranch branchProjectName remoteBranchName)
  ProjectAndBranchNames'Unambiguous (This p) -> resolveP p
  ProjectAndBranchNames'Unambiguous (That b) -> resolveB b
  ProjectAndBranchNames'Unambiguous (These p b) -> resolvePB p b
  where
    resolveB branchName = do
      remoteProjectId <-
        Cli.runTransaction (loadAssociatedRemoteProjectId currentProjectAndBranch) & onNothingM do
          Cli.returnEarly (Output.NoAssociatedRemoteProjectBranch Share.hardCodedUri currentProjectAndBranch)
      branch <- expectB (RemoteProjectKey'Id remoteProjectId) branchName
      pure ResolvedRemoteNames {branch, from = ResolvedRemoteNamesFrom'Branch}

    resolveP projectName = do
      assertProjectNameHasUserSlug projectName
      branch <- expectB (RemoteProjectKey'Name projectName) (unsafeFrom @Text "main")
      pure ResolvedRemoteNames {branch, from = ResolvedRemoteNamesFrom'Project}

    resolvePB projectName branchName = do
      assertProjectNameHasUserSlug projectName
      branch <- expectB (RemoteProjectKey'Name projectName) branchName
      pure ResolvedRemoteNames {branch, from = ResolvedRemoteNamesFrom'ProjectAndBranch}

    expectB remoteProjectKey remoteBranchName =
      case remoteProjectKey of
        RemoteProjectKey'Id remoteProjectId -> do
          remoteProjectName <- Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
          ProjectUtils.expectRemoteProjectBranchByName includeSquashed (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
        RemoteProjectKey'Name remoteProjectName ->
          ProjectUtils.expectRemoteProjectBranchByNames includeSquashed (ProjectAndBranch remoteProjectName remoteBranchName)

-- Resolve the local names to an actual local project (which may not exist yet), aborting on nonsense
-- inputs:
--
--   <project>/           ==>   if we already know the remote branch name is <branch>, abort if <project>/<branch>
--                              already exists
--
--   /<branch>            ==>   abort if not in a project
--
--                              abort if <branch> already exists in this project
--
--   <project>/<branch>   ==>   abort if <project>/<branch> already exists
--
--   <thing>              ==>   if we're not in a project, then treat as if it was <thing>/
--
--                              otherwise, <thing> is ambiguous, as we don't know if the user wants to clone into
--                              <thing>/<branch> (where <branch> is determined by the name of the remote branch we
--                              are cloning), or /<thing>
--
-- The resolved remote names are used to fill in missing local names (i.e. a one-argument clone). For example, if
-- `clone @foo/bar` resulted in treating `@foo/bar` as a contributor branch of the current project, then it is as if
-- the user typed `clone /@foo/bar` instead, which is equivalent to the two-arg `clone /@foo/bar /@foo/bar`.
resolveLocalNames ::
  (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch) ->
  ResolvedRemoteNames ->
  Maybe ProjectAndBranchNames ->
  Cli (ProjectAndBranch LocalProjectKey ProjectBranchName)
resolveLocalNames (ProjectAndBranch currentProject _) resolvedRemoteNames maybeLocalNames =
  resolve case maybeLocalNames of
    Nothing ->
      ProjectAndBranchNames'Unambiguous case resolvedRemoteNames.from of
        ResolvedRemoteNamesFrom'Branch -> That remoteBranchName
        ResolvedRemoteNamesFrom'Project -> This remoteProjectName
        ResolvedRemoteNamesFrom'ProjectAndBranch -> These remoteProjectName remoteBranchName
    Just localNames -> localNames
  where
    remoteBranchName = resolvedRemoteNames.branch.branchName
    remoteProjectName = resolvedRemoteNames.branch.projectName

    resolve names =
      case names of
        ProjectAndBranchNames'Ambiguous localProjectName localBranchName -> do
          Cli.returnEarly $
            Output.AmbiguousCloneLocal
              (ProjectAndBranch localProjectName remoteBranchName)
              (ProjectAndBranch currentProject.name localBranchName)
        ProjectAndBranchNames'Unambiguous (This localProjectName) -> resolveP localProjectName
        ProjectAndBranchNames'Unambiguous (That localBranchName) -> resolveB localBranchName
        ProjectAndBranchNames'Unambiguous (These localProjectName localBranchName) -> resolvePB localProjectName localBranchName

    resolveP localProjectName =
      go (LocalProjectKey'Name localProjectName) remoteBranchName

    resolveB localBranchName = do
      go (LocalProjectKey'Project currentProject) localBranchName

    resolvePB localProjectName localBranchName =
      go (LocalProjectKey'Name localProjectName) localBranchName

    go project branch = do
      void $
        Cli.runTransactionWithRollback \rollback ->
          assertLocalProjectBranchDoesntExist rollback (ProjectAndBranch project branch)
      pure (ProjectAndBranch project branch)

-- `cloneInto command local remote` clones `remote` into `local`, which is believed to not exist yet, but may (because
-- it takes some time to pull the remote).
cloneInto :: ProjectAndBranch LocalProjectKey ProjectBranchName -> Share.RemoteProjectBranch -> Cli ()
cloneInto localProjectBranch remoteProjectBranch = do
  let remoteProjectName = remoteProjectBranch.projectName
  let remoteBranchName = remoteProjectBranch.branchName
  let remoteProjectBranchNames = ProjectAndBranch remoteProjectName remoteBranchName

  branchHead <-
    downloadProjectBranchFromShare SyncV1 Share.NoSquashedHead remoteProjectBranch
      & onLeftM (Cli.returnEarly . Output.ShareError)

  localProjectAndBranch <-
    Cli.runTransactionWithRollback \rollback -> do
      -- Repeat the check from before, because (although it's highly unlikely) we could have a name conflict after
      -- downloading the remote branch
      maybeLocalProject <- assertLocalProjectBranchDoesntExist rollback localProjectBranch
      -- Create the local project (if necessary), and create the local branch
      (localProjectId, localProjectName) <-
        case maybeLocalProject of
          Left localProjectName -> do
            localProjectId <- Sqlite.unsafeIO (ProjectId <$> UUID.nextRandom)
            Queries.insertProject localProjectId localProjectName
            pure (localProjectId, localProjectName)
          Right localProject -> pure (localProject.projectId, localProject.name)
      localBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
      causalHashId <- Q.expectCausalHashIdByCausalHash branchHead
      let description = "Cloned from " <> into @Text (ProjectAndBranch remoteProjectName remoteBranchName)
      Queries.insertProjectBranch
        description
        causalHashId
        Sqlite.ProjectBranch
          { projectId = localProjectId,
            branchId = localBranchId,
            name = localProjectBranch.branch,
            parentBranchId = Nothing
          }
      Queries.insertBranchRemoteMapping
        localProjectId
        localBranchId
        remoteProjectBranch.projectId
        Share.hardCodedUri
        remoteProjectBranch.branchId
      pure (ProjectAndBranch (localProjectId, localProjectName) localBranchId)

  Cli.respond $
    Output.ClonedProjectBranch
      remoteProjectBranchNames
      ( ProjectAndBranch
          (localProjectAndBranch ^. #project . _2)
          localProjectBranch.branch
      )

  let newProjectAndBranch = (over #project fst localProjectAndBranch)
  Cli.switchProject newProjectAndBranch

-- Return the remote project id associated with the given project branch
loadAssociatedRemoteProjectId ::
  ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch ->
  Sqlite.Transaction (Maybe Sqlite.RemoteProjectId)
loadAssociatedRemoteProjectId (ProjectAndBranch project branch) =
  fmap fst <$> Queries.loadRemoteProjectBranch projectId Share.hardCodedUri branchId
  where
    projectId = project.projectId
    branchId = branch.branchId

assertProjectNameHasUserSlug :: ProjectName -> Cli ()
assertProjectNameHasUserSlug projectName =
  void $
    projectNameUserSlug projectName
      & onNothing (Cli.returnEarly (Output.ProjectNameRequiresUserSlug projectName))

-- Assert that a local project+branch with this name doesn't already exist. If it does exist, we can't clone over it.
assertLocalProjectBranchDoesntExist ::
  (forall void. Output.Output -> Sqlite.Transaction void) ->
  ProjectAndBranch LocalProjectKey ProjectBranchName ->
  Sqlite.Transaction (Either ProjectName Sqlite.Project)
assertLocalProjectBranchDoesntExist rollback = \case
  ProjectAndBranch (LocalProjectKey'Name projectName) branchName ->
    Queries.loadProjectByName projectName >>= \case
      Nothing -> pure (Left projectName)
      Just project -> go project branchName
  ProjectAndBranch (LocalProjectKey'Project project) branchName -> go project branchName
  where
    go project branchName = do
      Queries.projectBranchExistsByName project.projectId branchName & onTrueM do
        rollback (Output.ProjectAndBranchNameAlreadyExists (ProjectAndBranch project.name branchName))
      pure (Right project)
