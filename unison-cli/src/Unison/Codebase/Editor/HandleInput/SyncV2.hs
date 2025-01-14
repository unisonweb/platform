module Unison.Codebase.Editor.HandleInput.SyncV2
  ( handleSyncToFile,
    handleSyncFromFile,
    handleSyncFromCodebase,
    handleSyncFromCodeserver,
  )
where

import Control.Lens
import Control.Monad.Reader (MonadReader (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Project
import Unison.Codebase (CodebasePath)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Init qualified as Init
import Unison.Codebase.SqliteCodebase qualified as SqliteCodebase
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Share.SyncV2 qualified as SyncV2
import Unison.SyncV2.Types (BranchRef)
import Unison.Cli.DownloadUtils (SyncVersion, downloadProjectBranchFromShare)

handleSyncToFile :: FilePath -> ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName) -> Cli ()
handleSyncToFile destSyncFile branchToSync = do
  pp <- Cli.getCurrentProjectPath
  projectBranch <- Project.resolveProjectBranchInProject (pp ^. #project) branchToSync
  causalHash <- Cli.runTransaction $ Project.getProjectBranchCausalHash (projectBranch ^. #branch)
  let branchRef = into @BranchRef $ ProjectAndBranch (projectBranch ^. #project . #name) (projectBranch ^. #branch . #name)
  Cli.Env {codebase} <- ask
  liftIO (SyncV2.syncToFile codebase causalHash (Just branchRef) destSyncFile) >>= \case
    Left err -> Cli.respond (Output.SyncPullError err)
    Right _ -> pure ()

handleSyncFromFile :: Text -> FilePath -> ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleSyncFromFile description srcSyncFile branchToSync = do
  pp <- Cli.getCurrentProjectPath
  projectBranch <- Project.resolveProjectBranchInProject (pp ^. #project) (over #branch Just branchToSync)
  let shouldValidate = True
  SyncV2.syncFromFile shouldValidate srcSyncFile >>= \case
    Left err -> Cli.respond (Output.SyncPullError err)
    Right causalHash -> do
      Cli.setProjectBranchRootToCausalHash (projectBranch ^. #branch) description causalHash

handleSyncFromCodebase :: Text -> CodebasePath -> ProjectAndBranch ProjectName ProjectBranchName -> ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleSyncFromCodebase description srcCodebasePath srcBranch destBranch = do
  Cli.Env {codebase} <- ask
  pp <- Cli.getCurrentProjectPath
  projectBranch <- Project.resolveProjectBranchInProject (pp ^. #project) (over #branch Just destBranch)
  r <- liftIO $ Init.withOpenCodebase SqliteCodebase.init "sync-src" srcCodebasePath Init.DontLock (Init.MigrateAfterPrompt Init.Backup Init.Vacuum) \srcCodebase -> do
    Codebase.withConnection srcCodebase \srcConn -> do
      maySrcCausalHash <- Codebase.runTransaction srcCodebase $ do
        let ProjectAndBranch srcProjName srcBranchName = srcBranch
        runMaybeT do
          project <- MaybeT (Q.loadProjectByName srcProjName)
          branch <- MaybeT (Q.loadProjectBranchByName (project ^. #projectId) srcBranchName)
          lift $ Project.getProjectBranchCausalHash branch
      case maySrcCausalHash of
        Nothing -> pure $ Left (Output.SyncFromCodebaseMissingProjectBranch srcBranch)
        Just srcCausalHash -> do
          let shouldValidate = True
          Right . fmap (const srcCausalHash) <$> liftIO (SyncV2.syncFromCodebase shouldValidate srcConn codebase srcCausalHash)

  case r of
    Left openCodebaseErr -> Cli.respond (Output.OpenCodebaseError srcCodebasePath openCodebaseErr)
    Right (Left errOutput) -> Cli.respond errOutput
    Right (Right (Right causalHash)) -> do
      Cli.setProjectBranchRootToCausalHash (projectBranch ^. #branch) description causalHash
    Right (Right (Left syncErr)) -> do
      Cli.respond (Output.SyncPullError syncErr)

handleSyncFromCodebase :: Text -> CodebasePath -> ProjectAndBranch ProjectName ProjectBranchName -> ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleSyncFromCodebase description srcCodebasePath srcBranch destBranch = do
  Cli.Env {codebase} <- ask
  pp <- Cli.getCurrentProjectPath
  projectBranch <- Project.resolveProjectBranchInProject (pp ^. #project) (over #branch Just destBranch)
  r <- liftIO $ Init.withOpenCodebase SqliteCodebase.init "sync-src" srcCodebasePath Init.DontLock (Init.MigrateAfterPrompt Init.Backup Init.Vacuum) \srcCodebase -> do
    Codebase.withConnection srcCodebase \srcConn -> do
      maySrcCausalHash <- Codebase.runTransaction srcCodebase $ do
        let ProjectAndBranch srcProjName srcBranchName = srcBranch
        runMaybeT do
          project <- MaybeT (Q.loadProjectByName srcProjName)
          branch <- MaybeT (Q.loadProjectBranchByName (project ^. #projectId) srcBranchName)
          lift $ Project.getProjectBranchCausalHash branch
      case maySrcCausalHash of
        Nothing -> pure $ Left (error "Todo proper error")
        Just srcCausalHash -> do
          let shouldValidate = True
          fmap (const srcCausalHash) <$> liftIO (SyncV2.syncFromCodebase shouldValidate srcConn codebase srcCausalHash)

  case r of
    Left _err -> pure $ error "Todo proper error"
    Right (Left syncErr) -> Cli.respond (Output.SyncPullError syncErr)
    Right (Right causalHash) -> do
      Cli.setProjectBranchRootToCausalHash (projectBranch ^. #branch) description causalHash

handleSyncFromCodeserver :: SyncVersion -> Projects.IncludeSquashedHead -> Projects.RemoteProjectBranch -> Cli (Either Output.ShareError CausalHash)
handleSyncFromCodeserver = downloadProjectBranchFromShare
