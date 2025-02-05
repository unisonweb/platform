-- | Utility functions for downloading remote entities and storing them locally in SQLite.
--
-- These are shared by commands like `pull` and `clone`.
module Unison.Cli.DownloadUtils
  ( downloadProjectBranchFromShare,
    downloadLooseCodeFromShare,
    SyncVersion (..),
  )
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO, readTVar, readTVarIO)
import Data.List.NonEmpty (pattern (:|))
import Data.Set qualified as Set
import System.Console.Regions qualified as Console.Regions
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase.Editor.HandleInput.AuthLogin (ensureAuthenticatedWithCodeserver)
import Unison.Codebase.Editor.Input (SyncVersion (..))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.RemoteRepo (ReadShareLooseCode, shareUserHandleToText)
import Unison.Codebase.Editor.RemoteRepo qualified as RemoteRepo
import Unison.Codebase.Path qualified as Path
import Unison.Core.Project (ProjectAndBranch (..))
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Prelude
import Unison.Share.API.Hash qualified as Share
import Unison.Share.Codeserver qualified as Codeserver
import Unison.Share.Sync qualified as Share
import Unison.Share.Sync.Types qualified as Share
import Unison.Share.SyncV2 qualified as SyncV2
import Unison.Share.Types (codeserverBaseURL)
import Unison.Sync.Common qualified as Sync.Common
import Unison.Sync.Types qualified as Share
import Unison.SyncV2.Types qualified as SyncV2

-- | Download a project/branch from Share.
downloadProjectBranchFromShare ::
  (HasCallStack) =>
  SyncVersion ->
  Share.IncludeSquashedHead ->
  Share.RemoteProjectBranch ->
  Cli (Either Output.ShareError CausalHash)
downloadProjectBranchFromShare syncVersion useSquashed branch =
  Cli.labelE \done -> do
    let remoteProjectBranchName = branch.branchName
    causalHashJwt <-
      case (useSquashed, branch.squashedBranchHead) of
        (Share.IncludeSquashedHead, Nothing) -> done Output.ShareExpectedSquashedHead
        (Share.IncludeSquashedHead, Just squashedHead) -> pure squashedHead
        (Share.NoSquashedHead, _) -> pure branch.branchHead
    exists <- Cli.runTransaction (Queries.causalExistsByHash32 (Share.hashJWTHash causalHashJwt))
    when (not exists) do
      case syncVersion of
        SyncV1 -> do
          let repoInfo = Share.RepoInfo (into @Text (ProjectAndBranch branch.projectName remoteProjectBranchName))
          Cli.with withEntitiesDownloadedProgressCallback \(downloadedCallback, getNumDownloaded) -> do
            result <- Share.downloadEntities Share.hardCodedBaseUrl repoInfo causalHashJwt downloadedCallback
            numDownloaded <- liftIO getNumDownloaded
            result & onLeft \err0 -> do
              done case err0 of
                Share.SyncError err -> Output.ShareErrorDownloadEntities err
                Share.TransportError err -> Output.ShareErrorTransport err
            Cli.respond (Output.DownloadedEntities numDownloaded)
        SyncV2 -> do
          let branchRef = SyncV2.BranchRef (into @Text (ProjectAndBranch branch.projectName remoteProjectBranchName))
          -- TODO: Fill this in.
          let knownHashes = Set.empty
          let downloadedCallback = \_ -> pure ()
          let shouldValidate = not $ Codeserver.isCustomCodeserver Codeserver.defaultCodeserver
          result <- SyncV2.syncFromCodeserver shouldValidate Share.hardCodedBaseUrl branchRef causalHashJwt knownHashes downloadedCallback
          result & onLeft \err0 -> do
            done case err0 of
              Share.SyncError pullErr ->
                Output.ShareErrorPullV2 pullErr
              Share.TransportError err -> Output.ShareErrorTransport err
    pure (Sync.Common.hash32ToCausalHash (Share.hashJWTHash causalHashJwt))

-- | Download loose code from Share.
downloadLooseCodeFromShare :: ReadShareLooseCode -> Cli (Either Output.ShareError CausalHash)
downloadLooseCodeFromShare namespace = do
  let codeserver = Codeserver.resolveCodeserver namespace.server
  let baseURL = codeserverBaseURL codeserver

  -- Auto-login to share if pulling from a non-public path
  when (not (RemoteRepo.isPublic namespace)) do
    _userInfo <- ensureAuthenticatedWithCodeserver codeserver
    pure ()

  let shareFlavoredPath =
        Share.Path $
          shareUserHandleToText namespace.repo
            :| map NameSegment.toUnescapedText (Path.toList namespace.path)

  Cli.labelE \done -> do
    (causalHash, numDownloaded) <-
      Cli.with withEntitiesDownloadedProgressCallback \(downloadedCallback, getNumDownloaded) -> do
        causalHash <-
          Share.pull baseURL shareFlavoredPath downloadedCallback & onLeftM \err0 ->
            done case err0 of
              Share.SyncError err -> Output.ShareErrorPull err
              Share.TransportError err -> Output.ShareErrorTransport err
        numDownloaded <- liftIO getNumDownloaded
        pure (causalHash, numDownloaded)
    Cli.respond (Output.DownloadedEntities numDownloaded)
    pure causalHash

-- Provide the given action a callback that display to the terminal.
withEntitiesDownloadedProgressCallback :: ((Int -> IO (), IO Int) -> IO a) -> IO a
withEntitiesDownloadedProgressCallback action = do
  entitiesDownloadedVar <- newTVarIO 0
  Console.Regions.displayConsoleRegions do
    Console.Regions.withConsoleRegion Console.Regions.Linear \region -> do
      Console.Regions.setConsoleRegion region do
        entitiesDownloaded <- readTVar entitiesDownloadedVar
        pure $
          "\n  Downloaded "
            <> tShow entitiesDownloaded
            <> " entities...\n\n"
      action ((\n -> atomically (modifyTVar' entitiesDownloadedVar (+ n))), readTVarIO entitiesDownloadedVar)
