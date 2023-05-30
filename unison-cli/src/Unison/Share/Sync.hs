{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Share.Sync
  ( -- ** Get causal hash by path
    getCausalHashByPath,
    GetCausalHashByPathError (..),

    -- ** Push
    checkAndSetPush,
    CheckAndSetPushError (..),
    fastForwardPush,
    FastForwardPushError (..),
    uploadEntities,

    -- ** Pull
    pull,
    PullError (..),
    downloadEntities,
  )
where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Foldable qualified as Foldable (find)
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Proxy
import Data.Sequence.NonEmpty (NESeq ((:<||)))
import Data.Sequence.NonEmpty qualified as NESeq (fromList, nonEmptySeq, (><|))
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy
import Ki qualified
import Network.HTTP.Client qualified as Http.Client
import Network.HTTP.Types qualified as HTTP
import Servant.API qualified as Servant ((:<|>) (..), (:>))
import Servant.Client (BaseUrl)
import Servant.Client qualified as Servant
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Auth.HTTPClient qualified as Auth
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Debug qualified as Debug
import Unison.Hash32 (Hash32)
import Unison.Prelude
import Unison.Share.API.Hash qualified as Share
import Unison.Share.Sync.Types
import Unison.Sqlite qualified as Sqlite
import Unison.Sync.API qualified as Share (API)
import Unison.Sync.Common (causalHashToHash32, entityToTempEntity, expectEntity, hash32ToCausalHash)
import Unison.Sync.Types qualified as Share
import Unison.Util.Monoid (foldMapM)

------------------------------------------------------------------------------------------------------------------------
-- Pile of constants

-- | The maximum number of downloader threads, during a pull.
maxSimultaneousPullDownloaders :: Int
maxSimultaneousPullDownloaders = 5

-- | The maximum number of push workers at a time. Each push worker reads from the database and uploads entities.
maxSimultaneousPushWorkers :: Int
maxSimultaneousPushWorkers = 5

------------------------------------------------------------------------------------------------------------------------
-- Push

-- | Perform a check-and-set push (initially of just a causal hash, but ultimately all of its dependencies that the
-- server is missing, too) to Unison Share.
--
-- This flavor of push takes the expected state of the server, and the desired state we want to set; if our expectation
-- is off, we won't proceed with the push.
checkAndSetPush ::
  -- | The Unison Share URL.
  BaseUrl ->
  -- | The repo+path to push to.
  Share.Path ->
  -- | The hash that we expect this repo+path to be at on Unison Share. If not, we'll get back a hash mismatch error.
  -- This prevents accidentally pushing over data that we didn't know was there.
  Maybe Hash32 ->
  -- | The hash of our local causal to push.
  CausalHash ->
  -- | Callback that's given a number of entities we just uploaded.
  (Int -> IO ()) ->
  Cli (Either (SyncError CheckAndSetPushError) ())
checkAndSetPush unisonShareUrl path expectedHash causalHash uploadedCallback = do
  Cli.Env {authHTTPClient} <- ask

  Cli.label \done -> do
    let failed :: SyncError CheckAndSetPushError -> Cli void
        failed = done . Left

    let updatePathError :: Share.UpdatePathError -> Cli void
        updatePathError err =
          failed (SyncError (CheckAndSetPushError'UpdatePath (Share.pathRepoInfo path) err))

    let updatePath :: Cli Share.UpdatePathResponse
        updatePath = do
          liftIO request & onLeftM \err -> failed (TransportError err)
          where
            request :: IO (Either CodeserverTransportError Share.UpdatePathResponse)
            request =
              httpUpdatePath
                authHTTPClient
                unisonShareUrl
                Share.UpdatePathRequest
                  { path,
                    expectedHash,
                    newHash = causalHashToHash32 causalHash
                  }

    -- Maybe the server already has this causal; try just setting its remote path. Commonly, it will respond that it
    -- needs this causal (UpdatePathMissingDependencies).
    dependencies <-
      updatePath >>= \case
        Share.UpdatePathSuccess -> done (Right ())
        Share.UpdatePathFailure err ->
          case err of
            Share.UpdatePathError'MissingDependencies (Share.NeedDependencies dependencies) -> pure dependencies
            _ -> updatePathError err

    -- Upload the causal and all of its dependencies.
    uploadEntities unisonShareUrl (Share.pathRepoInfo path) dependencies uploadedCallback & onLeftM \err ->
      failed (CheckAndSetPushError'UploadEntities <$> err)

    -- After uploading the causal and all of its dependencies, try setting the remote path again.
    updatePath >>= \case
      Share.UpdatePathSuccess -> pure (Right ())
      Share.UpdatePathFailure err -> updatePathError err

-- | Perform a fast-forward push (initially of just a causal hash, but ultimately all of its dependencies that the
-- server is missing, too) to Unison Share.
--
-- This flavor of push provides the server with a chain of causal hashes leading from its current state to our desired
-- state.
fastForwardPush ::
  -- | The Unison Share URL.
  BaseUrl ->
  -- | The repo+path to push to.
  Share.Path ->
  -- | The hash of our local causal to push.
  CausalHash ->
  -- | Callback that's given a number of entities we just uploaded.
  (Int -> IO ()) ->
  Cli (Either (SyncError FastForwardPushError) ())
fastForwardPush unisonShareUrl path localHeadHash uploadedCallback = do
  Cli.label \done -> do
    let succeeded :: Cli void
        succeeded =
          done (Right ())

    let failed :: SyncError FastForwardPushError -> Cli void
        failed = done . Left

    let fastForwardPathError :: Share.FastForwardPathError -> Cli void
        fastForwardPathError err =
          failed (SyncError (FastForwardPushError'FastForwardPath path err))

    remoteHeadHash <-
      getCausalHashByPath unisonShareUrl path >>= \case
        Left err -> failed (FastForwardPushError'GetCausalHash <$> err)
        Right Nothing -> fastForwardPathError Share.FastForwardPathError'NoHistory
        Right (Just remoteHeadHash) -> pure (Share.hashJWTHash remoteHeadHash)

    let doLoadCausalSpineBetween = do
          -- (Temporary?) optimization - perform the "is ancestor?" check within sqlite before reconstructing the
          -- actual path.
          let isBefore :: Sqlite.Transaction Bool
              isBefore = do
                maybeHashIds <-
                  runMaybeT $
                    (,)
                      <$> MaybeT (Q.loadCausalHashIdByCausalHash (hash32ToCausalHash remoteHeadHash))
                      <*> MaybeT (Q.loadCausalHashIdByCausalHash localHeadHash)
                case maybeHashIds of
                  Nothing -> pure False
                  Just (remoteHeadHashId, localHeadHashId) -> Q.before remoteHeadHashId localHeadHashId
          isBefore >>= \case
            False -> pure Nothing
            True -> loadCausalSpineBetween remoteHeadHash (causalHashToHash32 localHeadHash)

    let doUpload :: List.NonEmpty CausalHash -> Cli ()
        -- Maybe we could save round trips here by including the tail (or the head *and* the tail) as "extra hashes",
        -- but we don't have that API yet. So, we only upload the head causal entity (which we don't even know for sure
        -- the server doesn't have yet), and will (eventually) end up uploading the casuals in the tail that the server
        -- needs.
        doUpload (headHash :| _tailHashes) =
          request & onLeftM \err -> failed (FastForwardPushError'UploadEntities <$> err)
          where
            request =
              uploadEntities
                unisonShareUrl
                (Share.pathRepoInfo path)
                (NESet.singleton (causalHashToHash32 headHash))
                uploadedCallback

    localInnerHashes <-
      Cli.runTransaction doLoadCausalSpineBetween >>= \case
        -- After getting the remote causal hash, we can tell from a local computation that this wouldn't be a
        -- fast-forward push, so we don't bother trying - just report the error now.
        Nothing -> failed (SyncError (FastForwardPushError'NotFastForward path))
        -- The path from remote-to-local, excluding local, was empty. So, remote == local; there's nothing to push.
        Just [] -> succeeded
        -- drop remote hash
        Just (_ : localInnerHashes) -> pure (map hash32ToCausalHash localInnerHashes)

    doUpload (localHeadHash :| localInnerHashes)

    let doFastForwardPath :: Cli Share.FastForwardPathResponse
        doFastForwardPath = do
          Cli.Env {authHTTPClient} <- ask
          let request =
                httpFastForwardPath
                  authHTTPClient
                  unisonShareUrl
                  Share.FastForwardPathRequest
                    { expectedHash = remoteHeadHash,
                      hashes =
                        causalHashToHash32 <$> List.NonEmpty.fromList (localInnerHashes ++ [localHeadHash]),
                      path
                    }
          liftIO request & onLeftM \err -> failed (TransportError err)

    doFastForwardPath >>= \case
      Share.FastForwardPathSuccess -> succeeded
      Share.FastForwardPathFailure err -> fastForwardPathError err

-- Return a list (in oldest-to-newest order) of hashes along the causal spine that connects the given arguments,
-- excluding the newest hash (second argument).
loadCausalSpineBetween :: Hash32 -> Hash32 -> Sqlite.Transaction (Maybe [Hash32])
loadCausalSpineBetween earlierHash laterHash =
  dagbfs (== earlierHash) Q.loadCausalParentsByHash laterHash

data Step a
  = DeadEnd
  | KeepSearching (List.NonEmpty a)
  | FoundGoal a

-- | @dagbfs goal children root@ searches breadth-first through the monadic tree formed by applying @chilred@ to each
-- node (initially @root@), until it finds a goal node (i.e. when @goal@ returns True).
--
-- Returns the nodes along a path from root to goal in bottom-up or goal-to-root order, excluding the root node (because
-- it was provided as an input ;))
--
-- For example, when searching a tree that looks like
--
--                    1
--                   / \
--                  2   3
--                 / \   \
--                4  [5]  6
--
-- (where the goal is marked [5]), we'd return
--
--                Just [5,2]
--
-- And (as another example), if the root node is the goal,
--
--                   [1]
--                   / \
--                  2   3
--                 / \   \
--                4   5   6
--
-- we'd return
--
--                Just []
dagbfs :: forall a m. (Monad m) => (a -> Bool) -> (a -> m [a]) -> a -> m (Maybe [a])
dagbfs goal children =
  let -- The loop state: all distinct paths from the root to the frontier (not including the root, because it's implied,
      -- as an input to this function), in reverse order, with the invariant that we haven't found a goal state yet.
      -- (Otherwise, we wouldn't still be in this loop, we'd return!).
      --
      -- For example, say we are exploring the tree
      --
      --                    1
      --                   / \
      --                  2   3
      --                 / \   \
      --                4   5   6
      --
      -- Graphically, the frontier here is the nodes 4, 5, and 6; we know that, because we haven't drawn any nodes below
      -- them. (This is a BFS algorithm that discovers children on-the-fly, so maybe node 5 (for example) has children,
      -- and maybe it doesn't).
      --
      -- The loop state, in this case, would be these three paths:
      --
      --   [ 4, 2 ]
      --   [ 5, 2 ]
      --   [ 6, 3 ]
      --
      -- (Note, again, that we do not include the root).
      go :: NESeq (List.NonEmpty a) -> m (Maybe (List.NonEmpty a))
      go (path :<|| paths) =
        -- Step forward from the first path in our loop state (in the example above, [4, 2]).
        step (List.NonEmpty.head path) >>= \case
          -- If node 4 had no more children, we can toss that whole path: it didn't end in a goal. Now we either keep
          -- searching (as we would in the example, since we have two more paths to continue from), or we don't, because
          -- this was the only remaining path.
          DeadEnd ->
            case NESeq.nonEmptySeq paths of
              Nothing -> pure Nothing
              Just paths' -> go paths'
          -- If node 4 did have children, then maybe the search tree now looks like this.
          --
          --                1
          --               / \
          --              2   3
          --             / \   \
          --            4   5   6
          --           / \
          --          7   8
          --
          -- There are two cases to handle:
          --
          --   1. One of the children we just discovered (say 7) is a goal node. So we're done, and we'd return the path
          --
          --        [ 7, 4, 2 ]
          --
          --   2. No child we just discovered (7 nor 8) were a goal node. So we loop, putting our new path(s) at the end
          --      of the list (so we search paths fairly). In this case, we'd re-enter the loop with the following four
          --      paths:
          --
          --        [ 5, 2 ]      \ these two are are variable 'paths', the tail of the loop state.
          --        [ 6, 3 ]      /
          --        [ 7, 4, 2 ]   \ these two are new, just constructed by prepending each of [ 4, 2, 1 ]'s children
          --        [ 8, 4, 2 ]   / to itself, making two new paths to search
          KeepSearching ys -> go (append paths ((\y -> List.NonEmpty.cons y path) <$> NESeq.fromList ys))
          FoundGoal y -> pure (Just (List.NonEmpty.cons y path))

      -- Step forward from a single node. There are 3 possible outcomes:
      --
      --   1. We discover it has no children. (return DeadEnd)
      --   2. We discover is has children, none of which are a goal. (return KeepSearching)
      --   3. We discover it has children, (at least) one of which is a goal. (return FoundGoal)
      step :: a -> m (Step a)
      step x = do
        ys0 <- children x
        pure case List.NonEmpty.nonEmpty ys0 of
          Nothing -> DeadEnd
          Just ys ->
            case Foldable.find goal ys of
              Nothing -> KeepSearching ys
              Just y -> FoundGoal y
   in \root ->
        if goal root
          then pure (Just [])
          else
            step root >>= \case
              DeadEnd -> pure Nothing
              -- lts-18.28 doesn't have List.NonEmpty.singleton
              KeepSearching xs -> fmap List.NonEmpty.toList <$> go (NESeq.fromList ((:| []) <$> xs))
              FoundGoal x -> pure (Just [x])
  where
    -- Concatenate a seq and a non-empty seq.
    append :: Seq x -> NESeq x -> NESeq x
    append = (NESeq.><|)

------------------------------------------------------------------------------------------------------------------------
-- Pull

pull ::
  -- | The Unison Share URL.
  BaseUrl ->
  -- | The repo+path to pull from.
  Share.Path ->
  -- | Callback that's given a number of entities we just downloaded.
  (Int -> IO ()) ->
  Cli (Either (SyncError PullError) CausalHash)
pull unisonShareUrl repoPath downloadedCallback =
  getCausalHashByPath unisonShareUrl repoPath >>= \case
    Left err -> pure (Left (PullError'GetCausalHash <$> err))
    -- There's nothing at the remote path, so there's no causal to pull.
    Right Nothing -> pure (Left (SyncError (PullError'NoHistoryAtPath repoPath)))
    Right (Just hashJwt) ->
      downloadEntities unisonShareUrl (Share.pathRepoInfo repoPath) hashJwt downloadedCallback <&> \case
        Left err -> Left (PullError'DownloadEntities <$> err)
        Right () -> Right (hash32ToCausalHash (Share.hashJWTHash hashJwt))

------------------------------------------------------------------------------------------------------------------------
-- Download entities

downloadEntities ::
  -- | The Unison Share URL.
  BaseUrl ->
  -- | The repo to download from.
  Share.RepoInfo ->
  -- | The hash to download.
  Share.HashJWT ->
  -- | Callback that's given a number of entities we just downloaded.
  (Int -> IO ()) ->
  Cli (Either (SyncError Share.DownloadEntitiesError) ())
downloadEntities unisonShareUrl repoInfo hashJwt downloadedCallback = do
  Cli.Env {authHTTPClient, codebase} <- ask

  Cli.label \done -> do
    let failed :: SyncError Share.DownloadEntitiesError -> Cli void
        failed = done . Left

    let hash = Share.hashJWTHash hashJwt

    maybeTempEntities <-
      Cli.runTransaction (Q.entityLocation hash) >>= \case
        Just Q.EntityInMainStorage -> pure Nothing
        Just Q.EntityInTempStorage -> pure (Just (NESet.singleton hash))
        Nothing -> do
          let request =
                httpDownloadEntities
                  authHTTPClient
                  unisonShareUrl
                  Share.DownloadEntitiesRequest {repoInfo, hashes = NESet.singleton hashJwt}
          entities <-
            liftIO request >>= \case
              Left err -> failed (TransportError err)
              Right (Share.DownloadEntitiesFailure err) -> failed (SyncError err)
              Right (Share.DownloadEntitiesSuccess entities) -> pure entities
          tempEntities <- Cli.runTransaction (insertEntities entities)
          liftIO (downloadedCallback 1)
          pure (NESet.nonEmptySet tempEntities)

    whenJust maybeTempEntities \tempEntities -> do
      let doCompleteTempEntities =
            completeTempEntities
              authHTTPClient
              unisonShareUrl
              ( \action ->
                  Codebase.withConnection codebase \conn ->
                    action (Sqlite.runTransaction conn)
              )
              repoInfo
              downloadedCallback
              tempEntities
      liftIO doCompleteTempEntities & onLeftM \err ->
        failed err

    -- Since we may have just inserted and then deleted many temp entities, we attempt to recover some disk space by
    -- vacuuming after each pull. If the vacuum fails due to another open transaction on this connection, that's ok,
    -- we'll try vacuuming again next pull.
    _success <- liftIO (Codebase.withConnection codebase Sqlite.vacuum)
    pure (Right ())

type WorkerCount =
  TVar Int

newWorkerCount :: IO WorkerCount
newWorkerCount =
  newTVarIO 0

recordWorking :: WorkerCount -> STM ()
recordWorking sem =
  modifyTVar' sem (+ 1)

recordNotWorking :: WorkerCount -> STM ()
recordNotWorking sem =
  modifyTVar' sem \n -> n - 1

-- What the dispatcher is to do
data DispatcherJob
  = DispatcherForkWorker (NESet Share.HashJWT)
  | DispatcherReturnEarlyBecauseDownloaderFailed (SyncError Share.DownloadEntitiesError)
  | DispatcherDone

-- | Finish downloading entities from Unison Share (or return the first failure to download something).
--
-- Precondition: the entities were *already* downloaded at some point in the past, and are now sitting in the
-- `temp_entity` table, waiting for their dependencies to arrive so they can be flushed to main storage.
completeTempEntities ::
  AuthenticatedHttpClient ->
  BaseUrl ->
  (forall a. ((forall x. Sqlite.Transaction x -> IO x) -> IO a) -> IO a) ->
  Share.RepoInfo ->
  (Int -> IO ()) ->
  NESet Hash32 ->
  IO (Either (SyncError Share.DownloadEntitiesError) ())
completeTempEntities httpClient unisonShareUrl connect repoInfo downloadedCallback initialNewTempEntities = do
  -- The set of hashes we still need to download
  hashesVar <- newTVarIO Set.empty

  -- The set of hashes that we haven't inserted yet, but will soon, because we've committed to downloading them.
  uninsertedHashesVar <- newTVarIO Set.empty

  -- The entities payloads (along with the jwts that we used to download them) that we've downloaded
  entitiesQueue <- newTQueueIO

  -- The sets of new (at the time of inserting, anyway) temp entity rows, which we need to elaborate, then download.
  newTempEntitiesQueue <- newTQueueIO

  -- How many workers (downloader / inserter / elaborator) are currently doing stuff.
  workerCount <- newWorkerCount

  -- The first download error seen by a downloader, if any.
  downloaderFailedVar <- newEmptyTMVarIO

  -- Kick off the cycle of inserter->elaborator->dispatcher->downloader by giving the elaborator something to do
  atomically (writeTQueue newTempEntitiesQueue (Set.empty, Just initialNewTempEntities))

  Ki.scoped \scope -> do
    Ki.fork_ scope (inserter entitiesQueue newTempEntitiesQueue workerCount)
    Ki.fork_ scope (elaborator hashesVar uninsertedHashesVar newTempEntitiesQueue workerCount)
    dispatcher hashesVar uninsertedHashesVar entitiesQueue newTempEntitiesQueue workerCount downloaderFailedVar
  where
    -- Dispatcher thread: "dequeue" from `hashesVar`, fork one-shot downloaders.
    --
    -- We stop when either all of the following are true:
    --
    --   - There are no outstanding workers (downloaders, inserter, elaboraror)
    --   - The inserter thread doesn't have any outstanding work enqueued (in `entitiesQueue`)
    --   - The elaborator thread doesn't have any outstanding work enqueued (in `newTempEntitiesQueue`)
    --
    -- Or:
    --
    --   - Some downloader failed to download something
    dispatcher ::
      TVar (Set Share.HashJWT) ->
      TVar (Set Share.HashJWT) ->
      TQueue (NESet Share.HashJWT, NEMap Hash32 (Share.Entity Text Hash32 Share.HashJWT)) ->
      TQueue (Set Share.HashJWT, Maybe (NESet Hash32)) ->
      WorkerCount ->
      TMVar (SyncError Share.DownloadEntitiesError) ->
      IO (Either (SyncError Share.DownloadEntitiesError) ())
    dispatcher hashesVar uninsertedHashesVar entitiesQueue newTempEntitiesQueue workerCount downloaderFailedVar =
      Ki.scoped \scope ->
        let loop :: IO (Either (SyncError Share.DownloadEntitiesError) ())
            loop =
              atomically (checkIfDownloaderFailedMode <|> dispatchWorkMode <|> checkIfDoneMode) >>= \case
                DispatcherDone -> pure (Right ())
                DispatcherReturnEarlyBecauseDownloaderFailed err -> pure (Left err)
                DispatcherForkWorker hashes -> do
                  atomically do
                    -- Limit number of simultaneous downloaders (plus 2, for inserter and elaborator)
                    workers <- readTVar workerCount
                    check (workers < maxSimultaneousPullDownloaders + 2)
                    -- we do need to record the downloader as working outside of the worker thread, not inside.
                    -- otherwise, we might erroneously fall through the the teardown logic below and conclude there's
                    -- nothing more for the dispatcher to do, when in fact a downloader thread just hasn't made it as
                    -- far as recording its own existence
                    recordWorking workerCount
                  _ <-
                    Ki.fork @() scope do
                      downloader entitiesQueue workerCount hashes & onLeftM \err ->
                        void (atomically (tryPutTMVar downloaderFailedVar err))
                  loop
         in loop
      where
        checkIfDownloaderFailedMode :: STM DispatcherJob
        checkIfDownloaderFailedMode =
          DispatcherReturnEarlyBecauseDownloaderFailed <$> readTMVar downloaderFailedVar

        dispatchWorkMode :: STM DispatcherJob
        dispatchWorkMode = do
          hashes <- readTVar hashesVar
          check (not (Set.null hashes))
          let (hashes1, hashes2) = Set.splitAt 50 hashes
          modifyTVar' uninsertedHashesVar (Set.union hashes1)
          writeTVar hashesVar hashes2
          pure (DispatcherForkWorker (NESet.unsafeFromSet hashes1))

        -- Check to see if there are no hashes left to download, no outstanding workers, and no work in either queue
        checkIfDoneMode :: STM DispatcherJob
        checkIfDoneMode = do
          workers <- readTVar workerCount
          check (workers == 0)
          isEmptyTQueue entitiesQueue >>= check
          isEmptyTQueue newTempEntitiesQueue >>= check
          pure DispatcherDone

    -- Downloader thread: download entities, (if successful) enqueue to `entitiesQueue`
    downloader ::
      TQueue (NESet Share.HashJWT, NEMap Hash32 (Share.Entity Text Hash32 Share.HashJWT)) ->
      WorkerCount ->
      NESet Share.HashJWT ->
      IO (Either (SyncError Share.DownloadEntitiesError) ())
    downloader entitiesQueue workerCount hashes = do
      httpDownloadEntities httpClient unisonShareUrl Share.DownloadEntitiesRequest {repoInfo, hashes} >>= \case
        Left err -> do
          atomically (recordNotWorking workerCount)
          pure (Left (TransportError err))
        Right (Share.DownloadEntitiesFailure err) -> do
          atomically (recordNotWorking workerCount)
          pure (Left (SyncError err))
        Right (Share.DownloadEntitiesSuccess entities) -> do
          downloadedCallback (NESet.size hashes)
          atomically do
            writeTQueue entitiesQueue (hashes, entities)
            recordNotWorking workerCount
          pure (Right ())

    -- Inserter thread: dequeue from `entitiesQueue`, insert entities, enqueue to `newTempEntitiesQueue`
    inserter ::
      TQueue (NESet Share.HashJWT, NEMap Hash32 (Share.Entity Text Hash32 Share.HashJWT)) ->
      TQueue (Set Share.HashJWT, Maybe (NESet Hash32)) ->
      WorkerCount ->
      IO Void
    inserter entitiesQueue newTempEntitiesQueue workerCount =
      connect \runTransaction ->
        forever do
          (hashJwts, entities) <-
            atomically do
              entities <- readTQueue entitiesQueue
              recordWorking workerCount
              pure entities
          newTempEntities0 <-
            runTransaction do
              NEMap.toList entities & foldMapM \(hash, entity) ->
                upsertEntitySomewhere hash entity <&> \case
                  Q.EntityInMainStorage -> Set.empty
                  Q.EntityInTempStorage -> Set.singleton hash
          atomically do
            writeTQueue newTempEntitiesQueue (NESet.toSet hashJwts, NESet.nonEmptySet newTempEntities0)
            recordNotWorking workerCount

    -- Elaborator thread: dequeue from `newTempEntitiesQueue`, elaborate, "enqueue" to `hashesVar`
    elaborator ::
      TVar (Set Share.HashJWT) ->
      TVar (Set Share.HashJWT) ->
      TQueue (Set Share.HashJWT, Maybe (NESet Hash32)) ->
      WorkerCount ->
      IO Void
    elaborator hashesVar uninsertedHashesVar newTempEntitiesQueue workerCount =
      connect \runTransaction ->
        forever do
          maybeNewTempEntities <-
            atomically do
              (hashJwts, mayNewTempEntities) <- readTQueue newTempEntitiesQueue
              -- Avoid unnecessary retaining of these hashes to keep memory usage more stable. This algorithm would
              -- still be correct if we never delete from `uninsertedHashes`.
              --
              -- We remove the inserted hashes from uninsertedHashesVar at this point rather than right after insertion
              -- in order to ensure that no running transaction of the elaborator is viewing a snapshot that precedes
              -- the snapshot that inserted those hashes.
              modifyTVar' uninsertedHashesVar \uninsertedHashes -> Set.difference uninsertedHashes hashJwts
              case mayNewTempEntities of
                Nothing -> pure Nothing
                Just newTempEntities -> do
                  recordWorking workerCount
                  pure (Just newTempEntities)
          whenJust maybeNewTempEntities \newTempEntities -> do
            newElaboratedHashes <- runTransaction (elaborateHashes newTempEntities)
            atomically do
              uninsertedHashes <- readTVar uninsertedHashesVar
              hashes0 <- readTVar hashesVar
              writeTVar hashesVar $! Set.union (Set.difference newElaboratedHashes uninsertedHashes) hashes0
              recordNotWorking workerCount

-- | Insert entities into the database, and return the subset that went into temp storage (`temp_entitiy`) rather than
-- of main storage (`object` / `causal`) due to missing dependencies.
insertEntities :: NEMap Hash32 (Share.Entity Text Hash32 Share.HashJWT) -> Sqlite.Transaction (Set Hash32)
insertEntities entities =
  NEMap.toList entities & foldMapM \(hash, entity) ->
    upsertEntitySomewhere hash entity <&> \case
      Q.EntityInMainStorage -> Set.empty
      Q.EntityInTempStorage -> Set.singleton hash

------------------------------------------------------------------------------------------------------------------------
-- Get causal hash by path

-- | Get the causal hash of a path hosted on Unison Share.
getCausalHashByPath ::
  -- | The Unison Share URL.
  BaseUrl ->
  Share.Path ->
  Cli (Either (SyncError GetCausalHashByPathError) (Maybe Share.HashJWT))
getCausalHashByPath unisonShareUrl repoPath = do
  Cli.Env {authHTTPClient} <- ask
  liftIO (httpGetCausalHashByPath authHTTPClient unisonShareUrl (Share.GetCausalHashByPathRequest repoPath)) <&> \case
    Left err -> Left (TransportError err)
    Right (Share.GetCausalHashByPathSuccess maybeHashJwt) -> Right maybeHashJwt
    Right (Share.GetCausalHashByPathNoReadPermission _) ->
      Left (SyncError (GetCausalHashByPathErrorNoReadPermission repoPath))
    Right (Share.GetCausalHashByPathInvalidRepoInfo err repoInfo) ->
      Left (SyncError (GetCausalHashByPathErrorInvalidRepoInfo err repoInfo))
    Right Share.GetCausalHashByPathUserNotFound ->
      Left (SyncError $ GetCausalHashByPathErrorUserNotFound (Share.pathRepoInfo repoPath))

------------------------------------------------------------------------------------------------------------------------
-- Upload entities

data UploadDispatcherJob
  = UploadDispatcherReturnFailure (SyncError Share.UploadEntitiesError)
  | UploadDispatcherForkWorkerWhenAvailable (NESet Hash32)
  | UploadDispatcherForkWorker (NESet Hash32)
  | UploadDispatcherDone

-- | Upload a set of entities to Unison Share. If the server responds that it cannot yet store any hash(es) due to
-- missing dependencies, send those dependencies too, and on and on, until the server stops responding that it's missing
-- anything.
--
-- Returns true on success, false on failure (because the user does not have write permission).
uploadEntities ::
  BaseUrl ->
  Share.RepoInfo ->
  NESet Hash32 ->
  (Int -> IO ()) ->
  Cli (Either (SyncError Share.UploadEntitiesError) ())
uploadEntities unisonShareUrl repoInfo hashes0 uploadedCallback = do
  Cli.Env {authHTTPClient, codebase} <- ask

  liftIO do
    hashesVar <- newTVarIO (NESet.toSet hashes0)
    -- Semantically, this is the set of hashes we've uploaded so far, but we do delete from it when it's safe to, so it
    -- doesn't grow unbounded. It's used to filter out hashes that would be duplicate uploads: the server, when
    -- responding to any particular upload request, may declare that it still needs some hashes that we're in the
    -- process of uploading from another thread.
    dedupeVar <- newTVarIO Set.empty
    nextWorkerIdVar <- newTVarIO 0
    workersVar <- newTVarIO Set.empty
    workerFailedVar <- newEmptyTMVarIO

    Ki.scoped \scope ->
      dispatcher
        scope
        authHTTPClient
        (Codebase.runTransaction codebase)
        hashesVar
        dedupeVar
        nextWorkerIdVar
        workersVar
        workerFailedVar
  where
    dispatcher ::
      Ki.Scope ->
      AuthenticatedHttpClient ->
      (forall a. Sqlite.Transaction a -> IO a) ->
      TVar (Set Hash32) ->
      TVar (Set Hash32) ->
      TVar Int ->
      TVar (Set Int) ->
      TMVar (SyncError Share.UploadEntitiesError) ->
      IO (Either (SyncError Share.UploadEntitiesError) ())
    dispatcher scope httpClient runTransaction hashesVar dedupeVar nextWorkerIdVar workersVar workerFailedVar = do
      loop
      where
        loop :: IO (Either (SyncError Share.UploadEntitiesError) ())
        loop =
          doJob [checkForFailureMode, dispatchWorkMode, checkIfDoneMode]

        doJob :: [STM UploadDispatcherJob] -> IO (Either (SyncError Share.UploadEntitiesError) ())
        doJob jobs =
          atomically (asum jobs) >>= \case
            UploadDispatcherReturnFailure err -> pure (Left err)
            UploadDispatcherForkWorkerWhenAvailable hashes -> doJob [forkWorkerMode hashes, checkForFailureMode]
            UploadDispatcherForkWorker hashes -> do
              workerId <-
                atomically do
                  workerId <- readTVar nextWorkerIdVar
                  writeTVar nextWorkerIdVar $! workerId + 1
                  modifyTVar' workersVar (Set.insert workerId)
                  pure workerId
              _ <-
                Ki.fork @() scope do
                  worker httpClient runTransaction hashesVar dedupeVar workersVar workerFailedVar workerId hashes
              loop
            UploadDispatcherDone -> pure (Right ())

        checkForFailureMode :: STM UploadDispatcherJob
        checkForFailureMode = do
          err <- readTMVar workerFailedVar
          pure (UploadDispatcherReturnFailure err)

        dispatchWorkMode :: STM UploadDispatcherJob
        dispatchWorkMode = do
          hashes <- readTVar hashesVar
          when (Set.null hashes) retry
          let (hashes1, hashes2) = Set.splitAt 50 hashes
          modifyTVar' dedupeVar (Set.union hashes1)
          writeTVar hashesVar hashes2
          pure (UploadDispatcherForkWorkerWhenAvailable (NESet.unsafeFromSet hashes1))

        forkWorkerMode :: NESet Hash32 -> STM UploadDispatcherJob
        forkWorkerMode hashes = do
          workers <- readTVar workersVar
          when (Set.size workers >= maxSimultaneousPushWorkers) retry
          pure (UploadDispatcherForkWorker hashes)

        checkIfDoneMode :: STM UploadDispatcherJob
        checkIfDoneMode = do
          workers <- readTVar workersVar
          when (not (Set.null workers)) retry
          pure UploadDispatcherDone

    worker ::
      AuthenticatedHttpClient ->
      (forall a. Sqlite.Transaction a -> IO a) ->
      TVar (Set Hash32) ->
      TVar (Set Hash32) ->
      TVar (Set Int) ->
      TMVar (SyncError Share.UploadEntitiesError) ->
      Int ->
      NESet Hash32 ->
      IO ()
    worker httpClient runTransaction hashesVar dedupeVar workersVar workerFailedVar workerId hashes = do
      entities <-
        fmap NEMap.fromAscList do
          runTransaction do
            for (NESet.toAscList hashes) \hash -> do
              entity <- expectEntity hash
              pure (hash, entity)

      result <-
        httpUploadEntities httpClient unisonShareUrl Share.UploadEntitiesRequest {entities, repoInfo} <&> \case
          Left err -> Left (TransportError err)
          Right response ->
            case response of
              Share.UploadEntitiesSuccess -> Right Set.empty
              Share.UploadEntitiesFailure err ->
                case err of
                  Share.UploadEntitiesError'NeedDependencies (Share.NeedDependencies moreHashes) ->
                    Right (NESet.toSet moreHashes)
                  err -> Left (SyncError err)

      case result of
        Left err -> void (atomically (tryPutTMVar workerFailedVar err))
        Right moreHashes -> do
          uploadedCallback (NESet.size hashes)
          maybeYoungestWorkerThatWasAlive <-
            atomically do
              -- Record ourselves as "dead". The only work we have left to do is remove the hashes we just uploaded from
              -- the `dedupe` set, but whether or not we are "alive" is relevant only to:
              --
              --   - The main dispatcher thread, which terminates when there are no more hashes to upload, and no alive
              --     workers. It is not important for us to delete from the `dedupe` set in this case.
              --
              --   - Other worker threads, each of which independently decides when it is safe to delete the set of
              --     hashes they just uploaded from the `dedupe` set (as we are doing now).
              !workers <- Set.delete workerId <$> readTVar workersVar
              writeTVar workersVar workers
              -- Add more work (i.e. hashes to upload) to the work queue (really a work set), per the response we just
              -- got from the server. Remember to only add hashes that aren't in the `dedupe` set (see the comment on
              -- the dedupe set above for more info).
              when (not (Set.null moreHashes)) do
                dedupe <- readTVar dedupeVar
                hashes0 <- readTVar hashesVar
                writeTVar hashesVar $! Set.union (Set.difference moreHashes dedupe) hashes0
              pure (Set.lookupMax workers)
          -- Block until we are sure that the server does not have any uncommitted transactions that see a version of
          -- the database that does not include the entities we just uploaded. After that point, it's fine to remove the
          -- hashes of the entities we just uploaded from the `dedupe` set, because they will never be relevant for any
          -- subsequent deduping operations. If we didn't delete from the `dedupe` set, this algorithm would still be
          -- correct, it would just use an unbounded amount of memory to remember all the hashes we've uploaded so far.
          whenJust maybeYoungestWorkerThatWasAlive \youngestWorkerThatWasAlive -> do
            atomically do
              workers <- readTVar workersVar
              whenJust (Set.lookupMin workers) \oldestWorkerAlive ->
                when (oldestWorkerAlive <= youngestWorkerThatWasAlive) retry
          atomically (modifyTVar' dedupeVar (`Set.difference` (NESet.toSet hashes)))

------------------------------------------------------------------------------------------------------------------------
-- Database operations

-- | "Elaborate" a set of `temp_entity` hashes.
--
-- For each hash, then we ought to instead download its missing dependencies (which themselves are
--    elaborated by this same procedure, in case we have any of *them* already in temp storage, too.
-- 3. If it's in main storage, we should ignore it.
--
-- In the end, we return a set of hashes that correspond to entities we actually need to download.
elaborateHashes :: NESet Hash32 -> Sqlite.Transaction (Set Share.HashJWT)
elaborateHashes hashes =
  Q.elaborateHashes (NESet.toList hashes) <&> Set.fromList . coerce @[Text] @[Share.HashJWT]

-- | Upsert a downloaded entity "somewhere" -
--
--   1. Nowhere if we already had the entity (in main or temp storage).
--   2. In main storage if we already have all of its dependencies in main storage.
--   3. In temp storage otherwise.
upsertEntitySomewhere ::
  Hash32 ->
  Share.Entity Text Hash32 Share.HashJWT ->
  Sqlite.Transaction Q.EntityLocation
upsertEntitySomewhere hash entity =
  Q.entityLocation hash >>= \case
    Just location -> pure location
    Nothing -> do
      missingDependencies1 :: Map Hash32 Share.HashJWT <-
        Share.entityDependencies entity
          & foldMapM
            ( \hashJwt -> do
                let hash = Share.hashJWTHash hashJwt
                Q.entityExists hash <&> \case
                  True -> Map.empty
                  False -> Map.singleton hash hashJwt
            )
      case NEMap.nonEmptyMap missingDependencies1 of
        Nothing -> do
          _id <- Q.saveTempEntityInMain v2HashHandle hash (entityToTempEntity Share.hashJWTHash entity)
          pure Q.EntityInMainStorage
        Just missingDependencies -> do
          Q.insertTempEntity
            hash
            (entityToTempEntity Share.hashJWTHash entity)
            ( coerce
                @(NEMap Hash32 Share.HashJWT)
                @(NEMap Hash32 Text)
                missingDependencies
            )
          pure Q.EntityInTempStorage

------------------------------------------------------------------------------------------------------------------------
-- HTTP calls

httpGetCausalHashByPath ::
  Auth.AuthenticatedHttpClient ->
  BaseUrl ->
  Share.GetCausalHashByPathRequest ->
  IO (Either CodeserverTransportError Share.GetCausalHashByPathResponse)
httpFastForwardPath ::
  Auth.AuthenticatedHttpClient ->
  BaseUrl ->
  Share.FastForwardPathRequest ->
  IO (Either CodeserverTransportError Share.FastForwardPathResponse)
httpUpdatePath ::
  Auth.AuthenticatedHttpClient ->
  BaseUrl ->
  Share.UpdatePathRequest ->
  IO (Either CodeserverTransportError Share.UpdatePathResponse)
httpDownloadEntities ::
  Auth.AuthenticatedHttpClient ->
  BaseUrl ->
  Share.DownloadEntitiesRequest ->
  IO (Either CodeserverTransportError Share.DownloadEntitiesResponse)
httpUploadEntities ::
  Auth.AuthenticatedHttpClient ->
  BaseUrl ->
  Share.UploadEntitiesRequest ->
  IO (Either CodeserverTransportError Share.UploadEntitiesResponse)
( httpGetCausalHashByPath,
  httpFastForwardPath,
  httpUpdatePath,
  httpDownloadEntities,
  httpUploadEntities
  ) =
    let ( httpGetCausalHashByPath
            Servant.:<|> httpFastForwardPath
            Servant.:<|> httpUpdatePath
            Servant.:<|> httpDownloadEntities
            Servant.:<|> httpUploadEntities
          ) =
            let pp :: Proxy ("ucm" Servant.:> "v1" Servant.:> "sync" Servant.:> Share.API)
                pp = Proxy
             in Servant.hoistClient pp hoist (Servant.client pp)
     in ( go httpGetCausalHashByPath,
          go httpFastForwardPath,
          go httpUpdatePath,
          go httpDownloadEntities,
          go httpUploadEntities
        )
    where
      hoist :: Servant.ClientM a -> ReaderT Servant.ClientEnv (ExceptT CodeserverTransportError IO) a
      hoist m = do
        clientEnv <- Reader.ask
        liftIO (Servant.runClientM m clientEnv) >>= \case
          Right a -> pure a
          Left err -> do
            Debug.debugLogM Debug.Sync (show err)
            throwError case err of
              Servant.FailureResponse _req resp ->
                case HTTP.statusCode $ Servant.responseStatusCode resp of
                  401 -> Unauthenticated (Servant.baseUrl clientEnv)
                  -- The server should provide semantically relevant permission-denied messages
                  -- when possible, but this should catch any we miss.
                  403 -> PermissionDenied (Text.Lazy.toStrict . Text.Lazy.decodeUtf8 $ Servant.responseBody resp)
                  408 -> Timeout
                  429 -> RateLimitExceeded
                  504 -> Timeout
                  _ -> UnexpectedResponse resp
              Servant.DecodeFailure msg resp -> DecodeFailure msg resp
              Servant.UnsupportedContentType _ct resp -> UnexpectedResponse resp
              Servant.InvalidContentTypeHeader resp -> UnexpectedResponse resp
              Servant.ConnectionError _ -> UnreachableCodeserver (Servant.baseUrl clientEnv)

      go ::
        (req -> ReaderT Servant.ClientEnv (ExceptT CodeserverTransportError IO) resp) ->
        Auth.AuthenticatedHttpClient ->
        BaseUrl ->
        req ->
        IO (Either CodeserverTransportError resp)
      go f (Auth.AuthenticatedHttpClient httpClient) unisonShareUrl req =
        (Servant.mkClientEnv httpClient unisonShareUrl)
          { Servant.makeClientRequest = \url request ->
              -- Disable client-side timeouts
              (Servant.defaultMakeClientRequest url request)
                { Http.Client.responseTimeout = Http.Client.responseTimeoutNone
                }
          }
          & runReaderT (f req)
          & runExceptT
