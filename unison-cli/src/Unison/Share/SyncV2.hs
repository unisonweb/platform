{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Share.SyncV2
  ( syncFromFile,
    syncToFile,
    syncFromCodebase,
  )
where

import Codec.Serialise qualified as CBOR
import Conduit (ConduitT)
import Conduit qualified as C
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader (ask)
import Control.Monad.ST (ST, stToIO)
import Control.Monad.State
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as A8
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Conduit.Attoparsec qualified as C
import Data.Conduit.List qualified as C
import Data.Conduit.Zlib qualified as C
import Data.Map qualified as Map
import Data.Text.IO qualified as Text
import Servant.Conduit ()
import System.Console.Regions qualified as Console.Regions
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Debug qualified as Debug
import Unison.Hash qualified as Hash
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Prelude
import Unison.Share.Sync.Types
import Unison.Sqlite qualified as Sqlite
import Unison.Sync.Common (causalHashToHash32, entityToTempEntity, hash32ToCausalHash, tempEntityToEntity)
import Unison.Sync.Common qualified as Sync
import Unison.Sync.Types (entityHashes_)
import Unison.Sync.Types qualified as Sync
import Unison.SyncV2.Types (BytesEntity, CBORBytes)
import Unison.SyncV2.Types qualified as SyncV2
import Unison.Util.Servant.CBOR qualified as CBOR
import Unison.Util.Timing qualified as Timing
import UnliftIO qualified as IO

type Stream i o = ConduitT i o StreamM ()

type SyncErr = SyncError SyncV2.PullError

type StreamM = (ExceptT SyncErr (C.ResourceT IO))

batchSize :: Int
batchSize = 5000

------------------------------------------------------------------------------------------------------------------------
-- Download entities

validateAndSave :: Bool -> (Codebase.Codebase IO v a) -> [(Hash32, BytesEntity)] -> StreamM ()
validateAndSave shouldValidate codebase entities = do
  let validateEntities =
        runExceptT $ when shouldValidate (batchValidateEntities entities)
  -- Validation is slow, run it in parallel with insertion, but don't commit the transaction until we're done
  -- validation.
  ExceptT . liftIO $ IO.withAsync validateEntities \validationTask -> do
    Timing.time "Inserting entities" $ Codebase.runTransactionExceptT codebase do
      for_ entities \(hash, entity) -> do
        void . lift $ Q.saveTempEntityInMain v2HashHandle hash (bytesToTemp entity)
      lift (Sqlite.unsafeIO (IO.wait validationTask)) >>= \case
        Left err -> throwError err
        Right _ -> pure ()

-- | Syncs a stream which could send entities in any order.
syncUnsortedStream ::
  Bool ->
  (Codebase.Codebase IO v a) ->
  Stream () SyncV2.EntityChunk ->
  StreamM ()
syncUnsortedStream shouldValidate codebase stream = do
  Debug.debugLogM Debug.Temp $ "Syncing unsorted stream"
  allResults <- C.runConduit $ stream C..| C.sinkList
  sortedEntities <- ExceptT $ Timing.time "Unpacking chunks" $ liftIO $ Codebase.runTransactionExceptT codebase $ do unpackChunks allResults
  -- let sortedEntities = sortDependencyFirst allEntities
  validateAndSave shouldValidate codebase sortedEntities

-- | Syncs a stream which sends entities which are already sorted in dependency order.
syncSortedStream ::
  Bool ->
  (Codebase.Codebase IO v a) ->
  Stream () SyncV2.EntityChunk ->
  StreamM ()
syncSortedStream shouldValidate codebase stream = do
  Debug.debugLogM Debug.Temp $ "Syncing sorted stream"
  let handler :: Stream [SyncV2.EntityChunk] o
      handler = C.mapM_C \chunkBatch -> do
        entityBatch <- mapExceptT lift . ExceptT $ Codebase.runTransactionExceptT codebase do for chunkBatch unpackChunk
        validateAndSave shouldValidate codebase (catMaybes entityBatch)
  C.runConduit $ stream C..| C.chunksOf batchSize C..| handler

unpackChunk :: SyncV2.EntityChunk -> ExceptT SyncErr Sqlite.Transaction (Maybe (Hash32, BytesEntity))
unpackChunk = \case
  SyncV2.EntityChunk {hash, entityCBOR = entityBytes} -> do
    -- Only want entities we don't already have
    lift (Q.entityLocation $ hash32FromBS hash) >>= \case
      Just Q.EntityInMainStorage -> pure Nothing
      _ -> do
        (Just . (hash32FromBS hash,)) <$> unpackEntity entityBytes
    where
      unpackEntity :: (CBORBytes BytesEntity) -> ExceptT SyncErr Sqlite.Transaction BytesEntity
      unpackEntity entityBytes = do
        case CBOR.deserialiseOrFailCBORBytes entityBytes of
          Left err -> do throwError $ (SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorDeserializationFailure err)
          Right entity -> pure entity

unpackChunks :: [SyncV2.EntityChunk] -> ExceptT SyncErr Sqlite.Transaction [(Hash32, BytesEntity)]
unpackChunks xs = do
  for xs unpackChunk
    <&> catMaybes

batchValidateEntities :: [(Hash32, BytesEntity)] -> ExceptT SyncErr IO ()
batchValidateEntities _entities = undefined

-- -- mismatches <- fmap catMaybes $ liftIO $ IO.pooledForConcurrently entities \(hash, entity) -> do
--   pure Nothing
-- -- IO.evaluate $ EV.validateTempEntity hash entity
-- for_ mismatches \case
--   err@(Share.EntityHashMismatch et (Share.HashMismatchForEntity {supplied, computed})) ->
--     let expectedMismatches = case et of
--           Share.TermComponentType -> expectedComponentHashMismatches
--           Share.DeclComponentType -> expectedComponentHashMismatches
--           Share.CausalType -> expectedCausalHashMismatches
--           _ -> mempty
--      in case Map.lookup supplied expectedMismatches of
--           Just expected
--             | expected == computed -> pure ()
--           _ -> do
--             throwError . SyncError . SyncV2.PullError'DownloadEntities . SyncV2.DownloadEntitiesEntityValidationFailure $ err
--   err -> do
--     throwError . SyncError . SyncV2.PullError'DownloadEntities . SyncV2.DownloadEntitiesEntityValidationFailure $ err

streamIntoCodebase :: Bool -> Codebase.Codebase IO v a -> SyncV2.StreamInitInfo -> Stream () SyncV2.EntityChunk -> StreamM ()
streamIntoCodebase shouldValidate codebase SyncV2.StreamInitInfo {version, entitySorting, numEntities = numEntities} stream = ExceptT do
  withStreamProgressCallback (fromIntegral <$> numEntities) \countC -> runExceptT do
    let stream' = stream C..| countC
    case version of
      (SyncV2.Version 1) -> pure ()
      v -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorUnsupportedVersion v

    case entitySorting of
      SyncV2.DependenciesFirst -> syncSortedStream shouldValidate codebase stream'
      SyncV2.Unsorted -> syncUnsortedStream shouldValidate codebase stream'

afterSyncChecks :: Codebase.Codebase IO v a -> Hash32 -> ExceptT (SyncError SyncV2.PullError) IO ()
afterSyncChecks codebase hash = do
  lift (didCausalSuccessfullyImport codebase hash) >>= \case
    False -> do
      throwError (SyncError (SyncV2.PullError'Sync . SyncV2.SyncErrorExpectedResultNotInMain . hash32ToCausalHash $ hash))
    True -> pure ()
  void $ liftIO (Codebase.withConnection codebase Sqlite.vacuum)
  where
    -- Verify that the expected hash made it into main storage.
    didCausalSuccessfullyImport :: Codebase.Codebase IO v a -> Hash32 -> IO Bool
    didCausalSuccessfullyImport codebase hash = do
      let expectedHash = hash32ToCausalHash hash
      isJust <$> (Codebase.runTransaction codebase $ Q.loadCausalByCausalHash expectedHash)

-- | Topologically sort entities based on their dependencies.
_sortDependencyFirst :: [(Hash32, BytesEntity)] -> [(Hash32, BytesEntity)]
_sortDependencyFirst entities = entities

-- let adjList = entities <&> \(hash32, entity) -> ((hash32, entity), hash32, Set.toList $ Share.entityDependencies (tempEntityToEntity entity))
--     (graph, vertexInfo, _vertexForKey) = Graph.graphFromEdges adjList
--  in Graph.reverseTopSort graph <&> \v -> (view _1 $ vertexInfo v)

syncFromFile ::
  Bool ->
  -- | Location of the sync-file
  FilePath ->
  Cli (Either (SyncError SyncV2.PullError) CausalHash)
syncFromFile shouldValidate syncFilePath = do
  Cli.Env {codebase} <- ask
  runExceptT do
    Debug.debugLogM Debug.Temp $ "Kicking off sync"
    mapExceptT liftIO $ Timing.time "File Sync" $ do
      header <- mapExceptT C.runResourceT $ do
        let stream = C.sourceFile syncFilePath C..| C.ungzip C..| decodeUnframedEntities
        (header, rest) <- initializeStream stream
        streamIntoCodebase shouldValidate codebase header rest
        pure header
      afterSyncChecks codebase (SyncV2.rootCausalHash header)
      pure . hash32ToCausalHash $ SyncV2.rootCausalHash header

syncFromCodebase ::
  Bool ->
  -- | The codebase to sync from.
  Sqlite.Connection ->
  (Codebase.Codebase IO v a) ->
  -- | The hash to sync.
  CausalHash ->
  IO (Either (SyncError SyncV2.PullError) ())
syncFromCodebase shouldValidate srcConn destCodebase causalHash = do
  liftIO . C.runResourceT . runExceptT $ withEntityStream srcConn causalHash Nothing \_total entityStream -> do
    (header, rest) <- initializeStream entityStream
    streamIntoCodebase shouldValidate destCodebase header rest
    mapExceptT liftIO (afterSyncChecks destCodebase (causalHashToHash32 causalHash))

withEntityStream ::
  (MonadIO m) =>
  Sqlite.Connection ->
  CausalHash ->
  Maybe SyncV2.BranchRef ->
  (Int -> Stream () SyncV2.DownloadEntitiesChunk -> m r) ->
  m r
withEntityStream conn rootHash mayBranchRef callback = do
  entities <- liftIO $ withEntityLoadingCallback $ \counter -> do
    Sqlite.runTransaction conn (depsForCausal rootHash counter)
  liftIO $ Text.hPutStrLn IO.stderr $ "Finished loading entities, writing sync-file."
  let totalEntities = fromIntegral $ Map.size entities
  let initialChunk =
        SyncV2.InitialC
          ( SyncV2.StreamInitInfo
              { rootCausalHash = causalHashToHash32 rootHash,
                version = SyncV2.Version 1,
                entitySorting = SyncV2.DependenciesFirst,
                numEntities = Just $ fromIntegral totalEntities,
                rootBranchRef = mayBranchRef
              }
          )
  let contents =
        entities
          & fmap (Sync.entityToTempEntity id)
          & Map.toList
          -- & sortDependencyFirst
          & ( fmap \(hash, entity) ->
                let entityCBOR = (CBOR.serialiseCBORBytes $ tempToBytes entity)
                 in SyncV2.EntityC (SyncV2.EntityChunk {hash = hash32ToBS hash, entityCBOR})
            )
          & (initialChunk :)
  let stream = C.yieldMany contents
  callback totalEntities stream

syncToFile ::
  Codebase.Codebase IO v a ->
  CausalHash ->
  Maybe SyncV2.BranchRef ->
  FilePath ->
  IO (Either SyncErr ())
syncToFile codebase rootHash mayBranchRef destFilePath = do
  liftIO $ Codebase.withConnection codebase \conn -> do
    C.runResourceT $
      withEntityStream conn rootHash mayBranchRef \mayTotal stream -> do
        withStreamProgressCallback (Just mayTotal) \countC -> runExceptT do
          C.runConduit $ stream C..| countC C..| C.map (BL.toStrict . CBOR.serialise) C..| C.sinkFile destFilePath

-- | Collect all dependencies of a given causal hash.
depsForCausal :: CausalHash -> (Int -> IO ()) -> Sqlite.Transaction (Map Hash32 (Sync.Entity Text Hash32 Hash32))
depsForCausal causalHash counter = do
  flip execStateT mempty $ expandEntities (causalHashToHash32 causalHash)
  where
    expandEntities :: Hash32 -> ((StateT (Map Hash32 (Sync.Entity Text Hash32 Hash32)) Sqlite.Transaction)) ()
    expandEntities hash32 = do
      gets (Map.member hash32) >>= \case
        True -> pure ()
        False -> do
          entity <- lift $ Sync.expectEntity hash32
          modify (Map.insert hash32 entity)
          lift . Sqlite.unsafeIO $ counter 1
          traverseOf_ Sync.entityHashes_ expandEntities entity

-- | Gets the framed chunks from a NetString framed stream.
_unNetString :: ConduitT ByteString ByteString StreamM ()
_unNetString = do
  bs <- C.sinkParser $ do
    len <- A8.decimal
    _ <- A8.char ':'
    bs <- A.take len
    _ <- A8.char ','
    pure bs
  C.yield bs

_decodeFramedEntity :: ByteString -> StreamM SyncV2.DownloadEntitiesChunk
_decodeFramedEntity bs = do
  case CBOR.deserialiseOrFail (BL.fromStrict bs) of
    Left err -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorDeserializationFailure err
    Right chunk -> pure chunk

-- Expects a stream of tightly-packed CBOR entities without any framing/separators.
decodeUnframedEntities :: ConduitT ByteString SyncV2.DownloadEntitiesChunk StreamM ()
decodeUnframedEntities = C.transPipe (mapExceptT (lift . stToIO)) $ do
  C.await >>= \case
    Nothing -> pure ()
    Just bs -> do
      d <- newDecoder
      loop bs d
  where
    newDecoder :: ConduitT ByteString SyncV2.DownloadEntitiesChunk (ExceptT SyncErr (ST s)) (Maybe ByteString -> ST s (CBOR.IDecode s (SyncV2.DownloadEntitiesChunk)))
    newDecoder = do
      (lift . lift) CBOR.deserialiseIncremental >>= \case
        CBOR.Done _ _ _ -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorStreamFailure "Invalid initial decoder"
        CBOR.Fail _ _ err -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorDeserializationFailure err
        CBOR.Partial k -> pure k
    loop :: ByteString -> (Maybe ByteString -> ST s (CBOR.IDecode s (SyncV2.DownloadEntitiesChunk))) -> ConduitT ByteString SyncV2.DownloadEntitiesChunk (ExceptT SyncErr (ST s)) ()
    loop bs k = do
      (lift . lift) (k (Just bs)) >>= \case
        CBOR.Fail _ _ err -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorDeserializationFailure err
        CBOR.Partial k' -> do
          -- We need more input, try to get some
          nextBS <- C.await
          case nextBS of
            Nothing -> do
              -- No more input, try to finish up the decoder.
              (lift . lift) (k' Nothing) >>= \case
                CBOR.Done _ _ a -> C.yield a
                CBOR.Fail _ _ err -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorDeserializationFailure err
                CBOR.Partial _ -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorStreamFailure "Unexpected end of input"
            Just bs' ->
              -- Have some input, keep going.
              loop bs' k'
        CBOR.Done rem _ a -> do
          C.yield a
          if BS.null rem
            then do
              -- If we had no leftovers, we can check if there's any input left.
              C.await >>= \case
                Nothing -> pure ()
                Just bs'' -> do
                  -- If we have input left, start up a new decoder.
                  k <- newDecoder
                  loop bs'' k
            else do
              -- We have leftovers, start a new decoder and use those.
              k <- newDecoder
              loop rem k

-- | Peel the header off the stream and parse the remaining entity chunks.
initializeStream :: Stream () SyncV2.DownloadEntitiesChunk -> StreamM (SyncV2.StreamInitInfo, Stream () SyncV2.EntityChunk)
initializeStream stream = do
  (streamRemainder, init) <- stream C.$$+ C.headC
  Debug.debugM Debug.Temp "Got initial chunk: " init
  case init of
    Nothing -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorMissingInitialChunk
    Just chunk -> do
      case chunk of
        SyncV2.InitialC info -> do
          let entityStream = C.unsealConduitT streamRemainder C..| C.mapM parseEntity
          pure $ (info, entityStream)
        SyncV2.EntityC _ -> do
          Debug.debugLogM Debug.Temp $ "Got unexpected entity chunk"
          throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorMissingInitialChunk
        SyncV2.ErrorC (SyncV2.ErrorChunk err) -> throwError . SyncError . SyncV2.PullError'DownloadEntities $ err
  where
    parseEntity :: SyncV2.DownloadEntitiesChunk -> StreamM SyncV2.EntityChunk
    parseEntity = \case
      SyncV2.EntityC chunk -> pure chunk
      SyncV2.ErrorC (SyncV2.ErrorChunk err) -> throwError . SyncError $ SyncV2.PullError'DownloadEntities err
      SyncV2.InitialC {} -> throwError . SyncError $ SyncV2.PullError'Sync SyncV2.SyncErrorMisplacedInitialChunk

-- Provide the given action a callback that display to the terminal.
withStreamProgressCallback :: (MonadIO m, MonadUnliftIO n) => Maybe Int -> (ConduitT i i m () -> n a) -> n a
withStreamProgressCallback total action = do
  entitiesDownloadedVar <- IO.newTVarIO (0 :: Int)
  IO.withRunInIO \toIO -> do
    Console.Regions.displayConsoleRegions do
      Console.Regions.withConsoleRegion Console.Regions.Linear \region -> do
        Console.Regions.setConsoleRegion region do
          entitiesDownloaded <- IO.readTVar entitiesDownloadedVar
          pure $
            "\n  Processed "
              <> tShow entitiesDownloaded
              <> maybe "" (\total -> " / " <> tShow total) total
              <> " entities...\n\n"
        toIO $ action $ C.awaitForever \i -> do
          liftIO $ IO.atomically (IO.modifyTVar' entitiesDownloadedVar (+ 1))
          C.yield i

withEntityLoadingCallback :: (MonadUnliftIO m) => ((Int -> m ()) -> m a) -> m a
withEntityLoadingCallback action = do
  counterVar <- IO.newTVarIO (0 :: Int)
  IO.withRunInIO \toIO -> do
    Console.Regions.displayConsoleRegions do
      Console.Regions.withConsoleRegion Console.Regions.Linear \region -> do
        Console.Regions.setConsoleRegion region do
          processed <- IO.readTVar counterVar
          pure $
            "\n  Loading "
              <> tShow processed
              <> " entities...\n\n"
        toIO $ action $ \i -> do
          liftIO $ IO.atomically (IO.modifyTVar' counterVar (+ i))

hash32ToBS :: Hash32 -> ByteString
hash32ToBS = Hash.toByteString . Hash32.toHash

hash32FromBS :: ByteString -> Hash32
hash32FromBS = Hash32.fromHash . Hash.fromByteString

tempToBytes :: TempEntity -> BytesEntity
tempToBytes = entityToTempEntity id . mapEntity . tempEntityToEntity
  where
    mapEntity = over entityHashes_ hash32ToBS

bytesToTemp :: BytesEntity -> TempEntity
bytesToTemp = entityToTempEntity id . mapEntity . tempEntityToEntity
  where
    mapEntity = over entityHashes_ hash32FromBS
