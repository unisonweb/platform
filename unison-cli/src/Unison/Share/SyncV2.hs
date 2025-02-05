{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Share.SyncV2
  ( syncFromFile,
    syncToFile,
    syncFromCodebase,
    syncFromCodeserver,
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
import Data.Conduit.Combinators qualified as C
import Data.Conduit.List qualified as CL
import Data.Conduit.Zlib qualified as C
import Data.Foldable qualified as Foldable
import Data.Graph qualified as Graph
import Data.Map qualified as Map
import Data.Proxy
import Data.Set qualified as Set
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Network.HTTP.Client qualified as Http.Client
import Network.HTTP.Types qualified as HTTP
import Servant.API qualified as Servant
import Servant.Client.Streaming qualified as Servant
import Servant.Conduit ()
import Servant.Types.SourceT qualified as Servant
import System.Console.Regions qualified as Console.Regions
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Auth.HTTPClient qualified as Auth
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Hash32 (Hash32)
import Unison.Prelude
import Unison.Share.API.Hash qualified as Share
import Unison.Share.ExpectedHashMismatches (expectedCausalHashMismatches, expectedComponentHashMismatches)
import Unison.Share.Sync.Types
import Unison.Sqlite qualified as Sqlite
import Unison.Sync.Common (causalHashToHash32, hash32ToCausalHash, tempEntityToEntity)
import Unison.Sync.Common qualified as Sync
import Unison.Sync.EntityValidation qualified as EV
import Unison.Sync.Types qualified as Share
import Unison.Sync.Types qualified as Sync
import Unison.SyncV2.API (Routes (downloadEntitiesStream))
import Unison.SyncV2.API qualified as SyncV2
import Unison.SyncV2.Types (CBORBytes, CBORStream, DependencyType (..))
import Unison.SyncV2.Types qualified as SyncV2
import Unison.Util.Servant.CBOR qualified as CBOR
import Unison.Util.Timing qualified as Timing
import UnliftIO qualified as IO

type Stream i o = ConduitT i o StreamM ()

type SyncErr = SyncError SyncV2.PullError

-- The base monad we use within the conduit pipeline.
type StreamM = (ExceptT SyncErr (C.ResourceT IO))

-- | The number of entities to process in a single transaction.
--
-- SQLite transactions have some fixed overhead, so setting this too low can really slow things down,
-- but going too high here means we may be waiting on the network to get a full batch when we could be starting work.
batchSize :: Int
batchSize = 5000

------------------------------------------------------------------------------------------------------------------------
-- Main methods
------------------------------------------------------------------------------------------------------------------------

-- | Sync a given causal hash and its dependencies to a sync-file.
syncToFile ::
  Codebase.Codebase IO v a ->
  -- | Root hash to sync
  CausalHash ->
  -- | Optional name of the branch begin synced
  Maybe SyncV2.BranchRef ->
  -- | Location of the sync-file
  FilePath ->
  IO (Either SyncErr ())
syncToFile codebase rootHash mayBranchRef destFilePath = do
  liftIO $ Codebase.withConnection codebase \conn -> do
    C.runResourceT $
      withCodebaseEntityStream conn rootHash mayBranchRef \mayTotal stream -> do
        withStreamProgressCallback (Just mayTotal) \countC -> runExceptT do
          C.runConduit $
            stream
              C..| countC
              C..| C.map (BL.toStrict . CBOR.serialise)
              C..| C.transPipe liftIO C.gzip
              C..| C.sinkFile destFilePath

syncFromFile ::
  -- | Whether to validate entities as they're imported.
  Bool ->
  -- | Location of the sync-file
  FilePath ->
  Cli (Either (SyncError SyncV2.PullError) CausalHash)
syncFromFile shouldValidate syncFilePath = do
  Cli.Env {codebase} <- ask
  -- Every insert into SQLite checks the temp entity tables, but syncv2 doesn't actually use them, so it's faster
  -- if we clear them out before starting a sync.
  Cli.runTransaction Q.clearTempEntityTables
  runExceptT do
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
  -- Every insert into SQLite checks the temp entity tables, but syncv2 doesn't actually use them, so it's faster
  -- if we clear them out before starting a sync.
  Sqlite.runTransaction srcConn Q.clearTempEntityTables
  liftIO . C.runResourceT . runExceptT $ withCodebaseEntityStream srcConn causalHash Nothing \_total entityStream -> do
    (header, rest) <- initializeStream entityStream
    streamIntoCodebase shouldValidate destCodebase header rest
    mapExceptT liftIO (afterSyncChecks destCodebase (causalHashToHash32 causalHash))

syncFromCodeserver ::
  Bool ->
  -- | The Unison Share URL.
  Servant.BaseUrl ->
  -- | The branch to download from.
  SyncV2.BranchRef ->
  -- | The hash to download.
  Share.HashJWT ->
  -- | Set of known hashes to avoid downloading.
  -- If provided we'll skip the negotiation stage.
  Set CausalHash ->
  Cli (Either (SyncError SyncV2.PullError) ())
syncFromCodeserver shouldValidate unisonShareUrl branchRef hashJwt providedKnownHashes = do
  Cli.Env {authHTTPClient, codebase} <- ask
  -- Every insert into SQLite checks the temp entity tables, but syncv2 doesn't actually use them, so it's faster
  -- if we clear them out before starting a sync.
  Cli.runTransaction Q.clearTempEntityTables
  runExceptT do
    knownHashes <-
      if Set.null providedKnownHashes
        then ExceptT $ negotiateKnownCausals unisonShareUrl branchRef hashJwt
        else pure (Set.map Sync.causalHashToHash32 providedKnownHashes)
    let hash = Share.hashJWTHash hashJwt
    ExceptT $ do
      (Cli.runTransaction (Q.entityLocation hash)) >>= \case
        Just Q.EntityInMainStorage -> pure $ Right ()
        _ -> do
          Timing.time "Entity Download" $ do
            liftIO . C.runResourceT . runExceptT $ httpStreamEntities
              authHTTPClient
              unisonShareUrl
              SyncV2.DownloadEntitiesRequest {branchRef, causalHash = hashJwt, knownHashes}
              \header stream -> do
                streamIntoCodebase shouldValidate codebase header stream
    mapExceptT liftIO (afterSyncChecks codebase hash)

------------------------------------------------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------------------------------------------------

-- | Validate that the provided entities match their expected hashes, and if so, save them to the codebase.
validateAndSave :: Bool -> (Codebase.Codebase IO v a) -> Vector (Hash32, TempEntity) -> StreamM ()
validateAndSave shouldValidate codebase entities = do
  let validateEntities =
        runExceptT $ when shouldValidate (batchValidateEntities entities)
  -- Validation is slow, so we run it in parallel with insertion (which can also be slow),
  -- but we don't commit the transaction until we're done validation to avoid inserting invalid entities.
  ExceptT . liftIO $ IO.withAsync validateEntities \validationTask -> do
    Timing.time "Inserting entities" $ Codebase.runTransactionExceptT codebase do
      for_ entities \(hash, entity) -> do
        void . lift $ Q.saveTempEntityInMain v2HashHandle hash entity
      lift (Sqlite.unsafeIO (IO.wait validationTask)) >>= \case
        Left err -> throwError err
        Right _ -> pure ()

-- | Validate that a batch of entities matches the hashes they're keyed by, throwing an error if any of them fail validation.
batchValidateEntities :: Vector (Hash32, TempEntity) -> ExceptT SyncErr IO ()
batchValidateEntities entities = do
  mismatches <- fmap Vector.catMaybes $ liftIO $ IO.pooledForConcurrently entities \(hash, entity) -> do
    IO.evaluate $ EV.validateTempEntity hash entity
  for_ mismatches \case
    err@(Share.EntityHashMismatch et (Share.HashMismatchForEntity {supplied, computed})) ->
      let expectedMismatches = case et of
            Share.TermComponentType -> expectedComponentHashMismatches
            Share.DeclComponentType -> expectedComponentHashMismatches
            Share.CausalType -> expectedCausalHashMismatches
            _ -> mempty
       in case Map.lookup supplied expectedMismatches of
            Just expected
              | expected == computed -> pure ()
            _ -> do
              throwError . SyncError . SyncV2.PullError'DownloadEntities . SyncV2.DownloadEntitiesEntityValidationFailure $ err
    err -> do
      throwError . SyncError . SyncV2.PullError'DownloadEntities . SyncV2.DownloadEntitiesEntityValidationFailure $ err

-- | Syncs a stream which could send entities in any order.
syncUnsortedStream ::
  Bool ->
  (Codebase.Codebase IO v a) ->
  Stream () SyncV2.EntityChunk ->
  StreamM ()
syncUnsortedStream shouldValidate codebase stream = do
  allEntities <-
    C.runConduit $
      stream
        C..| CL.chunksOf batchSize
        C..| unpackChunks codebase
        C..| validateBatch
        C..| C.concat
        C..| C.sinkVector @Vector
  let sortedEntities = sortDependencyFirst allEntities
  liftIO $ withEntitySavingCallback (Just $ Vector.length allEntities) \countC -> do
    Codebase.runTransaction codebase $ for_ sortedEntities \(hash, entity) -> do
      r <- Q.saveTempEntityInMain v2HashHandle hash entity
      Sqlite.unsafeIO $ countC 1
      pure r
  where
    validateBatch :: Stream (Vector (Hash32, TempEntity)) (Vector (Hash32, TempEntity))
    validateBatch = C.iterM \entities -> do
      when shouldValidate (mapExceptT lift $ batchValidateEntities entities)

-- | Syncs a stream which sends entities which are already sorted in dependency order.
-- This allows us to stream them directly into the codebase as they're received.
syncSortedStream ::
  Bool ->
  (Codebase.Codebase IO v a) ->
  Stream () SyncV2.EntityChunk ->
  StreamM ()
syncSortedStream shouldValidate codebase stream = do
  let handler :: Stream (Vector (Hash32, TempEntity)) o
      handler = C.mapM_C \entityBatch -> do
        validateAndSave shouldValidate codebase entityBatch
  C.runConduit $
    stream
      C..| CL.chunksOf batchSize
      C..| unpackChunks codebase
      C..| handler

-- | Topologically sort entities based on their dependencies, returning a list in dependency-first order.
sortDependencyFirst :: (Foldable f, Functor f) => f (Hash32, TempEntity) -> [(Hash32, TempEntity)]
sortDependencyFirst entities = do
  let adjList = entities <&> \(hash32, entity) -> ((hash32, entity), hash32, Set.toList $ Share.entityDependencies (tempEntityToEntity entity))
      (graph, vertexInfo, _vertexForKey) = Graph.graphFromEdges (Foldable.toList adjList)
   in Graph.reverseTopSort graph <&> \v -> (view _1 $ vertexInfo v)

-- | Unpack a single entity chunk, returning the entity if it's not already in the codebase, Nothing otherwise.
unpackChunk :: SyncV2.EntityChunk -> ExceptT SyncErr Sqlite.Transaction (Maybe (Hash32, TempEntity))
unpackChunk = \case
  SyncV2.EntityChunk {hash, entityCBOR = entityBytes} -> do
    -- Only want entities we don't already have
    lift (Q.entityLocation hash) >>= \case
      Just Q.EntityInMainStorage -> pure Nothing
      _ -> do
        (Just . (hash,)) <$> unpackEntity entityBytes
    where
      unpackEntity :: (CBORBytes TempEntity) -> ExceptT SyncErr Sqlite.Transaction TempEntity
      unpackEntity entityBytes = do
        case CBOR.deserialiseOrFailCBORBytes entityBytes of
          Left err -> do throwError $ (SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorDeserializationFailure err)
          Right entity -> pure entity

unpackChunks :: Codebase.Codebase IO v a -> Stream [SyncV2.EntityChunk] (Vector (Hash32, TempEntity))
unpackChunks codebase = C.mapM \xs -> ExceptT . lift . Codebase.runTransactionExceptT codebase $ do
  for xs unpackChunk
    <&> catMaybes
    <&> Vector.fromList

-- | Stream entities from one codebase into another.
streamIntoCodebase ::
  -- | Whether to validate entities as they're imported.
  Bool ->
  Codebase.Codebase IO v a ->
  SyncV2.StreamInitInfo ->
  Stream () SyncV2.EntityChunk ->
  StreamM ()
streamIntoCodebase shouldValidate codebase SyncV2.StreamInitInfo {version, entitySorting, numEntities = numEntities} stream = ExceptT do
  withStreamProgressCallback (fromIntegral <$> numEntities) \countC -> runExceptT do
    -- Add a counter to the stream to track how many entities we've processed.
    let stream' = stream C..| countC
    case version of
      (SyncV2.Version 1) -> pure ()
      v -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorUnsupportedVersion v

    case entitySorting of
      SyncV2.DependenciesFirst -> syncSortedStream shouldValidate codebase stream'
      SyncV2.Unsorted -> syncUnsortedStream shouldValidate codebase stream'

-- | A sanity-check to verify that the hash we expected to import from the stream was successfully loaded into the codebase.
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

-- | Load and stream entities for a given causal hash from a codebase into a stream.
withCodebaseEntityStream ::
  (MonadIO m) =>
  Sqlite.Connection ->
  CausalHash ->
  Maybe SyncV2.BranchRef ->
  -- | Callback to call with the total count of entities and the stream.
  (Int -> Stream () SyncV2.DownloadEntitiesChunk -> m r) ->
  m r
withCodebaseEntityStream conn rootHash mayBranchRef callback = do
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
          & sortDependencyFirst
          & ( fmap \(hash, entity) ->
                let entityCBOR = (CBOR.serialiseCBORBytes entity)
                 in SyncV2.EntityC (SyncV2.EntityChunk {hash, entityCBOR})
            )
          & (initialChunk :)
  let stream = C.yieldMany contents
  callback totalEntities stream
  where
    -- Collect all dependencies of a given causal hash.
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

-- | Unpacks a stream of tightly-packed CBOR entities without any framing/separators.
decodeUnframedEntities :: forall a. (CBOR.Serialise a) => Stream ByteString a
decodeUnframedEntities = C.transPipe (mapExceptT (lift . stToIO)) $ do
  C.await >>= \case
    Nothing -> pure ()
    Just bs -> do
      d <- newDecoder
      loop bs d
  where
    newDecoder :: ConduitT ByteString a (ExceptT SyncErr (ST s)) (Maybe ByteString -> ST s (CBOR.IDecode s a))
    newDecoder = do
      (lift . lift) CBOR.deserialiseIncremental >>= \case
        CBOR.Done _ _ _ -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorStreamFailure "Invalid initial decoder"
        CBOR.Fail _ _ err -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorDeserializationFailure err
        CBOR.Partial k -> pure k
    loop :: ByteString -> (Maybe ByteString -> ST s (CBOR.IDecode s a)) -> ConduitT ByteString a (ExceptT SyncErr (ST s)) ()
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

------------------------------------------------------------------------------------------------------------------------
-- Servant stuff

type SyncAPI = ("ucm" Servant.:> "v2" Servant.:> "sync" Servant.:> SyncV2.API)

syncAPI :: Proxy SyncAPI
syncAPI = Proxy @SyncAPI

downloadEntitiesStreamClientM :: SyncV2.DownloadEntitiesRequest -> Servant.ClientM (Servant.SourceT IO (CBORStream SyncV2.DownloadEntitiesChunk))
causalDependenciesStreamClientM :: SyncV2.CausalDependenciesRequest -> Servant.ClientM (Servant.SourceT IO (CBORStream SyncV2.CausalDependenciesChunk))
SyncV2.Routes
  { downloadEntitiesStream = downloadEntitiesStreamClientM,
    causalDependenciesStream = causalDependenciesStreamClientM
  } = Servant.client syncAPI

-- | Helper for running clientM that returns a stream of entities.
-- You MUST consume the stream within the callback, it will be closed when the callback returns.
withConduit :: forall r chunk. (CBOR.Serialise chunk) => Servant.ClientEnv -> (Stream () chunk -> StreamM r) -> Servant.ClientM (Servant.SourceIO (CBORStream chunk)) -> StreamM r
withConduit clientEnv callback clientM = do
  ExceptT $ withRunInIO \runInIO -> do
    Servant.withClientM clientM clientEnv $ \case
      Left err -> pure . Left . TransportError $ (handleClientError clientEnv err)
      Right sourceT -> do
        conduit <- liftIO $ Servant.fromSourceIO sourceT
        (runInIO . runExceptT $ callback (conduit C..| unpackCBORBytesStream))

unpackCBORBytesStream :: (CBOR.Serialise a) => Stream (CBORStream a) a
unpackCBORBytesStream =
  C.map (BL.toStrict . coerce @_ @BL.ByteString) C..| decodeUnframedEntities

handleClientError :: Servant.ClientEnv -> Servant.ClientError -> CodeserverTransportError
handleClientError clientEnv err =
  case err of
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

-- | Stream entities from the codeserver.
httpStreamEntities ::
  Auth.AuthenticatedHttpClient ->
  Servant.BaseUrl ->
  SyncV2.DownloadEntitiesRequest ->
  (SyncV2.StreamInitInfo -> Stream () SyncV2.EntityChunk -> StreamM ()) ->
  StreamM ()
httpStreamEntities (Auth.AuthenticatedHttpClient httpClient) unisonShareUrl req callback = do
  let clientEnv =
        (Servant.mkClientEnv httpClient unisonShareUrl)
          { Servant.makeClientRequest = \url request ->
              -- Disable client-side timeouts
              (Servant.defaultMakeClientRequest url request)
                <&> \r ->
                  r
                    { Http.Client.responseTimeout = Http.Client.responseTimeoutNone
                    }
          }
  (downloadEntitiesStreamClientM req) & withConduit clientEnv \stream -> do
    (init, entityStream) <- initializeStream stream
    callback init entityStream

-- | Peel the header off the stream and parse the remaining entity chunks into EntityChunks
initializeStream :: Stream () SyncV2.DownloadEntitiesChunk -> StreamM (SyncV2.StreamInitInfo, Stream () SyncV2.EntityChunk)
initializeStream stream = do
  (streamRemainder, init) <- stream C.$$+ C.headC
  case init of
    Nothing -> throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorMissingInitialChunk
    Just chunk -> do
      case chunk of
        SyncV2.InitialC info -> do
          let entityStream = C.unsealConduitT streamRemainder C..| C.mapM parseEntity
          pure $ (info, entityStream)
        SyncV2.EntityC _ -> do
          throwError . SyncError . SyncV2.PullError'Sync $ SyncV2.SyncErrorMissingInitialChunk
        SyncV2.ErrorC (SyncV2.ErrorChunk err) -> throwError . SyncError . SyncV2.PullError'DownloadEntities $ err
  where
    parseEntity :: SyncV2.DownloadEntitiesChunk -> StreamM SyncV2.EntityChunk
    parseEntity = \case
      SyncV2.EntityC chunk -> pure chunk
      SyncV2.ErrorC (SyncV2.ErrorChunk err) -> throwError . SyncError $ SyncV2.PullError'DownloadEntities err
      SyncV2.InitialC {} -> throwError . SyncError $ SyncV2.PullError'Sync SyncV2.SyncErrorMisplacedInitialChunk

------------------------------------------------------------------------------------------------------------------------
-- Causal Dependency negotiation
------------------------------------------------------------------------------------------------------------------------

httpStreamCausalDependencies ::
  forall r.
  Auth.AuthenticatedHttpClient ->
  Servant.BaseUrl ->
  SyncV2.CausalDependenciesRequest ->
  (Stream () SyncV2.CausalDependenciesChunk -> StreamM r) ->
  StreamM r
httpStreamCausalDependencies (Auth.AuthenticatedHttpClient httpClient) unisonShareUrl req callback = do
  let clientEnv =
        (Servant.mkClientEnv httpClient unisonShareUrl)
          { Servant.makeClientRequest = \url request ->
              -- Disable client-side timeouts
              (Servant.defaultMakeClientRequest url request)
                <&> \r ->
                  r
                    { Http.Client.responseTimeout = Http.Client.responseTimeoutNone
                    }
          }
  (causalDependenciesStreamClientM req) & withConduit clientEnv callback

-- | Ask Share for the dependencies of a given hash jwt,
-- then filter them to get the set of causals which we have and don't need sent.
negotiateKnownCausals ::
  -- | The Unison Share URL.
  Servant.BaseUrl ->
  -- | The branch to download from.
  SyncV2.BranchRef ->
  -- | The hash to download.
  Share.HashJWT ->
  Cli (Either (SyncError SyncV2.PullError) (Set Hash32))
negotiateKnownCausals unisonShareUrl branchRef hashJwt = do
  Cli.Env {authHTTPClient, codebase} <- ask
  liftIO $ Text.hPutStrLn IO.stderr $ "  🔎 Identifying missing entities..."
  Timing.time "Causal Negotiation" $ do
    liftIO . C.runResourceT . runExceptT $ httpStreamCausalDependencies
      authHTTPClient
      unisonShareUrl
      SyncV2.CausalDependenciesRequest {branchRef, rootCausal = hashJwt}
      \stream -> do
        Set.fromList <$> C.runConduit (stream C..| C.map unpack C..| findKnownDeps codebase C..| C.sinkList)
  where
    -- Go through the dependencies of the remote root from top-down, yielding all causal hashes that we already
    -- have until we find one in the causal spine we already have, then yield that one and stop since we'll implicitly
    -- have all of its dependencies.
    findKnownDeps :: Codebase.Codebase IO v a -> Stream (Hash32, DependencyType) Hash32
    findKnownDeps codebase = do
      C.await >>= \case
        Just (hash, LibDependency) -> do
          -- We yield all lib dependencies we have, it's possible we don't have any of the causal spine in common, but _do_ have
          -- some of the libraries we can still save a lot of work.
          whenM (lift $ haveCausalHash codebase hash) (C.yield hash)
          -- We continue regardless.
          findKnownDeps codebase
        Just (hash, CausalSpineDependency) -> do
          lift (haveCausalHash codebase hash) >>= \case
            True -> do
              -- If we find a causal hash we have in the spine, we don't need to look further,
              -- we can pass it on, then hang up the stream.
              C.yield hash
            False -> do
              -- Otherwise we keep looking, maybe we'll have one further in.
              findKnownDeps codebase
        Nothing -> pure ()
    unpack :: SyncV2.CausalDependenciesChunk -> (Hash32, DependencyType)
    unpack = \case
      SyncV2.CausalHashDepC {causalHash, dependencyType} -> (causalHash, dependencyType)
    haveCausalHash :: Codebase.Codebase IO v a -> Hash32 -> StreamM Bool
    haveCausalHash codebase causalHash = do
      liftIO $ Codebase.runTransaction codebase do
        Q.causalExistsByHash32 causalHash

------------------------------------------------------------------------------------------------------------------------
-- Progress Tracking
------------------------------------------------------------------------------------------------------------------------

counterProgress :: (MonadIO m, MonadUnliftIO n) => (Int -> Text) -> ((Int -> m ()) -> n a) -> n a
counterProgress msgBuilder action = do
  counterVar <- IO.newTVarIO (0 :: Int)
  IO.withRunInIO \toIO -> do
    Console.Regions.displayConsoleRegions do
      Console.Regions.withConsoleRegion Console.Regions.Linear \region -> do
        Console.Regions.setConsoleRegion region do
          num <- IO.readTVar counterVar
          pure $ msgBuilder num
        toIO $ action $ \i -> do
          liftIO $ IO.atomically (IO.modifyTVar' counterVar (+ i))

-- | Track how many entities have been downloaded using a counter stream.
withStreamProgressCallback :: (MonadIO m, MonadUnliftIO n) => Maybe Int -> (ConduitT i i m () -> n a) -> n a
withStreamProgressCallback total action = do
  let msg n = "\n  📦 Unpacked  " <> tShow n <> maybe "" (\total -> " / " <> tShow total) total <> " entities...\n\n"
  let action' f = action (C.iterM \_i -> f 1)
  counterProgress msg action'

-- | Track how many entities have been saved.
withEntitySavingCallback :: (MonadUnliftIO m) => Maybe Int -> ((Int -> m ()) -> m a) -> m a
withEntitySavingCallback total action = do
  let msg n = "\n  💾 Saved  " <> tShow n <> maybe "" (\total -> " / " <> tShow total) total <> " new entities...\n\n"
  counterProgress msg action

-- | Track how many entities have been loaded.
withEntityLoadingCallback :: (MonadUnliftIO m) => ((Int -> m ()) -> m a) -> m a
withEntityLoadingCallback action = do
  let msg n = "\n  📦 Unpacked  " <> tShow n <> " entities...\n\n"
  counterProgress msg action
