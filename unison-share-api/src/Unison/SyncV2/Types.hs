{-# LANGUAGE RecordWildCards #-}

module Unison.SyncV2.Types
  ( DownloadEntitiesRequest (..),
    DownloadEntitiesChunk (..),
    EntityChunk (..),
    ErrorChunk (..),
    StreamInitInfo (..),
    SyncError (..),
    DownloadEntitiesError (..),
    CausalDependenciesRequest (..),
    CausalDependenciesChunk (..),
    CBORBytes (..),
    CBORStream(..),
    EntityKind (..),
    serialiseCBORBytes,
    deserialiseOrFailCBORBytes,
    BranchRef (..),
    PullError (..),
    EntitySorting (..),
    Version (..),
  )
where

import Codec.CBOR.Encoding qualified as CBOR
import Codec.Serialise (Serialise (..))
import Codec.Serialise qualified as CBOR
import Codec.Serialise.Decoding qualified as CBOR
import Control.Exception (Exception)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16, Word64)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.TempEntity (TempEntity)
import Unison.Core.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Hash32 (Hash32)
import Unison.Prelude (From (..))
import Unison.Server.Orphans ()
import Unison.Share.API.Hash (HashJWT)
import Unison.Sync.Types qualified as SyncV1
import Unison.Util.Servant.CBOR

newtype BranchRef = BranchRef {unBranchRef :: Text}
  deriving (Serialise, Eq, Show, Ord, ToJSON, FromJSON) via Text

instance From (ProjectAndBranch ProjectName ProjectBranchName) BranchRef where
  from pab = BranchRef $ from pab

data GetCausalHashErrorTag
  = GetCausalHashNoReadPermissionTag
  | GetCausalHashUserNotFoundTag
  | GetCausalHashInvalidBranchRefTag
  deriving stock (Show, Eq, Ord)

instance Serialise GetCausalHashErrorTag where
  encode GetCausalHashNoReadPermissionTag = CBOR.encodeWord8 0
  encode GetCausalHashUserNotFoundTag = CBOR.encodeWord8 1
  encode GetCausalHashInvalidBranchRefTag = CBOR.encodeWord8 2
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure GetCausalHashNoReadPermissionTag
      1 -> pure GetCausalHashUserNotFoundTag
      2 -> pure GetCausalHashInvalidBranchRefTag
      _ -> fail "invalid tag"

data DownloadEntitiesRequest = DownloadEntitiesRequest
  { causalHash :: HashJWT,
    branchRef :: BranchRef,
    knownHashes :: Set Hash32
  }

instance Serialise DownloadEntitiesRequest where
  encode (DownloadEntitiesRequest {causalHash, branchRef, knownHashes}) =
    encode causalHash <> encode branchRef <> encode knownHashes
  decode = DownloadEntitiesRequest <$> decode <*> decode <*> decode

instance FromJSON DownloadEntitiesRequest where
  parseJSON = withObject "DownloadEntitiesRequest" $ \o -> do
    causalHash <- o .: "causalHash"
    branchRef <- o .: "branchRef"
    knownHashes <- o .: "knownHashes"
    pure DownloadEntitiesRequest {causalHash, branchRef, knownHashes}

instance ToJSON DownloadEntitiesRequest where
  toJSON (DownloadEntitiesRequest {causalHash, branchRef, knownHashes}) =
    object
      [ "causalHash" .= causalHash,
        "branchRef" .= branchRef,
        "knownHashes" .= knownHashes
      ]

data DownloadEntitiesError
  = DownloadEntitiesNoReadPermission BranchRef
  | -- | msg, branchRef
    DownloadEntitiesInvalidBranchRef Text BranchRef
  | -- | userHandle
    DownloadEntitiesUserNotFound Text
  | -- | project shorthand
    DownloadEntitiesProjectNotFound Text
  | DownloadEntitiesEntityValidationFailure SyncV1.EntityValidationError
  deriving stock (Eq, Show, Ord)

data DownloadEntitiesErrorTag
  = NoReadPermissionTag
  | InvalidBranchRefTag
  | UserNotFoundTag
  | ProjectNotFoundTag
  | EntityValidationFailureTag
  deriving stock (Eq, Show, Ord)

instance Serialise DownloadEntitiesErrorTag where
  encode = \case
    NoReadPermissionTag -> CBOR.encodeWord8 0
    InvalidBranchRefTag -> CBOR.encodeWord8 1
    UserNotFoundTag -> CBOR.encodeWord8 2
    ProjectNotFoundTag -> CBOR.encodeWord8 3
    EntityValidationFailureTag -> CBOR.encodeWord8 4
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure NoReadPermissionTag
      1 -> pure InvalidBranchRefTag
      2 -> pure UserNotFoundTag
      3 -> pure ProjectNotFoundTag
      4 -> pure EntityValidationFailureTag
      _ -> fail "invalid tag"

instance Serialise DownloadEntitiesError where
  encode = \case
    DownloadEntitiesNoReadPermission branchRef -> CBOR.encode NoReadPermissionTag <> CBOR.encode branchRef
    DownloadEntitiesInvalidBranchRef msg branchRef -> CBOR.encode InvalidBranchRefTag <> CBOR.encode (msg, branchRef)
    DownloadEntitiesUserNotFound userHandle -> CBOR.encode UserNotFoundTag <> CBOR.encode userHandle
    DownloadEntitiesProjectNotFound projectShorthand -> CBOR.encode ProjectNotFoundTag <> CBOR.encode projectShorthand
    DownloadEntitiesEntityValidationFailure err -> CBOR.encode EntityValidationFailureTag <> CBOR.encode err

  decode = do
    tag <- CBOR.decode
    case tag of
      NoReadPermissionTag -> DownloadEntitiesNoReadPermission <$> CBOR.decode
      InvalidBranchRefTag -> uncurry DownloadEntitiesInvalidBranchRef <$> CBOR.decode
      UserNotFoundTag -> DownloadEntitiesUserNotFound <$> CBOR.decode
      ProjectNotFoundTag -> DownloadEntitiesProjectNotFound <$> CBOR.decode
      EntityValidationFailureTag -> DownloadEntitiesEntityValidationFailure <$> CBOR.decode

data EntitySorting
  = -- all dependencies of an entity are guaranteed to be sent before the entity itself
    DependenciesFirst
  | -- no guarantees.
    Unsorted
  deriving (Show, Eq, Ord)

instance Serialise EntitySorting where
  encode = \case
    DependenciesFirst -> CBOR.encodeWord8 0
    Unsorted -> CBOR.encodeWord8 1
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure DependenciesFirst
      1 -> pure Unsorted
      _ -> fail "invalid tag"

newtype Version = Version Word16
  deriving stock (Show)
  deriving newtype (Eq, Ord, Serialise)

data StreamInitInfo = StreamInitInfo
  { version :: Version,
    entitySorting :: EntitySorting,
    numEntities :: Maybe Word64,
    rootCausalHash :: Hash32,
    rootBranchRef :: Maybe BranchRef
  }
  deriving (Show, Eq, Ord)

decodeMapKey :: (Serialise r) => Text -> Map Text UnknownCBORBytes -> CBOR.Decoder s r
decodeMapKey k m =
  optionalDecodeMapKey k m >>= \case
    Nothing -> fail $ "Expected key: " <> Text.unpack k
    Just x -> pure x

optionalDecodeMapKey :: (Serialise r) => Text -> Map Text UnknownCBORBytes -> CBOR.Decoder s (Maybe r)
optionalDecodeMapKey k m =
  case Map.lookup k m of
    Nothing -> pure Nothing
    Just bs -> Just <$> decodeUnknownCBORBytes bs

-- | Serialised as a map to be future compatible, allowing for future expansion.
instance Serialise StreamInitInfo where
  encode (StreamInitInfo {version, entitySorting, numEntities, rootCausalHash, rootBranchRef}) =
    CBOR.encode
      ( Map.fromList $
          [ ("v" :: Text, serialiseUnknownCBORBytes version),
            ("es", serialiseUnknownCBORBytes entitySorting),
            ("rc", serialiseUnknownCBORBytes rootCausalHash)
          ]
            <> maybe [] (\ne -> [("ne", serialiseUnknownCBORBytes ne)]) numEntities
            <> maybe [] (\br -> [("br", serialiseUnknownCBORBytes br)]) rootBranchRef
      )
  decode = do
    m <- CBOR.decode
    version <- decodeMapKey "v" m
    entitySorting <- decodeMapKey "es" m
    numEntities <- (optionalDecodeMapKey "ne" m)
    rootCausalHash <- decodeMapKey "rc" m
    rootBranchRef <- optionalDecodeMapKey "br" m
    pure StreamInitInfo {version, entitySorting, numEntities, rootCausalHash, rootBranchRef}

data EntityChunk = EntityChunk
  { hash :: Hash32,
    entityCBOR :: CBORBytes TempEntity
  }
  deriving (Show, Eq, Ord)

instance Serialise EntityChunk where
  encode (EntityChunk {hash, entityCBOR}) = CBOR.encode hash <> CBOR.encode entityCBOR
  decode = EntityChunk <$> CBOR.decode <*> CBOR.decode

data ErrorChunk = ErrorChunk
  { err :: DownloadEntitiesError
  }
  deriving (Show, Eq, Ord)

instance Serialise ErrorChunk where
  encode (ErrorChunk {err}) = CBOR.encode err
  decode = ErrorChunk <$> CBOR.decode

-- | A chunk of the download entities response stream.
data DownloadEntitiesChunk
  = InitialC StreamInitInfo
  | EntityC EntityChunk
  | ErrorC ErrorChunk
  deriving (Show, Eq, Ord)

data DownloadEntitiesChunkTag = InitialChunkTag | EntityChunkTag | ErrorChunkTag
  deriving (Show, Eq, Ord)

instance Serialise DownloadEntitiesChunkTag where
  encode InitialChunkTag = CBOR.encodeWord8 0
  encode EntityChunkTag = CBOR.encodeWord8 1
  encode ErrorChunkTag = CBOR.encodeWord8 2
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure InitialChunkTag
      1 -> pure EntityChunkTag
      2 -> pure ErrorChunkTag
      _ -> fail "invalid tag"

instance Serialise DownloadEntitiesChunk where
  encode (EntityC ec) = encode EntityChunkTag <> CBOR.encode ec
  encode (ErrorC ec) = encode ErrorChunkTag <> CBOR.encode ec
  encode (InitialC ic) = encode InitialChunkTag <> encode ic
  decode = do
    tag <- decode
    case tag of
      InitialChunkTag -> InitialC <$> decode
      EntityChunkTag -> EntityC <$> decode
      ErrorChunkTag -> ErrorC <$> decode

-- | An error occurred while pulling code from Unison Share.
data PullError
  = PullError'DownloadEntities DownloadEntitiesError
  | PullError'Sync SyncError
  deriving stock (Show, Eq, Ord)
  deriving anyclass (Exception)

data SyncError
  = SyncErrorExpectedResultNotInMain CausalHash
  | SyncErrorDeserializationFailure CBOR.DeserialiseFailure
  | SyncErrorMissingInitialChunk
  | SyncErrorMisplacedInitialChunk
  | SyncErrorStreamFailure Text
  | SyncErrorUnsupportedVersion Version
  deriving stock (Show, Eq, Ord)

data EntityKind
  = CausalEntity
  | NamespaceEntity
  | TermEntity
  | TypeEntity
  | PatchEntity
  deriving (Show, Eq, Ord)

instance Serialise EntityKind where
  encode = \case
    CausalEntity -> CBOR.encodeWord8 0
    NamespaceEntity -> CBOR.encodeWord8 1
    TermEntity -> CBOR.encodeWord8 2
    TypeEntity -> CBOR.encodeWord8 3
    PatchEntity -> CBOR.encodeWord8 4
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure CausalEntity
      1 -> pure NamespaceEntity
      2 -> pure TermEntity
      3 -> pure TypeEntity
      4 -> pure PatchEntity
      _ -> fail "invalid tag"

------------------------------------------------------------------------------------------------------------------------
-- Causal Dependencies

data CausalDependenciesRequest = CausalDependenciesRequest
  { branchRef :: BranchRef,
    rootCausal :: HashJWT
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON CausalDependenciesRequest where
  toJSON (CausalDependenciesRequest branchRef rootCausal) =
    object
      [ "branch_ref" .= branchRef,
        "root_causal" .= rootCausal
      ]

instance FromJSON CausalDependenciesRequest where
  parseJSON = Aeson.withObject "CausalDependenciesRequest" \obj -> do
    branchRef <- obj .: "branch_ref"
    rootCausal <- obj .: "root_causal"
    pure CausalDependenciesRequest {..}

instance Serialise CausalDependenciesRequest where
  encode (CausalDependenciesRequest {branchRef, rootCausal}) =
    encode branchRef <> encode rootCausal
  decode = CausalDependenciesRequest <$> decode <*> decode

-- | A chunk of the download entities response stream.
data CausalDependenciesChunk
  = HashC Hash32
  deriving (Show, Eq, Ord)

data CausalDependenciesChunkTag = HashChunkTag
  deriving (Show, Eq, Ord)

instance Serialise CausalDependenciesChunkTag where
  encode = \case
    HashChunkTag -> CBOR.encodeWord8 0
  decode = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> pure HashChunkTag
      _ -> fail "invalid tag"

instance Serialise CausalDependenciesChunk where
  encode = \case
    (HashC ch) -> do
      encode HashChunkTag <> CBOR.encode ch
  decode = do
    tag <- decode
    case tag of
      HashChunkTag -> HashC <$> CBOR.decode
