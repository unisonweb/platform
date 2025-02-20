{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Orphans where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.Serialise (Serialise (..))
import Codec.Serialise qualified as CBOR
import Codec.Serialise.Class qualified as CBOR
import Control.Lens
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Binary
import Data.ByteString.Short (ShortByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.OpenApi
import Data.Proxy
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Servant
import Servant.Docs (DocCapture (DocCapture), DocQueryParam (..), ParamKind (..), ToCapture (..), ToParam (..))
import U.Codebase.HashTags
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.Causal qualified as SqliteCausal
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.LocalIds qualified as LocalIds
import U.Codebase.Sqlite.Patch.Format qualified as PatchFormat
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Util.Base32Hex (Base32Hex (..))
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash (..))
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as CT
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.Hash (Hash (..))
import Unison.Hash qualified as Hash
import Unison.Hash32 (Hash32 (..))
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Prelude
import Unison.Project
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Share.API.Hash (HashJWT (..))
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Unison.Sqlite qualified as Sqlite
import Unison.Syntax.HashQualified qualified as HQ (parseText)
import Unison.Syntax.HashQualifiedPrime qualified as HQ' (parseText)
import Unison.Syntax.Name qualified as Name (parseTextEither, toText)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Pretty (Width (..))

instance ToJSON Hash where
  toJSON h = String $ Hash.toBase32HexText h

instance FromJSON Hash where
  parseJSON = Aeson.withText "Hash" $ pure . Hash.unsafeFromBase32HexText

deriving via Hash instance ToJSON CausalHash

deriving via Hash instance FromJSON CausalHash

instance ToJSON ShortHash where
  toJSON = Aeson.String . SH.toText

instance ToJSONKey ShortHash where
  toJSONKey = contramap SH.toText (toJSONKey @Text)

instance FromJSON ShortHash where
  parseJSON = Aeson.withText "ShortHash" \txt ->
    case SH.fromText txt of
      Nothing -> fail $ "Invalid Shorthash" <> Text.unpack txt
      Just sh -> pure sh

instance FromJSONKey ShortHash where
  fromJSONKey =
    Aeson.FromJSONKeyTextParser \txt ->
      case SH.fromText txt of
        Nothing -> fail $ "Invalid Shorthash" <> Text.unpack txt
        Just sh -> pure sh

instance FromHttpApiData ShortCausalHash where
  parseUrlPiece = maybe (Left "Invalid ShortCausalHash") Right . SCH.fromText

-- | Always renders to the form: #abcdef
instance ToHttpApiData ShortHash where
  toQueryParam = SH.toText

-- | Accepts shorthashes of any of the following forms:
-- @abcdef
-- @@builtin
-- #abcdef
-- ##builtin
-- abcdef
instance FromHttpApiData ShortHash where
  parseUrlPiece txt =
    Text.replace "@" "#" txt
      & \t ->
        ( if Text.isPrefixOf "#" t
            then t
            else ("#" <> t)
        )
          & SH.fromText
          & maybe (Left "Invalid ShortCausalHash") Right

instance ToSchema ShortHash where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

-- | Always renders to the form: #abcdef
instance ToHttpApiData Reference.Reference where
  toQueryParam = Reference.toText

-- | Always renders to the form: #abcdef
instance ToHttpApiData Referent.Referent where
  toQueryParam = Referent.toText

-- | Accepts shorthashes of any of the following forms:
-- @abcdef
-- @@builtin
-- #abcdef
-- ##builtin
-- abcdef
instance FromHttpApiData Reference.Reference where
  parseUrlPiece txt =
    Text.replace "@" "#" txt
      & \t ->
        ( if Text.isPrefixOf "#" t
            then t
            else ("#" <> t)
        )
          & Reference.fromText
          & mapLeft Text.pack

-- | Accepts shorthashes of any of the following forms:
-- @abcdef
-- @@builtin
-- #abcdef
-- ##builtin
-- abcdef
instance FromHttpApiData Referent.Referent where
  parseUrlPiece txt =
    Text.replace "@" "#" txt
      & \t ->
        ( if Text.isPrefixOf "#" t
            then t
            else ("#" <> t)
        )
          & Referent.fromText
          & maybe (Left "Invalid Referent") Right

instance ToSchema Reference where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

deriving via ShortByteString instance Binary Hash

deriving via Hash instance Binary CausalHash

deriving via Text instance ToHttpApiData ShortCausalHash

instance (ToJSON b, ToJSON a) => ToJSON (DisplayObject b a) where
  toJSON = \case
    BuiltinObject b -> object ["tag" Aeson..= String "BuiltinObject", "contents" Aeson..= b]
    MissingObject sh -> object ["tag" Aeson..= String "MissingObject", "contents" Aeson..= sh]
    UserObject a -> object ["tag" Aeson..= String "UserObject", "contents" Aeson..= a]

instance (FromJSON a, FromJSON b) => FromJSON (DisplayObject b a) where
  parseJSON = withObject "DisplayObject" \o -> do
    tag <- o .: "tag"
    case tag of
      "BuiltinObject" -> BuiltinObject <$> o .: "contents"
      "MissingObject" -> MissingObject <$> o .: "contents"
      "UserObject" -> UserObject <$> o .: "contents"
      _ -> fail $ "Invalid tag: " <> Text.unpack tag

deriving instance (ToSchema b, ToSchema a) => ToSchema (DisplayObject b a)

-- [21/10/07] Hello, this is Mitchell. Name refactor in progress. Changing internal representation from a flat text to a
-- list of segments (in reverse order) plus an "is absolute?" bit.
--
-- To preserve backwards compatibility (for now, anyway -- is this even important long term?), the ToJSON and ToSchema
-- instances below treat Name as before.

instance ToJSON Name where
  toEncoding = toEncoding . Name.toText
  toJSON = toJSON . Name.toText

instance ToJSONKey Name where
  toJSONKey = contramap Name.toText (toJSONKey @Text)

instance ToSchema Name where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToJSON NameSegment where
  toJSON = toJSON . NameSegment.toEscapedText

instance ToJSONKey NameSegment where
  toJSONKey = contramap NameSegment.toEscapedText (toJSONKey @Text)

deriving anyclass instance ToParamSchema ShortCausalHash

instance ToParamSchema ShortHash where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "@abcdef"

instance ToParamSchema Reference.Reference where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "@abcdef"

instance ToParamSchema Referent.Referent where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "@abcdef"

instance ToParamSchema Name where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "base.List.map"

instance ToParamSchema Path.Path where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "base.List"

instance ToParamSchema Path.Relative where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "base.List"

instance ToParam (QueryParam "name" Name) where
  toParam _ =
    DocQueryParam
      "name"
      []
      "A definition name. See API documentation to determine how it should be qualified."
      Normal

instance FromHttpApiData Name where
  parseQueryParam = Name.parseTextEither

deriving via Int instance FromHttpApiData Width

deriving via Int instance ToHttpApiData Width

deriving anyclass instance ToParamSchema Width

instance ToJSON ConstructorType where
  toJSON = \case
    CT.Data -> String "Data"
    CT.Effect -> String "Effect"

instance FromHttpApiData Path.Relative where
  parseUrlPiece txt = case Path.parsePath' (Text.unpack txt) of
    Left s -> Left s
    Right (Path.RelativePath' p) -> Right p
    Right (Path.AbsolutePath' _) -> Left $ "Expected relative path, but " <> txt <> " was absolute."

instance ToHttpApiData Path.Relative where
  toUrlPiece = tShow

instance FromHttpApiData Path.Absolute where
  parseUrlPiece txt = case Path.parsePath' (Text.unpack txt) of
    Left s -> Left s
    Right (Path.RelativePath' _) -> Left $ "Expected absolute path, but " <> txt <> " was relative."
    Right (Path.AbsolutePath' p) -> Right p

instance ToHttpApiData Path.Absolute where
  toUrlPiece = tShow

instance FromHttpApiData Path.Path' where
  parseUrlPiece txt = Path.parsePath' (Text.unpack txt)

instance ToHttpApiData Path.Path' where
  toUrlPiece = tShow

instance FromHttpApiData Path.Path where
  parseUrlPiece txt = case Path.parsePath' (Text.unpack txt) of
    Left s -> Left s
    Right (Path.RelativePath' p) -> Right (Path.unrelative p)
    Right (Path.AbsolutePath' _) -> Left $ "Expected relative path, but " <> txt <> " was absolute."

instance ToCapture (Capture "hash" ShortHash) where
  toCapture _ =
    DocCapture
      "hash"
      "A shorthash for a term or type. E.g. @abcdef, #abcdef, @@builtin, ##builtin, abcdef"

instance ToCapture (Capture "hash" Reference.Reference) where
  toCapture _ =
    DocCapture
      "hash"
      "A hash reference for a type. E.g. @abcdef, #abcdef, @@builtin, ##builtin, abcdef"

instance ToCapture (Capture "hash" Referent.Referent) where
  toCapture _ =
    DocCapture
      "hash"
      "A hash reference for a term. E.g. @abcdef, #abcdef, @@builtin, ##builtin, abcdef"

instance ToCapture (Capture "fqn" Name) where
  toCapture _ =
    DocCapture
      "fqn"
      "The fully qualified name of a definition."

instance ToCapture (Capture "namespace" Path.Path) where
  toCapture _ =
    DocCapture
      "namespace"
      "E.g. base.List"

instance ToJSON Path.Path where
  toJSON p = Aeson.String (Path.toText p)

instance FromJSON Path.Path where
  parseJSON = Aeson.withText "Path" \txt -> case Path.parsePath (Text.unpack txt) of
    Left s -> fail (Text.unpack s)
    Right p -> pure p

instance ToJSON Path.Absolute where
  toJSON p = Aeson.String (Path.toText p)

instance FromJSON Path.Absolute where
  parseJSON = Aeson.withText "Path" \txt -> case Path.parsePath' (Text.unpack txt) of
    Left s -> fail (Text.unpack s)
    Right (Path.AbsolutePath' p) -> pure p
    Right (Path.RelativePath' _) -> fail "Expected an absolute path but received a relative path."

instance ToSchema Path.Path where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToSchema Path.Absolute where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToJSON (HQ.HashQualified Name) where
  toJSON = Aeson.String . HQ.toTextWith Name.toText

instance ToJSON (HQ.HashQualified NameSegment) where
  toJSON = Aeson.String . HQ.toTextWith NameSegment.toEscapedText

instance ToJSON (HQ'.HashQualified Name) where
  toJSON = Aeson.String . HQ'.toTextWith Name.toText

instance ToJSON (HQ'.HashQualified NameSegment) where
  toJSON = Aeson.String . HQ'.toTextWith NameSegment.toEscapedText

instance FromJSON (HQ'.HashQualified Name) where
  parseJSON = Aeson.withText "HashQualified'" \txt ->
    maybe (fail "Invalid HashQualified' Name") pure $ HQ'.parseText txt

instance FromJSON (HQ.HashQualified Name) where
  parseJSON = Aeson.withText "HashQualified" \txt ->
    maybe (fail "Invalid HashQualified Name") pure $ HQ.parseText txt

instance FromJSON (HQ'.HashQualified NameSegment) where
  parseJSON = Aeson.withText "HashQualified'" \txt -> do
    hqName <- maybe (fail "Invalid HashQualified' NameSegment") pure $ HQ'.parseText txt
    for hqName \name -> case Name.segments name of
      (ns :| []) -> pure ns
      _ -> fail $ "Expected a single name segment but received several: " <> Text.unpack txt

instance FromJSON (HQ.HashQualified NameSegment) where
  parseJSON = Aeson.withText "HashQualified" \txt -> do
    hqName <- maybe (fail "Invalid HashQualified' NameSegment") pure $ HQ.parseText txt
    for hqName \name -> case Name.segments name of
      (ns :| []) -> pure ns
      _ -> fail $ "Expected a single name segment but received several: " <> Text.unpack txt

instance FromHttpApiData (HQ.HashQualified Name) where
  parseQueryParam txt =
    Text.replace "@" "#" txt
      & HQ.parseText
      & maybe (Left "Invalid Hash Qualified Name. Expected one of the following forms: name@hash, name, @hash") Right

instance FromHttpApiData (HQ'.HashQualified Name) where
  parseQueryParam txt =
    Text.replace "@" "#" txt
      & HQ'.parseText
      & maybe (Left "Invalid Hash Qualified Name. Expected one of the following forms: name@hash, name") Right

instance ToParamSchema (HQ.HashQualified n) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "name@hash"

instance ToParamSchema (HQ'.HashQualified n) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "name@hash"

instance ToHttpApiData Name where
  toQueryParam = Name.toText

deriving newtype instance ToSchema NameSegment

deriving anyclass instance (ToSchema n) => ToSchema (HQ.HashQualified n)

deriving anyclass instance (ToSchema n) => ToSchema (HQ'.HashQualified n)

deriving via Text instance Sqlite.FromField ProjectName

instance FromHttpApiData ProjectName where
  parseQueryParam = mapLeft tShow . tryInto @ProjectName

instance ToParamSchema ProjectName where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "[@handle/]name"

instance ToCapture (Capture "project-name" ProjectName) where
  toCapture _ =
    DocCapture
      "project-name"
      "The name of a project. E.g. @handle/slug"

instance ToSchema ProjectName

deriving via Text instance ToJSON ProjectName

deriving via Text instance FromJSON ProjectName

deriving via Text instance Sqlite.FromField ProjectBranchName

instance FromHttpApiData ProjectBranchName where
  parseQueryParam = mapLeft tShow . tryInto @ProjectBranchName

instance ToSchema ProjectBranchName

instance ToParamSchema ProjectBranchName where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & example ?~ Aeson.String "[@handle/]name"

instance ToCapture (Capture "branch-name" ProjectBranchName) where
  toCapture _ =
    DocCapture
      "branch-name"
      "The name of a branch in a project. E.g. @handle/name"

deriving via Text instance ToJSON ProjectBranchName

deriving via Text instance FromJSON ProjectBranchName

-- CBOR encodings

deriving via Text instance Serialise Hash32

deriving via Text instance Serialise HashJWT

data SyncTag
  = TermComponentTag
  | DeclComponentTag
  | PatchTag
  | NamespaceTag
  | CausalTag
  deriving (Eq, Show)

instance Serialise SyncTag where
  encode = \case
    TermComponentTag -> CBOR.encodeWord 0
    DeclComponentTag -> CBOR.encodeWord 1
    PatchTag -> CBOR.encodeWord 2
    NamespaceTag -> CBOR.encodeWord 3
    CausalTag -> CBOR.encodeWord 4

  decode = do
    tag <- CBOR.decodeWord
    case tag of
      0 -> pure TermComponentTag
      1 -> pure DeclComponentTag
      2 -> pure PatchTag
      3 -> pure NamespaceTag
      4 -> pure CausalTag
      _ -> fail $ "Unknown tag: " <> show tag

newtype ComponentBody t d = ComponentBody {unComponentBody :: (LocalIds.LocalIds' t d, ByteString)}

instance (Serialise t, Serialise d) => Serialise (ComponentBody t d) where
  encode (ComponentBody (LocalIds.LocalIds {textLookup, defnLookup}, bytes)) =
    CBOR.encodeVector textLookup
      <> CBOR.encodeVector defnLookup
      <> CBOR.encodeBytes bytes

  decode = do
    textLookup <- CBOR.decodeVector
    defnLookup <- CBOR.decodeVector
    bytes <- CBOR.decodeBytes
    pure $ ComponentBody (LocalIds.LocalIds {textLookup, defnLookup}, bytes)

instance Serialise TempEntity where
  encode = \case
    Entity.TC (TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent elements)) ->
      CBOR.encode TermComponentTag
        <> CBOR.encodeVector (coerce @(Vector (LocalIds.LocalIds' Text Hash32, ByteString)) @(Vector (ComponentBody Text Hash32)) elements)
    Entity.DC (DeclFormat.SyncDecl (DeclFormat.SyncLocallyIndexedComponent elements)) ->
      CBOR.encode DeclComponentTag
        <> CBOR.encodeVector (coerce @(Vector (LocalIds.LocalIds' Text Hash32, ByteString)) @(Vector (ComponentBody Text Hash32)) elements)
    Entity.P (PatchFormat.SyncDiff {}) -> error "Serializing Diffs are not supported"
    Entity.P (PatchFormat.SyncFull (PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup}) bytes) ->
      CBOR.encode PatchTag
        <> CBOR.encodeVector patchTextLookup
        <> CBOR.encodeVector patchHashLookup
        <> CBOR.encodeVector patchDefnLookup
        <> CBOR.encodeBytes bytes
    Entity.N (BranchFormat.SyncDiff {}) -> error "Serializing Diffs are not supported"
    Entity.N (BranchFormat.SyncFull (BranchFormat.LocalIds {branchTextLookup, branchDefnLookup, branchPatchLookup, branchChildLookup}) (BranchFormat.LocalBranchBytes bytes)) ->
      CBOR.encode NamespaceTag
        <> CBOR.encodeVector branchTextLookup
        <> CBOR.encodeVector branchDefnLookup
        <> CBOR.encodeVector branchPatchLookup
        <> CBOR.encodeVector branchChildLookup
        <> CBOR.encodeBytes bytes
    Entity.C (SqliteCausal.SyncCausalFormat {valueHash, parents}) ->
      CBOR.encode CausalTag
        <> CBOR.encode valueHash
        <> CBOR.encodeVector parents

  decode = do
    CBOR.decode >>= \case
      TermComponentTag -> do
        elements <- coerce @(Vector (ComponentBody Text Hash32)) @(Vector (LocalIds.LocalIds' Text Hash32, ByteString)) <$> CBOR.decodeVector
        pure $ Entity.TC (TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent elements))
      DeclComponentTag -> do
        elements <- coerce @(Vector (ComponentBody Text Hash32)) @(Vector (LocalIds.LocalIds' Text Hash32, ByteString)) <$> CBOR.decodeVector
        pure $ Entity.DC (DeclFormat.SyncDecl (DeclFormat.SyncLocallyIndexedComponent elements))
      PatchTag -> do
        patchTextLookup <- CBOR.decodeVector
        patchHashLookup <- CBOR.decodeVector
        patchDefnLookup <- CBOR.decodeVector
        bytes <- CBOR.decodeBytes
        pure $ Entity.P (PatchFormat.SyncFull (PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup}) bytes)
      NamespaceTag -> do
        branchTextLookup <- CBOR.decodeVector
        branchDefnLookup <- CBOR.decodeVector
        branchPatchLookup <- CBOR.decodeVector
        branchChildLookup <- CBOR.decodeVector
        bytes <- CBOR.decodeBytes
        pure $ Entity.N (BranchFormat.SyncFull (BranchFormat.LocalIds {branchTextLookup, branchDefnLookup, branchPatchLookup, branchChildLookup}) (BranchFormat.LocalBranchBytes bytes))
      CausalTag -> do
        valueHash <- CBOR.decode
        parents <- CBOR.decodeVector
        pure $ Entity.C (SqliteCausal.SyncCausalFormat {valueHash, parents})

encodeVectorWith :: (a -> CBOR.Encoding) -> Vector.Vector a -> CBOR.Encoding
encodeVectorWith f xs =
  CBOR.encodeListLen (fromIntegral $ Vector.length xs)
    <> (foldr (\a b -> f a <> b) mempty xs)

instance Ord CBOR.DeserialiseFailure where
  compare (CBOR.DeserialiseFailure o s) (CBOR.DeserialiseFailure o' s') = compare (o, s) (o', s')
