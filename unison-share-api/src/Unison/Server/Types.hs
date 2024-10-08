{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Server.Types where

-- Types common to endpoints --
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.ByteString.Lazy qualified as LZ
import Data.Map qualified as Map
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text
import Servant qualified
import Servant.API (FromHttpApiData (..), Get, Header, Headers, JSON, addHeader)
import Servant.Docs qualified as Docs
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Codebase.Path qualified as Path
import Unison.Core.Project (ProjectBranchName)
import Unison.Hash qualified as Hash
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Prelude
import Unison.Project (ProjectName)
import Unison.Server.Doc (Doc)
import Unison.Server.Orphans ()
import Unison.Server.Syntax qualified as Syntax
import Unison.ShortHash (ShortHash)
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty (Width (..))

type APIHeaders x =
  Headers
    '[ Header "Cache-Control" String
     ]
    x

type APIGet c = Get '[JSON] (APIHeaders c)

type HashQualifiedName = Text

type UnisonName = Text

type UnisonHash = Text

data NamespaceDetails = NamespaceDetails
  { fqn :: Path.Path,
    hash :: UnisonHash,
    readme :: Maybe Doc
  }
  deriving (Generic, Show)

instance Docs.ToSample NamespaceDetails where
  toSamples _ =
    [ ( "When no value is provided for `namespace`, the root namespace `.` is "
          <> "listed by default",
        NamespaceDetails
          Path.empty
          "#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"
          Nothing
      )
    ]

instance ToJSON NamespaceDetails where
  toJSON NamespaceDetails {..} =
    object
      [ "fqn" .= fqn,
        "hash" .= hash,
        "readme" .= readme
      ]

deriving instance ToSchema NamespaceDetails

-- | A hash qualified name, unlike HashQualified, the hash is required
data ExactName name ref = ExactName
  { name :: name,
    ref :: ref
  }
  deriving stock (Show, Eq, Functor, Ord)

instance Bifunctor ExactName where
  bimap l r (ExactName a b) = ExactName (l a) (r b)

instance Bifoldable ExactName where
  bifoldMap l r (ExactName a b) = l a <> r b

instance Bitraversable ExactName where
  bitraverse l r (ExactName a b) = ExactName <$> (l a) <*> (r b)

deriving via Bool instance FromHttpApiData Suffixify

deriving anyclass instance ToParamSchema Suffixify

instance ToJSON TypeDefinition where
  toJSON TypeDefinition {..} =
    object
      [ "typeNames" .= typeNames,
        "bestTypeName" .= bestTypeName,
        "defnTypeTag" .= defnTypeTag,
        "typeDefinition" .= typeDefinition,
        "typeDocs" .= typeDocs
      ]

deriving instance ToSchema TypeDefinition

instance ToJSON TermDefinition where
  toJSON TermDefinition {..} =
    object
      [ "termNames" .= termNames,
        "bestTermName" .= bestTermName,
        "defnTermTag" .= defnTermTag,
        "termDefinition" .= termDefinition,
        "signature" .= signature,
        "termDocs" .= termDocs
      ]

deriving instance ToSchema TermDefinition

instance ToJSON DefinitionDisplayResults where
  toJSON DefinitionDisplayResults {..} =
    object
      [ "termDefinitions" .= termDefinitions,
        "typeDefinitions" .= typeDefinitions,
        "missingDefinitions" .= missingDefinitions
      ]

deriving instance ToSchema DefinitionDisplayResults

newtype Suffixify = Suffixify {suffixified :: Bool}
  deriving (Eq, Ord, Show, Generic)

data TermDefinition = TermDefinition
  { termNames :: [HashQualifiedName],
    bestTermName :: HashQualifiedName,
    defnTermTag :: TermTag,
    termDefinition :: DisplayObject Syntax.SyntaxText Syntax.SyntaxText,
    signature :: Syntax.SyntaxText,
    termDocs :: [(HashQualifiedName, UnisonHash, Doc)]
  }
  deriving (Eq, Show, Generic)

data TypeDefinition = TypeDefinition
  { typeNames :: [HashQualifiedName],
    bestTypeName :: HashQualifiedName,
    defnTypeTag :: TypeTag,
    typeDefinition :: DisplayObject Syntax.SyntaxText Syntax.SyntaxText,
    typeDocs :: [(HashQualifiedName, UnisonHash, Doc)]
  }
  deriving (Eq, Show, Generic)

data DefinitionDisplayResults = DefinitionDisplayResults
  { termDefinitions :: Map UnisonHash TermDefinition,
    typeDefinitions :: Map UnisonHash TypeDefinition,
    missingDefinitions :: [HashQualifiedName]
  }
  deriving (Eq, Show, Generic)

instance Semigroup DefinitionDisplayResults where
  DefinitionDisplayResults terms1 types1 missing1 <> DefinitionDisplayResults terms2 types2 missing2 =
    DefinitionDisplayResults (terms1 `Map.union` terms2) (types1 `Map.union` types2) (missing1 ++ missing2)

instance Monoid DefinitionDisplayResults where
  mempty = DefinitionDisplayResults mempty mempty mempty

data TermTag = Doc | Test | Plain | Constructor TypeTag
  deriving (Eq, Ord, Show, Generic)

data TypeTag = Ability | Data
  deriving (Eq, Ord, Show, Generic)

-- | A type for semantic diffing of definitions.
-- Includes special-cases for when the name in a definition has changed but the hash hasn't
-- (rename/alias), and when the hash has changed but the name hasn't (update propagation).
data SemanticSyntaxDiff
  = Old [Syntax.SyntaxSegment]
  | New [Syntax.SyntaxSegment]
  | Both [Syntax.SyntaxSegment]
  | --  (fromSegment, toSegment) (shared annotation)
    SegmentChange (String, String) (Maybe Syntax.Element)
  | -- (shared segment) (fromAnnotation, toAnnotation)
    AnnotationChange String (Maybe Syntax.Element, Maybe Syntax.Element)
  deriving (Eq, Show, Generic)

deriving instance ToSchema SemanticSyntaxDiff

instance ToJSON SemanticSyntaxDiff where
  toJSON = \case
    Old segments ->
      object
        [ "diffTag" .= ("old" :: Text),
          "elements" .= segments
        ]
    New segments ->
      object
        [ "diffTag" .= ("new" :: Text),
          "elements" .= segments
        ]
    Both segments ->
      object
        [ "diffTag" .= ("both" :: Text),
          "elements" .= segments
        ]
    SegmentChange (fromSegment, toSegment) annotation ->
      object
        [ "diffTag" .= ("segmentChange" :: Text),
          "fromSegment" .= fromSegment,
          "toSegment" .= toSegment,
          "annotation" .= annotation
        ]
    AnnotationChange segment (fromAnnotation, toAnnotation) ->
      object
        [ "diffTag" .= ("annotationChange" :: Text),
          "segment" .= segment,
          "fromAnnotation" .= fromAnnotation,
          "toAnnotation" .= toAnnotation
        ]

-- | A diff of the syntax of a term or type
--
-- It doesn't make sense to diff builtins with ABTs, so in that case we just provide the
-- undiffed syntax.
data DisplayObjectDiff
  = DisplayObjectDiff (DisplayObject [SemanticSyntaxDiff] [SemanticSyntaxDiff])
  | MismatchedDisplayObjects (DisplayObject Syntax.SyntaxText Syntax.SyntaxText) (DisplayObject Syntax.SyntaxText Syntax.SyntaxText)
  deriving stock (Show, Eq, Generic)

deriving instance ToSchema DisplayObjectDiff

data NamedTerm = NamedTerm
  { -- The name of the term, should be hash qualified if conflicted, otherwise name only.
    termName :: HQ'.HashQualified Name,
    termHash :: ShortHash,
    termType :: Maybe Syntax.SyntaxText,
    termTag :: TermTag
  }
  deriving (Eq, Generic, Show)

instance ToJSON NamedTerm where
  toJSON (NamedTerm n h typ tag) =
    Aeson.object
      [ "termName" .= HQ'.toTextWith Name.toText n,
        "termHash" .= h,
        "termType" .= typ,
        "termTag" .= tag
      ]

instance FromJSON NamedTerm where
  parseJSON = Aeson.withObject "NamedTerm" \obj -> do
    termName <- obj .: "termName"
    termHash <- obj .: "termHash"
    termType <- obj .: "termType"
    termTag <- obj .: "termTag"
    pure $ NamedTerm {..}

deriving instance ToSchema NamedTerm

data NamedType = NamedType
  { typeName :: HQ'.HashQualified Name,
    typeHash :: ShortHash,
    typeTag :: TypeTag
  }
  deriving (Eq, Generic, Show)

instance ToJSON NamedType where
  toJSON (NamedType n h tag) =
    Aeson.object
      [ "typeName" .= HQ'.toTextWith Name.toText n,
        "typeHash" .= h,
        "typeTag" .= tag
      ]

instance FromJSON NamedType where
  parseJSON = Aeson.withObject "NamedType" \obj -> do
    typeName <- obj .: "typeName"
    typeHash <- obj .: "typeHash"
    typeTag <- obj .: "typeTag"
    pure $ NamedType {..}

deriving instance ToSchema NamedType

instance ToJSON TermTag where
  toJSON = \case
    Doc -> "Doc"
    Test -> "Test"
    Plain -> "Plain"
    Constructor tt -> case tt of
      Ability -> "AbilityConstructor"
      Data -> "DataConstructor"

instance FromJSON TermTag where
  parseJSON Null = pure Plain
  parseJSON v =
    v
      & Aeson.withText "TermTag" \case
        "Doc" -> pure Doc
        "Test" -> pure Test
        "Plain" -> pure Plain
        "AbilityConstructor" -> pure $ Constructor Ability
        "DataConstructor" -> pure $ Constructor Data
        txt -> fail $ "Invalid TermTag" <> Text.unpack txt

deriving instance ToSchema TermTag

instance ToJSON TypeTag where
  toJSON = \case
    Ability -> "Ability"
    Data -> "Data"

instance FromJSON TypeTag where
  parseJSON = Aeson.withText "TypeTag" \case
    "Ability" -> pure Ability
    "Data" -> pure Data
    txt -> fail $ "Invalid TypeTag" <> Text.unpack txt

deriving instance ToSchema TypeTag

-- Helpers

munge :: Text -> LZ.ByteString
munge = Text.encodeUtf8 . Text.Lazy.fromStrict

mungeShow :: (Show s) => s -> LZ.ByteString
mungeShow = mungeString . show

mungeString :: String -> LZ.ByteString
mungeString = Text.encodeUtf8 . Text.Lazy.pack

defaultWidth :: Width
defaultWidth = 80

mayDefaultWidth :: Maybe Width -> Width
mayDefaultWidth = fromMaybe defaultWidth

setCacheControl :: v -> APIHeaders v
setCacheControl = addHeader @"Cache-Control" "public"

v2CausalBranchToUnisonHash :: V2Branch.CausalBranch m -> UnisonHash
v2CausalBranchToUnisonHash b =
  ("#" <>) . Hash.toBase32HexText . unCausalHash $ V2Causal.causalHash b

data TermDiffResponse = TermDiffResponse
  { project :: ProjectName,
    oldBranch :: ProjectBranchName,
    newBranch :: ProjectBranchName,
    oldTerm :: TermDefinition,
    newTerm :: TermDefinition,
    diff :: DisplayObjectDiff
  }
  deriving (Eq, Show, Generic)

deriving instance ToSchema TermDiffResponse

instance Docs.ToSample TermDiffResponse where
  toSamples _ = []

instance ToJSON TermDiffResponse where
  toJSON (TermDiffResponse {diff, project, oldBranch, newBranch, oldTerm, newTerm}) =
    case diff of
      DisplayObjectDiff dispDiff ->
        object
          [ "diff" .= dispDiff,
            "diffKind" .= ("diff" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldTerm" .= oldTerm,
            "newTerm" .= newTerm
          ]
      MismatchedDisplayObjects {} ->
        object
          [ "diffKind" .= ("mismatched" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldTerm" .= oldTerm,
            "newTerm" .= newTerm
          ]

data TypeDiffResponse = TypeDiffResponse
  { project :: ProjectName,
    oldBranch :: ProjectBranchName,
    newBranch :: ProjectBranchName,
    oldType :: TypeDefinition,
    newType :: TypeDefinition,
    diff :: DisplayObjectDiff
  }
  deriving (Eq, Show, Generic)

deriving instance ToSchema TypeDiffResponse

instance Docs.ToSample TypeDiffResponse where
  toSamples _ = []

instance ToJSON TypeDiffResponse where
  toJSON (TypeDiffResponse {diff, project, oldBranch, newBranch, oldType, newType}) =
    case diff of
      DisplayObjectDiff dispDiff ->
        object
          [ "diff" .= dispDiff,
            "diffKind" .= ("diff" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldType" .= oldType,
            "newType" .= newType
          ]
      MismatchedDisplayObjects {} ->
        object
          [ "diffKind" .= ("mismatched" :: Text),
            "project" .= project,
            "oldBranchRef" .= oldBranch,
            "newBranchRef" .= newBranch,
            "oldType" .= oldType,
            "newType" .= newType
          ]

-- | Servant utility for a query param that's required, providing a useful error message if it's missing.
type RequiredQueryParam = Servant.QueryParam' '[Servant.Required, Servant.Strict]
