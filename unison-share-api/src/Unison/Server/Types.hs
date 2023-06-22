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
import Data.OpenApi
  ( OpenApiType (..),
    ToParamSchema (..),
    ToSchema (..),
  )
import Data.OpenApi.Lens qualified as OpenApi
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text
import Servant.API
  ( Capture,
    FromHttpApiData (..),
    Get,
    Header,
    Headers,
    JSON,
    QueryParam,
    addHeader,
  )
import Servant.Docs (DocCapture (..), DocQueryParam (..), ParamKind (..), ToParam)
import Servant.Docs qualified as Docs
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.DisplayObject
  ( DisplayObject,
  )
import Unison.Hash qualified as Hash
import Unison.HashQualified qualified as HQ
import Unison.HashQualified' qualified as HQ'
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Server.Doc (Doc)
import Unison.Server.Orphans ()
import Unison.Server.Syntax (SyntaxText)
import Unison.ShortHash (ShortHash)
import Unison.Syntax.HashQualified qualified as HQ (fromText)
import Unison.Util.Pretty (Width (..))

type APIHeaders x =
  Headers
    '[ Header "Cache-Control" String
     ]
    x

type APIGet c = Get '[JSON] (APIHeaders c)

type HashQualifiedName = Text

type NamespaceFQN = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

-- | A hash qualified name, unlike HashQualified, the hash is required
data ExactName name ref = ExactName
  { name :: name,
    ref :: ref
  }
  deriving stock (Show, Eq, Ord)

instance ToParamSchema (ExactName Name ShortHash) where
  toParamSchema _ =
    mempty
      & OpenApi.type_ ?~ OpenApiString
      & OpenApi.example ?~ Aeson.String "base.List"

instance ToParam (QueryParam "exact-name" (ExactName Name ShortHash)) where
  toParam _ =
    DocQueryParam
      "exact-name"
      []
      "The fully qualified name of a namespace with a hash, denoted by a '@'. E.g. base.List.map@abc"
      Normal

instance Docs.ToCapture (Capture "fqn" (ExactName Name ShortHash)) where
  toCapture _ =
    DocCapture
      "fqn"
      "The fully qualified name of a namespace with a hash, denoted by a '@'. E.g. base.List.map@abc"

exactToHQ :: ExactName name ShortHash -> HQ.HashQualified name
exactToHQ (ExactName {name, ref}) = HQ.HashQualified name ref

exactToHQ' :: ExactName name ShortHash -> HQ'.HashQualified name
exactToHQ' (ExactName {name, ref}) = HQ'.HashQualified name ref

instance Bifunctor ExactName where
  bimap l r (ExactName a b) = ExactName (l a) (r b)

instance Bifoldable ExactName where
  bifoldMap l r (ExactName a b) = l a <> r b

instance Bitraversable ExactName where
  bitraverse l r (ExactName a b) = ExactName <$> (l a) <*> (r b)

instance FromHttpApiData (ExactName Name ShortHash) where
  parseQueryParam txt =
    -- # is special in URLs, so we use @ for hash qualification instead;
    -- e.g. ".base.List.map@abc"
    -- e.g. ".base.Nat@@Nat"
    case HQ.fromText (Text.replace "@" "#" txt) of
      Nothing -> Left "Invalid absolute name with Hash"
      Just hq' -> case hq' of
        HQ.NameOnly _ -> Left "A name and hash are required, but only a name was provided"
        HQ.HashOnly _ -> Left "A name and hash are required, but only a hash was provided"
        HQ.HashQualified name ref -> Right $ ExactName {name, ref}

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
    termDefinition :: DisplayObject SyntaxText SyntaxText,
    signature :: SyntaxText,
    termDocs :: [(HashQualifiedName, UnisonHash, Doc)]
  }
  deriving (Eq, Show, Generic)

data TypeDefinition = TypeDefinition
  { typeNames :: [HashQualifiedName],
    bestTypeName :: HashQualifiedName,
    defnTypeTag :: TypeTag,
    typeDefinition :: DisplayObject SyntaxText SyntaxText,
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

data UnisonRef
  = TypeRef UnisonHash
  | TermRef UnisonHash
  deriving (Eq, Ord, Show, Generic)

unisonRefToText :: UnisonRef -> Text
unisonRefToText = \case
  TypeRef r -> r
  TermRef r -> r

data NamedTerm = NamedTerm
  { -- The name of the term, should be hash qualified if conflicted, otherwise name only.
    termName :: HQ'.HashQualified NameSegment,
    termHash :: ShortHash,
    termType :: Maybe SyntaxText,
    termTag :: TermTag
  }
  deriving (Eq, Generic, Show)

instance ToJSON NamedTerm where
  toJSON (NamedTerm n h typ tag) =
    Aeson.object
      [ "termName" .= HQ'.toTextWith NameSegment.toText n,
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
  { typeName :: HQ'.HashQualified NameSegment,
    typeHash :: ShortHash,
    typeTag :: TypeTag
  }
  deriving (Eq, Generic, Show)

instance ToJSON NamedType where
  toJSON (NamedType n h tag) =
    Aeson.object
      [ "typeName" .= HQ'.toTextWith NameSegment.toText n,
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

discard :: (Applicative m) => a -> m ()
discard = const $ pure ()

mayDefaultWidth :: Maybe Width -> Width
mayDefaultWidth = fromMaybe defaultWidth

setCacheControl :: v -> APIHeaders v
setCacheControl = addHeader @"Cache-Control" "public"

branchToUnisonHash :: Branch.Branch m -> UnisonHash
branchToUnisonHash b =
  ("#" <>) . Hash.toBase32HexText . unCausalHash $ Branch.headHash b

v2CausalBranchToUnisonHash :: V2Branch.CausalBranch m -> UnisonHash
v2CausalBranchToUnisonHash b =
  ("#" <>) . Hash.toBase32HexText . unCausalHash $ V2Causal.causalHash b
