{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unison.Server.Types where

-- Types common to endpoints --
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Map as Map
import Data.OpenApi
  ( OpenApiType (..),
    ToParamSchema (..),
    ToSchema (..),
  )
import qualified Data.OpenApi.Lens as OpenApi
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text
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
import qualified Servant.Docs as Docs
import qualified U.Codebase.Branch as V2Branch
import qualified U.Codebase.Causal as V2Causal
import qualified U.Codebase.HashTags as V2
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Editor.DisplayObject
  ( DisplayObject,
  )
import qualified Unison.Hash as Hash
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Server.Doc (Doc)
import Unison.Server.Orphans ()
import Unison.Server.Syntax (SyntaxText)
import Unison.ShortHash (ShortHash)
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
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TypeDefinition

instance ToJSON TermDefinition where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TermDefinition

instance ToJSON DefinitionDisplayResults where
  toEncoding = genericToEncoding defaultOptions

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

data FoundEntry
  = FoundTerm NamedTerm
  | FoundType NamedType
  deriving (Eq, Show, Generic)

instance ToJSON FoundEntry where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema FoundEntry

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
      [ "termName" .= n,
        "termHash" .= h,
        "termType" .= typ,
        "termTag" .= tag
      ]

deriving instance ToSchema NamedTerm

data NamedType = NamedType
  { typeName :: HQ'.HashQualified NameSegment,
    typeHash :: ShortHash,
    typeTag :: TypeTag
  }
  deriving (Eq, Generic, Show)

instance ToJSON NamedType where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema NamedType

instance ToJSON TermTag where
  toJSON = \case
    Doc -> "doc"
    Test -> "test"
    Plain -> "plain"
    Constructor tt -> case tt of
      Ability -> "ability-constructor"
      Data -> "data-constructor"

deriving instance ToSchema TermTag

instance ToJSON TypeTag where
  toEncoding = genericToEncoding defaultOptions

deriving instance ToSchema TypeTag

-- Helpers

munge :: Text -> LZ.ByteString
munge = Text.encodeUtf8 . Text.Lazy.fromStrict

mungeShow :: Show s => s -> LZ.ByteString
mungeShow = mungeString . show

mungeString :: String -> LZ.ByteString
mungeString = Text.encodeUtf8 . Text.Lazy.pack

defaultWidth :: Width
defaultWidth = 80

discard :: Applicative m => a -> m ()
discard = const $ pure ()

mayDefaultWidth :: Maybe Width -> Width
mayDefaultWidth = fromMaybe defaultWidth

setCacheControl :: v -> APIHeaders v
setCacheControl = addHeader @"Cache-Control" "public"

branchToUnisonHash :: Branch.Branch m -> UnisonHash
branchToUnisonHash b =
  ("#" <>) . Hash.base32Hex . Causal.unCausalHash $ Branch.headHash b

v2CausalBranchToUnisonHash :: V2Branch.CausalBranch m -> UnisonHash
v2CausalBranchToUnisonHash b =
  ("#" <>) . Hash.base32Hex . V2.unCausalHash $ V2Causal.causalHash b
