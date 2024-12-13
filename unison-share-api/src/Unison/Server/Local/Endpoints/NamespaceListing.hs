{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Local.Endpoints.NamespaceListing (serve, NamespaceListingAPI, NamespaceListing (..), NamespaceObject (..), NamedNamespace (..), NamedPatch (..), KindExpression (..)) where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Servant
  ( QueryParam,
    (:>),
  )
import Servant.Docs
  ( DocQueryParam (..),
    ParamKind (Normal),
    ToParam (..),
    ToSample (..),
  )
import Servant.OpenApi ()
import U.Codebase.Branch (NamespaceStats (..))
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (CausalHash (..))
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Hash qualified as Hash
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Server.Backend (Backend)
import Unison.Server.Backend qualified as Backend
import Unison.Server.Types
  ( APIGet,
    HashQualifiedName,
    NamedTerm (..),
    NamedType (..),
    UnisonHash,
    UnisonName,
    v2CausalBranchToUnisonHash,
  )
import Unison.Symbol (Symbol)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Pretty (Width)
import Unison.Var (Var)

type NamespaceListingAPI =
  "list"
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "namespace" Path.Path
    :> APIGet NamespaceListing

instance ToParam (QueryParam "namespace" Text) where
  toParam _ =
    DocQueryParam
      "namespace"
      [".", ".base.List", "foo.bar"]
      "The fully qualified name of a namespace. The leading `.` is optional."
      Normal

instance ToSample NamespaceListing where
  toSamples _ =
    [ ( "When no value is provided for `namespace`, the root namespace `.` is "
          <> "listed by default",
        NamespaceListing
          "."
          "#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"
          [Subnamespace $ NamedNamespace "base" "#19d1o9hi5n642t8jttg" 237]
      )
    ]

data NamespaceListing = NamespaceListing
  { namespaceListingFQN :: UnisonName,
    namespaceListingHash :: UnisonHash,
    namespaceListingChildren :: [NamespaceObject]
  }
  deriving stock (Generic, Show)

instance ToJSON NamespaceListing where
  toJSON NamespaceListing {..} =
    object
      [ "namespaceListingFQN" .= namespaceListingFQN,
        "namespaceListingHash" .= namespaceListingHash,
        "namespaceListingChildren" .= namespaceListingChildren
      ]

instance FromJSON NamespaceListing where
  parseJSON = withObject "NamespaceListing" $ \o -> do
    namespaceListingFQN <- o .: "namespaceListingFQN"
    namespaceListingHash <- o .: "namespaceListingHash"
    namespaceListingChildren <- o .: "namespaceListingChildren"
    pure NamespaceListing {..}

deriving instance ToSchema NamespaceListing

data NamespaceObject
  = Subnamespace NamedNamespace
  | TermObject NamedTerm
  | TypeObject NamedType
  | PatchObject NamedPatch
  deriving (Generic, Show)

instance ToJSON NamespaceObject where
  toJSON = \case
    Subnamespace ns -> object ["tag" .= String "Subnamespace", "contents" .= ns]
    TermObject t -> object ["tag" .= String "TermObject", "contents" .= t]
    TypeObject t -> object ["tag" .= String "TypeObject", "contents" .= t]
    PatchObject p -> object ["tag" .= String "PatchObject", "contents" .= p]

instance FromJSON NamespaceObject where
  parseJSON = withObject "NamespaceObject" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "Subnamespace" -> Subnamespace <$> o .: "contents"
      "TermObject" -> TermObject <$> o .: "contents"
      "TypeObject" -> TypeObject <$> o .: "contents"
      "PatchObject" -> PatchObject <$> o .: "contents"
      _ -> fail "Invalid NamespaceObject"

deriving instance ToSchema NamespaceObject

data NamedNamespace = NamedNamespace
  { namespaceName :: UnisonName,
    namespaceHash :: UnisonHash,
    namespaceSize :: Int
  }
  deriving (Generic, Show)

instance ToJSON NamedNamespace where
  toJSON NamedNamespace {..} =
    object
      [ "namespaceName" .= namespaceName,
        "namespaceHash" .= namespaceHash,
        "namespaceSize" .= namespaceSize
      ]

instance FromJSON NamedNamespace where
  parseJSON = withObject "NamedNamespace" $ \o -> do
    namespaceName <- o .: "namespaceName"
    namespaceHash <- o .: "namespaceHash"
    namespaceSize <- o .: "namespaceSize"
    pure NamedNamespace {..}

deriving instance ToSchema NamedNamespace

newtype NamedPatch = NamedPatch {patchName :: HashQualifiedName}
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

instance ToJSON NamedPatch where
  toJSON NamedPatch {..} =
    object
      [ "patchName" .= patchName
      ]

instance FromJSON NamedPatch where
  parseJSON = withObject "NamedPatch" $ \o -> do
    patchName <- o .: "patchName"
    pure NamedPatch {..}

newtype KindExpression = KindExpression {kindExpressionText :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

instance ToJSON KindExpression where
  toJSON KindExpression {..} =
    object
      [ "kindExpressionText" .= kindExpressionText
      ]

backendListEntryToNamespaceObject ::
  (Var v) =>
  PPE.PrettyPrintEnv ->
  Maybe Width ->
  Backend.ShallowListEntry v a ->
  NamespaceObject
backendListEntryToNamespaceObject ppe typeWidth = \case
  Backend.ShallowTermEntry te ->
    TermObject $ Backend.termEntryToNamedTerm ppe typeWidth te
  Backend.ShallowTypeEntry te -> TypeObject $ Backend.typeEntryToNamedType te
  Backend.ShallowBranchEntry name hash (NamespaceStats {numContainedTerms, numContainedTypes, numContainedPatches}) ->
    Subnamespace $
      NamedNamespace
        { namespaceName = NameSegment.toEscapedText name,
          namespaceHash = "#" <> Hash.toBase32HexText (unCausalHash hash),
          namespaceSize = numContainedTerms + numContainedTypes + numContainedPatches
        }
  Backend.ShallowPatchEntry name ->
    PatchObject . NamedPatch $ NameSegment.toEscapedText name

serve ::
  Codebase IO Symbol Ann ->
  Either ShortCausalHash CausalHash ->
  Maybe Path.Path ->
  Maybe Path.Path ->
  Backend.Backend IO NamespaceListing
serve codebase root mayRelativeTo mayNamespaceName = do
  rootCausal <- Backend.hoistBackend (Codebase.runTransaction codebase) $ Backend.normaliseRootCausalHash root

  -- Relative and Listing Path resolution
  --
  -- The full listing path is a combination of the relativeToPath (prefix) and the namespace path
  --
  -- For example:
  --            "base.List"    <>    "Nonempty"
  --                ↑                    ↑
  --         relativeToPath        namespacePath
  --
  -- resulting in "base.List.map" which we can use via the root branch (usually the codebase hash)
  -- to look up the namespace listing and present shallow name, so that the
  -- definition "base.List.Nonempty.map", simple has the name "map"
  --
  let relativeToPath = fromMaybe mempty mayRelativeTo
  let namespacePath = fromMaybe mempty mayNamespaceName
  let path = relativeToPath <> namespacePath
  (listingCausal, listingBranch) <-
    (lift . Codebase.runTransaction codebase) do
      listingCausal <- Codebase.getShallowCausalAtPath path rootCausal
      listingBranch <- V2Causal.value listingCausal
      pure (listingCausal, listingBranch)
  -- TODO: Currently the ppe is just used to render the types returned from the namespace
  -- listing, which are currently unused because we don't show types in the side-bar.
  -- If we ever show types on hover we need to build and use a proper PPE here, but it's not
  -- shallowPPE <- liftIO $ Backend.shallowPPE codebase listingBranch
  let shallowPPE = PPE.empty
  let listingFQN = Path.toText path
  let listingHash = v2CausalBranchToUnisonHash listingCausal
  listingEntries <- lift (Backend.lsBranch codebase listingBranch)
  makeNamespaceListing shallowPPE listingFQN listingHash listingEntries

makeNamespaceListing ::
  ( PPE.PrettyPrintEnv ->
    UnisonName ->
    UnisonHash ->
    [Backend.ShallowListEntry Symbol a] ->
    Backend IO NamespaceListing
  )
makeNamespaceListing ppe fqn hash entries =
  pure . NamespaceListing fqn hash $
    fmap
      (backendListEntryToNamespaceObject ppe Nothing)
      entries
