{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Local.Endpoints.FuzzyFind where

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
    noSamples,
  )
import Servant.OpenApi ()
import Text.FuzzyFind qualified as FZF
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPE
import Unison.Server.Backend qualified as Backend
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    ExactName (..),
    HashQualifiedName,
    NamedTerm,
    NamedType,
    UnisonName,
    mayDefaultWidth,
  )
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Util.Pretty (Width)

type FuzzyFindAPI =
  "find"
    :> QueryParam "relativeTo" Path.Path
    :> QueryParam "limit" Int
    :> QueryParam "renderWidth" Width
    :> QueryParam "query" String
    :> APIGet [(FZF.Alignment, FoundResult)]

instance ToSample FZF.Alignment where
  toSamples _ = noSamples

instance ToParam (QueryParam "limit" Int) where
  toParam _ =
    DocQueryParam
      "limit"
      ["1", "10", "20"]
      "The maximum number of results to return. Defaults to 10."
      Normal

instance ToParam (QueryParam "query" String) where
  toParam _ =
    DocQueryParam
      "query"
      ["foo", "ff", "td nr"]
      "Space-separated subsequences to find in the name of a type or term."
      Normal

instance ToJSON FZF.Alignment where
  toJSON (FZF.Alignment {score, result}) =
    object ["score" .= score, "result" .= result]

instance ToJSON FZF.Result where
  toJSON (FZF.Result {segments}) = object ["segments" .= toJSON segments]

instance ToJSON FZF.ResultSegment where
  toJSON = \case
    FZF.Gap s -> object ["tag" .= String "Gap", "contents" .= s]
    FZF.Match s -> object ["tag" .= String "Match", "contents" .= s]

deriving instance ToSchema FZF.Alignment

deriving anyclass instance ToSchema FZF.Result

deriving instance ToSchema FZF.ResultSegment

data FoundTerm = FoundTerm
  { bestFoundTermName :: HashQualifiedName,
    namedTerm :: NamedTerm
  }
  deriving (Generic, Show)

data FoundType = FoundType
  { bestFoundTypeName :: HashQualifiedName,
    typeDef :: DisplayObject SyntaxText SyntaxText,
    namedType :: NamedType
  }
  deriving (Generic, Show)

instance ToJSON FoundType where
  toJSON (FoundType {bestFoundTypeName, typeDef, namedType}) =
    object
      [ "bestFoundTypeName" .= bestFoundTypeName,
        "typeDef" .= typeDef,
        "namedType" .= namedType
      ]

deriving instance ToSchema FoundType

instance ToJSON FoundTerm where
  toJSON (FoundTerm {bestFoundTermName, namedTerm}) =
    object
      [ "bestFoundTermName" .= bestFoundTermName,
        "namedTerm" .= namedTerm
      ]

deriving instance ToSchema FoundTerm

data FoundResult
  = FoundTermResult FoundTerm
  | FoundTypeResult FoundType
  deriving (Generic, Show)

instance ToJSON FoundResult where
  toJSON = \case
    FoundTermResult ft -> object ["tag" .= String "FoundTermResult", "contents" .= ft]
    FoundTypeResult ft -> object ["tag" .= String "FoundTypeResult", "contents" .= ft]

deriving instance ToSchema FoundResult

instance ToSample FoundResult where
  toSamples _ = noSamples

serveFuzzyFind ::
  forall m.
  (MonadIO m) =>
  Codebase m Symbol Ann ->
  Either SCH.ShortCausalHash CausalHash ->
  Maybe Path.Path ->
  Maybe Int ->
  Maybe Width ->
  Maybe String ->
  Backend.Backend m [(FZF.Alignment, FoundResult)]
serveFuzzyFind codebase root relativeTo limit typeWidth query = do
  let path = fromMaybe mempty relativeTo
  rootCausal <-
    Backend.hoistBackend (Codebase.runTransaction codebase) do
      Backend.normaliseRootCausalHash root
  (localNamesOnly, ppe) <- Backend.namesAtPathFromRootBranchHash codebase rootCausal path
  let alignments ::
        ( [ ( FZF.Alignment,
              UnisonName,
              [Backend.FoundRef]
            )
          ]
        )
      alignments =
        take (fromMaybe 10 limit) $ Backend.fuzzyFind localNamesOnly (fromMaybe "" query)
  lift (join <$> traverse (loadEntry (PPE.suffixifiedPPE ppe)) alignments)
  where
    loadEntry ppe (a, n, refs) = do
      for refs \case
        Backend.FoundTermRef r ->
          ( \te ->
              ( a,
                FoundTermResult
                  . FoundTerm
                    (Backend.bestNameForTerm @Symbol ppe (mayDefaultWidth typeWidth) r)
                  $ Backend.termEntryToNamedTerm ppe typeWidth te
              )
          )
            <$> Backend.termListEntry codebase (ExactName (Name.unsafeParseText n) (Cv.referent1to2 r))
        Backend.FoundTypeRef r ->
          Codebase.runTransaction codebase do
            te <- Backend.typeListEntry codebase (ExactName (Name.unsafeParseText n) r)
            let namedType = Backend.typeEntryToNamedType te
            let typeName = Backend.bestNameForType @Symbol ppe (mayDefaultWidth typeWidth) r
            typeHeader <- Backend.typeDeclHeader codebase ppe r
            let ft = FoundType typeName typeHeader namedType
            pure (a, FoundTypeResult ft)
