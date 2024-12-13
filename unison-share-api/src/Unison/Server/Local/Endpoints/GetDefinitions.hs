{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Local.Endpoints.GetDefinitions where

import Servant
  ( QueryParam,
    QueryParams,
    (:>),
  )
import Servant.Docs
  ( DocQueryParam (..),
    ParamKind (..),
    ToParam (..),
    ToSample (..),
    noSamples,
  )
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.Codebase.ShortCausalHash
  ( ShortCausalHash,
  )
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Server.Backend qualified as Backend
import Unison.Server.Local.Definitions qualified as Local
import Unison.Server.Types
  ( APIGet,
    DefinitionDisplayResults,
    Suffixify (..),
    defaultWidth,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Pretty (Width)

type DefinitionsAPI =
  "getDefinition"
    :> QueryParam "relativeTo" Path.Path
    :> QueryParams "names" (HQ.HashQualified Name)
    :> QueryParam "renderWidth" Width
    :> QueryParam "suffixifyBindings" Suffixify
    :> APIGet DefinitionDisplayResults

instance ToParam (QueryParam "renderWidth" Width) where
  toParam _ =
    DocQueryParam
      "renderWidth"
      ["80", "100", "120"]
      ( "The preferred maximum line width (in characters) of the source code of "
          <> "definitions to be rendered. "
          <> "If left absent, the render width is assumed to be "
          <> show defaultWidth
          <> "."
      )
      Normal

instance ToParam (QueryParam "suffixifyBindings" Suffixify) where
  toParam _ =
    DocQueryParam
      "suffixifyBindings"
      ["True", "False"]
      ( "If True or absent, renders definitions using the shortest unambiguous "
          <> "suffix. If False, uses the fully qualified name. "
      )
      Normal

instance ToParam (QueryParam "relativeTo" Path.Path) where
  toParam _ =
    DocQueryParam
      "relativeTo"
      []
      ( "The namespace relative to which names will be resolved and displayed. "
          <> "If left absent, the root namespace will be used."
          <> "E.g. base.List"
      )
      Normal

instance ToParam (QueryParam "namespace" Path.Path) where
  toParam _ =
    DocQueryParam
      "namespace"
      []
      ( "The namespace required by the endpoint."
          <> "If left absent, the relativeTo namespace will be used."
          <> "E.g. base.List"
      )
      Normal

instance ToParam (QueryParams "names" (HQ.HashQualified Name)) where
  toParam _ =
    DocQueryParam
      "names"
      [".base.List", "foo.bar", "@abc123"]
      ("A fully qualified name, hash-qualified name, " <> "or hash.")
      List

instance ToSample DefinitionDisplayResults where
  toSamples _ = noSamples

serveDefinitions ::
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Either ShortCausalHash CausalHash ->
  Maybe Path.Path ->
  [HQ.HashQualified Name] ->
  Maybe Width ->
  Maybe Suffixify ->
  Backend.Backend IO DefinitionDisplayResults
serveDefinitions rt codebase root relativePath hqns width suff =
  do
    rootCausalHash <- Backend.hoistBackend (Codebase.runTransaction codebase) . Backend.normaliseRootCausalHash $ root
    hqns
      & foldMapM
        ( Local.prettyDefinitionsForHQName
            (fromMaybe mempty relativePath)
            rootCausalHash
            width
            (fromMaybe (Suffixify True) suff)
            rt
            codebase
        )
