{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Server.Local.Endpoints.Projects
  ( projectListingEndpoint,
    projectBranchListingEndpoint,
    ListProjectsEndpoint,
    ListProjectBranchesEndpoint,
  )
where

import Data.OpenApi (ToParamSchema)
import GHC.Generics ()
import Servant
import Servant.Docs
import Servant.Docs qualified as Docs
import U.Codebase.Sqlite.Project qualified as SqliteProject
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Project (ProjectName)
import Unison.Server.Backend (Backend)
import Unison.Server.Local.Endpoints.Projects.Queries qualified as PG
import Unison.Server.Local.Endpoints.Projects.Queries qualified as PQ
import Unison.Server.Local.Endpoints.Projects.Types
import Unison.Symbol (Symbol)

type ListProjectsEndpoint =
  QueryParam "query" Query
    :> Get '[JSON] [ProjectListing]

type ListProjectBranchesEndpoint =
  QueryParam "query" Query
    :> Get '[JSON] [ProjectBranchListing]

newtype PrefixFilter = PrefixFilter
  { prefix :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (FromHttpApiData)

instance ToParamSchema PrefixFilter

instance ToParam (QueryParam "prefix" PrefixFilter) where
  toParam _ =
    DocQueryParam
      "prefix"
      ["my-proj"]
      "Filter by project or branch prefix"
      Normal

instance Docs.ToSample PrefixFilter where
  toSamples _ =
    singleSample $ PrefixFilter "my-proj"

newtype Query = Query
  { getQuery :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (FromHttpApiData)

instance ToParamSchema Query

instance ToParam (QueryParam "query" Query) where
  toParam _ =
    DocQueryParam
      "query"
      ["my-proj"]
      "Filter for results containing the given text."
      Normal

instance Docs.ToSample Query where
  toSamples _ =
    singleSample $ Query "my-proj"

projectListingEndpoint ::
  Codebase IO Symbol Ann ->
  -- Infix Query
  Maybe Query ->
  Backend IO [ProjectListing]
projectListingEndpoint codebase mayQuery = liftIO . Codebase.runTransaction codebase $ do
  PQ.listProjects (getQuery <$> mayQuery)

projectBranchListingEndpoint ::
  Codebase IO Symbol Ann ->
  ProjectName ->
  Maybe Query ->
  Backend IO [ProjectBranchListing]
projectBranchListingEndpoint codebase projectName mayQuery = liftIO . Codebase.runTransaction codebase . fmap fold . runMaybeT $ do
  SqliteProject.Project {projectId} <- MaybeT $ Q.loadProjectByName projectName
  lift (PG.listProjectBranches projectId (getQuery <$> mayQuery))
