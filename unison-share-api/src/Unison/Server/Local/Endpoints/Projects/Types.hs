module Unison.Server.Local.Endpoints.Projects.Types
  ( ProjectListing (..),
    ProjectBranchListing (..),
  )
where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.OpenApi
import GHC.Generics ()
import Servant.Docs
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName), ProjectName (UnsafeProjectName))
import Unison.Prelude
import Unison.Server.Orphans ()
import Unison.Sqlite (FromRow (..), field)

data ProjectListing = ProjectListing
  { projectName :: ProjectName,
    mostRecentActiveBranch :: Maybe ProjectBranchName
  }
  deriving stock (Show, Generic)

instance FromRow ProjectListing where
  fromRow = ProjectListing <$> field <*> field

instance ToSchema ProjectListing

instance ToJSON ProjectListing where
  toJSON (ProjectListing projectName mostRecentActiveBranch) =
    Aeson.object
      [ "projectName" Aeson..= projectName,
        "activeBranchRef" Aeson..= mostRecentActiveBranch
      ]

instance ToSample ProjectListing where
  toSamples _ =
    singleSample $ ProjectListing (UnsafeProjectName "my-project") Nothing

data ProjectBranchListing = ProjectBranchListing
  { branchName :: ProjectBranchName
  }
  deriving stock (Show, Generic)

instance ToSchema ProjectBranchListing

instance ToJSON ProjectBranchListing where
  toJSON ProjectBranchListing {branchName} =
    Aeson.object ["branchName" Aeson..= branchName]

instance ToSample ProjectBranchListing where
  toSamples _ =
    singleSample $ ProjectBranchListing (UnsafeProjectBranchName "my-branch")
