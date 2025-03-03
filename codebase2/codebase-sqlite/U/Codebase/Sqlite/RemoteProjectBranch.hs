module U.Codebase.Sqlite.RemoteProjectBranch
  ( RemoteProjectBranch (..),
  )
where

import Network.URI (URI)
import Network.URI.Orphans.Sqlite ()
import U.Codebase.Sqlite.DbId (CausalHashId, RemoteProjectBranchId, RemoteProjectId)
import Unison.Core.Orphans.Sqlite ()
import Unison.Core.Project (ProjectBranchName)
import Unison.Prelude
import Unison.Sqlite (FromRow, ToRow)

data RemoteProjectBranch = RemoteProjectBranch
  { projectId :: RemoteProjectId,
    branchId :: RemoteProjectBranchId,
    host :: URI,
    name :: ProjectBranchName,
    -- Note that there's no guarantee that the causals for this hash have been downloaded/synced into the codebase.
    lastKnownCausalHash :: CausalHashId
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToRow, FromRow)
