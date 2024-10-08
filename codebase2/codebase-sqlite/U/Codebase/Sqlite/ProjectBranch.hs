module U.Codebase.Sqlite.ProjectBranch
  ( ProjectBranch (..),
  )
where

import U.Codebase.Sqlite.DbId (ProjectBranchId, ProjectId)
import Unison.Core.Orphans.Sqlite ()
import Unison.Core.Project (ProjectBranchName)
import Unison.Prelude
import Unison.Sqlite (FromRow)

-- | A project branch.
data ProjectBranch = ProjectBranch
  { projectId :: !ProjectId,
    branchId :: !ProjectBranchId,
    name :: !ProjectBranchName,
    parentBranchId :: !(Maybe ProjectBranchId)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow)
