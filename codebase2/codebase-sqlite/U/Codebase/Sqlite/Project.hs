module U.Codebase.Sqlite.Project
  ( Project (..),
  )
where

import U.Codebase.Sqlite.DbId (ProjectId)
import Unison.Core.Orphans.Sqlite ()
import Unison.Core.Project (ProjectName)
import Unison.Prelude
import Unison.Sqlite (FromRow)

-- | A project.
data Project = Project
  { projectId :: !ProjectId,
    name :: !ProjectName
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromRow)
