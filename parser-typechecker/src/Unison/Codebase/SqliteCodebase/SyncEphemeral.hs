module Unison.Codebase.SqliteCodebase.SyncEphemeral where

import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.DbId (SchemaVersion)
import Unison.Hash (Hash)
import Unison.Prelude

data Dependencies = Dependencies
  { definitions :: Set Hash,
    branches :: Set Hash
  }

data Error
  = SrcWrongSchema SchemaVersion
  | DestWrongSchema SchemaVersion
  | DisappearingBranch CausalHash
  deriving stock (Show)
  deriving anyclass (Exception)
