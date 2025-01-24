module U.Codebase.Sqlite.Causal
  ( DbCausal,
    GDbCausal (..),
    SyncCausalFormat,
    SyncCausalFormat' (..),
  )
where

import Data.Vector (Vector)
import U.Codebase.Sqlite.DbId (BranchHashId, CausalHashId)
import Unison.Prelude

data GDbCausal causalHash valueHash = DbCausal
  { selfHash :: causalHash,
    valueHash :: valueHash,
    parents :: Set causalHash
  }

type DbCausal = GDbCausal CausalHashId BranchHashId

data SyncCausalFormat' causalHash valueHash = SyncCausalFormat
  { valueHash :: valueHash,
    parents :: Vector causalHash
  }
  deriving stock (Eq, Show)

type SyncCausalFormat = SyncCausalFormat' CausalHashId BranchHashId
