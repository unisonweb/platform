{-# LANGUAGE DeriveAnyClass #-}

-- | Open codebase error type.
module Unison.Codebase.Init.OpenCodebaseError
  ( OpenCodebaseError (..),
  )
where

import U.Codebase.Sqlite.DbId (SchemaVersion)
import Unison.Prelude

-- | An error that can occur when attempting to open a codebase.
data OpenCodebaseError
  = -- | The codebase doesn't exist.
    OpenCodebaseDoesntExist
  | -- | The codebase exists, but its schema version is unknown to this application.
    OpenCodebaseUnknownSchemaVersion SchemaVersion
  | OpenCodebaseFileLockFailed FilePath {- path to the lockfile -}
  | -- | The codebase exists, but requires a migration before it can be used.
    OpenCodebaseRequiresMigration
      -- current version
      SchemaVersion
      -- required version
      SchemaVersion
  deriving stock (Show, Eq)
  deriving anyclass (Exception)
