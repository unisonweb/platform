module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema4To5 (migrateSchema4To5) where

import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Sqlite qualified as Sqlite

-- | The 4 to 5 migration adds initial support for out-of-order sync i.e. Unison Share
migrateSchema4To5 :: Sqlite.Transaction ()
migrateSchema4To5 = do
  Q.expectSchemaVersion 4
  Q.addTempEntityTables
  Q.setSchemaVersion 5
