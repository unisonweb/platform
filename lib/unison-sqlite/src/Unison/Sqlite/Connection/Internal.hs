module Unison.Sqlite.Connection.Internal
  ( Connection (..),
  )
where

import Data.Map (Map)
import Data.Text (Text)
import Database.SQLite.Simple qualified as Sqlite
import UnliftIO.STM (TVar)

-- | A /non-thread safe/ connection to a SQLite database.
data Connection = Connection
  { name :: String,
    file :: FilePath,
    conn :: Sqlite.Connection,
    statementCache :: TVar (Map Text Sqlite.Statement)
  }

instance Show Connection where
  show (Connection name file _conn _statementCache) =
    "Connection { name = " ++ show name ++ ", file = " ++ show file ++ " }"
