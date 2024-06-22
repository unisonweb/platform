{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Unison.Sqlite.Connection
  ( -- * Connection management
    Connection (..),
    withConnection,

    -- * Executing queries

    -- ** Without results
    execute,
    executeStatements,

    -- ** With results
    queryStreamRow,
    queryStreamCol,
    queryListRow,
    queryListCol,
    queryMaybeRow,
    queryMaybeCol,
    queryOneRow,
    queryOneCol,

    -- *** With checks
    queryListRowCheck,
    queryListColCheck,
    queryMaybeRowCheck,
    queryMaybeColCheck,
    queryOneRowCheck,
    queryOneColCheck,

    -- * Rows modified
    rowsModified,

    -- * Vacuum (into)
    vacuum,
    vacuumInto,

    -- * Low-level operations

    -- ** Transaction
    begin,
    beginImmediate,
    commit,
    rollback,

    -- ** Savepoint
    withSavepoint,
    withSavepointIO,
    savepoint,
    rollbackTo,
    release,

    -- * Exceptions
    ExpectedAtMostOneRowException (..),
    ExpectedExactlyOneRowException (..),
  )
where

import Database.SQLite.Simple qualified as Sqlite
import Database.SQLite.Simple.FromField qualified as Sqlite
import Database.SQLite3 qualified as Direct.Sqlite
import Debug.Pretty.Simple (pTraceShowM)
import Debug.RecoverRTTI (anythingToString)
import GHC.Stack (currentCallStack)
import System.Environment qualified as Env
import Unison.Debug qualified as Debug
import Unison.Prelude
import Unison.Sqlite.Connection.Internal (Connection (..))
import Unison.Sqlite.Exception
import Unison.Sqlite.Sql (Sql (..))
import Unison.Sqlite.Sql qualified as Sql
import UnliftIO.Exception

-- | Perform an action with a connection to a SQLite database.
--
-- Note: the connection is created with @PRAGMA foreign_keys = ON@ automatically, to work around the fact that SQLite
-- does not automatically enforce foreign key integrity, because it elected to maintain backwards compatibility with
-- code that was written before the foreign key integrity feature was implemented.
withConnection ::
  (MonadUnliftIO m) =>
  -- | Connection name, for debugging.
  String ->
  -- | Path to SQLite database file.
  FilePath ->
  (Connection -> m a) ->
  m a
withConnection name file =
  bracket (liftIO (openConnection name file)) (liftIO . closeConnection)

-- Open a connection to a SQLite database.
openConnection ::
  -- Connection name, for debugging.
  String ->
  -- Path to SQLite database file.
  FilePath ->
  IO Connection
openConnection name file = do
  sqliteURI <-
    Env.lookupEnv "UNISON_READONLY" <&> \case
      Nothing -> file
      Just "" -> file
      _ -> "file:" <> file <> "?mode=ro"
  conn0 <- Sqlite.open sqliteURI `catch` rethrowAsSqliteConnectException name file
  let conn = Connection {conn = conn0, file, name}
  execute conn [Sql.sql| PRAGMA foreign_keys = ON |]
  execute conn [Sql.sql| PRAGMA busy_timeout = 60000 |]
  pure conn

-- Close a connection opened with 'openConnection'.
closeConnection :: Connection -> IO ()
closeConnection (Connection _ _ conn) =
  -- FIXME if this throws an exception, it won't be under `SomeSqliteException`
  -- Possible fixes:
  --   1. Add close exception to the hierarchy, e.g. `SqliteCloseException`
  --   2. Always ignore exceptions thrown by `close` (Mitchell prefers this one)
  Sqlite.close conn

-- An internal type, for making prettier debug logs

data Query = Query
  { sql :: Text,
    params :: [Sqlite.SQLData],
    result :: Maybe String,
    callStack :: [String]
  }

instance Show Query where
  show Query {sql, params, result, callStack} =
    concat
      [ "Query { sql = ",
        show sql,
        if null params then "" else ", params = " ++ show params,
        maybe "" (\r -> ", results = " ++ show r) result,
        if null callStack then "" else ", callStack = " ++ show callStack,
        " }"
      ]

logQuery :: Sql -> Maybe a -> IO ()
logQuery (Sql sql params) result =
  Debug.whenDebug Debug.Sqlite do
    callStack <- currentCallStack
    pTraceShowM
      Query
        { sql,
          params,
          result = anythingToString <$> result,
          callStack
        }

-- Without results

execute :: Connection -> Sql -> IO ()
execute conn@(Connection _ _ conn0) sql@(Sql s params) = do
  logQuery sql Nothing
  doExecute `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteQueryException
      SqliteQueryExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
          sql
        }
  where
    doExecute :: IO ()
    doExecute =
      Sqlite.withStatement conn0 (coerce s) \(Sqlite.Statement statement) -> do
        bindParameters statement params
        void (Direct.Sqlite.step statement)

-- | Execute one or more semicolon-delimited statements.
--
-- This function does not support parameters, and is mostly useful for executing DDL and migrations.
executeStatements :: Connection -> Text -> IO ()
executeStatements conn@(Connection _ _ connection) sql = do
  logQuery (Sql sql []) Nothing
  Direct.Sqlite.exec (Sqlite.connectionHandle connection) sql `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteQueryException
      SqliteQueryExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
          sql = Sql sql []
        }

-- With results, without checks

queryStreamRow :: Sqlite.FromRow a => Connection -> Sql -> (IO (Maybe a) -> IO r) -> IO r
queryStreamRow conn@(Connection _ _ conn0) sql@(Sql s params) callback =
  run `catch` \(exception :: Sqlite.SQLError) ->
    throwSqliteQueryException
      SqliteQueryExceptionInfo
        { connection = conn,
          exception = SomeSqliteExceptionReason exception,
          sql
        }
  where
    run =
      bracket (Sqlite.openStatement conn0 (coerce s)) Sqlite.closeStatement \statement -> do
        Sqlite.bind statement params
        callback (Sqlite.nextRow statement)

queryStreamCol ::
  forall a r.
  (Sqlite.FromField a) =>
  Connection ->
  Sql ->
  (IO (Maybe a) -> IO r) ->
  IO r
queryStreamCol =
  coerce
    @(Connection -> Sql -> (IO (Maybe (Sqlite.Only a)) -> IO r) -> IO r)
    @(Connection -> Sql -> (IO (Maybe a) -> IO r) -> IO r)
    queryStreamRow

queryListRow :: forall a. (Sqlite.FromRow a) => Connection -> Sql -> IO [a]
queryListRow conn@(Connection _ _ conn0) sql@(Sql s params) = do
  result <-
    doQuery
      `catch` \(exception :: Sqlite.SQLError) ->
        throwSqliteQueryException
          SqliteQueryExceptionInfo
            { connection = conn,
              exception = SomeSqliteExceptionReason exception,
              sql
            }
  logQuery sql (Just result)
  pure result
  where
    doQuery :: IO [a]
    doQuery =
      Sqlite.withStatement conn0 (coerce s) \statement -> do
        bindParameters (coerce statement) params
        let loop :: [a] -> IO [a]
            loop rows =
              Sqlite.nextRow statement >>= \case
                Nothing -> pure (reverse rows)
                Just row -> loop (row : rows)
        loop []

queryListCol :: forall a. (Sqlite.FromField a) => Connection -> Sql -> IO [a]
queryListCol =
  coerce @(Connection -> Sql -> IO [Sqlite.Only a]) @(Connection -> Sql -> IO [a]) queryListRow

queryMaybeRow :: (Sqlite.FromRow a) => Connection -> Sql -> IO (Maybe a)
queryMaybeRow conn s =
  queryListRowCheck conn s \case
    [] -> Right Nothing
    [x] -> Right (Just x)
    xs -> Left (ExpectedAtMostOneRowException (anythingToString xs))

queryMaybeCol :: forall a. (Sqlite.FromField a) => Connection -> Sql -> IO (Maybe a)
queryMaybeCol conn s =
  coerce @(IO (Maybe (Sqlite.Only a))) @(IO (Maybe a)) (queryMaybeRow conn s)

queryOneRow :: (Sqlite.FromRow a) => Connection -> Sql -> IO a
queryOneRow conn s =
  queryListRowCheck conn s \case
    [x] -> Right x
    xs -> Left (ExpectedExactlyOneRowException (anythingToString xs))

queryOneCol :: forall a. (Sqlite.FromField a) => Connection -> Sql -> IO a
queryOneCol conn s = do
  coerce @(IO (Sqlite.Only a)) @(IO a) (queryOneRow conn s)

-- With results, with checks

queryListRowCheck ::
  (Sqlite.FromRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  ([a] -> Either e r) ->
  IO r
queryListRowCheck conn s check =
  gqueryListCheck conn s (mapLeft SomeSqliteExceptionReason . check)

gqueryListCheck ::
  (Sqlite.FromRow a) =>
  Connection ->
  Sql ->
  ([a] -> Either SomeSqliteExceptionReason r) ->
  IO r
gqueryListCheck conn sql check = do
  xs <- queryListRow conn sql
  case check xs of
    Left exception ->
      throwSqliteQueryException
        SqliteQueryExceptionInfo
          { connection = conn,
            exception,
            sql
          }
    Right result -> pure result

queryListColCheck ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  ([a] -> Either e r) ->
  IO r
queryListColCheck conn s check =
  queryListRowCheck conn s (coerce @([a] -> Either e r) @([Sqlite.Only a] -> Either e r) check)

queryMaybeRowCheck ::
  (Sqlite.FromRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  (a -> Either e r) ->
  IO (Maybe r)
queryMaybeRowCheck conn s check =
  gqueryListCheck conn s \case
    [] -> pure Nothing
    [x] -> bimap SomeSqliteExceptionReason Just (check x)
    xs -> Left (SomeSqliteExceptionReason (ExpectedAtMostOneRowException (anythingToString xs)))

queryMaybeColCheck ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  (a -> Either e r) ->
  IO (Maybe r)
queryMaybeColCheck conn s check =
  queryMaybeRowCheck conn s (coerce @(a -> Either e r) @(Sqlite.Only a -> Either e r) check)

queryOneRowCheck ::
  (Sqlite.FromRow a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  (a -> Either e r) ->
  IO r
queryOneRowCheck conn s check =
  gqueryListCheck conn s \case
    [x] -> mapLeft SomeSqliteExceptionReason (check x)
    xs -> Left (SomeSqliteExceptionReason (ExpectedExactlyOneRowException (anythingToString xs)))

queryOneColCheck ::
  forall a e r.
  (Sqlite.FromField a, SqliteExceptionReason e) =>
  Connection ->
  Sql ->
  (a -> Either e r) ->
  IO r
queryOneColCheck conn s check =
  queryOneRowCheck conn s (coerce @(a -> Either e r) @(Sqlite.Only a -> Either e r) check)

-- Rows modified

rowsModified :: Connection -> IO Int
rowsModified (Connection _ _ conn) =
  Sqlite.changes conn

-- Vacuum

-- | @VACUUM@, and return whether or not the vacuum succeeded. A vacuum fails if the connection has any open
-- transactions.
vacuum :: Connection -> IO Bool
vacuum conn =
  try (execute conn [Sql.sql| VACUUM |]) >>= \case
    Left SqliteBusyException -> pure False
    Left exception -> throwIO exception
    Right () -> pure True

-- | @VACUUM INTO@
vacuumInto :: Connection -> FilePath -> IO ()
vacuumInto conn file =
  execute conn [Sql.sql| VACUUM INTO :file |]

-- Low-level

-- | @BEGIN@
begin :: Connection -> IO ()
begin conn =
  execute conn [Sql.sql| BEGIN |]

-- | @BEGIN IMMEDIATE@
beginImmediate :: Connection -> IO ()
beginImmediate conn =
  execute conn [Sql.sql| BEGIN IMMEDIATE |]

-- | @COMMIT@
commit :: Connection -> IO ()
commit conn =
  execute conn [Sql.sql| COMMIT |]

-- | @ROLLBACK@
rollback :: Connection -> IO ()
rollback conn =
  execute conn [Sql.sql| ROLLBACK |]

-- | Perform an action within a named savepoint. The action is provided a rollback action.
withSavepoint :: (MonadUnliftIO m) => Connection -> Text -> (m () -> m a) -> m a
withSavepoint conn name action =
  withRunInIO \runInIO ->
    withSavepointIO conn name \rollback ->
      runInIO (action (liftIO rollback))

withSavepointIO :: Connection -> Text -> (IO () -> IO a) -> IO a
withSavepointIO conn name action = do
  uninterruptibleMask \restore -> do
    savepoint conn name
    result <-
      restore (action doRollbackTo) `onException` do
        doRollbackTo
        doRelease
    doRelease
    pure result
  where
    doRollbackTo = rollbackTo conn name
    doRelease = release conn name

-- | @SAVEPOINT@
savepoint :: Connection -> Text -> IO ()
savepoint conn name =
  execute conn (Sql ("SAVEPOINT " <> name) [])

-- | @ROLLBACK TO@
rollbackTo :: Connection -> Text -> IO ()
rollbackTo conn name =
  execute conn (Sql ("ROLLBACK TO " <> name) [])

-- | @RELEASE@
release :: Connection -> Text -> IO ()
release conn name =
  execute conn (Sql ("RELEASE " <> name) [])

-----------------------------------------------------------------------------------------------------------------------
-- Utils

bindParameters :: Direct.Sqlite.Statement -> [Sqlite.SQLData] -> IO ()
bindParameters statement =
  loop 1
  where
    loop :: Direct.Sqlite.ParamIndex -> [Sqlite.SQLData] -> IO ()
    loop !i = \case
      [] -> pure ()
      p : ps -> do
        Direct.Sqlite.bindSQLData statement i p
        loop (i + 1) ps

------------------------------------------------------------------------------------------------------------------------
-- Exceptions

-- | A query was expected to return exactly one row, but it did not. The exception carries a string representation of
-- the rows that were actually returned.
newtype ExpectedExactlyOneRowException = ExpectedExactlyOneRowException
  { rows :: String
  }
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)

-- | A query was expected to return exactly one row, but it did not. The exception carries a string representation of
-- the rows that were actually returned.
newtype ExpectedAtMostOneRowException = ExpectedAtMostOneRowException
  { rows :: String
  }
  deriving stock (Show)
  deriving anyclass (SqliteExceptionReason)
