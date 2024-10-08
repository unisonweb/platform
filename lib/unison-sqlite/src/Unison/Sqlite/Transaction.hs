module Unison.Sqlite.Transaction
  ( -- * Transaction management
    Transaction,
    runTransaction,
    runTransactionWithRollback,
    runWriteTransaction,
    savepoint,

    -- ** Unsafe things
    unsafeIO,
    unsafeGetConnection,
    unsafeUnTransaction,

    -- * Executing queries

    -- ** Without results
    execute,
    executeStatements,

    -- ** With results
    queryListRow,
    queryListCol,
    queryMaybeRow,
    queryMaybeCol,
    queryOneRow,
    queryOneCol,

    -- *** With checks
    queryListColCheck,
    queryMaybeColCheck,
    queryOneRowCheck,
    queryOneColCheck,

    -- * Rows modified
    rowsModified,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (fromException), onException, throwIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Text qualified as Text
import Data.Unique (Unique, newUnique)
import Database.SQLite.Simple qualified as Sqlite
import Database.SQLite.Simple.FromField qualified as Sqlite
import System.Random qualified as Random
import Unison.Prelude
import Unison.Sqlite.Connection (Connection (..))
import Unison.Sqlite.Connection qualified as Connection
import Unison.Sqlite.Exception (SqliteExceptionReason, SqliteQueryException, pattern SqliteBusyException)
import Unison.Sqlite.Sql (Sql)
import UnliftIO.Exception (catchAny, trySyncOrAsync, uninterruptibleMask)
import Unsafe.Coerce (unsafeCoerce)

newtype Transaction a
  = Transaction (Connection -> IO a)
  -- Omit MonadIO instance because transactions may be retried
  -- Omit MonadThrow instance so we always throw SqliteException (via *Check) with lots of context
  deriving (Applicative, Functor, Monad) via (ReaderT Connection IO)

instance (Monoid a) => Monoid (Transaction a) where
  mempty :: (Monoid a) => Transaction a
  mempty = pure mempty

instance (Semigroup a) => Semigroup (Transaction a) where
  (<>) :: Transaction a -> Transaction a -> Transaction a
  (<>) = liftA2 (<>)

-- | Run a transaction on the given connection.
runTransaction :: (MonadIO m, HasCallStack) => Connection -> Transaction a -> m a
runTransaction conn (Transaction f) = liftIO do
  uninterruptibleMask \restore -> do
    Connection.begin conn
    -- Catch all exceptions (sync or async), because we want to ROLLBACK the BEGIN no matter what.
    trySyncOrAsync @_ @SomeException (restore (f conn)) >>= \case
      Left exception -> do
        ignoringExceptions (Connection.rollback conn)
        case fromException exception of
          Just SqliteBusyException -> do
            restore (threadDelay transactionRetryDelay)
            runWriteTransaction_ restore conn (f conn)
          _ -> throwIO exception
      Right result -> do
        Connection.commit conn
        pure result
{-# SPECIALIZE runTransaction :: Connection -> Transaction a -> IO a #-}

-- An internal exception type that allows `runTransactionWithRollback`
data RollingBack
  = forall a. RollingBack !Unique !a
  deriving anyclass (Exception)

instance Show RollingBack where
  show _ = ""

-- | Run a transaction on the given connection, providing a function that can short-circuit (and roll back) the
-- transaction.
runTransactionWithRollback ::
  (MonadIO m, HasCallStack) =>
  Connection ->
  ((forall void. a -> Transaction void) -> Transaction a) ->
  m a
runTransactionWithRollback conn transaction = liftIO do
  token <- newUnique
  try (runTransaction conn (transaction \x -> unsafeIO (throwIO (RollingBack token x)))) >>= \case
    Left exception@(RollingBack token2 x)
      | token == token2 -> pure (unsafeCoerce x)
      | otherwise -> throwIO exception
    Right x -> pure x
{-# SPECIALIZE runTransactionWithRollback :: Connection -> ((forall void. a -> Transaction void) -> Transaction a) -> IO a #-}

-- | Run a transaction that is known to perform at least one write.
--
-- The action is provided a function that peels off the 'Transaction' newtype without sending the corresponding
-- BEGIN/COMMIT statements.
--
-- The transaction is never retried, so it is (more) safe to interleave arbitrary IO actions.
runWriteTransaction :: (HasCallStack, MonadUnliftIO m) => Connection -> ((forall x. Transaction x -> m x) -> m a) -> m a
runWriteTransaction conn f =
  withRunInIO \runInIO ->
    uninterruptibleMask \restore ->
      runWriteTransaction_
        restore
        conn
        (runInIO (f (\transaction -> liftIO (unsafeUnTransaction transaction conn))))
{-# SPECIALIZE runWriteTransaction :: Connection -> ((forall x. Transaction x -> IO x) -> IO a) -> IO a #-}

runWriteTransaction_ :: (HasCallStack) => (forall x. IO x -> IO x) -> Connection -> IO a -> IO a
runWriteTransaction_ restore conn transaction = do
  keepTryingToBeginImmediate restore conn
  result <- restore transaction `onException` ignoringExceptions (Connection.rollback conn)
  Connection.commit conn
  pure result

-- @BEGIN IMMEDIATE@ until success.
keepTryingToBeginImmediate :: (HasCallStack) => (forall x. IO x -> IO x) -> Connection -> IO ()
keepTryingToBeginImmediate restore conn =
  let loop =
        try @_ @SqliteQueryException (Connection.beginImmediate conn) >>= \case
          Left SqliteBusyException -> do
            restore (threadDelay transactionRetryDelay)
            loop
          Left exception -> throwIO exception
          Right () -> pure ()
   in loop

ignoringExceptions :: IO () -> IO ()
ignoringExceptions action =
  action `catchAny` \_ -> pure ()

-- | Perform an atomic sub-computation within a transaction; if it returns 'Left', it's rolled back.
savepoint :: Transaction (Either a a) -> Transaction a
savepoint (Transaction action) = do
  Transaction \conn -> do
    -- Generate a random name for the savepoint, so the caller isn't burdened with coming up with a name. Seems
    -- extremely unlikely for this to go wrong (i.e. some super nested withSavepoint call that ends up generating the
    -- same savepoint name twice in a single scope).
    name <- Text.pack <$> replicateM 10 (Random.randomRIO ('a', 'z'))
    Connection.withSavepointIO conn name \rollback ->
      action conn >>= \case
        Left result -> do
          rollback
          pure result
        Right result -> pure result

-- | Perform IO inside a transaction, which should be idempotent, because it may be run more than once if the
-- transaction needs to retry.
--
-- /Warning/: attempting to run a transaction inside a transaction will cause an exception!
unsafeIO :: (HasCallStack) => IO a -> Transaction a
unsafeIO action =
  Transaction \_ -> action

unsafeGetConnection :: Transaction Connection
unsafeGetConnection =
  Transaction pure

-- | Unwrap the transaction newtype, throwing away the sending of BEGIN/COMMIT + automatic retry.
unsafeUnTransaction :: Transaction a -> Connection -> IO a
unsafeUnTransaction (Transaction action) =
  action

-- Without results

execute :: (HasCallStack) => Sql -> Transaction ()
execute s =
  Transaction \conn -> Connection.execute conn s

executeStatements :: (HasCallStack) => Text -> Transaction ()
executeStatements s =
  Transaction \conn -> Connection.executeStatements conn s

-- With results, without checks

queryListRow :: (Sqlite.FromRow a, HasCallStack) => Sql -> Transaction [a]
queryListRow s =
  Transaction \conn -> Connection.queryListRow conn s

queryListCol :: (Sqlite.FromField a, HasCallStack) => Sql -> Transaction [a]
queryListCol s =
  Transaction \conn -> Connection.queryListCol conn s

queryMaybeRow :: (Sqlite.FromRow a, HasCallStack) => Sql -> Transaction (Maybe a)
queryMaybeRow s =
  Transaction \conn -> Connection.queryMaybeRow conn s

queryMaybeCol :: (Sqlite.FromField a, HasCallStack) => Sql -> Transaction (Maybe a)
queryMaybeCol s =
  Transaction \conn -> Connection.queryMaybeCol conn s

queryOneRow :: (Sqlite.FromRow a, HasCallStack) => Sql -> Transaction a
queryOneRow s =
  Transaction \conn -> Connection.queryOneRow conn s

queryOneCol :: (Sqlite.FromField a, HasCallStack) => Sql -> Transaction a
queryOneCol s =
  Transaction \conn -> Connection.queryOneCol conn s

-- With results, with parameters, with checks

queryListColCheck ::
  (Sqlite.FromField a, SqliteExceptionReason e, HasCallStack) =>
  Sql ->
  ([a] -> Either e r) ->
  Transaction r
queryListColCheck sql check =
  Transaction \conn -> Connection.queryListColCheck conn sql check

queryMaybeColCheck ::
  (Sqlite.FromField a, SqliteExceptionReason e, HasCallStack) =>
  Sql ->
  (a -> Either e r) ->
  Transaction (Maybe r)
queryMaybeColCheck s check =
  Transaction \conn -> Connection.queryMaybeColCheck conn s check

queryOneRowCheck ::
  (Sqlite.FromRow a, SqliteExceptionReason e, HasCallStack) =>
  Sql ->
  (a -> Either e r) ->
  Transaction r
queryOneRowCheck s check =
  Transaction \conn -> Connection.queryOneRowCheck conn s check

queryOneColCheck ::
  (Sqlite.FromField a, SqliteExceptionReason e, HasCallStack) =>
  Sql ->
  (a -> Either e r) ->
  Transaction r
queryOneColCheck s check =
  Transaction \conn -> Connection.queryOneColCheck conn s check

-- Rows modified

rowsModified :: Transaction Int
rowsModified =
  Transaction Connection.rowsModified

transactionRetryDelay :: Int
transactionRetryDelay = 100_000
