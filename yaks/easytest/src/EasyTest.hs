{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EasyTest where

import Control.Applicative
import Control.Concurrent
import qualified Control.Concurrent.Async as A
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word
import GHC.Stack
import System.Random (Random)
import qualified System.Random as Random

data Status = Failed | Passed !Int | Skipped | Pending

combineStatus :: Status -> Status -> Status
combineStatus Skipped s = s
combineStatus s Skipped = s
combineStatus _ Pending = Pending
combineStatus Pending _ = Pending
combineStatus Failed _ = Failed
combineStatus _ Failed = Failed
combineStatus (Passed n) (Passed m) = Passed (n + m)

data Env = Env
  { rng :: TVar Random.StdGen,
    messages :: String,
    results :: TBQueue (Maybe (TMVar (String, Status))),
    note_ :: String -> IO (),
    allow :: String
  }

newtype Test a = Test (ReaderT Env IO (Maybe a))

io :: IO a -> Test a
io = liftIO

atomicLogger :: IO (String -> IO ())
atomicLogger = do
  lock <- newMVar ()
  pure $ \msg ->
    -- force msg before acquiring lock
    let dummy = foldl' (\_ ch -> ch == 'a') True msg
     in dummy `seq` bracket (takeMVar lock) (\_ -> putMVar lock ()) (\_ -> putStrLn msg)

expect' :: (HasCallStack) => Bool -> Test ()
expect' False = crash "unexpected"
expect' True = pure ()

expect :: (HasCallStack) => Bool -> Test ()
expect False = crash "unexpected"
expect True = ok

expectEqual' :: (HasCallStack, Eq a, Show a) => a -> a -> Test ()
expectEqual' expected actual =
  if expected == actual
    then pure ()
    else crash $ unlines ["", show actual, "** did not equal expected value **", show expected]

expectEqual :: (HasCallStack, Eq a, Show a) => a -> a -> Test ()
expectEqual expected actual =
  if expected == actual
    then ok
    else crash $ unlines ["", show actual, "** did not equal expected value **", show expected]

expectNotEqual :: (HasCallStack, Eq a, Show a) => a -> a -> Test ()
expectNotEqual forbidden actual =
  if forbidden /= actual
    then ok
    else crash $ unlines ["", show actual, "** did equal the forbidden value **", show forbidden]

expectJust :: (HasCallStack) => Maybe a -> Test a
expectJust Nothing = crash "expected Just, got Nothing"
expectJust (Just a) = ok >> pure a

-- | This will succeed on any `Right` value. It can be combined with further expectations, like
--   @`expectEqual` x `<=<` expectRight@.
--
--  __NB__: If the `Left` type has a `Show` instance, prefer `expectRight'` for better failure messages.
expectRight :: (HasCallStack) => Either e a -> Test a
expectRight (Left _) = crash "expected Right, got Left"
expectRight (Right a) = ok >> pure a

-- | Like `expectRight`, but will `show` the `Left` when it fails. Use this unless the `Left` type doesn’t have a `Show`
--   instance.
expectRight' :: (HasCallStack, Show e) => Either e a -> Test a
expectRight' (Left e) = crash $ "expected Right, got Left " <> showsPrec 11 e ""
expectRight' (Right a) = ok >> pure a

expectLeft :: (HasCallStack) => Either e a -> Test e
expectLeft (Left e) = ok >> pure e
expectLeft (Right _) = crash "expected Left, got Right"

tests :: [Test ()] -> Test ()
tests = msum

-- | Run all tests whose scope starts with the given prefix
runOnly :: String -> Test a -> IO ()
runOnly prefix t = do
  logger <- atomicLogger
  seed <- abs <$> Random.randomIO :: IO Int
  run' seed logger prefix t

-- | Run all tests with the given seed and whose scope starts with the given prefix
rerunOnly :: Int -> String -> Test a -> IO ()
rerunOnly seed prefix t = do
  logger <- atomicLogger
  run' seed logger prefix t

run :: Test a -> IO ()
run = runOnly ""

rerun :: Int -> Test a -> IO ()
rerun seed = rerunOnly seed []

run' :: Int -> (String -> IO ()) -> String -> Test a -> IO ()
run' seed note allow (Test t) = do
  let !rng = Random.mkStdGen seed
  resultsQ <- atomically (newTBQueue 50)
  rngVar <- newTVarIO rng
  note $ "Randomness seed for this run is " ++ show seed ++ ""
  results <- atomically $ newTVar Map.empty
  rs <- A.async . forever $ do
    -- note, totally fine if this bombs once queue is empty
    Just result <- atomically $ readTBQueue resultsQ
    (msgs, passed) <- atomically $ takeTMVar result
    atomically $ modifyTVar results (Map.insertWith combineStatus msgs passed)
    resultsMap <- readTVarIO results
    case Map.findWithDefault Skipped msgs resultsMap of
      Skipped -> pure ()
      Pending -> note $ "🚧  " ++ msgs
      Passed n -> note $ "\129412  " ++ (if n <= 1 then msgs else "(" ++ show n ++ ") " ++ msgs)
      Failed -> note $ "💥  " ++ msgs
  let line = "------------------------------------------------------------"
  note "Raw test output to follow ... "
  note line
  e <- try (runReaderT (void t) (Env rngVar [] resultsQ note allow)) :: IO (Either SomeException ())
  case e of
    Left e -> note $ "Exception while running tests: " ++ show e
    Right () -> pure ()
  atomically $ writeTBQueue resultsQ Nothing
  _ <- A.waitCatch rs
  resultsMap <- readTVarIO results
  let resultsList = Map.toList resultsMap
      succeededList = [n | (_, Passed n) <- resultsList]
      succeeded = length succeededList
      -- totalTestCases = foldl' (+) 0 succeededList
      failures = [a | (a, Failed) <- resultsList]
      failed = length failures
      pendings = [a | (a, Pending) <- resultsList]
      pending = length pendings
      pendingSuffix = if pending == 0 then "👍 🎉" else ""
      testsPlural n = show n ++ " " ++ if n == 1 then "test" else "tests"
  note line
  note "\n"
  when (pending > 0) $ do
    note $ "🚧  " ++ testsPlural pending ++ " still pending (pending scopes below):"
    note $ "    " ++ intercalate "\n    " (map (show . takeWhile (/= '\n')) pendings)
  case failures of
    [] ->
      case succeeded of
        0 -> do
          note "😶  hmm ... no test results recorded"
          note "Tip: use `ok`, `expect`, or `crash` to record results"
          note "Tip: if running via `runOnly` or `rerunOnly`, check for typos"
        n -> note $ "✅  " ++ testsPlural n ++ " passed, no failures! " ++ pendingSuffix
    (hd : _) -> do
      note $ "  " ++ show succeeded ++ (if failed == 0 then " PASSED" else " passed")
      note $ "  " ++ show (length failures) ++ (if failed == 0 then " failed" else " FAILED (failed scopes below)")
      note $ "    " ++ intercalate "\n    " (map (show . takeWhile (/= '\n')) failures)
      note ""
      note "  To rerun with same random seed:\n"
      note $ "    EasyTest.rerun " ++ show seed
      note $ "    EasyTest.rerunOnly " ++ show seed ++ " " ++ "\"" ++ hd ++ "\""
      note "\n"
      note line
      note "❌"
      fail "test failures"

-- | Label a test. Can be nested. A `'.'` is placed between nested
-- scopes, so `scope "foo" . scope "bar"` is equivalent to `scope "foo.bar"`
scope :: String -> Test a -> Test a
scope msg (Test t) = wrap . Test $ do
  env <- ask
  let messages' = case messages env of [] -> msg; ms -> ms ++ ('.' : msg)
  let env' = env {messages = messages', allow = drop (length msg + 1) (allow env)}
  if null (allow env) || take (length (allow env)) msg `isPrefixOf` allow env
    then liftIO (runWrap env' t)
    else putResult Skipped >> pure Nothing

-- | Log a message
note :: String -> Test ()
note msg = do
  note_ <- asks note_
  liftIO $ note_ msg
  pure ()

-- | Log a showable value
note' :: (Show s) => s -> Test ()
note' = note . show

-- | Generate a random value
random :: (Random a) => Test a
random = do
  rng <- asks rng
  liftIO . atomically $ do
    rng0 <- readTVar rng
    let (a, rng1) = Random.random rng0
    writeTVar rng rng1
    pure a

-- | Generate a bounded random value. Inclusive on both sides.
random' :: (Random a) => a -> a -> Test a
random' lower upper = do
  rng <- asks rng
  liftIO . atomically $ do
    rng0 <- readTVar rng
    let (a, rng1) = Random.randomR (lower, upper) rng0
    writeTVar rng rng1
    pure a

bool :: Test Bool
bool = random

word8 :: Test Word8
word8 = random

-- | Generate a random `Char`
char :: Test Char
char = random

-- | Generate a random `Int`
int :: Test Int
int = random

-- | Generate a random `Double`
double :: Test Double
double = random

-- | Generate a random `Word`
word :: Test Word
word = random

-- | Generate a random `Int` in the given range
-- Note: `int' 0 5` includes both `0` and `5`
int' :: Int -> Int -> Test Int
int' = random'

-- | Generate a random `Char` in the given range
-- Note: `char' 'a' 'z'` includes both `'a'` and `'z'`.
char' :: Char -> Char -> Test Char
char' = random'

-- | Generate a random `Double` in the given range
-- Note: `double' 0 1` includes both `0` and `1`.
double' :: Double -> Double -> Test Double
double' = random'

-- | Generate a random `Double` in the given range
-- Note: `word' 0 10` includes both `0` and `10`.
word' :: Word -> Word -> Test Word
word' = random'

-- | Generate a random `Double` in the given range
-- Note: `word8' 0 10` includes both `0` and `10`.
word8' :: Word8 -> Word8 -> Test Word8
word8' = random'

-- | Sample uniformly from the given list of possibilities
pick :: [a] -> Test a
pick as =
  let n = length as; ind = picker n as
   in do
        _ <- if (n > 0) then pure () else crash "pick called with empty list"
        i <- int' 0 (n - 1)
        Just a <- pure (ind i)
        pure a

picker :: Int -> [a] -> (Int -> Maybe a)
picker _ [] = const Nothing
picker _ [a] = \i -> if i == 0 then Just a else Nothing
picker size as = go
  where
    lsize = size `div` 2
    rsize = size - lsize
    (l, r) = splitAt lsize as
    lpicker = picker lsize l
    rpicker = picker rsize r
    go i = if i < lsize then lpicker i else rpicker (i - lsize)

-- | Alias for `replicateM`
listOf :: Int -> Test a -> Test [a]
listOf = replicateM

-- | Generate a list of lists of the given sizes,
-- an alias for `sizes `forM` \n -> listOf n gen`
listsOf :: [Int] -> Test a -> Test [[a]]
listsOf sizes gen = sizes `forM` \n -> listOf n gen

-- | Alias for `liftA2 (,)`.
pair :: Test a -> Test b -> Test (a, b)
pair = liftA2 (,)

-- | Alias for 'pair'.
tuple2 :: (Random a, Random b) => Test (a, b)
tuple2 =
  (,) <$> random <*> random

-- | Generate a random 3-tuple.
tuple3 :: (Random a, Random b, Random c) => Test (a, b, c)
tuple3 =
  (,,) <$> random <*> random <*> random

-- | Generate a random 4-tuple.
tuple4 :: (Random a, Random b, Random c, Random d) => Test (a, b, c, d)
tuple4 =
  (,,,) <$> random <*> random <*> random <*> random

-- | Generate a `Data.Map k v` of the given size.
mapOf :: (Ord k) => Int -> Test k -> Test v -> Test (Map k v)
mapOf n k v = Map.fromList <$> listOf n (pair k v)

-- | Generate a `[Data.Map k v]` of the given sizes.
mapsOf :: (Ord k) => [Int] -> Test k -> Test v -> Test [Map k v]
mapsOf sizes k v = sizes `forM` \n -> mapOf n k v

-- | Catch all exceptions that could occur in the given `Test`
wrap :: Test a -> Test a
wrap (Test t) = Test $ do
  env <- ask
  lift $ runWrap env t

runWrap :: Env -> ReaderT Env IO (Maybe a) -> IO (Maybe a)
runWrap env t = do
  e <- try $ runReaderT t env
  case e of
    Left e -> do
      note_ env (messages env ++ " EXCEPTION!!!: " ++ show (e :: SomeException))
      runReaderT (putResult Failed) env
      pure Nothing
    Right a -> pure a

-- | A test with a setup and teardown
using :: IO r -> (r -> IO ()) -> (r -> Test a) -> Test a
using r cleanup use = Test $ do
  r <- liftIO r
  env <- ask
  let Test t = use r
  a <- liftIO (runWrap env t)
  liftIO (cleanup r)
  pure a

-- | The current scope
currentScope :: Test String
currentScope = asks messages

-- | Prepend the current scope to a logging message
noteScoped :: String -> Test ()
noteScoped msg = do
  s <- currentScope
  note (s ++ (if null s then "" else " ") ++ msg)

-- | Record a successful test at the current scope
ok :: Test ()
ok = Test (Just <$> putResult (Passed 1))

-- | Skip any tests depending on the return value.
done :: Test a
done = Test (pure Nothing)

-- | Explicitly skip this test
skip :: Test ()
skip = Test (Nothing <$ putResult Skipped)

-- | Record a failure at the current scope
crash :: (HasCallStack) => String -> Test a
crash msg = do
  let trace = callStack
      msg' = msg ++ " " ++ prettyCallStack trace
  Test (Just <$> putResult Failed) >> noteScoped ("FAILURE " ++ msg') >> Test (pure Nothing)

-- | Overwrites the env so that note_ (the logger) is a no op
nologging :: (HasCallStack) => Test a -> Test a
nologging (Test t) = Test $ do
  env <- ask
  liftIO $ runWrap (env {note_ = \_ -> pure ()}) t

-- | Run a test under a new scope, without logs and suppressing all output
attempt :: Test a -> Test (Maybe a)
attempt (Test t) = nologging $ do
  env <- ask
  let msg = "internal attempt"
  let messages' = case messages env of [] -> msg; ms -> ms ++ ('.' : msg)
  liftIO $ runWrap env {messages = messages', allow = "not visible"} t

-- | Placeholder wrapper for a failing test. The test being wrapped is expected/known to fail.
-- Will produce a failure if the test being wrapped suddenly becomes a success.
pending :: (HasCallStack) => Test a -> Test a
pending test = do
  m <- attempt test
  case m of
    Just _ ->
      crash "This pending test should not pass!"
    Nothing ->
      ok >> Test (pure Nothing)

putResult :: Status -> ReaderT Env IO ()
putResult passed = do
  msgs <- asks messages
  allow <- asks (null . allow)
  r <- liftIO . atomically $ newTMVar (msgs, if allow then passed else Skipped)
  q <- asks results
  lift . atomically $ writeTBQueue q (Just r)

instance MonadReader Env Test where
  ask = Test $ do
    allow <- asks (null . allow)
    if allow then Just <$> ask else pure Nothing
  local f (Test t) = Test (local f t)
  reader f = Test (Just <$> reader f)

instance Monad Test where
  return = pure
  Test a >>= f = Test $ do
    a <- a
    case a of
      Nothing -> pure Nothing
      Just a -> let Test t = f a in t

instance MonadFail Test where
  fail = crash

instance MonadThrow Test where
  throwM = Test . throwM

instance MonadCatch Test where
  catch (Test m) f =
    Test $ Catch.catch m (\e -> case f e of Test m' -> m')

instance Functor Test where
  fmap = liftM

instance Applicative Test where
  pure a = Test $ do
    allow <- asks (null . allow)
    pure $ if allow then Just a else Nothing
  (<*>) = ap

instance MonadIO Test where
  liftIO io = do
    s <- asks (null . allow)
    if s
      then wrap $ Test (Just <$> liftIO io)
      else Test (pure Nothing)

instance Alternative Test where
  empty = Test (pure Nothing)
  Test t1 <|> Test t2 = Test $ do
    env <- ask
    (rng1, rng2) <- liftIO . atomically $ do
      currentRng <- readTVar (rng env)
      let (rng1, rng2) = Random.split currentRng
      (,) <$> newTVar rng1 <*> newTVar rng2
    lift $ do
      r1 <- runWrap (env {rng = rng1}) t1
      (<|> r1) <$> runWrap (env {rng = rng2}) t2

instance MonadPlus Test where
  mzero = empty
  mplus = (<|>)

-- | Run a test in a separate thread, not blocking for its result.
fork :: Test a -> Test ()
fork t = void (fork' t)

-- | Run a test in a separate thread, return a future which can be used
-- to block on its result.
fork' :: Test a -> Test (Test a)
fork' (Test t) = do
  env <- ask
  tmvar <- liftIO newEmptyTMVarIO
  liftIO . atomically $ writeTBQueue (results env) (Just tmvar)
  r <- liftIO . A.async $ runWrap env t
  waiter <- liftIO . A.async $ do
    e <- A.waitCatch r
    _ <- atomically $ tryPutTMVar tmvar (messages env, Skipped)
    case e of
      Left _ -> pure Nothing
      Right a -> pure a
  pure $ do
    a <- liftIO (A.wait waiter)
    case a of
      Nothing -> empty
      Just a -> pure a
