{-# LANGUAGE DataKinds #-}

-- | The main CLI monad.
module Unison.Cli.Monad
  ( -- * Cli monad
    Cli,
    ReturnType (..),
    SourceName,
    runCli,

    -- * Envronment
    Env (..),

    -- * Immutable state
    LoopState (..),
    loopState0,
    getProjectPathIds,

    -- * Lifting IO actions
    ioE,

    -- * Acquiring resources
    with,
    with_,
    withE,

    -- * Short-circuiting
    label,
    labelE,
    returnEarly,
    returnEarlyWithoutOutput,
    haltRepl,

    -- * Changing the current directory
    cd,
    popd,
    switchProject,

    -- * Communicating output to the user
    respond,
    respondNumbered,
    setNumberedArgs,

    -- * Debug-timing actions
    time,

    -- * Running transactions
    runTransaction,
    runTransactionWithRollback,
    runTransactionWithRollback2,

    -- * Internal
    setMostRecentProjectPath,

    -- * Misc types
    LoadSourceResult (..),
  )
where

import Control.Exception (throwIO)
import Control.Lens
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState)
import Control.Monad.State.Strict qualified as State
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.List.NonEmpty qualified as NonEmpty
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (diffAbsoluteTime)
import Data.Unique (Unique, newUnique)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import U.Codebase.Sqlite.DbId (ProjectBranchId, ProjectId)
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Auth.CredentialManager (CredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Input (Input)
import Unison.Codebase.Editor.Output (NumberedArgs, NumberedOutput, Output)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime (Runtime)
import Unison.Core.Project (ProjectAndBranch (..))
import Unison.Debug qualified as Debug
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Server.CodebaseServer qualified as Server
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile qualified as UF
import Unsafe.Coerce (unsafeCoerce)

-- | The main command-line app monad.
--
-- * It is a reader monad of 'Env'.
--
-- * It is a state monad of 'LoopState'.
--
-- * It is a short-circuiting monad: a @Cli@ computation can short-circuit with success or failure in a delimited scope.
--
-- * It is a resource monad: resources can be acquired in callback-style.
--
-- * It is an IO monad: you can do IO things, but throwing synchronous exceptions is discouraged. Use the built-in
-- short-circuiting mechanism instead.
newtype Cli a = Cli
  { unCli ::
      forall r.
      Env ->
      (a -> LoopState -> IO (ReturnType r, LoopState)) ->
      LoopState ->
      IO (ReturnType r, LoopState)
  }
  deriving stock (Functor)

instance Applicative Cli where
  pure x = Cli \_ k -> k x
  (<*>) = ap

instance Monad Cli where
  return = pure
  Cli mx >>= f =
    Cli \env k ->
      mx env \a -> unCli (f a) env k

instance MonadIO Cli where
  liftIO mx =
    Cli \_ k s -> do
      x <- mx
      k x s

instance MonadReader Env Cli where
  ask = Cli \env k -> k env
  local f m = Cli \env -> unCli m (f env)

instance MonadState LoopState Cli where
  get = Cli \_ k s -> k s s
  put s = Cli \_ k _ -> k () s

-- | What a Cli action returns: a value, an instruction to continue processing input, or an instruction to stop
-- processing input.
data ReturnType a
  = Success a
  | Continue
  | HaltRepl
  deriving stock (Eq, Show)

-- | Name used for a source-file/source buffer
type SourceName = Text

-- | The command-line app monad environment.
--
-- Get the environment with 'ask'.
data Env = Env
  { authHTTPClient :: AuthenticatedHttpClient,
    codebase :: Codebase IO Symbol Ann,
    credentialManager :: CredentialManager,
    -- | Generate a unique name.
    generateUniqueName :: IO Parser.UniqueName,
    -- | How to load source code.
    loadSource :: SourceName -> IO LoadSourceResult,
    -- | How to write source code. Bool = make new fold?
    writeSource :: SourceName -> Text -> Bool -> IO (),
    -- | What to do with output for the user.
    notify :: Output -> IO (),
    -- | What to do with numbered output for the user.
    notifyNumbered :: NumberedOutput -> IO NumberedArgs,
    runtime :: Runtime Symbol,
    sandboxedRuntime :: Runtime Symbol,
    nativeRuntime :: Runtime Symbol,
    serverBaseUrl :: Maybe Server.BaseUrl,
    ucmVersion :: UCMVersion,
    -- | Whether we're running in a transcript test or not.
    -- Avoid using this except when absolutely necessary.
    isTranscriptTest :: Bool
  }
  deriving stock (Generic)

-- | The command-line app monad mutable state.
--
-- There's an additional pseudo @"currentPath"@ field lens, for convenience.
data LoopState = LoopState
  { -- the current position in the codebase, with the head being the most recent lcoation.
    projectPathStack :: List.NonEmpty PP.ProjectPathIds,
    -- TBD
    -- , _activeEdits :: Set Branch.EditGuid

    -- The file name last modified, and whether to skip the next file
    -- change event for that path (we skip file changes if the file has
    -- just been modified programmatically)
    latestFile :: Maybe (FilePath, Bool),
    -- Nothing means the file didn't parse
    -- Just (Left) means the file parsed but didn't typecheck
    -- Just (Right) means the file parsed and typechecked
    latestTypecheckedFile :: Maybe (Either (UF.UnisonFile Symbol Ann) (UF.TypecheckedUnisonFile Symbol Ann)),
    -- The previous user input. Used to request confirmation of
    -- questionable user commands.
    lastInput :: Maybe Input,
    -- A 1-indexed list of strings that can be referenced by index at the
    -- CLI prompt.  e.g. Given ["Foo.bat", "Foo.cat"],
    -- `rename 2 Foo.foo` will rename `Foo.cat` to `Foo.foo`.
    numberedArgs :: NumberedArgs,
    -- The result of the last run, along with a unison file that
    -- captures the state of dependencies when the last run occurred
    lastRunResult :: Maybe (Term Symbol Ann, Type Symbol Ann, UF.TypecheckedUnisonFile Symbol Ann)
  }
  deriving stock (Generic)

-- | Create an initial loop state given a root branch and the current path.
loopState0 :: PP.ProjectPathIds -> LoopState
loopState0 p = do
  LoopState
    { projectPathStack = pure p,
      latestFile = Nothing,
      latestTypecheckedFile = Nothing,
      lastInput = Nothing,
      numberedArgs = [],
      lastRunResult = Nothing
    }

-- | Run a @Cli@ action down to @IO@.
runCli :: Env -> LoopState -> Cli a -> IO (ReturnType a, LoopState)
runCli env s0 (Cli action) =
  action env (\x s1 -> pure (Success x, s1)) s0

feed :: (a -> LoopState -> IO (ReturnType b, LoopState)) -> (ReturnType a, LoopState) -> IO (ReturnType b, LoopState)
feed k = \case
  (Success x, s) -> k x s
  (Continue, s) -> pure (Continue, s)
  (HaltRepl, s) -> pure (HaltRepl, s)

-- | The result of calling 'loadSource'.
data LoadSourceResult
  = InvalidSourceNameError
  | LoadError
  | LoadSuccess Text

-- | Lift an action of type @IO (Either e a)@, given a continuation for @e@.
ioE :: IO (Either e a) -> (e -> Cli a) -> Cli a
ioE action errK =
  liftIO action >>= \case
    Left err -> errK err
    Right value -> pure value

short :: (forall r. ReturnType r) -> Cli a
short r = Cli \_env _k s -> pure (r, s)

-- | Short-circuit the processing of the current input.
returnEarly :: Output -> Cli a
returnEarly x = do
  respond x
  returnEarlyWithoutOutput

-- | Variant of 'returnEarly' that doesn't take a final output message.
returnEarlyWithoutOutput :: Cli a
returnEarlyWithoutOutput =
  short Continue

-- | Stop processing inputs from the user.
haltRepl :: Cli a
haltRepl = short HaltRepl

-- | Wrap a continuation with 'Cli'.
--
-- Useful for resource acquisition:
--
-- @
-- with (bracket create destroy) \\resource ->
--   ...
-- @
with :: (forall x. (a -> IO x) -> IO x) -> (a -> Cli b) -> Cli b
with resourceK action =
  Cli \env k s ->
    resourceK (runCli env s . action) >>= feed k

-- | A variant of 'with' for actions that don't acquire a resource (like 'Control.Exception.bracket_').
with_ :: (forall x. IO x -> IO x) -> Cli a -> Cli a
with_ resourceK action =
  Cli \env k s ->
    resourceK (runCli env s action) >>= feed k

-- | A variant of 'with' for the variant of bracketing function that may return a Left rather than call the provided
-- continuation.
withE :: (forall x. (a -> IO x) -> IO (Either e x)) -> (Either e a -> Cli b) -> Cli b
withE resourceK action =
  Cli \env k s ->
    resourceK (\a -> runCli env s (action (Right a))) >>= \case
      Left err -> runCli env s (action (Left err)) >>= feed k
      Right result -> feed k result

data X
  = forall a. X !Unique !LoopState a
  deriving anyclass (Exception)

instance Show X where
  show _ = "<internal exception type>"

-- | Create a label that can be jumped to.
--
-- @
-- x \<- label \\j0 -\> do
--   ...
--   label \\j1 -> do
--     ...
--     j0 someValue
--     ... -- We don't get here
--   ... -- We don't get here
-- -- x is bound to someValue
-- @
label :: forall a. ((forall void. a -> Cli void) -> Cli a) -> Cli a
label f =
  Cli \env k s0 -> do
    n <- newUnique
    let bail :: forall void. a -> Cli void
        bail a = do
          s1 <- State.get
          liftIO (throwIO (X n s1 a))
    try (runCli env s0 (f bail)) >>= \case
      Left err@(X m s1 a)
        | n == m -> k (unsafeCoerce a) s1
        | otherwise -> throwIO err
      Right a -> feed k a

-- | A variant of @label@ for the common case that early-return values are tagged with a Left.
labelE :: ((forall void. a -> Cli void) -> Cli b) -> Cli (Either a b)
labelE f =
  label \goto ->
    Right <$> f (goto . Left)

-- | Time an action.
time :: String -> Cli a -> Cli a
time label action =
  if Debug.shouldDebug Debug.Timing
    then Cli \env k s -> do
      systemStart <- getSystemTime
      cpuPicoStart <- getCPUTime
      a <- unCli action env (\a loopState -> pure (Success a, loopState)) s
      cpuPicoEnd <- getCPUTime
      systemEnd <- getSystemTime
      let systemDiff =
            diffTimeToNanos
              (diffAbsoluteTime (systemToTAITime systemEnd) (systemToTAITime systemStart))
      let cpuDiff = picosToNanos (cpuPicoEnd - cpuPicoStart)
      printf "%s: %s (cpu), %s (system)\n" label (renderNanos cpuDiff) (renderNanos systemDiff)
      feed k a
    else action
  where
    diffTimeToNanos :: DiffTime -> Double
    diffTimeToNanos =
      picosToNanos . diffTimeToPicoseconds

    picosToNanos :: Integer -> Double
    picosToNanos =
      (/ 1_000) . realToFrac

    -- Render nanoseconds, trying to fit into 4 characters.
    renderNanos :: Double -> String
    renderNanos ns
      | ns < 0.5 = "0 ns"
      | ns < 995 = printf "%.0f ns" ns
      | ns < 9_950 = printf "%.2f µs" us
      | ns < 99_500 = printf "%.1f µs" us
      | ns < 995_000 = printf "%.0f µs" us
      | ns < 9_950_000 = printf "%.2f ms" ms
      | ns < 99_500_000 = printf "%.1f ms" ms
      | ns < 995_000_000 = printf "%.0f ms" ms
      | ns < 9_950_000_000 = printf "%.2f s" s
      | ns < 99_500_000_000 = printf "%.1f s" s
      | otherwise = printf "%.0f s" s
      where
        us = ns / 1_000
        ms = ns / 1_000_000
        s = ns / 1_000_000_000

getProjectPathIds :: Cli PP.ProjectPathIds
getProjectPathIds = do
  NonEmpty.head <$> use #projectPathStack

cd :: Path.Absolute -> Cli ()
cd path = do
  pp <- getProjectPathIds
  let newPP = pp & PP.absPath_ .~ path
  setMostRecentProjectPath newPP
  #projectPathStack %= NonEmpty.cons newPP

switchProject :: ProjectAndBranch ProjectId ProjectBranchId -> Cli ()
switchProject pab@(ProjectAndBranch projectId branchId) = do
  Env {codebase} <- ask
  let newPP = PP.ProjectPath projectId branchId Path.absoluteEmpty
  #projectPathStack %= NonEmpty.cons newPP
  runTransaction $ do Q.setMostRecentBranch projectId branchId
  setMostRecentProjectPath newPP
  liftIO $ Codebase.preloadProjectBranch codebase pab

-- | Pop the latest path off the stack, if it's not the only path in the stack.
--
-- Returns whether anything was popped.
popd :: Cli Bool
popd = do
  state <- State.get
  case List.NonEmpty.uncons (projectPathStack state) of
    (_, Nothing) -> pure False
    (_, Just paths) -> do
      setMostRecentProjectPath (List.NonEmpty.head paths)
      State.put state {projectPathStack = paths}
      pure True

setMostRecentProjectPath :: PP.ProjectPathIds -> Cli ()
setMostRecentProjectPath loc =
  runTransaction $ Codebase.setCurrentProjectPath loc

respond :: Output -> Cli ()
respond output = do
  Env {notify} <- ask
  liftIO (notify output)

respondNumbered :: NumberedOutput -> Cli ()
respondNumbered output = do
  Env {notifyNumbered} <- ask
  args <- liftIO (notifyNumbered output)
  setNumberedArgs args

-- | Updates the numbered args, but only if the new args are non-empty.
setNumberedArgs :: NumberedArgs -> Cli ()
setNumberedArgs args = do
  unless (null args) do
    #numberedArgs .= args

runTransaction :: Sqlite.Transaction a -> Cli a
runTransaction action = do
  Env {codebase} <- ask
  liftIO (Codebase.runTransaction codebase action)

-- | Run a transaction that can abort early with an output message.
-- todo: rename to runTransactionWithReturnEarly
runTransactionWithRollback :: ((forall void. Output -> Sqlite.Transaction void) -> Sqlite.Transaction a) -> Cli a
runTransactionWithRollback action = do
  Env {codebase} <- ask
  liftIO (Codebase.runTransactionWithRollback codebase \rollback -> Right <$> action (\output -> rollback (Left output)))
    & onLeftM returnEarly

-- | Run a transaction that can abort early.
-- todo: rename to runTransactionWithRollback
runTransactionWithRollback2 :: ((forall void. a -> Sqlite.Transaction void) -> Sqlite.Transaction a) -> Cli a
runTransactionWithRollback2 action = do
  env <- ask
  liftIO (Codebase.runTransactionWithRollback env.codebase action)
