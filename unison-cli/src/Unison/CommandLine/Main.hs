module Unison.CommandLine.Main
  ( main,
  )
where

import Compat (withInterruptHandler)
import Control.Concurrent.Async qualified as Async
import Control.Exception (catch, finally, mask)
import Control.Lens (preview, (?~), (^.))
import Control.Monad.Catch (MonadMask)
import Crypto.Random qualified as Random
import Data.Configurator.Types (Config)
import Data.IORef
import Data.Text qualified as Text
import Data.Text.Lazy.IO qualified as Text.Lazy
import Ki qualified
import System.Console.Haskeline qualified as Line
import System.IO (hGetEcho, hPutStrLn, hSetEcho, stderr, stdin)
import System.IO.Error (isDoesNotExistError)
import Text.Pretty.Simple (pShow)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Auth.CredentialManager (newCredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Auth.Tokens qualified as AuthN
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.ProjectUtils (projectBranchPathPrism)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput qualified as HandleInput
import Unison.Codebase.Editor.Input (Event, Input (..))
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Runtime
import Unison.CommandLine
import Unison.CommandLine.Completion (haskelineTabComplete)
import Unison.CommandLine.InputPatterns qualified as IP
import Unison.CommandLine.OutputMessages (notifyNumbered, notifyUser)
import Unison.CommandLine.OutputMessages qualified as OutputMessages
import Unison.CommandLine.Types (ShouldWatchFiles (..))
import Unison.CommandLine.Welcome qualified as Welcome
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyTerminal
import Unison.Project (ProjectAndBranch (..))
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Server.CodebaseServer qualified as Server
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Util.Pretty qualified as P
import Unison.Util.TQueue qualified as Q
import UnliftIO qualified
import UnliftIO.STM

getUserInput ::
  forall m v a.
  (MonadIO m, MonadMask m) =>
  Codebase m v a ->
  AuthenticatedHttpClient ->
  IO (Branch m) ->
  Path.Absolute ->
  [String] ->
  m Input
getUserInput codebase authHTTPClient getRoot currentPath numberedArgs =
  Line.runInputT
    settings
    (haskelineCtrlCHandling go)
  where
    -- Catch ctrl-c and simply re-render the prompt.
    haskelineCtrlCHandling :: Line.InputT m b -> Line.InputT m b
    haskelineCtrlCHandling act = do
      -- We return a Maybe result to ensure we don't nest an action within the masked exception
      -- handler.
      Line.handleInterrupt (pure Nothing) (Line.withInterrupt (Just <$> act)) >>= \case
        Nothing -> haskelineCtrlCHandling act
        Just a -> pure a
    go :: Line.InputT m Input
    go = do
      promptString <-
        case preview projectBranchPathPrism currentPath of
          Nothing -> pure ((P.green . P.shown) currentPath)
          Just (ProjectAndBranch projectId branchId, restPath) -> do
            lift (Codebase.runTransaction codebase (Queries.loadProjectAndBranchNames projectId branchId)) <&> \case
              -- If the project branch has been deleted from sqlite, just show a borked prompt
              Nothing -> P.red "???"
              Just (projectName, branchName) ->
                P.sep
                  " "
                  ( catMaybes
                      [ Just
                          ( OutputMessages.prettyProjectAndBranchName
                              (ProjectAndBranch projectName branchName)
                          ),
                        case restPath of
                          Path.Empty -> Nothing
                          _ -> (Just . P.green . P.shown) restPath
                      ]
                  )
      line <- Line.getInputLine (P.toANSI 80 (promptString <> fromString prompt))
      case line of
        Nothing -> pure QuitI
        Just l -> case words l of
          [] -> go
          ws -> do
            liftIO (parseInput (Branch.head <$> getRoot) currentPath numberedArgs IP.patternMap ws) >>= \case
              Left msg -> do
                liftIO $ putPrettyLn msg
                go
              Right i -> pure i
    settings :: Line.Settings m
    settings = Line.Settings tabComplete (Just ".unisonHistory") True
    tabComplete = haskelineTabComplete IP.patternMap codebase authHTTPClient currentPath

main ::
  FilePath ->
  Welcome.Welcome ->
  Path.Absolute ->
  Config ->
  [Either Event Input] ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Maybe Server.BaseUrl ->
  UCMVersion ->
  (Branch IO -> STM ()) ->
  (Path.Absolute -> STM ()) ->
  ShouldWatchFiles ->
  IO ()
main dir welcome initialPath config initialInputs runtime sbRuntime codebase serverBaseUrl ucmVersion notifyBranchChange notifyPathChange shouldWatchFiles = Ki.scoped \scope -> do
  rootVar <- newEmptyTMVarIO
  initialRootCausalHash <- Codebase.runTransaction codebase Operations.expectRootCausalHash
  _ <- Ki.fork scope $ do
    root <- Codebase.getRootBranch codebase
    atomically $ do
      -- Try putting the root, but if someone else as already written over the root, don't
      -- overwrite it.
      void $ tryPutTMVar rootVar root
    -- Start forcing thunks in a background thread.
    -- This might be overly aggressive, maybe we should just evaluate the top level but avoid
    -- recursive "deep*" things.
    UnliftIO.concurrently_
      (UnliftIO.evaluate root)
      (UnliftIO.evaluate IOSource.typecheckedFile) -- IOSource takes a while to compile, we should start compiling it on startup
  let initialState = Cli.loopState0 initialRootCausalHash rootVar initialPath
  Ki.fork_ scope $ do
    let loop lastRoot = do
          -- This doesn't necessarily notify on _every_ update, but the LSP only needs the
          -- most recent version at any given time, so it's fine to skip some intermediate
          -- versions.
          currentRoot <- atomically do
            currentRoot <- readTMVar rootVar
            guard $ Just currentRoot /= lastRoot
            notifyBranchChange currentRoot
            pure (Just currentRoot)
          loop currentRoot
    loop Nothing
  eventQueue <- Q.newIO
  welcomeEvents <- Welcome.run codebase welcome
  initialInputsRef <- newIORef $ welcomeEvents ++ initialInputs
  pageOutput <- newIORef True
  cancelFileSystemWatch <- case shouldWatchFiles of
    ShouldNotWatchFiles -> pure (pure ())
    ShouldWatchFiles -> watchFileSystem eventQueue dir
  credentialManager <- newCredentialManager
  let tokenProvider = AuthN.newTokenProvider credentialManager
  authHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion
  initialEcho <- hGetEcho stdin
  let restoreEcho = (\currentEcho -> when (currentEcho /= initialEcho) $ hSetEcho stdin initialEcho)
  let getInput :: Cli.LoopState -> IO Input
      getInput loopState = do
        currentEcho <- hGetEcho stdin
        liftIO $ restoreEcho currentEcho
        getUserInput
          codebase
          authHTTPClient
          (atomically . readTMVar $ loopState ^. #root)
          (loopState ^. #currentPath)
          (loopState ^. #numberedArgs)
  let loadSourceFile :: Text -> IO Cli.LoadSourceResult
      loadSourceFile fname =
        if allow $ Text.unpack fname
          then
            let handle :: IOException -> IO Cli.LoadSourceResult
                handle e =
                  case e of
                    _ | isDoesNotExistError e -> return Cli.InvalidSourceNameError
                    _ -> return Cli.LoadError
                go = do
                  contents <- readUtf8 $ Text.unpack fname
                  return $ Cli.LoadSuccess contents
             in catch go handle
          else return Cli.InvalidSourceNameError
  let notify :: Output -> IO ()
      notify =
        notifyUser dir
          >=> ( \o ->
                  ifM
                    (readIORef pageOutput)
                    (putPrettyNonempty o)
                    (putPrettyLnUnpaged o)
              )

  let cleanup :: IO ()
      cleanup = cancelFileSystemWatch
      awaitInput :: Cli.LoopState -> IO (Either Event Input)
      awaitInput loopState = do
        -- use up buffered input before consulting external events
        readIORef initialInputsRef >>= \case
          h : t -> writeIORef initialInputsRef t >> pure h
          [] ->
            -- Race the user input and file watch.
            Async.race (atomically $ Q.peek eventQueue) (getInput loopState) >>= \case
              Left _ -> do
                let e = Left <$> atomically (Q.dequeue eventQueue)
                writeIORef pageOutput False
                e
              x -> do
                writeIORef pageOutput True
                pure x

  let env =
        Cli.Env
          { authHTTPClient,
            codebase,
            config,
            credentialManager,
            loadSource = loadSourceFile,
            generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG,
            notify,
            notifyNumbered = \o ->
              let (p, args) = notifyNumbered o
               in putPrettyNonempty p $> args,
            runtime,
            sandboxedRuntime = sbRuntime,
            serverBaseUrl,
            ucmVersion
          }

  (onInterrupt, waitForInterrupt) <- buildInterruptHandler

  mask \restore -> do
    -- Handle inputs until @HaltRepl@, staying in the loop on Ctrl+C or synchronous exception.
    let loop0 :: Cli.LoopState -> IO ()
        loop0 s0 = do
          let step = do
                input <- awaitInput s0
                (result, resultState) <- Cli.runCli env s0 (HandleInput.loop input)
                let sNext = case input of
                      Left _ -> resultState
                      Right inp -> resultState & #lastInput ?~ inp
                pure (result, sNext)
          UnliftIO.race waitForInterrupt (UnliftIO.tryAny (restore step)) >>= \case
            -- SIGINT
            Left () -> do
              hPutStrLn stderr "\nAborted."
              loop0 s0
            -- Exception during command execution
            Right (Left e) -> do
              Text.Lazy.hPutStrLn stderr ("Encountered exception:\n" <> pShow e)
              loop0 s0
            Right (Right (result, s1)) -> do
              when ((s0 ^. #currentPath) /= (s1 ^. #currentPath :: Path.Absolute)) (atomically . notifyPathChange $ s1 ^. #currentPath)
              case result of
                Cli.Success () -> loop0 s1
                Cli.Continue -> loop0 s1
                Cli.HaltRepl -> pure ()

    withInterruptHandler onInterrupt (loop0 initialState `finally` cleanup)

-- | Installs a posix interrupt handler for catching SIGINT.
-- This replaces GHC's default sigint handler which throws a UserInterrupt async exception
-- and kills the entire process.
--
-- Returns an IO action which blocks until a ctrl-c is detected. It may be used multiple
-- times.
buildInterruptHandler :: IO (IO (), IO ())
buildInterruptHandler = do
  ctrlCMarker <- UnliftIO.newEmptyMVar
  let onInterrupt = void $ UnliftIO.tryPutMVar ctrlCMarker ()
  let waitForInterrupt = UnliftIO.takeMVar ctrlCMarker
  pure $ (onInterrupt, waitForInterrupt)
