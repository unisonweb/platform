module Unison.CommandLine.Main
  ( main,
  )
where

import Compat (withInterruptHandler)
import Control.Concurrent.Async qualified as Async
import Control.Exception (catch, displayException, finally, mask)
import Control.Lens ((?~))
import Control.Lens.Lens
import Crypto.Random qualified as Random
import Data.IORef
import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Ki qualified
import System.Console.Haskeline (Settings (autoAddHistory))
import System.Console.Haskeline qualified as Line
import System.Console.Haskeline.History qualified as Line
import System.IO (hGetEcho, hPutStrLn, hSetEcho, stderr, stdin)
import System.IO.Error (isDoesNotExistError)
import Unison.Auth.CredentialManager (newCredentialManager)
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Auth.Tokens qualified as AuthN
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.Pretty qualified as P
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.HandleInput qualified as HandleInput
import Unison.Codebase.Editor.Input (Event, Input (..))
import Unison.Codebase.Editor.Output (NumberedArgs, Output)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime qualified as Runtime
import Unison.CommandLine
import Unison.CommandLine.Completion (haskelineTabComplete)
import Unison.CommandLine.InputPatterns qualified as IP
import Unison.CommandLine.OutputMessages (notifyNumbered, notifyUser)
import Unison.CommandLine.Types (ShouldWatchFiles (..))
import Unison.CommandLine.Welcome qualified as Welcome
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyTerminal
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Server.CodebaseServer qualified as Server
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Util.Pretty qualified as P
import Unison.Util.TQueue qualified as Q
import UnliftIO qualified
import UnliftIO.Directory qualified as Directory
import UnliftIO.STM

getUserInput ::
  Codebase IO Symbol Ann ->
  AuthenticatedHttpClient ->
  PP.ProjectPath ->
  IO (Branch IO) ->
  NumberedArgs ->
  IO Input
getUserInput codebase authHTTPClient pp currentProjectRoot numberedArgs =
  Line.runInputT
    settings
    (haskelineCtrlCHandling go)
  where
    -- Catch ctrl-c and simply re-render the prompt.
    haskelineCtrlCHandling :: Line.InputT IO b -> Line.InputT IO b
    haskelineCtrlCHandling act = do
      -- We return a Maybe result to ensure we don't nest an action within the masked exception
      -- handler.
      Line.handleInterrupt (pure Nothing) (Line.withInterrupt (Just <$> act)) >>= \case
        Nothing -> haskelineCtrlCHandling act
        Just a -> pure a
    go :: Line.InputT IO Input
    go = do
      let promptString = P.prettyProjectPath pp
      let fullPrompt = P.toANSI 80 (promptString <> fromString prompt)
      line <- Line.getInputLine fullPrompt
      case line of
        Nothing -> pure QuitI
        Just l -> case words l of
          [] -> go
          ws -> do
            liftIO (parseInput codebase pp currentProjectRoot numberedArgs IP.patternMap ws) >>= \case
              Left msg -> do
                -- We still add history that failed to parse so the user can easily reload
                -- the input and fix it.
                Line.modifyHistory $ Line.addHistoryUnlessConsecutiveDupe $ l
                liftIO $ putPrettyLn msg
                go
              Right Nothing -> do
                -- Ctrl-c or some input cancel, re-run the prompt
                go
              Right (Just (expandedArgs, i)) -> do
                let expandedArgs' = IP.unifyArgument <$> expandedArgs
                    expandedArgsStr = unwords expandedArgs'
                when (expandedArgs' /= ws) $ do
                  liftIO . putStrLn $ fullPrompt <> expandedArgsStr
                Line.modifyHistory $ Line.addHistoryUnlessConsecutiveDupe $ expandedArgsStr
                pure i
    settings :: Line.Settings IO
    settings =
      Line.Settings
        { complete = tabComplete,
          historyFile = Just ".unisonHistory",
          autoAddHistory = False
        }
    tabComplete = haskelineTabComplete IP.patternMap codebase authHTTPClient pp

loopStateProjectPath ::
  Codebase IO Symbol Ann ->
  Cli.LoopState ->
  IO PP.ProjectPath
loopStateProjectPath codebase loopState = do
  let ppIds = NEL.head $ Cli.projectPathStack loopState
  ppIds & PP.projectAndBranch_ %%~ \pabIds -> liftIO . Codebase.runTransaction codebase $ ProjectUtils.expectProjectAndBranchByIds pabIds

main ::
  FilePath ->
  Welcome.Welcome ->
  PP.ProjectPathIds ->
  [Either Event Input] ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Maybe Server.BaseUrl ->
  UCMVersion ->
  (PP.ProjectPathIds -> IO ()) ->
  ShouldWatchFiles ->
  IO ()
main dir welcome ppIds initialInputs runtime sbRuntime nRuntime codebase serverBaseUrl ucmVersion lspCheckForChanges shouldWatchFiles = Ki.scoped \scope -> do
  _ <- Ki.fork scope do
    -- Pre-load the project root in the background so it'll be ready when a command needs it.
    projectRoot <- Codebase.expectProjectBranchRoot codebase ppIds.project ppIds.branch
    -- Start forcing thunks in a background thread.
    UnliftIO.concurrently_
      (UnliftIO.evaluate projectRoot)
      (UnliftIO.evaluate IOSource.typecheckedFile) -- IOSource takes a while to compile, we should start compiling it on startup
  let initialState = Cli.loopState0 ppIds
  eventQueue <- Q.newIO
  initialInputsRef <- newIORef $ Welcome.run welcome ++ initialInputs
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
        let PP.ProjectAndBranch projId branchId = PP.toProjectAndBranch $ NonEmpty.head loopState.projectPathStack
        let getProjectRoot = liftIO $ Codebase.expectProjectBranchRoot codebase projId branchId
        pp <- loopStateProjectPath codebase loopState
        getUserInput
          codebase
          authHTTPClient
          pp
          getProjectRoot
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

  let writeSource :: Text -> Text -> Bool -> IO ()
      writeSource fp contents addFold = do
        path <- Directory.canonicalizePath (Text.unpack fp)
        prependUtf8
          path
          if addFold
            then contents <> "\n\n---- Anything below this line is ignored by Unison.\n\n"
            else contents <> "\n\n"

  let env =
        Cli.Env
          { authHTTPClient,
            codebase,
            credentialManager,
            loadSource = loadSourceFile,
            writeSource,
            generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG,
            notify,
            notifyNumbered = \o ->
              let (p, args) = notifyNumbered o
               in putPrettyNonempty p $> args,
            runtime,
            sandboxedRuntime = sbRuntime,
            nativeRuntime = nRuntime,
            serverBaseUrl,
            ucmVersion,
            isTranscriptTest = False
          }

  (onInterrupt, waitForInterrupt) <- buildInterruptHandler

  mask \restore -> do
    -- Handle inputs until @HaltRepl@, staying in the loop on Ctrl+C or synchronous exception.
    let loop0 :: Cli.LoopState -> IO ()
        loop0 s0 = do
          -- It's always possible the previous command changed the branch head, so tell the LSP to check if the current
          -- path or project has changed.
          lspCheckForChanges (NEL.head $ Cli.projectPathStack s0)
          let step = do
                input <- awaitInput s0
                (!result, resultState) <- Cli.runCli env s0 (HandleInput.loop input)
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
              Text.hPutStrLn stderr ("Encountered exception:\n" <> Text.pack (displayException e))
              loop0 s0
            Right (Right (result, s1)) -> do
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
