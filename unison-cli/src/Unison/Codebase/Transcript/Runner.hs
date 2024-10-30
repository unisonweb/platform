{-# LANGUAGE DeriveAnyClass #-}

-- | Execute transcripts.
module Unison.Codebase.Transcript.Runner
  ( Error (..),
    Runner,
    withRunner,
  )
where

import Control.Lens (use, (?~))
import Crypto.Random qualified as Random
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IORef
import Data.List (isSubsequenceOf)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.These (These (..))
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Client qualified as HTTP
import System.Environment (lookupEnv)
import System.IO qualified as IO
import Text.Megaparsec qualified as P
import U.Codebase.Sqlite.DbId qualified as Db
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Auth.CredentialManager qualified as AuthN
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Auth.Tokens qualified as AuthN
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput qualified as HandleInput
import Unison.Codebase.Editor.Input (Event (UnisonFileChanged), Input (..))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Codebase.Transcript
import Unison.Codebase.Transcript.Parser qualified as Transcript
import Unison.Codebase.Verbosity (Verbosity, isSilent)
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.CommandLine
import Unison.CommandLine.InputPattern (InputPattern (aliases, patternName))
import Unison.CommandLine.InputPatterns (validInputs)
import Unison.CommandLine.OutputMessages (notifyNumbered, notifyUser)
import Unison.CommandLine.Welcome (asciiartUnison)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyTerminal
import Unison.Project (ProjectAndBranch (..), ProjectAndBranchNames (ProjectAndBranchNames'Unambiguous))
import Unison.Runtime.Interface qualified as RTI
import Unison.Server.Backend qualified as Backend
import Unison.Server.CodebaseServer qualified as Server
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.TQueue qualified as Q
import UnliftIO qualified
import UnliftIO.STM
import Prelude hiding (readFile, writeFile)

-- | Render transcript errors at a width of 65 chars.
terminalWidth :: Pretty.Width
terminalWidth = 65

-- | If provided, this access token will be used on all
-- requests which use the Authenticated HTTP Client; i.e. all codeserver interactions.
--
-- It's useful in scripted contexts or when running transcripts against a codeserver.
accessTokenEnvVarKey :: String
accessTokenEnvVarKey = "UNISON_SHARE_ACCESS_TOKEN"

type Runner =
  String ->
  Text ->
  (FilePath, Codebase IO Symbol Ann) ->
  IO (Either Error Text)

withRunner ::
  forall m r.
  (UnliftIO.MonadUnliftIO m) =>
  -- | Whether to treat this transcript run as a transcript test, which will try to make output deterministic
  Bool ->
  Verbosity ->
  UCMVersion ->
  FilePath ->
  (Runner -> m r) ->
  m r
withRunner isTest verbosity ucmVersion nrtp action = do
  withRuntimes nrtp \runtime sbRuntime nRuntime -> do
    action \transcriptName transcriptSrc (codebaseDir, codebase) -> do
      Server.startServer (Backend.BackendEnv {Backend.useNamesIndex = False}) Server.defaultCodebaseServerOpts runtime codebase \baseUrl -> do
        let parsed = Transcript.stanzas transcriptName transcriptSrc
        result <- for parsed \stanzas -> do
          liftIO $ run isTest verbosity codebaseDir stanzas codebase runtime sbRuntime nRuntime ucmVersion (tShow baseUrl)
        pure . join $ first ParseError result
  where
    withRuntimes ::
      FilePath -> (Runtime.Runtime Symbol -> Runtime.Runtime Symbol -> Runtime.Runtime Symbol -> m a) -> m a
    withRuntimes nrtp action =
      RTI.withRuntime False RTI.Persistent ucmVersion \runtime -> do
        RTI.withRuntime True RTI.Persistent ucmVersion \sbRuntime -> do
          action runtime sbRuntime
            =<< liftIO (RTI.startNativeRuntime ucmVersion nrtp)

run ::
  -- | Whether to treat this transcript run as a transcript test, which will try to make output deterministic
  Bool ->
  Verbosity ->
  FilePath ->
  [Stanza] ->
  Codebase IO Symbol Ann ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  UCMVersion ->
  Text ->
  IO (Either Error Text)
run isTest verbosity dir stanzas codebase runtime sbRuntime nRuntime ucmVersion baseURL = UnliftIO.try do
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  (initialPP, emptyCausalHashId) <- Codebase.runTransaction codebase do
    (_, emptyCausalHashId) <- Codebase.emptyCausalHash
    initialPP <- Codebase.expectCurrentProjectPath
    pure (initialPP, emptyCausalHashId)

  unless (isSilent verbosity) . putPrettyLn $
    Pretty.lines
      [ asciiartUnison,
        "",
        "Running the provided transcript file...",
        ""
      ]
  mayShareAccessToken <- fmap Text.pack <$> lookupEnv accessTokenEnvVarKey
  credMan <- AuthN.newCredentialManager
  let tokenProvider :: AuthN.TokenProvider
      tokenProvider =
        case mayShareAccessToken of
          Nothing -> do
            AuthN.newTokenProvider credMan
          Just accessToken ->
            \_codeserverID -> pure $ Right accessToken
  seedRef <- newIORef (0 :: Int)
  -- Queue of Stanzas and Just index, or Nothing if the stanza was programmatically generated
  -- e.g. a unison-file update by a command like 'edit'
  inputQueue <- Q.newIO @(Stanza, Maybe Int)
  -- Queue of UCM commands to run.
  -- Nothing indicates the end of a ucm block.
  cmdQueue <- Q.newIO @(Maybe UcmLine)
  -- Queue of scratch file updates triggered by UCM itself, e.g. via `edit`, `update`, etc.
  ucmScratchFileUpdatesQueue <- Q.newIO @(ScratchFileName, Text)
  unisonFiles <- newIORef Map.empty
  out <- newIORef mempty
  hidden <- newIORef Shown
  allowErrors <- newIORef False
  hasErrors <- newIORef False
  mStanza <- newIORef Nothing
  traverse_ (atomically . Q.enqueue inputQueue) (stanzas `zip` (Just <$> [1 :: Int ..]))
  let patternMap =
        Map.fromList $
          validInputs
            >>= (\p -> (patternName p, p) : ((,p) <$> aliases p))
  let output' :: Bool -> String -> IO ()
      output' inputEcho msg = do
        hide <- readIORef hidden
        unless (hideOutput inputEcho hide) $ modifyIORef' out (\acc -> acc <> pure msg)

      hideOutput :: Bool -> Hidden -> Bool
      hideOutput inputEcho = \case
        Shown -> False
        HideOutput -> True && (not inputEcho)
        HideAll -> True

      output, outputEcho :: String -> IO ()
      output = output' False
      outputEcho = output' True

      apiRequest :: APIRequest -> IO ()
      apiRequest req = do
        output . Text.unpack $ Transcript.formatAPIRequest req <> "\n"
        case req of
          APIComment {} -> pure ()
          GetRequest path -> do
            req <- case HTTP.parseRequest (Text.unpack $ baseURL <> path) of
              Left err -> dieWithMsg (show err)
              Right req -> pure req
            respBytes <- HTTP.httpLbs req httpManager
            case Aeson.eitherDecode (HTTP.responseBody respBytes) of
              Right (v :: Aeson.Value) -> do
                let prettyBytes = Aeson.encodePretty' (Aeson.defConfig {Aeson.confCompare = compare}) v
                output . (<> "\n") . BL.unpack $ prettyBytes
              Left err -> dieWithMsg ("Error decoding response from " <> Text.unpack path <> ": " <> err)

      awaitInput :: Cli (Either Event Input)
      awaitInput = do
        cmd <- atomically (Q.tryDequeue cmdQueue)
        case cmd of
          -- end of ucm block
          Just Nothing -> do
            liftIO (output "\n```\n")
            liftIO dieUnexpectedSuccess
            atomically $ void $ do
              scratchFileUpdates <- Q.flush ucmScratchFileUpdatesQueue
              -- Push them onto the front stanza queue in the correct order.
              for (reverse scratchFileUpdates) \(fp, contents) -> do
                let fenceDescription = "unison :added-by-ucm " <> fp
                -- Output blocks for any scratch file updates the ucm block triggered.
                Q.undequeue inputQueue (Left $ CMarkCodeBlock Nothing fenceDescription contents, Nothing)
            awaitInput
          -- ucm command to run
          Just (Just ucmLine) -> do
            case ucmLine of
              p@(UcmComment {}) -> do
                liftIO . output . Text.unpack $ "\n" <> Transcript.formatUcmLine p
                awaitInput
              p@(UcmCommand context lineTxt) -> do
                curPath <- Cli.getCurrentProjectPath
                -- We're either going to run the command now (because we're in the right context), else we'll switch to
                -- the right context first, then run the command next.
                maybeSwitchCommand <-
                  case context of
                    UcmContextProject (ProjectAndBranch projectName branchName) -> Cli.runTransaction do
                      Project {projectId, name = projectName} <-
                        Q.loadProjectByName projectName
                          >>= \case
                            Nothing -> do
                              projectId <- Sqlite.unsafeIO (Db.ProjectId <$> UUID.nextRandom)
                              Q.insertProject projectId projectName
                              pure $ Project {projectId, name = projectName}
                            Just project -> pure project
                      projectBranch <-
                        Q.loadProjectBranchByName projectId branchName >>= \case
                          Nothing -> do
                            branchId <- Sqlite.unsafeIO (Db.ProjectBranchId <$> UUID.nextRandom)
                            let projectBranch = ProjectBranch {projectId, parentBranchId = Nothing, branchId, name = branchName}
                            Q.insertProjectBranch "Branch Created" emptyCausalHashId projectBranch
                            pure projectBranch
                          Just projBranch -> pure projBranch
                      let projectAndBranchIds = ProjectAndBranch projectBranch.projectId projectBranch.branchId
                      pure
                        if (PP.toProjectAndBranch . PP.toIds $ curPath) == projectAndBranchIds
                          then Nothing
                          else Just (ProjectSwitchI (ProjectAndBranchNames'Unambiguous (These projectName branchName)))
                case maybeSwitchCommand of
                  Just switchCommand -> do
                    atomically $ Q.undequeue cmdQueue (Just p)
                    pure (Right switchCommand)
                  Nothing -> do
                    case words . Text.unpack $ lineTxt of
                      [] -> awaitInput
                      args -> do
                        liftIO . output . Text.unpack $ "\n" <> Transcript.formatUcmLine p <> "\n"
                        numberedArgs <- use #numberedArgs
                        PP.ProjectAndBranch projId branchId <- PP.toProjectAndBranch . NonEmpty.head <$> use #projectPathStack
                        let getProjectRoot = liftIO $ Codebase.expectProjectBranchRoot codebase projId branchId
                        liftIO (parseInput codebase curPath getProjectRoot numberedArgs patternMap args) >>= \case
                          -- invalid command is treated as a failure
                          Left msg -> do
                            liftIO $ writeIORef hasErrors True
                            liftIO (readIORef allowErrors) >>= \case
                              True -> do
                                liftIO (output . Pretty.toPlain terminalWidth $ ("\n" <> msg <> "\n"))
                                awaitInput
                              False -> do
                                liftIO (dieWithMsg $ Pretty.toPlain terminalWidth msg)
                          -- No input received from this line, try again.
                          Right Nothing -> awaitInput
                          Right (Just (_expandedArgs, input)) -> pure $ Right input
          Nothing -> do
            liftIO (dieUnexpectedSuccess)
            liftIO (writeIORef hidden Shown)
            liftIO (writeIORef allowErrors False)
            maybeStanza <- atomically (Q.tryDequeue inputQueue)
            _ <- liftIO (writeIORef mStanza maybeStanza)
            case maybeStanza of
              Nothing -> liftIO do
                clearCurrentLine
                putStrLn "\r✔️   Completed transcript."
                pure $ Right QuitI
              Just (s, midx) -> do
                unless (Verbosity.isSilent verbosity) . liftIO $ do
                  clearCurrentLine
                  putStr $
                    maybe
                      "\r⏩   Skipping non-executable Markdown block."
                      ( \idx ->
                          "\r⚙️   Processing stanza "
                            ++ show idx
                            ++ " of "
                            ++ show (length stanzas)
                            ++ "."
                      )
                      midx
                  IO.hFlush IO.stdout
                either
                  ( \node -> do
                      liftIO . output . Text.unpack $ Transcript.formatNode node
                      awaitInput
                  )
                  ( \block -> case block of
                      Unison hide errOk filename txt -> do
                        liftIO (writeIORef hidden hide)
                        liftIO . outputEcho . Text.unpack $ Transcript.formatProcessedBlock block
                        liftIO (writeIORef allowErrors errOk)
                        -- Open a ucm block which will contain the output from UCM
                        -- after processing the UnisonFileChanged event.
                        liftIO (output "``` ucm\n")
                        -- Close the ucm block after processing the UnisonFileChanged event.
                        atomically . Q.enqueue cmdQueue $ Nothing
                        let sourceName = fromMaybe "scratch.u" filename
                        liftIO $ updateVirtualFile sourceName txt
                        pure $ Left (UnisonFileChanged sourceName txt)
                      API apiRequests -> do
                        liftIO (output "``` api\n")
                        liftIO (for_ apiRequests apiRequest)
                        liftIO (output "```\n\n")
                        awaitInput
                      Ucm hide errOk cmds -> do
                        liftIO (writeIORef hidden hide)
                        liftIO (writeIORef allowErrors errOk)
                        liftIO (writeIORef hasErrors False)
                        liftIO (output "``` ucm")
                        traverse_ (atomically . Q.enqueue cmdQueue . Just) cmds
                        atomically . Q.enqueue cmdQueue $ Nothing
                        awaitInput
                  )
                  s

      loadPreviousUnisonBlock name = do
        ufs <- readIORef unisonFiles
        case Map.lookup name ufs of
          Just uf ->
            return (Cli.LoadSuccess uf)
          Nothing ->
            -- This lets transcripts use the `load` command, as in:
            --
            -- .> load someFile.u
            --
            -- Important for Unison syntax that can't be embedded in
            -- transcripts (like docs, which use ``` in their syntax).
            let f = Cli.LoadSuccess <$> readUtf8 (Text.unpack name)
             in f <|> pure Cli.InvalidSourceNameError

      writeSource :: ScratchFileName -> Text -> Bool -> IO ()
      writeSource fp contents _addFold = do
        shouldShowSourceChanges <- (== Shown) <$> readIORef hidden
        when shouldShowSourceChanges $ do
          atomically (Q.enqueue ucmScratchFileUpdatesQueue (fp, contents))
        updateVirtualFile fp contents

      updateVirtualFile :: ScratchFileName -> Text -> IO ()
      updateVirtualFile fp contents = do
        liftIO (modifyIORef' unisonFiles (Map.insert fp contents))

      print :: Output.Output -> IO ()
      print o = do
        msg <- notifyUser dir o
        errOk <- readIORef allowErrors
        let rendered = Pretty.toPlain terminalWidth (Pretty.border 2 msg)
        output rendered
        when (Output.isFailure o) $
          if errOk
            then writeIORef hasErrors True
            else dieWithMsg rendered

      printNumbered :: Output.NumberedOutput -> IO Output.NumberedArgs
      printNumbered o = do
        let (msg, numberedArgs) = notifyNumbered o
        errOk <- readIORef allowErrors
        let rendered = Pretty.toPlain terminalWidth (Pretty.border 2 msg)
        output rendered
        when (Output.isNumberedFailure o) $
          if errOk
            then writeIORef hasErrors True
            else dieWithMsg rendered
        pure numberedArgs

      -- Looks at the current stanza and decides if it is contained in the
      -- output so far. Appends it if not.
      appendFailingStanza :: IO ()
      appendFailingStanza = do
        stanzaOpt <- readIORef mStanza
        currentOut <- readIORef out
        let stnz = maybe "" (Text.unpack . Transcript.formatStanza . fst) stanzaOpt
        unless (stnz `isSubsequenceOf` concat currentOut) $
          modifyIORef' out (\acc -> acc <> pure stnz)

      -- output ``` and new lines then call transcriptFailure
      dieWithMsg :: forall a. String -> IO a
      dieWithMsg msg = do
        output "\n```\n\n"
        appendFailingStanza
        transcriptFailure out $
          "The transcript failed due to an error in the stanza above. The error is:\n\n" <> Text.pack msg

      dieUnexpectedSuccess :: IO ()
      dieUnexpectedSuccess = do
        errOk <- readIORef allowErrors
        hasErr <- readIORef hasErrors
        when (errOk && not hasErr) $ do
          output "\n```\n\n"
          appendFailingStanza
          transcriptFailure out "The transcript was expecting an error in the stanza above, but did not encounter one."

  authenticatedHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion

  let env =
        Cli.Env
          { authHTTPClient = authenticatedHTTPClient,
            codebase,
            credentialManager = credMan,
            generateUniqueName = do
              i <- atomicModifyIORef' seedRef \i -> let !i' = i + 1 in (i', i)
              pure (Parser.uniqueBase32Namegen (Random.drgNewSeed (Random.seedFromInteger (fromIntegral i)))),
            loadSource = loadPreviousUnisonBlock,
            writeSource,
            notify = print,
            notifyNumbered = printNumbered,
            runtime,
            sandboxedRuntime = sbRuntime,
            nativeRuntime = nRuntime,
            serverBaseUrl = Nothing,
            ucmVersion,
            isTranscriptTest = isTest
          }

  let loop :: Cli.LoopState -> IO Text
      loop s0 = do
        Cli.runCli env s0 awaitInput >>= \case
          (Cli.Success input, s1) -> do
            let next s =
                  loop case input of
                    Left _ -> s
                    Right inp -> s & #lastInput ?~ inp
            Cli.runCli env s1 (HandleInput.loop input) >>= \case
              (Cli.Success (), s2) -> next s2
              (Cli.Continue, s2) -> next s2
              (Cli.HaltRepl, _) -> onHalt
          (Cli.Continue, s1) -> loop s1
          (Cli.HaltRepl, _) -> onHalt
        where
          onHalt = do
            texts <- readIORef out
            pure $ Text.concat (Text.pack <$> toList (texts :: Seq String))

  loop (Cli.loopState0 (PP.toIds initialPP))

transcriptFailure :: IORef (Seq String) -> Text -> IO b
transcriptFailure out msg = do
  texts <- readIORef out
  UnliftIO.throwIO . RunFailure $ mconcat (Text.pack <$> toList texts) <> "\n\n\128721\n\n" <> msg <> "\n"

data Error
  = ParseError (P.ParseErrorBundle Text Void)
  | RunFailure Text
  deriving stock (Show)
  deriving anyclass (Exception)
