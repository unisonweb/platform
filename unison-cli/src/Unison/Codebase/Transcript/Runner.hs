{-# LANGUAGE DeriveAnyClass #-}

-- | Execute transcripts.
module Unison.Codebase.Transcript.Runner
  ( Error (..),
    Runner,
    withRunner,
  )
where

import CMark qualified
import Control.Lens (use, (?~))
import Crypto.Random qualified as Random
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IORef
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
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
  IO (Either Error (Seq Stanza))

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
withRunner isTest verbosity ucmVersion nrtp action =
  withRuntimes nrtp \runtime sbRuntime nRuntime ->
    action \transcriptName transcriptSrc (codebaseDir, codebase) ->
      Server.startServer
        Backend.BackendEnv {Backend.useNamesIndex = False}
        Server.defaultCodebaseServerOpts
        runtime
        codebase
        \baseUrl ->
          either
            (pure . Left . ParseError)
            (run isTest verbosity codebaseDir codebase runtime sbRuntime nRuntime ucmVersion $ tShow baseUrl)
            $ Transcript.stanzas transcriptName transcriptSrc
  where
    withRuntimes ::
      FilePath -> (Runtime.Runtime Symbol -> Runtime.Runtime Symbol -> Runtime.Runtime Symbol -> m a) -> m a
    withRuntimes nrtp action =
      RTI.withRuntime False RTI.Persistent ucmVersion \runtime ->
        RTI.withRuntime True RTI.Persistent ucmVersion \sbRuntime ->
          action runtime sbRuntime =<< liftIO (RTI.startNativeRuntime ucmVersion nrtp)

isGeneratedBlock :: ProcessedBlock -> Bool
isGeneratedBlock = \case
  Ucm InfoTags {generated} _ -> generated
  Unison InfoTags {generated} _ -> generated
  API InfoTags {generated} _ -> generated

run ::
  -- | Whether to treat this transcript run as a transcript test, which will try to make output deterministic
  Bool ->
  Verbosity ->
  FilePath ->
  Codebase IO Symbol Ann ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  Runtime.Runtime Symbol ->
  UCMVersion ->
  Text ->
  [Stanza] ->
  IO (Either Error (Seq Stanza))
run isTest verbosity dir codebase runtime sbRuntime nRuntime ucmVersion baseURL stanzas = UnliftIO.try do
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  (initialPP, emptyCausalHashId) <-
    Codebase.runTransaction codebase . liftA2 (,) Codebase.expectCurrentProjectPath $ snd <$> Codebase.emptyCausalHash

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
        maybe
          (AuthN.newTokenProvider credMan)
          (\accessToken _codeserverID -> pure $ Right accessToken)
          mayShareAccessToken
  -- Queue of Stanzas and Just index, or Nothing if the stanza was programmatically generated
  -- e.g. a unison-file update by a command like 'edit'
  inputQueue <-
    Q.prepopulatedIO . Seq.fromList $
      filter (either (const True) (not . isGeneratedBlock)) stanzas `zip` (Just <$> [1 :: Int ..])
  -- Queue of UCM commands to run.
  -- Nothing indicates the end of a ucm block.
  cmdQueue <- Q.newIO @(Maybe UcmLine)
  -- Queue of scratch file updates triggered by UCM itself, e.g. via `edit`, `update`, etc.
  ucmScratchFileUpdatesQueue <- Q.newIO @(ScratchFileName, Text)
  ucmOutput <- newIORef mempty
  unisonFiles <- newIORef Map.empty
  out <- newIORef mempty
  currentTags <- newIORef Nothing
  isHidden <- newIORef Shown
  allowErrors <- newIORef False
  expectFailure <- newIORef False
  hasErrors <- newIORef False
  mBlock <- newIORef Nothing
  let patternMap = Map.fromList $ (\p -> (patternName p, p) : ((,p) <$> aliases p)) =<< validInputs
  let output' :: Bool -> Stanza -> IO ()
      output' inputEcho msg = do
        hide <- hideOutput inputEcho
        unless hide $ modifyIORef' out (<> pure msg)

      hideOutput' :: Bool -> Hidden -> Bool
      hideOutput' inputEcho = \case
        Shown -> False
        HideOutput -> not inputEcho
        HideAll -> True

      hideOutput :: Bool -> IO Bool
      hideOutput inputEcho = hideOutput' inputEcho <$> readIORef isHidden

      output, outputEcho :: Stanza -> IO ()
      output = output' False
      outputEcho = output' True

      outputUcmLine :: UcmLine -> IO ()
      outputUcmLine line = do
        prev <- readIORef ucmOutput
        modifyIORef' ucmOutput (<> ((if not (null prev) then pure (UcmOutputLine "\n") else mempty) <> pure line))

      outputUcmResult :: Pretty.Pretty Pretty.ColorText -> IO ()
      outputUcmResult line = do
        hide <- hideOutput False
        unless hide . outputUcmLine . UcmOutputLine . Text.pack $
          -- We shorten the terminal width, because "Transcript" manages a 2-space indent for output lines.
          Pretty.toPlain (terminalWidth - 2) line

      maybeDieWithMsg :: Pretty.Pretty Pretty.ColorText -> IO ()
      maybeDieWithMsg msg = do
        liftIO $ writeIORef hasErrors True
        liftIO (liftA2 (,) (readIORef allowErrors) (readIORef expectFailure)) >>= \case
          (False, False) -> liftIO . dieWithMsg $ Pretty.toPlain terminalWidth msg
          (True, True) ->
            transcriptFailure
              out
              ( Text.pack . Pretty.toPlain terminalWidth $
                  Pretty.lines
                    [ "The stanza above marked with `:error :bug` is now failing with",
                      Pretty.border 2 msg,
                      "so you can remove `:bug` and close any appropriate Github issues. If the error message is \
                      \different from the expected error message, open a new issue and reference it in this transcript."
                    ]
              )
              Nothing
          (_, _) -> pure ()

      apiRequest :: APIRequest -> IO [APIRequest]
      apiRequest req = do
        hide <- hideOutput False
        case req of
          -- We just discard this, because the runner will produce new output lines.
          APIResponseLine {} -> pure []
          APIComment {} -> pure $ pure req
          GetRequest path ->
            either
              (([] <$) . maybeDieWithMsg . Pretty.string . show)
              ( either
                  ( ([] <$)
                      . maybeDieWithMsg
                      . (("Error decoding response from " <> Pretty.text path <> ": ") <>)
                      . Pretty.string
                  )
                  ( \(v :: Aeson.Value) ->
                      pure $
                        if hide
                          then [req]
                          else
                            [ req,
                              APIResponseLine . Text.pack . BL.unpack $
                                Aeson.encodePretty' (Aeson.defConfig {Aeson.confCompare = compare}) v
                            ]
                  )
                  . Aeson.eitherDecode
                  . HTTP.responseBody
                  <=< flip HTTP.httpLbs httpManager
              )
              . HTTP.parseRequest
              . Text.unpack
              $ baseURL <> path

      endUcmBlock = do
        liftIO $ do
          tags <- readIORef currentTags
          ucmOut <- readIORef ucmOutput
          unless (null ucmOut && tags == Nothing) . outputEcho . pure $
            Ucm (fromMaybe defaultInfoTags' {generated = True} tags) ucmOut
          writeIORef ucmOutput []
          dieUnexpectedSuccess
        atomically $ void $ do
          scratchFileUpdates <- Q.flush ucmScratchFileUpdatesQueue
          -- Push them onto the front stanza queue in the correct order.
          for (reverse scratchFileUpdates) \(fp, contents) ->
            -- Output blocks for any scratch file updates the ucm block triggered.
            Q.undequeue inputQueue (pure $ Unison (defaultInfoTags $ pure fp) {generated = True} contents, Nothing)
        Cli.returnEarlyWithoutOutput

      processUcmLine p =
        case p of
          -- We just discard this, because the runner will produce new output lines.
          UcmOutputLine {} -> Cli.returnEarlyWithoutOutput
          UcmComment {} -> do
            liftIO $ outputUcmLine p
            Cli.returnEarlyWithoutOutput
          UcmCommand context lineTxt -> do
            curPath <- Cli.getCurrentProjectPath
            -- We're either going to run the command now (because we're in the right context), else we'll switch to
            -- the right context first, then run the command next.
            maybeSwitchCommand <- case context of
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
                      let projectBranch =
                            ProjectBranch {projectId, parentBranchId = Nothing, branchId, name = branchName}
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
                atomically . Q.undequeue cmdQueue $ Just p
                pure $ Right switchCommand
              Nothing -> do
                case words . Text.unpack $ lineTxt of
                  [] -> Cli.returnEarlyWithoutOutput
                  args -> do
                    liftIO $ outputUcmLine p
                    numberedArgs <- use #numberedArgs
                    PP.ProjectAndBranch projId branchId <-
                      PP.toProjectAndBranch . NonEmpty.head <$> use #projectPathStack
                    let getProjectRoot = liftIO $ Codebase.expectProjectBranchRoot codebase projId branchId
                    liftIO (parseInput codebase curPath getProjectRoot numberedArgs patternMap args)
                      >>= either
                        -- invalid command is treated as a failure
                        ( \msg -> do
                            liftIO $ outputUcmResult msg
                            liftIO $ maybeDieWithMsg msg
                            Cli.returnEarlyWithoutOutput
                        )
                        -- No input received from this line, try again.
                        (maybe Cli.returnEarlyWithoutOutput $ pure . Right . snd)

      startProcessedBlock block = case block of
        Unison infoTags txt -> do
          liftIO do
            writeIORef isHidden $ hidden infoTags
            outputEcho $ pure block
            writeIORef allowErrors $ expectingError infoTags
            writeIORef expectFailure $ hasBug infoTags
          -- Open a ucm block which will contain the output from UCM after processing the `UnisonFileChanged` event.
          -- Close the ucm block after processing the UnisonFileChanged event.
          atomically . Q.enqueue cmdQueue $ Nothing
          let sourceName = fromMaybe "scratch.u" $ additionalTags infoTags
          liftIO $ updateVirtualFile sourceName txt
          pure . Left $ UnisonFileChanged sourceName txt
        API infoTags apiRequests -> do
          liftIO do
            writeIORef isHidden $ hidden infoTags
            writeIORef allowErrors $ expectingError infoTags
            writeIORef expectFailure $ hasBug infoTags
            outputEcho . pure . API infoTags . fold =<< traverse apiRequest apiRequests
          Cli.returnEarlyWithoutOutput
        Ucm infoTags cmds -> do
          liftIO do
            writeIORef currentTags $ pure infoTags
            writeIORef isHidden $ hidden infoTags
            writeIORef allowErrors $ expectingError infoTags
            writeIORef expectFailure $ hasBug infoTags
            writeIORef hasErrors False
          traverse_ (atomically . Q.enqueue cmdQueue . Just) cmds
          atomically . Q.enqueue cmdQueue $ Nothing
          Cli.returnEarlyWithoutOutput

      showStatus alwaysShow indicator msg = unless (not alwaysShow && Verbosity.isSilent verbosity) do
        clearCurrentLine
        putStr $ "\r" <> indicator <> "   " <> msg
        IO.hFlush IO.stdout

      finishTranscript = do
        showStatus True "✔️" "Completed transcript.\n"
        pure $ Right QuitI

      processStanza stanza midx = do
        liftIO . showStatus False "⚙️" $
          maybe
            "Processing UCM-generated stanza."
            (\idx -> "Processing stanza " <> show idx <> " of " <> show (length stanzas) <> ".")
            midx
        either
          (bypassStanza . Left)
          ( \block ->
              if isGeneratedBlock block
                then bypassStanza $ pure block
                else do
                  liftIO . writeIORef mBlock $ pure block
                  startProcessedBlock block
          )
          stanza

      bypassStanza stanza = do
        liftIO $ output stanza
        Cli.returnEarlyWithoutOutput

      whatsNext = do
        liftIO dieUnexpectedSuccess
        liftIO $ writeIORef currentTags Nothing
        liftIO $ writeIORef isHidden Shown
        liftIO $ writeIORef allowErrors False
        liftIO $ writeIORef expectFailure False
        maybe (liftIO finishTranscript) (uncurry processStanza) =<< atomically (Q.tryDequeue inputQueue)

      awaitInput :: Cli (Either Event Input)
      awaitInput = maybe whatsNext (maybe endUcmBlock processUcmLine) =<< atomically (Q.tryDequeue cmdQueue)

      loadPreviousUnisonBlock name =
        maybe
          -- This lets transcripts use the `load` command, as in:
          --
          -- .> load someFile.u
          (fmap Cli.LoadSuccess (readUtf8 $ Text.unpack name) <|> pure Cli.InvalidSourceNameError)
          (pure . Cli.LoadSuccess)
          . Map.lookup name
          =<< readIORef unisonFiles

      writeSource :: ScratchFileName -> Text -> Bool -> IO ()
      writeSource fp contents _addFold = do
        shouldShowSourceChanges <- (== Shown) <$> readIORef isHidden
        when shouldShowSourceChanges . atomically $ Q.enqueue ucmScratchFileUpdatesQueue (fp, contents)
        updateVirtualFile fp contents

      updateVirtualFile :: ScratchFileName -> Text -> IO ()
      updateVirtualFile fp = modifyIORef' unisonFiles . Map.insert fp

      print :: Output.Output -> IO ()
      print o = do
        msg <- notifyUser dir o
        outputUcmResult msg
        when (Output.isFailure o) $ maybeDieWithMsg msg

      printNumbered :: Output.NumberedOutput -> IO Output.NumberedArgs
      printNumbered o = do
        let (msg, numberedArgs) = notifyNumbered o
        outputUcmResult msg
        when (Output.isNumberedFailure o) $ maybeDieWithMsg msg
        pure numberedArgs

      -- Looks at the current stanza and decides if it is contained in the
      -- output so far. Appends it if not.
      appendFailingStanza :: IO ()
      appendFailingStanza = do
        blockOpt <- readIORef mBlock
        currentOut <- readIORef out
        maybe
          (pure ())
          (\block -> unless (elem (pure block) currentOut) $ modifyIORef' out (<> pure (pure block)))
          blockOpt

      dieWithMsg :: forall a. String -> IO a
      dieWithMsg msg = do
        appendFailingStanza
        transcriptFailure out "The transcript failed due to an error in the stanza above. The error is:" . pure $
          Text.pack msg

      dieUnexpectedSuccess :: IO ()
      dieUnexpectedSuccess = do
        errOk <- readIORef allowErrors
        expectBug <- readIORef expectFailure
        hasErr <- readIORef hasErrors
        case (errOk, expectBug, hasErr) of
          (True, False, False) -> do
            appendFailingStanza
            transcriptFailure
              out
              "The transcript was expecting an error in the stanza above, but did not encounter one."
              Nothing
          (False, True, False) -> do
            appendFailingStanza
            transcriptFailure
              out
              "The stanza above with `:bug` is now passing! You can remove `:bug` and close any appropriate Github \
              \issues."
              Nothing
          (_, _, _) -> pure ()

  authenticatedHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion

  seedRef <- newIORef (0 :: Int)

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

  let loop :: Cli.LoopState -> IO (Seq Stanza)
      loop s0 = do
        Cli.runCli env s0 awaitInput >>= \case
          (Cli.Success input, s1) ->
            let next s = loop $ either (const s) (\inp -> s & #lastInput ?~ inp) input
             in Cli.runCli env s1 (HandleInput.loop input) >>= \case
                  (Cli.Success (), s2) -> next s2
                  (Cli.Continue, s2) -> next s2
                  (Cli.HaltRepl, _) -> onHalt
          (Cli.Continue, s1) -> loop s1
          (Cli.HaltRepl, _) -> onHalt
        where
          onHalt = readIORef out

  loop (Cli.loopState0 (PP.toIds initialPP))

transcriptFailure :: IORef (Seq Stanza) -> Text -> Maybe Text -> IO b
transcriptFailure out heading mbody = do
  texts <- readIORef out
  UnliftIO.throwIO . RunFailure $
    texts
      <> Seq.fromList
        ( Left
            <$> [ CMark.Node Nothing CMark.PARAGRAPH [CMark.Node Nothing (CMark.TEXT "🛑") []],
                  CMark.Node Nothing CMark.PARAGRAPH [CMark.Node Nothing (CMark.TEXT heading) []]
                ]
              <> foldr ((:) . CMarkCodeBlock Nothing "") [] mbody
        )

data Error
  = ParseError (P.ParseErrorBundle Text Void)
  | RunFailure (Seq Stanza)
  deriving stock (Show)
  deriving anyclass (Exception)
