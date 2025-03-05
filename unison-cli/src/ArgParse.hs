{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module handles parsing CLI arguments into 'Command's.
-- See the excellent documentation at https://hackage.haskell.org/package/optparse-applicative
module ArgParse where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Options.Applicative
  ( CommandFields,
    Mod,
    ParseError (ShowHelpText),
    Parser,
    ParserInfo,
    ParserPrefs,
    ReadM,
    action,
    argument,
    auto,
    columns,
    command,
    customExecParser,
    flag,
    footerDoc,
    fullDesc,
    headerDoc,
    help,
    helpShowGlobals,
    helper,
    hsubparser,
    info,
    infoOption,
    long,
    maybeReader,
    metavar,
    option,
    parserFailure,
    prefs,
    progDesc,
    renderFailure,
    short,
    showHelpOnError,
    strArgument,
    strOption,
    subparserInline,
  )
import Options.Applicative qualified as OptParse
import Options.Applicative.Builder.Internal (noGlobal {- https://github.com/pcapriotti/optparse-applicative/issues/461 -})
import Options.Applicative.Help (bold, (<+>))
import Options.Applicative.Help.Pretty qualified as P
import Stats
import System.Environment (lookupEnv)
import Text.Megaparsec qualified as Megaparsec
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Codebase.ProjectPath (ProjectPathNames)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.CommandLine.Types (ShouldWatchFiles (..))
import Unison.Core.Project (ProjectAndBranch, ProjectBranchName, ProjectName)
import Unison.HashQualified (HashQualified)
import Unison.LSP (LspFormattingConfig (..))
import Unison.Name (Name)
import Unison.Prelude
import Unison.PrettyTerminal qualified as PT
import Unison.Project qualified as Project
import Unison.Server.CodebaseServer (CodebaseServerOpts (..))
import Unison.Server.CodebaseServer qualified as Server
import Unison.Syntax.HashQualified qualified as HQ
import Unison.Util.Pretty (Width (..))

-- | Valid ways to provide source code to the run command
data RunSource
  = RunFromPipe (HashQualified Name)
  | RunFromSymbol ProjectPathNames
  | RunFromFile FilePath (HashQualified Name)
  | RunCompiled FilePath
  deriving (Show, Eq)

data ShouldForkCodebase
  = UseFork
  | DontFork
  deriving (Show, Eq)

data ShouldSaveCodebase
  = SaveCodebase (Maybe FilePath)
  | DontSaveCodebase
  deriving (Show, Eq)

data CodebasePathOption
  = CreateCodebaseWhenMissing FilePath
  | DontCreateCodebaseWhenMissing FilePath
  deriving (Show, Eq)

data ShouldExit = Exit | DoNotExit
  deriving (Show, Eq)

data IsHeadless = Headless | WithCLI
  deriving (Show, Eq)

-- | Represents commands the cli can run.
--
-- Note that this is not one-to-one with command-parsers since some are simple variants.
-- E.g. run, run.file, run.pipe
data Command
  = Launch
      IsHeadless
      CodebaseServerOpts
      -- Starting project
      (Maybe (ProjectAndBranch ProjectName ProjectBranchName))
      ShouldWatchFiles
  | PrintVersion
  | -- @deprecated in trunk after M2g. Remove the Init command completely after M2h has been released
    Init
  | Run RunSource [String]
  | Transcript ShouldForkCodebase ShouldSaveCodebase (Maybe RtsStatsPath) (NonEmpty FilePath)
  deriving (Show, Eq)

-- | Options shared by sufficiently many subcommands.
data GlobalOptions = GlobalOptions
  { codebasePathOption :: Maybe CodebasePathOption,
    exitOption :: ShouldExit,
    nativeRuntimePath :: Maybe FilePath,
    lspFormattingConfig :: LspFormattingConfig
  }
  deriving (Show, Eq)

-- | The root-level 'ParserInfo'.
rootParserInfo :: String -> String -> CodebaseServerOpts -> ParserInfo (GlobalOptions, Command)
rootParserInfo progName version envOpts =
  info
    (helper <*> versionOptionParser progName version <*> ((,) <$> globalOptionsParser <*> commandParser envOpts))
    ( fullDesc
        <> headerDoc (Just $ unisonHelp progName version)
    )

type UsageRenderer =
  -- | Optional sub-command to render help for
  Maybe String ->
  String

-- | Parse the command description, options, and usage information from provided cli arguments.
parseCLIArgs :: String -> String -> IO (UsageRenderer, GlobalOptions, Command)
parseCLIArgs progName version = do
  (Width cols) <- PT.getAvailableWidth
  envOpts <- codebaseServerOptsFromEnv
  let parserInfo = rootParserInfo progName version envOpts
  let preferences = prefs $ showHelpOnError <> helpShowGlobals <> columns cols <> subparserInline
  let usage = renderUsage progName parserInfo preferences
  (globalOptions, command) <- customExecParser preferences parserInfo
  pure $ (usage, globalOptions, command)

-- | Load default options from environment variables.
codebaseServerOptsFromEnv :: IO CodebaseServerOpts
codebaseServerOptsFromEnv = do
  token <- lookupEnv Server.ucmTokenVar
  host <- lookupEnv Server.ucmHostVar
  allowCorsHost <- lookupEnv Server.ucmAllowCorsHost
  port <- lookupEnv Server.ucmPortVar <&> (>>= readMaybe)
  codebaseUIPath <- lookupEnv Server.ucmUIVar
  pure $ CodebaseServerOpts {..}

-- | Purely renders the full help summary for the CLI, or an optional subcommand.
renderUsage :: String -> ParserInfo a -> ParserPrefs -> Maybe String -> String
renderUsage programName pInfo preferences subCommand =
  let showHelpFailure = parserFailure preferences pInfo (ShowHelpText subCommand) mempty
      (helpText, _exitCode) = renderFailure showHelpFailure programName
   in helpText

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info versionParser (fullDesc <> progDesc "Print the version of unison you're running"))

initCommand :: Mod CommandFields Command
initCommand = command "init" (info initParser (progDesc initHelp))
  where
    initHelp =
      "This command is has been removed. Use --codebase-create instead to create a codebase in the specified directory when starting the UCM."

runDesc :: String -> String -> String
runDesc cmd location =
  "Execute a definition from "
    <> location
    <> ", passing on the provided arguments. "
    <> " To pass flags to your program, use `"
    <> cmd
    <> " -- --my-flag`"

runSymbolCommand :: Mod CommandFields Command
runSymbolCommand =
  command "run" (info runSymbolParser (fullDesc <> progDesc help))
  where
    help =
      "Execute a definition from the codebase, passing on the provided arguments. "
        <> " To pass flags to your program, use `run <symbol> -- --my-flag`"

runFileCommand :: Mod CommandFields Command
runFileCommand =
  command "run.file" (info runFileParser (fullDesc <> progDesc help))
  where
    help =
      "Execute a definition from a file, passing on the provided arguments. "
        <> " To pass flags to your program, use `run.file <file> -- --my-flag`"

runPipeCommand :: Mod CommandFields Command
runPipeCommand =
  command "run.pipe" (info runPipeParser (fullDesc <> progDesc help))
  where
    help =
      "Execute a definition from stdin, passing on the provided arguments. "
        <> " To pass flags to your program, use `run -- --my-flag`"

runCompiledCommand :: Mod CommandFields Command
runCompiledCommand =
  command "run.compiled" (info runCompiledParser (fullDesc <> progDesc help))
  where
    help =
      "Execute a definition from a previously compiled file, passing on the provided arguments. "
        <> " To pass flags to your program, use `run <file> -- --my-flag`"

transcriptCommand :: Mod CommandFields Command
transcriptCommand =
  command "transcript" (info transcriptParser (fullDesc <> progDesc transcriptHelp <> footerDoc transcriptFooter))
  where
    transcriptHelp = "Execute transcript markdown files"
    transcriptFooter =
      Just . fold . List.intersperse P.line $
        [ "For each <transcript>.md file provided this executes the transcript and creates" <+> P.annotate bold "<transcript>.output.md" <+> "if successful.",
          "Exits after completion, and deletes the temporary directory created, unless --save-codebase is provided",
          "Multiple transcript files may be provided; they are processed in sequence" <+> "starting from the same codebase."
        ]

transcriptForkCommand :: Mod CommandFields Command
transcriptForkCommand =
  command "transcript.fork" (info transcriptForkParser (fullDesc <> progDesc transcriptHelp <> footerDoc transcriptFooter))
  where
    transcriptHelp = "Execute transcript markdown files in a sandboxed codebase"
    transcriptFooter =
      Just . fold . List.intersperse P.line $
        [ "For each <transcript>.md file provided this executes the transcript in a sandbox codebase and creates" <+> P.annotate bold "<transcript>.output.md" <+> "if successful.",
          "Exits after completion, and deletes the temporary directory created, unless --save-codebase is provided",
          "Multiple transcript files may be provided; they are processed in sequence" <+> "starting from the same codebase."
        ]

commandParser :: CodebaseServerOpts -> Parser Command
commandParser envOpts =
  hsubparser commands <|> launchParser envOpts WithCLI
  where
    commands =
      fold
        [ versionCommand,
          initCommand,
          runSymbolCommand,
          runCompiledCommand,
          runFileCommand,
          runPipeCommand,
          transcriptCommand,
          transcriptForkCommand,
          launchHeadlessCommand envOpts
        ]

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = do
  -- ApplicativeDo
  codebasePathOption <- codebasePathParser <|> codebaseCreateParser
  exitOption <- exitParser
  nativeRuntimePath <- nativeRuntimePathFlag
  lspFormattingConfig <- lspFormattingParser

  pure
    GlobalOptions {codebasePathOption, exitOption, nativeRuntimePath, lspFormattingConfig}

codebasePathParser :: Parser (Maybe CodebasePathOption)
codebasePathParser = do
  optString <-
    optional . strOption $
      long "codebase"
        <> short 'c'
        <> metavar "CODEBASE/PATH"
        <> help "The path to an existing codebase"
  pure (fmap DontCreateCodebaseWhenMissing optString)

codebaseCreateParser :: Parser (Maybe CodebasePathOption)
codebaseCreateParser = do
  path <-
    optional . strOption $
      long "codebase-create"
        <> short 'C'
        <> metavar "CODEBASE/PATH"
        <> help "The path to a new or existing codebase (one will be created if there isn't one)"
  pure (fmap CreateCodebaseWhenMissing path)

exitParser :: Parser ShouldExit
exitParser = flag DoNotExit Exit (long "exit" <> help exitHelp)
  where
    exitHelp = "Exit repl after the command."

lspFormattingParser :: Parser LspFormattingConfig
lspFormattingParser = flag LspFormatDisabled LspFormatEnabled (long "lsp-format" <> help lspFormatHelp)
  where
    lspFormatHelp = "[Experimental] Enable formatting of source files via LSP."

versionOptionParser :: String -> String -> Parser (a -> a)
versionOptionParser progName version =
  infoOption (progName <> " version: " <> version) (short 'v' <> long "version" <> help "Show version")

launchHeadlessCommand :: CodebaseServerOpts -> Mod CommandFields Command
launchHeadlessCommand envOpts =
  command "headless" (info (launchParser envOpts Headless) (progDesc headlessHelp))
  where
    headlessHelp = "Runs the codebase server without the command-line interface."

codebaseServerOptsParser :: CodebaseServerOpts -> Parser CodebaseServerOpts
codebaseServerOptsParser envOpts = do
  -- ApplicativeDo
  cliToken <- tokenFlag <|> pure (token envOpts)
  cliHost <- hostFlag <|> pure (host envOpts)
  cliPort <- portFlag <|> pure (port envOpts)
  cliAllowCorsHost <- allowCorsHostFlag <|> pure (allowCorsHost envOpts)
  cliCodebaseUIPath <- codebaseUIPathFlag <|> pure (codebaseUIPath envOpts)
  pure
    CodebaseServerOpts
      { token = cliToken <|> token envOpts,
        host = cliHost <|> host envOpts,
        port = cliPort <|> port envOpts,
        allowCorsHost = cliAllowCorsHost <|> allowCorsHost envOpts,
        codebaseUIPath = cliCodebaseUIPath <|> codebaseUIPath envOpts
      }
  where
    tokenFlag =
      optional . strOption $
        long "token"
          <> metavar "STRING"
          <> help "API auth token"
          <> noGlobal
    hostFlag =
      optional . strOption $
        long "host"
          <> metavar "STRING"
          <> help "Codebase server host"
          <> noGlobal
    portFlag =
      optional . option auto $
        long "port"
          <> metavar "NUMBER"
          <> help "Codebase server port"
          <> noGlobal
    allowCorsHostFlag =
      optional . strOption $
        long "allow-cors-host"
          <> metavar "STRING"
          <> help "Host that should be allowed to access api (cors)"
          <> noGlobal
    codebaseUIPathFlag =
      optional . strOption $
        long "ui"
          <> metavar "DIR"
          <> help "Path to codebase ui root"
          <> noGlobal

launchParser :: CodebaseServerOpts -> IsHeadless -> Parser Command
launchParser envOpts isHeadless = do
  -- ApplicativeDo
  codebaseServerOpts <- codebaseServerOptsParser envOpts
  startingProject <- startingProjectOption
  shouldWatchFiles <- noFileWatchFlag
  pure (Launch isHeadless codebaseServerOpts startingProject shouldWatchFiles)

initParser :: Parser Command
initParser = pure Init

versionParser :: Parser Command
versionParser = pure PrintVersion

runArgumentParser :: Parser [String]
runArgumentParser = many (strArgument (metavar "RUN-ARGS"))

runHQParser :: Parser (HashQualified Name)
runHQParser =
  argument (maybeReader (HQ.parseText . Text.pack)) (metavar "SYMBOL")

runProjectPathParser :: Parser PP.ProjectPathNames
runProjectPathParser =
  argument (maybeReader (eitherToMaybe . PP.parseProjectPath . Text.pack)) (metavar "@myproject/mybranch:.path.in.project")

runSymbolParser :: Parser Command
runSymbolParser =
  Run . RunFromSymbol <$> runProjectPathParser <*> runArgumentParser

runFileParser :: Parser Command
runFileParser =
  Run
    <$> ( RunFromFile
            <$> fileArgument "path/to/file"
            <*> runHQParser
        )
    <*> runArgumentParser

runPipeParser :: Parser Command
runPipeParser =
  Run . RunFromPipe <$> runHQParser <*> runArgumentParser

runCompiledParser :: Parser Command
runCompiledParser =
  Run . RunCompiled <$> fileArgument "path/to/file" <*> runArgumentParser

rtsStatsOption :: Parser (Maybe RtsStatsPath)
rtsStatsOption =
  let meta =
        metavar "FILE.json"
          <> long "rts-stats"
          <> help "Write json summary of rts stats to FILE"
          <> noGlobal
   in optional (option OptParse.str meta)

saveCodebaseFlag :: Parser ShouldSaveCodebase
saveCodebaseFlag = flag DontSaveCodebase (SaveCodebase Nothing) (long "save-codebase" <> help saveHelp)
  where
    saveHelp = "if set the resulting codebase will be saved to a new directory, otherwise it will be deleted"

saveCodebaseToFlag :: Parser ShouldSaveCodebase
saveCodebaseToFlag = do
  path <-
    optional . strOption $
      long "save-codebase-to"
        <> short 'S'
        <> help "Where the codebase should be created. Implies --save-codebase"
  pure
    ( case path of
        Just _ -> SaveCodebase path
        _ -> DontSaveCodebase
    )

startingProjectOption :: Parser (Maybe (ProjectAndBranch ProjectName ProjectBranchName))
startingProjectOption =
  let meta =
        metavar "project/branch"
          <> long "project"
          <> short 'p'
          <> help "Launch the UCM session at the provided project and branch."
          <> noGlobal
   in optional (option readProjectAndBranchNames meta)

noFileWatchFlag :: Parser ShouldWatchFiles
noFileWatchFlag =
  flag
    ShouldWatchFiles
    ShouldNotWatchFiles
    ( long "no-file-watch"
        <> help noFileWatchHelp
        <> noGlobal
    )
  where
    noFileWatchHelp = "If set, ucm will not respond to changes in unison files. Instead, you can use the 'load' command."

readAbsolutePath :: ReadM Path.Absolute
readAbsolutePath = do
  readPath' >>= \case
    Path.AbsolutePath' abs -> pure abs
    Path.RelativePath' rel ->
      OptParse.readerError $
        "Expected an absolute path, but the path "
          <> show rel
          <> " was relative. Try adding a `.` prefix, e.g. `.path.to.project`"

nativeRuntimePathFlag :: Parser (Maybe FilePath)
nativeRuntimePathFlag =
  optional . strOption $
    long "runtime-path"
      <> metavar "DIR"
      <> help "Path to native runtime files"
      <> noGlobal

readPath' :: ReadM Path.Path'
readPath' = do
  strPath <- OptParse.str
  case Path.parsePath' strPath of
    Left err -> OptParse.readerError (Text.unpack err)
    Right path' -> pure path'

readProjectAndBranchNames :: ReadM (ProjectAndBranch ProjectName ProjectBranchName)
readProjectAndBranchNames = do
  str <- OptParse.str
  case Megaparsec.parse Project.fullyQualifiedProjectAndBranchNamesParser "arg" str of
    Left errBundle -> OptParse.readerError $ Megaparsec.errorBundlePretty errBundle
    Right projectAndBranch -> pure projectAndBranch

fileArgument :: String -> Parser FilePath
fileArgument varName =
  strArgument
    ( metavar varName
        <> action "file" -- Autocomplete file names
    )

transcriptParser :: Parser Command
transcriptParser = do
  -- ApplicativeDo
  shouldSaveCodebaseTo <- saveCodebaseToFlag
  shouldSaveCodebase <- saveCodebaseFlag
  mrtsStatsFp <- rtsStatsOption
  files <- liftA2 (NE.:|) (fileArgument "FILE") (many (fileArgument "FILES..."))
  pure
    ( let saveCodebase = case shouldSaveCodebaseTo of
            DontSaveCodebase -> shouldSaveCodebase
            _ -> shouldSaveCodebaseTo
       in Transcript DontFork saveCodebase mrtsStatsFp files
    )

transcriptForkParser :: Parser Command
transcriptForkParser = do
  -- ApplicativeDo
  shouldSaveCodebaseTo <- saveCodebaseToFlag
  shouldSaveCodebase <- saveCodebaseFlag
  mrtsStatsFp <- rtsStatsOption
  files <- liftA2 (NE.:|) (fileArgument "FILE") (many (fileArgument "FILES..."))
  pure
    ( let saveCodebase = case shouldSaveCodebaseTo of
            DontSaveCodebase -> shouldSaveCodebase
            _ -> shouldSaveCodebaseTo
       in Transcript UseFork saveCodebase mrtsStatsFp files
    )

unisonHelp :: String -> String -> P.Doc
unisonHelp (fromString -> executable) (fromString -> version) =
  fold . List.intersperse P.line $
    [ mempty,
      "🌻",
      mempty,
      P.annotate P.bold "Usage instructions for the Unison Codebase Manager",
      "You are running version:" <+> version,
      mempty,
      "To get started just run" <+> P.annotate P.bold executable,
      mempty,
      "Use" <+> P.annotate P.bold (executable <+> "[command] --help") <+> "to show help for a command."
    ]
