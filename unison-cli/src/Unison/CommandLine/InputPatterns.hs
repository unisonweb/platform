-- | This module defines 'InputPattern' values for every supported input command.
module Unison.CommandLine.InputPatterns
  ( -- * Input commands
    add,
    aliasMany,
    aliasTerm,
    aliasType,
    api,
    authLogin,
    back,
    branchEmptyInputPattern,
    branchInputPattern,
    branchRenameInputPattern,
    branchesInputPattern,
    cd,
    clear,
    clone,
    compileScheme,
    createAuthor,
    debugClearWatchCache,
    debugDoctor,
    debugDumpNamespace,
    debugDumpNamespaceSimple,
    debugFileHashes,
    debugFormat,
    debugFuzzyOptions,
    debugLSPFoldRanges,
    debugNameDiff,
    debugNumberedArgs,
    debugTabCompletion,
    debugLspNameCompletion,
    debugTerm,
    debugTermVerbose,
    debugType,
    delete,
    deleteBranch,
    deleteNamespace,
    deleteNamespaceForce,
    deleteProject,
    deleteTerm,
    deleteTermVerbose,
    deleteType,
    deleteTypeVerbose,
    deleteVerbose,
    dependencies,
    dependents,
    diffNamespace,
    display,
    displayTo,
    docToMarkdown,
    docs,
    docsToHtml,
    edit,
    editNamespace,
    execute,
    find,
    findAll,
    findGlobal,
    findIn,
    findInAll,
    findShallow,
    findVerbose,
    findVerboseAll,
    forkLocal,
    help,
    helpTopics,
    history,
    ioTest,
    ioTestNative,
    ioTestAll,
    ioTestAllNative,
    libInstallInputPattern,
    load,
    makeStandalone,
    mergeBuiltins,
    mergeCommitInputPattern,
    mergeIOBuiltins,
    mergeInputPattern,
    mergeOldInputPattern,
    mergeOldPreviewInputPattern,
    mergeOldSquashInputPattern,
    moveAll,
    names,
    namespaceDependencies,
    previewAdd,
    previewUpdate,
    printVersion,
    projectCreate,
    projectCreateEmptyInputPattern,
    projectRenameInputPattern,
    projectSwitch,
    projectsInputPattern,
    pull,
    pullWithoutHistory,
    push,
    pushCreate,
    pushExhaustive,
    pushForce,
    quit,
    releaseDraft,
    renameBranch,
    renameTerm,
    renameType,
    reset,
    runScheme,
    saveExecuteResult,
    sfind,
    sfindReplace,
    textfind,
    test,
    testNative,
    testAll,
    testAllNative,
    todo,
    ui,
    undo,
    up,
    update,
    updateBuiltins,
    updateOld,
    updateOldNoPatch,
    upgrade,
    upgradeCommitInputPattern,
    view,
    viewGlobal,
    deprecatedViewRootReflog,
    branchReflog,
    projectReflog,
    globalReflog,

    -- * Misc
    formatStructuredArgument,
    helpFor,
    makeExample',
    makeExample,
    makeExampleEOS,
    makeExampleNoBackticks,
    patternMap,
    patternName,
    showPatternHelp,
    unifyArgument,
    validInputs,
  )
where

import Control.Lens.Cons qualified as Cons
import Data.Bitraversable (bitraverse)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Extra qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import Network.URI qualified as URI
import System.Console.Haskeline.Completion (Completion (Completion))
import System.Console.Haskeline.Completion qualified as Haskeline
import System.Console.Haskeline.Completion qualified as Line
import Text.Megaparsec qualified as Megaparsec
import Text.Numeral (defaultInflection)
import Text.Numeral.Language.ENG qualified as Numeral
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.DbId (ProjectBranchId)
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Auth.HTTPClient (AuthenticatedHttpClient)
import Unison.Cli.Pretty
  ( prettyPath',
    prettyProjectAndBranchName,
    prettyProjectBranchName,
    prettyProjectName,
    prettyProjectNameSlash,
    prettySlashProjectBranchName,
    prettyURI,
  )
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch.Merge qualified as Branch
import Unison.Codebase.Editor.Input (BranchIdG (..), DeleteOutput (..), DeleteTarget (..), Input)
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output.PushPull (PushPull (Pull, Push))
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace)
import Unison.Codebase.Editor.RemoteRepo qualified as RemoteRepo
import Unison.Codebase.Editor.SlurpResult qualified as SR
import Unison.Codebase.Editor.StructuredArgument (StructuredArgument)
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.Codebase.Editor.UriParser (readRemoteNamespaceParser)
import Unison.Codebase.Editor.UriParser qualified as UriParser
import Unison.Codebase.Path (Path, Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.PushBehavior qualified as PushBehavior
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.CommandLine.BranchRelativePath (BranchRelativePath (..), parseBranchRelativePath, parseIncrementalBranchRelativePath)
import Unison.CommandLine.BranchRelativePath qualified as BranchRelativePath
import Unison.CommandLine.Completion
import Unison.CommandLine.FZFResolvers qualified as Resolvers
import Unison.CommandLine.Helpers (aside, backtick, tip)
import Unison.CommandLine.InputPattern (ArgumentType (..), InputPattern (InputPattern), IsOptional (..), unionSuggestions)
import Unison.CommandLine.InputPattern qualified as I
import Unison.Core.Project (ProjectBranchName (..))
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude hiding (view)
import Unison.Project
  ( ProjectAndBranch (..),
    ProjectAndBranchNames (..),
    ProjectBranchNameOrLatestRelease (..),
    ProjectBranchSpecifier (..),
    ProjectName,
    Semver,
    branchWithOptionalProjectParser,
  )
import Unison.Referent qualified as Referent
import Unison.Server.Backend (ShallowListEntry (..))
import Unison.Server.Backend qualified as Backend
import Unison.Server.SearchResult (SearchResult)
import Unison.Server.SearchResult qualified as SR
import Unison.ShortHash (ShortHash)
import Unison.Syntax.HashQualified qualified as HQ (parseText, toText)
import Unison.Syntax.Name qualified as Name (parseTextEither, toText)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.ColorText qualified as CT
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty qualified as P
import Unison.Util.Pretty.MegaParsec (prettyPrintParseError)

formatStructuredArgument :: Maybe Int -> StructuredArgument -> Text
formatStructuredArgument schLength = \case
  SA.AbsolutePath path -> into @Text $ show path
  SA.Name name -> Name.toText name
  SA.HashQualified hqName -> HQ.toText hqName
  SA.Project projectName -> into @Text projectName
  SA.ProjectBranch (ProjectAndBranch mproj branch) ->
    maybe (Text.cons '/' . into @Text) (\project -> into @Text . ProjectAndBranch project) mproj branch
  -- also: ("#" <>) . Hash.toBase32HexText . unCausalHash
  SA.Namespace causalHash -> ("#" <>) . SCH.toText $ maybe SCH.fromFullHash SCH.fromHash schLength causalHash
  SA.NameWithBranchPrefix absBranchId name -> prefixBranchId absBranchId name
  SA.HashQualifiedWithBranchPrefix absBranchId hq'Name -> HQ'.toTextWith (prefixBranchId absBranchId) hq'Name
  SA.ShallowListEntry path entry -> entryToHQText path entry
  SA.SearchResult searchRoot searchResult -> HQ.toText $ searchResultToHQ searchRoot searchResult
  where
    -- E.g.
    -- prefixBranchId "#abcdef" "base.List.map" -> "#abcdef:.base.List.map"
    -- prefixBranchId ".base" "List.map" -> ".base.List.map"
    prefixBranchId :: Input.AbsBranchId -> Name -> Text
    prefixBranchId branchId name = case branchId of
      BranchAtSCH sch -> "#" <> SCH.toText sch <> ":" <> Name.toText (Name.makeAbsolute name)
      BranchAtPath pathPrefix -> Name.toText (Path.prefixNameIfRel (Path.AbsolutePath' pathPrefix) name)
      BranchAtProjectPath pp ->
        pp
          & PP.absPath_
            %~ (\pathPrefix -> Path.resolve pathPrefix (Path.fromName name))
          & PP.toNames
          & into @Text

    entryToHQText :: Path' -> ShallowListEntry v Ann -> Text
    entryToHQText pathArg =
      fixup . \case
        ShallowTypeEntry te -> Backend.typeEntryDisplayName te
        ShallowTermEntry te -> Backend.termEntryDisplayName te
        ShallowBranchEntry ns _ _ -> NameSegment.toEscapedText ns
        ShallowPatchEntry ns -> NameSegment.toEscapedText ns
      where
        fixup s =
          pathArgStr
            <> if Text.null pathArgStr || Text.isSuffixOf "." pathArgStr
              then s
              else "." <> s
        pathArgStr = Text.pack $ show pathArg

-- | Converts an arbitrary argument to a `String`. This is for cases where the
-- command /should/ accept a structured argument of some type, but currently
-- wants a `String`.
unifyArgument :: I.Argument -> String
unifyArgument = either id (Text.unpack . formatStructuredArgument Nothing)

showPatternHelp :: InputPattern -> P.Pretty CT.ColorText
showPatternHelp i =
  P.lines
    [ P.bold (fromString $ I.patternName i)
        <> fromString
          ( if not . null $ I.aliases i
              then " (or " <> intercalate ", " (I.aliases i) <> ")"
              else ""
          ),
      I.help i
    ]

shallowListEntryToHQ' :: ShallowListEntry v Ann -> HQ'.HashQualified Name
shallowListEntryToHQ' = \case
  ShallowTermEntry termEntry -> Backend.termEntryHQName termEntry
  ShallowTypeEntry typeEntry -> Backend.typeEntryHQName typeEntry
  ShallowBranchEntry ns _ _ -> HQ'.fromName $ Name.fromSegment ns
  ShallowPatchEntry ns -> HQ'.fromName $ Name.fromSegment ns

-- | restores the full hash to these search results, for _numberedArgs purposes
searchResultToHQ :: Maybe Path' -> SearchResult -> HQ.HashQualified Name
searchResultToHQ oprefix = \case
  SR.Tm' n r _ -> HQ.requalify (addPrefix <$> n) r
  SR.Tp' n r _ -> HQ.requalify (addPrefix <$> n) (Referent.Ref r)
  _ -> error "impossible match failure"
  where
    addPrefix :: Name -> Name
    addPrefix = maybe id Path.prefixNameIfRel oprefix

unsupportedStructuredArgument :: InputPattern -> Text -> I.Argument -> Either (P.Pretty CT.ColorText) String
unsupportedStructuredArgument command expected =
  either pure . const . Left . P.wrap $
    makeExample' command
      <> "can’t accept a numbered argument for"
      <> P.text expected
      <> "and it’s not yet possible to provide un-expanded numbers as arguments."

expectedButActually' :: Text -> String -> P.Pretty CT.ColorText
expectedButActually' expected actualValue =
  P.text $ "I expected " <> expected <> ", but couldn’t recognize “" <> Text.pack actualValue <> "” as one."

expectedButActually :: Text -> StructuredArgument -> Text -> P.Pretty CT.ColorText
expectedButActually expected actualValue actualType =
  P.text $
    "I expected "
      <> expected
      <> ", but the numbered argument resulted in “"
      <> formatStructuredArgument Nothing actualValue
      <> "”, which is "
      <> actualType
      <> "."

wrongStructuredArgument :: Text -> StructuredArgument -> P.Pretty CT.ColorText
wrongStructuredArgument expected actual =
  expectedButActually
    expected
    actual
    case actual of
      SA.Name _ -> "a name"
      SA.AbsolutePath _ -> "an absolute path"
      SA.Namespace _ -> "a namespace"
      SA.Project _ -> "a project"
      SA.ProjectBranch _ -> "a branch"
      SA.HashQualified _ -> "a hash-qualified name"
      SA.NameWithBranchPrefix _ _ -> "a name"
      SA.HashQualifiedWithBranchPrefix _ _ -> "a hash-qualified name"
      SA.ShallowListEntry _ _ -> "a name"
      SA.SearchResult _ _ -> "a search result"

wrongArgsLength :: Text -> [a] -> Either (P.Pretty CT.ColorText) b
wrongArgsLength expected args =
  let foundCount =
        case length args of
          0 -> "none"
          n -> fromMaybe (tShow n) $ Numeral.us_cardinal defaultInflection n
   in Left . P.text $ "I expected " <> expected <> ", but received " <> foundCount <> "."

patternName :: InputPattern -> P.Pretty P.ColorText
patternName = fromString . I.patternName

-- `example list ["foo", "bar"]` (haskell) becomes `list foo bar` (pretty)
makeExample, makeExampleNoBackticks :: InputPattern -> [P.Pretty CT.ColorText] -> P.Pretty CT.ColorText
makeExample p args = P.group . backtick $ makeExampleNoBackticks p args
makeExampleNoBackticks p args =
  P.group $ intercalateMap " " id (P.nonEmpty $ fromString (I.patternName p) : args)

makeExample' :: InputPattern -> P.Pretty CT.ColorText
makeExample' p = makeExample p []

makeExampleEOS :: InputPattern -> [P.Pretty CT.ColorText] -> P.Pretty CT.ColorText
makeExampleEOS p args =
  P.group $
    backtick (intercalateMap " " id (P.nonEmpty $ fromString (I.patternName p) : args)) <> "."

helpFor :: InputPattern -> P.Pretty CT.ColorText
helpFor = I.help

handleProjectArg :: I.Argument -> Either (P.Pretty CT.ColorText) ProjectName
handleProjectArg =
  either
    (\name -> first (const $ expectedButActually' "a project" name) . tryInto @ProjectName $ Text.pack name)
    \case
      SA.Project project -> pure project
      otherArgType -> Left $ wrongStructuredArgument "a project" otherArgType

handleMaybeProjectBranchArg ::
  I.Argument -> Either (P.Pretty CT.ColorText) (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
handleMaybeProjectBranchArg =
  either
    (megaparse branchWithOptionalProjectParser . Text.pack)
    \case
      SA.ProjectBranch pb -> pure pb
      otherArgType -> Left $ wrongStructuredArgument "a branch" otherArgType

handleProjectMaybeBranchArg ::
  I.Argument -> Either (P.Pretty CT.ColorText) (ProjectAndBranch ProjectName (Maybe ProjectBranchNameOrLatestRelease))
handleProjectMaybeBranchArg =
  either
    (\str -> first (const $ expectedButActually' "a project or branch" str) . tryInto $ Text.pack str)
    \case
      SA.Project proj -> pure $ ProjectAndBranch proj Nothing
      SA.ProjectBranch (ProjectAndBranch (Just proj) branch) ->
        pure . ProjectAndBranch proj . pure $ ProjectBranchNameOrLatestRelease'Name branch
      otherArgType -> Left $ wrongStructuredArgument "a project or branch" otherArgType

handleHashQualifiedNameArg :: I.Argument -> Either (P.Pretty CT.ColorText) (HQ.HashQualified Name)
handleHashQualifiedNameArg =
  either
    parseHashQualifiedName
    \case
      SA.Name name -> pure $ HQ.NameOnly name
      SA.NameWithBranchPrefix mprefix name ->
        pure . HQ.NameOnly $ foldr (Path.prefixNameIfRel . Path.AbsolutePath') name mprefix
      SA.HashQualified hqname -> pure hqname
      SA.HashQualifiedWithBranchPrefix mprefix hqname ->
        pure . HQ'.toHQ $ foldr (\prefix -> fmap $ Path.prefixNameIfRel (Path.AbsolutePath' prefix)) hqname mprefix
      SA.ShallowListEntry prefix entry ->
        pure . HQ'.toHQ . fmap (Path.prefixNameIfRel prefix) $ shallowListEntryToHQ' entry
      SA.SearchResult mpath result -> pure $ searchResultToHQ mpath result
      otherArgType -> Left $ wrongStructuredArgument "a hash-qualified name" otherArgType

handlePathArg :: I.Argument -> Either (P.Pretty CT.ColorText) Path
handlePathArg =
  either
    (first P.text . Path.parsePath)
    \case
      SA.Name name -> pure $ Path.fromName name
      SA.NameWithBranchPrefix _ name -> pure $ Path.fromName name
      otherArgType ->
        either
          (const . Left $ wrongStructuredArgument "a relative path" otherArgType)
          ( \name ->
              if Name.isRelative name
                then pure $ Path.fromName name
                else Left $ wrongStructuredArgument "a relative path" otherArgType
          )
          . handleNameArg
          $ pure otherArgType

handlePath'Arg :: I.Argument -> Either (P.Pretty CT.ColorText) Path'
handlePath'Arg =
  either
    (first P.text . Path.parsePath')
    \case
      SA.AbsolutePath path -> pure $ Path.absoluteToPath' path
      SA.Name name -> pure $ Path.fromName' name
      SA.NameWithBranchPrefix mprefix name ->
        pure . Path.fromName' $ foldr (Path.prefixNameIfRel . Path.AbsolutePath') name mprefix
      otherArgType ->
        bimap (const $ wrongStructuredArgument "a path" otherArgType) Path.fromName' . handleNameArg $ pure otherArgType

handleNewName :: I.Argument -> Either (P.Pretty CT.ColorText) Path.Split'
handleNewName =
  either
    (first P.text . Path.parseSplit')
    (const . Left $ "can’t use a numbered argument for a new name")

handleNewPath :: I.Argument -> Either (P.Pretty CT.ColorText) Path'
handleNewPath =
  either
    (first P.text . Path.parsePath')
    (const . Left $ "can’t use a numbered argument for a new namespace")

-- | When only a relative name is allowed.
handleSplitArg :: I.Argument -> Either (P.Pretty CT.ColorText) Path.Split
handleSplitArg =
  either
    (first P.text . Path.parseSplit)
    \case
      SA.Name name | Name.isRelative name -> pure $ Path.splitFromName name
      SA.NameWithBranchPrefix _ name | Name.isRelative name -> pure $ Path.splitFromName name
      otherNumArg -> Left $ wrongStructuredArgument "a relative name" otherNumArg

handleSplit'Arg :: I.Argument -> Either (P.Pretty CT.ColorText) Path.Split'
handleSplit'Arg =
  either
    (first P.text . Path.parseSplit')
    \case
      SA.Name name -> pure $ Path.splitFromName' name
      SA.NameWithBranchPrefix (BranchAtSCH _) name -> pure $ Path.splitFromName' name
      SA.NameWithBranchPrefix (BranchAtPath prefix) name ->
        pure . Path.splitFromName' $ Path.prefixNameIfRel (Path.AbsolutePath' prefix) name
      otherNumArg -> Left $ wrongStructuredArgument "a name" otherNumArg

handleProjectBranchNameArg :: I.Argument -> Either (P.Pretty CT.ColorText) ProjectBranchName
handleProjectBranchNameArg =
  either
    (first (const $ P.text "Wanted a branch name, but it wasn’t") . tryInto . Text.pack)
    \case
      SA.ProjectBranch (ProjectAndBranch _ branch) -> pure branch
      otherNumArg -> Left $ wrongStructuredArgument "a branch name" otherNumArg

handleBranchIdArg :: I.Argument -> Either (P.Pretty CT.ColorText) Input.BranchId
handleBranchIdArg =
  either
    (first P.text . Input.parseBranchId)
    \case
      SA.AbsolutePath path -> pure . BranchAtPath $ Path.absoluteToPath' path
      SA.Name name -> pure . BranchAtPath $ Path.fromName' name
      SA.NameWithBranchPrefix mprefix name ->
        pure $ case mprefix of
          BranchAtSCH _sch -> BranchAtPath . Path.fromName' $ name
          BranchAtPath prefix -> BranchAtPath . Path.fromName' $ Path.prefixNameIfRel (Path.AbsolutePath' prefix) name
          BranchAtProjectPath pp ->
            pp
              & PP.absPath_
                %~ (\pathPrefix -> Path.resolve pathPrefix (Path.fromName name))
              & BranchAtProjectPath
      SA.Namespace hash -> pure . BranchAtSCH $ SCH.fromFullHash hash
      otherNumArg -> Left $ wrongStructuredArgument "a branch id" otherNumArg

-- | TODO: Maybe remove?
_handleBranchIdOrProjectArg ::
  I.Argument ->
  Either (P.Pretty CT.ColorText) (These Input.BranchId (ProjectAndBranch (Maybe ProjectName) ProjectBranchName))
_handleBranchIdOrProjectArg =
  either
    (\str -> maybe (Left $ expectedButActually' "a branch" str) pure $ branchIdOrProject str)
    \case
      SA.Namespace hash -> pure . This . BranchAtSCH $ SCH.fromFullHash hash
      SA.AbsolutePath path -> pure . This . BranchAtPath $ Path.absoluteToPath' path
      SA.Name name -> pure . This . BranchAtPath $ Path.fromName' name
      SA.NameWithBranchPrefix (BranchAtSCH _) name -> pure . This . BranchAtPath $ Path.fromName' name
      SA.NameWithBranchPrefix (BranchAtPath prefix) name ->
        pure . This . BranchAtPath . Path.fromName' $ Path.prefixNameIfRel (Path.AbsolutePath' prefix) name
      SA.ProjectBranch pb -> pure $ That pb
      otherArgType -> Left $ wrongStructuredArgument "a branch" otherArgType
  where
    branchIdOrProject ::
      String ->
      Maybe
        ( These
            Input.BranchId
            (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
        )
    branchIdOrProject str =
      let branchIdRes = Input.parseBranchId str
          projectRes =
            tryInto @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
              (Text.pack str)
       in case (branchIdRes, projectRes) of
            (Left _, Left _) -> Nothing
            (Left _, Right pr) -> Just (That pr)
            (Right bid, Left _) -> Just (This bid)
            (Right bid, Right pr) -> Just (These bid pr)

handleBranchId2Arg :: I.Argument -> Either (P.Pretty P.ColorText) Input.BranchId2
handleBranchId2Arg =
  either
    Input.parseBranchId2
    \case
      SA.Namespace hash -> pure . Left $ SCH.fromFullHash hash
      SA.AbsolutePath path -> pure . pure . UnqualifiedPath $ Path.absoluteToPath' path
      SA.Name name -> pure . pure . UnqualifiedPath $ Path.fromName' name
      SA.NameWithBranchPrefix (BranchAtSCH _) name -> pure . pure . UnqualifiedPath $ Path.fromName' name
      SA.NameWithBranchPrefix (BranchAtPath prefix) name ->
        pure . pure . UnqualifiedPath . Path.fromName' $ Path.prefixNameIfRel (Path.AbsolutePath' prefix) name
      SA.ProjectBranch (ProjectAndBranch mproject branch) ->
        case mproject of
          Just proj -> pure . pure $ QualifiedBranchPath proj branch Path.absoluteEmpty
          Nothing -> pure . pure $ BranchPathInCurrentProject branch Path.absoluteEmpty
      otherNumArg -> Left $ wrongStructuredArgument "a branch id" otherNumArg

handleBranchRelativePathArg :: I.Argument -> Either (P.Pretty P.ColorText) BranchRelativePath
handleBranchRelativePathArg =
  either
    parseBranchRelativePath
    \case
      SA.AbsolutePath path -> pure . UnqualifiedPath $ Path.absoluteToPath' path
      SA.Name name -> pure . UnqualifiedPath $ Path.fromName' name
      SA.NameWithBranchPrefix (BranchAtSCH _) name -> pure . UnqualifiedPath $ Path.fromName' name
      SA.NameWithBranchPrefix (BranchAtPath prefix) name ->
        pure . UnqualifiedPath . Path.fromName' $ Path.prefixNameIfRel (Path.AbsolutePath' prefix) name
      SA.ProjectBranch (ProjectAndBranch mproject branch) ->
        case mproject of
          Just proj -> pure $ QualifiedBranchPath proj branch Path.absoluteEmpty
          Nothing -> pure $ BranchPathInCurrentProject branch Path.absoluteEmpty
      otherNumArg -> Left $ wrongStructuredArgument "a branch id" otherNumArg

hqNameToSplit' :: HQ.HashQualified Name -> Either ShortHash Path.HQSplit'
hqNameToSplit' = \case
  HQ.HashOnly hash -> Left hash
  HQ.NameOnly name -> pure . fmap HQ'.NameOnly $ Path.splitFromName' name
  HQ.HashQualified name hash -> pure . fmap (`HQ'.HashQualified` hash) $ Path.splitFromName' name

hqNameToSplit :: HQ.HashQualified Name -> Either ShortHash Path.HQSplit
hqNameToSplit = \case
  HQ.HashOnly hash -> Left hash
  HQ.NameOnly name -> pure . fmap HQ'.NameOnly $ Path.splitFromName name
  HQ.HashQualified name hash -> pure . fmap (`HQ'.HashQualified` hash) $ Path.splitFromName name

hq'NameToSplit' :: HQ'.HashQualified Name -> Path.HQSplit'
hq'NameToSplit' = \case
  HQ'.NameOnly name -> HQ'.NameOnly <$> Path.splitFromName' name
  HQ'.HashQualified name hash -> flip HQ'.HashQualified hash <$> Path.splitFromName' name

hq'NameToSplit :: HQ'.HashQualified Name -> Path.HQSplit
hq'NameToSplit = \case
  HQ'.NameOnly name -> HQ'.NameOnly <$> Path.splitFromName name
  HQ'.HashQualified name hash -> flip HQ'.HashQualified hash <$> Path.splitFromName name

handleHashQualifiedSplit'Arg :: I.Argument -> Either (P.Pretty CT.ColorText) Path.HQSplit'
handleHashQualifiedSplit'Arg =
  either
    (first P.text . Path.parseHQSplit')
    \case
      SA.Name name -> pure $ Path.hqSplitFromName' name
      hq@(SA.HashQualified name) -> first (const $ expectedButActually "a name" hq "a hash") $ hqNameToSplit' name
      SA.HashQualifiedWithBranchPrefix (BranchAtSCH _) hqname -> pure $ hq'NameToSplit' hqname
      SA.HashQualifiedWithBranchPrefix (BranchAtPath prefix) hqname ->
        pure . hq'NameToSplit' $ Path.prefixNameIfRel (Path.AbsolutePath' prefix) <$> hqname
      SA.ShallowListEntry prefix entry ->
        pure . hq'NameToSplit' . fmap (Path.prefixNameIfRel prefix) $ shallowListEntryToHQ' entry
      sr@(SA.SearchResult mpath result) ->
        first (const $ expectedButActually "a name" sr "a hash") . hqNameToSplit' $ searchResultToHQ mpath result
      otherNumArg -> Left $ wrongStructuredArgument "a name" otherNumArg

handleHashQualifiedSplitArg :: I.Argument -> Either (P.Pretty CT.ColorText) Path.HQSplit
handleHashQualifiedSplitArg =
  either
    (first P.text . Path.parseHQSplit)
    \case
      n@(SA.Name name) ->
        bitraverse
          ( \case
              Path.AbsolutePath' _ -> Left $ expectedButActually "a relative name" n "an absolute name"
              Path.RelativePath' p -> pure $ Path.unrelative p
          )
          pure
          $ Path.hqSplitFromName' name
      hq@(SA.HashQualified name) -> first (const $ expectedButActually "a name" hq "a hash") $ hqNameToSplit name
      SA.HashQualifiedWithBranchPrefix (BranchAtSCH _) hqname -> pure $ hq'NameToSplit hqname
      SA.HashQualifiedWithBranchPrefix (BranchAtPath prefix) hqname ->
        pure . hq'NameToSplit $ Path.prefixNameIfRel (Path.AbsolutePath' prefix) <$> hqname
      SA.ShallowListEntry _ entry -> pure . hq'NameToSplit $ shallowListEntryToHQ' entry
      sr@(SA.SearchResult mpath result) ->
        first (const $ expectedButActually "a name" sr "a hash") . hqNameToSplit $ searchResultToHQ mpath result
      otherNumArg -> Left $ wrongStructuredArgument "a relative name" otherNumArg

handleShortCausalHashArg :: I.Argument -> Either (P.Pretty CT.ColorText) ShortCausalHash
handleShortCausalHashArg =
  either
    (first (P.text . Text.pack) . Input.parseShortCausalHash)
    \case
      SA.Namespace hash -> pure $ SCH.fromFullHash hash
      otherNumArg -> Left $ wrongStructuredArgument "a causal hash" otherNumArg

handleShortHashOrHQSplit'Arg ::
  I.Argument -> Either (P.Pretty CT.ColorText) (Either ShortHash Path.HQSplit')
handleShortHashOrHQSplit'Arg =
  either
    (first P.text . Path.parseShortHashOrHQSplit')
    \case
      SA.HashQualified name -> pure $ hqNameToSplit' name
      SA.HashQualifiedWithBranchPrefix (BranchAtSCH _) hqname -> pure . pure $ hq'NameToSplit' hqname
      SA.HashQualifiedWithBranchPrefix (BranchAtPath prefix) hqname ->
        pure . pure $ hq'NameToSplit' (Path.prefixNameIfRel (Path.AbsolutePath' prefix) <$> hqname)
      SA.ShallowListEntry prefix entry ->
        pure . pure . hq'NameToSplit' . fmap (Path.prefixNameIfRel prefix) $ shallowListEntryToHQ' entry
      SA.SearchResult mpath result -> pure . hqNameToSplit' $ searchResultToHQ mpath result
      otherNumArg -> Left $ wrongStructuredArgument "a hash or name" otherNumArg

handleRelativeNameSegmentArg :: I.Argument -> Either (P.Pretty CT.ColorText) NameSegment
handleRelativeNameSegmentArg arg = do
  name <- handleNameArg arg
  let (segment NE.:| tail) = Name.reverseSegments name
  if Name.isRelative name && null tail
    then pure segment
    else Left $ P.text "Wanted a single relative name segment, but it wasn’t."

handleNameArg :: I.Argument -> Either (P.Pretty CT.ColorText) Name
handleNameArg =
  either
    (first P.text . Name.parseTextEither . Text.pack)
    \case
      SA.Name name -> pure name
      SA.NameWithBranchPrefix (BranchAtSCH _) name -> pure name
      SA.NameWithBranchPrefix (BranchAtPath prefix) name -> pure $ Path.prefixNameIfRel (Path.AbsolutePath' prefix) name
      SA.HashQualified hqname -> maybe (Left "can’t find a name from the numbered arg") pure $ HQ.toName hqname
      SA.HashQualifiedWithBranchPrefix (BranchAtSCH _) hqname -> pure $ HQ'.toName hqname
      SA.HashQualifiedWithBranchPrefix (BranchAtPath prefix) hqname ->
        pure . Path.prefixNameIfRel (Path.AbsolutePath' prefix) $ HQ'.toName hqname
      SA.ShallowListEntry prefix entry ->
        pure . HQ'.toName . fmap (Path.prefixNameIfRel prefix) $ shallowListEntryToHQ' entry
      SA.SearchResult mpath result ->
        maybe (Left "can’t find a name from the numbered arg") pure . HQ.toName $ searchResultToHQ mpath result
      otherNumArg -> Left $ wrongStructuredArgument "a name" otherNumArg

handlePullSourceArg ::
  I.Argument ->
  Either
    (P.Pretty CT.ColorText)
    (ReadRemoteNamespace (These ProjectName ProjectBranchNameOrLatestRelease))
handlePullSourceArg =
  either
    (megaparse (readRemoteNamespaceParser ProjectBranchSpecifier'NameOrLatestRelease) . Text.pack)
    \case
      SA.Project project -> pure . RemoteRepo.ReadShare'ProjectBranch $ This project
      SA.ProjectBranch (ProjectAndBranch project branch) ->
        pure . RemoteRepo.ReadShare'ProjectBranch . maybe That These project $
          ProjectBranchNameOrLatestRelease'Name branch
      otherNumArg -> Left $ wrongStructuredArgument "a source to pull from" otherNumArg

handlePushTargetArg ::
  I.Argument -> Either (P.Pretty CT.ColorText) (These ProjectName ProjectBranchName)
handlePushTargetArg =
  either
    (\str -> maybe (Left $ expectedButActually' "a target to push to" str) pure $ parsePushTarget str)
    $ \case
      SA.Project project -> pure $ This project
      SA.ProjectBranch (ProjectAndBranch project branch) -> pure $ maybe That These project branch
      otherNumArg -> Left $ wrongStructuredArgument "a target to push to" otherNumArg

handlePushSourceArg :: I.Argument -> Either (P.Pretty CT.ColorText) Input.PushSource
handlePushSourceArg =
  either
    (\str -> maybe (Left $ expectedButActually' "a source to push from" str) pure $ parsePushSource str)
    \case
      SA.Project project -> pure . Input.ProjySource $ This project
      SA.ProjectBranch (ProjectAndBranch project branch) -> pure . Input.ProjySource $ maybe That These project branch
      otherNumArg -> Left $ wrongStructuredArgument "a source to push from" otherNumArg

handleProjectAndBranchNamesArg :: I.Argument -> Either (P.Pretty CT.ColorText) ProjectAndBranchNames
handleProjectAndBranchNamesArg =
  either
    (\str -> first (const $ expectedButActually' "a project or branch" str) . tryInto @ProjectAndBranchNames $ Text.pack str)
    $ fmap ProjectAndBranchNames'Unambiguous . \case
      SA.Project project -> pure $ This project
      SA.ProjectBranch (ProjectAndBranch mproj branch) -> pure $ maybe That These mproj branch
      otherNumArg -> Left $ wrongStructuredArgument "a project or branch" otherNumArg

mergeBuiltins :: InputPattern
mergeBuiltins =
  InputPattern
    "builtins.merge"
    []
    I.Hidden
    [("namespace", Optional, namespaceArg)]
    "Adds the builtins (excluding `io` and misc) to the specified namespace. Defaults to `builtin.`"
    \case
      [] -> pure . Input.MergeBuiltinsI $ Nothing
      [p] -> Input.MergeBuiltinsI . Just <$> handlePathArg p
      args -> wrongArgsLength "no more than one argument" args

mergeIOBuiltins :: InputPattern
mergeIOBuiltins =
  InputPattern
    "builtins.mergeio"
    []
    I.Hidden
    [("namespace", Optional, namespaceArg)]
    "Adds all the builtins, including `io` and misc., to the specified namespace. Defaults to `builtin.`"
    \case
      [] -> pure . Input.MergeIOBuiltinsI $ Nothing
      [p] -> Input.MergeIOBuiltinsI . Just <$> handlePathArg p
      args -> wrongArgsLength "no more than one argument" args

updateBuiltins :: InputPattern
updateBuiltins =
  InputPattern
    "builtins.update"
    []
    I.Hidden
    []
    ( "Adds all the builtins that are missing from this namespace, "
        <> "and deprecate the ones that don't exist in this version of Unison."
    )
    (const . pure $ Input.UpdateBuiltinsI)

todo :: InputPattern
todo =
  InputPattern
    "todo"
    []
    I.Visible
    []
    ( P.wrap $
        makeExample' todo
          <> "lists the current namespace's outstanding issues, including conflicted names, dependencies with missing"
          <> "names, and merge precondition violations."
    )
    \case
      [] -> Right Input.TodoI
      args -> wrongArgsLength "no arguments" args

load :: InputPattern
load =
  InputPattern
    "load"
    []
    I.Visible
    [("scratch file", Optional, filePathArg)]
    ( P.wrapColumn2
        [ ( makeExample' load,
            "parses, typechecks, and evaluates the most recent scratch file."
          ),
          ( makeExample load ["<scratch file>"],
            "parses, typechecks, and evaluates the given scratch file."
          )
        ]
    )
    \case
      [] -> pure $ Input.LoadI Nothing
      [file] -> Input.LoadI . Just <$> unsupportedStructuredArgument load "a file name" file
      args -> wrongArgsLength "no more than one argument" args

clear :: InputPattern
clear =
  InputPattern
    "clear"
    []
    I.Visible
    []
    ( P.wrapColumn2
        [ ( makeExample' clear,
            "Clears the screen."
          )
        ]
    )
    \case
      [] -> pure Input.ClearI
      args -> wrongArgsLength "no arguments" args

add :: InputPattern
add =
  InputPattern
    "add"
    []
    I.Visible
    [("definition", ZeroPlus, noCompletionsArg)]
    ( "`add` adds to the codebase all the definitions from the most recently "
        <> "typechecked file."
    )
    $ fmap (Input.AddI . Set.fromList) . traverse handleNameArg

previewAdd :: InputPattern
previewAdd =
  InputPattern
    "add.preview"
    []
    I.Visible
    [("definition", ZeroPlus, noCompletionsArg)]
    ( "`add.preview` previews additions to the codebase from the most recently "
        <> "typechecked file. This command only displays cached typechecking "
        <> "results. Use `load` to reparse & typecheck the file if the context "
        <> "has changed."
    )
    $ fmap (Input.PreviewAddI . Set.fromList) . traverse handleNameArg

update :: InputPattern
update =
  InputPattern
    { patternName = "update",
      aliases = [],
      visibility = I.Visible,
      args = [],
      help =
        P.wrap $
          "Adds everything in the most recently typechecked file to the namespace,"
            <> "replacing existing definitions having the same name, and attempts to update all the existing dependents accordingly. If the process"
            <> "can't be completed automatically, the dependents will be added back to the scratch file"
            <> "for your review.",
      parse = \case
        [] -> pure Input.Update2I
        args -> wrongArgsLength "no arguments" args
    }

updateOldNoPatch :: InputPattern
updateOldNoPatch =
  InputPattern
    "update.old.nopatch"
    []
    I.Visible
    [("definition", ZeroPlus, noCompletionsArg)]
    ( P.wrap
        ( makeExample' updateOldNoPatch
            <> "works like"
            <> P.group (makeExample' updateOld <> ",")
            <> "except it doesn't add a patch entry for any updates. "
            <> "Use this when you want to make changes to definitions without "
            <> "pushing those changes to dependents beyond your codebase. "
            <> "An example is when updating docs, or when updating a term you "
            <> "just added."
        )
        <> P.wrapColumn2
          [ ( makeExample' updateOldNoPatch,
              "updates all definitions in the .u file."
            ),
            ( makeExample updateOldNoPatch ["foo", "bar"],
              "updates `foo`, `bar`, and their dependents from the .u file."
            )
          ]
    )
    $ fmap (Input.UpdateI Input.NoPatch . Set.fromList) . traverse handleNameArg

updateOld :: InputPattern
updateOld =
  InputPattern
    "update.old"
    []
    I.Visible
    [("patch", Optional, patchArg), ("definition", ZeroPlus, noCompletionsArg)]
    ( P.wrap
        ( makeExample' updateOld
            <> "works like"
            <> P.group (makeExample' add <> ",")
            <> "except that if a definition in the file has the same name as an"
            <> "existing definition, the name gets updated to point to the new"
            <> "definition. If the old definition has any dependents, `update` will"
            <> "add those dependents to a refactoring session, specified by an"
            <> "optional patch."
        )
        <> P.wrapColumn2
          [ ( makeExample' updateOld,
              "adds all definitions in the .u file, noting replacements in the"
                <> "default patch for the current namespace."
            ),
            ( makeExample updateOld ["<patch>"],
              "adds all definitions in the .u file, noting replacements in the"
                <> "specified patch."
            ),
            ( makeExample updateOld ["<patch>", "foo", "bar"],
              "adds `foo`, `bar`, and their dependents from the .u file, noting"
                <> "any replacements into the specified patch."
            )
          ]
    )
    \case
      patchStr : ws ->
        Input.UpdateI . Input.UsePatch <$> handleSplit'Arg patchStr <*> fmap Set.fromList (traverse handleNameArg ws)
      [] -> Right $ Input.UpdateI Input.DefaultPatch mempty

previewUpdate :: InputPattern
previewUpdate =
  InputPattern
    "update.old.preview"
    []
    I.Visible
    [("definition", ZeroPlus, noCompletionsArg)]
    ( "`update.old.preview` previews updates to the codebase from the most "
        <> "recently typechecked file. This command only displays cached "
        <> "typechecking results. Use `load` to reparse & typecheck the file if "
        <> "the context has changed."
    )
    $ fmap (Input.PreviewUpdateI . Set.fromList) . traverse handleNameArg

view :: InputPattern
view =
  InputPattern
    "view"
    []
    I.Visible
    [("definition to view", OnePlus, definitionQueryArg)]
    ( P.lines
        [ P.wrap $ makeExample view ["foo"] <> "shows definitions named `foo` within your current namespace.",
          P.wrap $ makeExample view [] <> "without arguments invokes a search to select definitions to view, which requires that `fzf` can be found within your PATH.",
          " ", -- hmm, this blankline seems to be ignored by pretty printer
          P.wrap $
            "Supports glob syntax, where ? acts a wildcard, so"
              <> makeExample view ["List.?"]
              <> "will show `List.map`, `List.filter`, etc, but "
              <> "not `List.map.doc` (since ? only matches 1 name segment)."
        ]
    )
    ( maybe
        (wrongArgsLength "at least one argument" [])
        ( fmap (Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionLocal)
            . traverse handleHashQualifiedNameArg
        )
        . NE.nonEmpty
    )

viewGlobal :: InputPattern
viewGlobal =
  InputPattern
    "view.global"
    []
    I.Visible
    [("definition to view", ZeroPlus, definitionQueryArg)]
    ( P.lines
        [ "`view.global foo` prints definitions of `foo` within your codebase.",
          "`view.global` without arguments invokes a search to select definitions to view, which requires that `fzf` can be found within your PATH."
        ]
    )
    ( maybe
        (wrongArgsLength "at least one argument" [])
        ( fmap (Input.ShowDefinitionI Input.ConsoleLocation Input.ShowDefinitionGlobal)
            . traverse handleHashQualifiedNameArg
        )
        . NE.nonEmpty
    )

display :: InputPattern
display =
  InputPattern
    "display"
    []
    I.Visible
    [("definition to display", OnePlus, definitionQueryArg)]
    ( P.lines
        [ "`display foo` prints a rendered version of the term `foo`.",
          "`display` without arguments invokes a search to select a definition to display, which requires that `fzf` can be found within your PATH."
        ]
    )
    $ maybe
      (wrongArgsLength "at least one argument" [])
      (fmap (Input.DisplayI Input.ConsoleLocation) . traverse handleHashQualifiedNameArg)
      . NE.nonEmpty

displayTo :: InputPattern
displayTo =
  InputPattern
    "display.to"
    []
    I.Visible
    [("destination file name", Required, filePathArg), ("definition to display", OnePlus, definitionQueryArg)]
    ( P.wrap $
        makeExample displayTo ["<filename>", "foo"]
          <> "prints a rendered version of the term `foo` to the given file."
    )
    $ \case
      file : defs ->
        maybe
          (wrongArgsLength "at least two arguments" [file])
          ( \defs -> do
              file <- unsupportedStructuredArgument displayTo "a file name" file
              names <- traverse handleHashQualifiedNameArg defs
              pure (Input.DisplayI (Input.FileLocation file Input.AboveFold) names)
          )
          $ NE.nonEmpty defs
      [] -> wrongArgsLength "at least two arguments" []

docs :: InputPattern
docs =
  InputPattern
    "docs"
    []
    I.Visible
    [("definition", OnePlus, definitionQueryArg)]
    ( P.lines
        [ "`docs foo` shows documentation for the definition `foo`.",
          "`docs` without arguments invokes a search to select which definition to view documentation for, which requires that `fzf` can be found within your PATH."
        ]
    )
    $ maybe (wrongArgsLength "at least one argument" []) (fmap Input.DocsI . traverse handleNameArg) . NE.nonEmpty

api :: InputPattern
api =
  InputPattern
    "api"
    []
    I.Visible
    []
    "`api` provides details about the API."
    (const $ pure Input.ApiI)

ui :: InputPattern
ui =
  InputPattern
    { patternName = "ui",
      aliases = [],
      visibility = I.Visible,
      args = [("definition to load", Optional, namespaceOrDefinitionArg)],
      help = P.wrap "`ui` opens the Local UI in the default browser.",
      parse = \case
        [] -> pure $ Input.UiI Path.relativeEmpty'
        [path] -> Input.UiI <$> handlePath'Arg path
        args -> wrongArgsLength "no more than one argument" args
    }

undo :: InputPattern
undo =
  InputPattern
    "undo"
    []
    I.Visible
    []
    "`undo` reverts the most recent change to the codebase."
    (const $ pure Input.UndoI)

textfind :: Bool -> InputPattern
textfind allowLib =
  InputPattern cmdName aliases I.Visible [("token", OnePlus, noCompletionsArg)] msg parse
  where
    (cmdName, aliases, alternate) =
      if allowLib
        then ("text.find.all", ["grep.all"], "Use `text.find` to exclude `lib` from search.")
        else ("text.find", ["grep"], "Use `text.find.all` to include search of `lib`.")
    parse = \case
      [] -> Left (P.text "Please supply at least one token.")
      words -> pure $ Input.TextFindI allowLib (untokenize $ [e | Left e <- words])
    msg =
      P.lines
        [ P.wrap $
            makeExample (textfind allowLib) ["token1", "\"99\"", "token2"]
              <> " finds terms with literals (text or numeric) containing"
              <> "`token1`, `99`, and `token2`.",
          "",
          P.wrap $
            "Numeric literals must be quoted (ex: \"42\")"
              <> "but single words need not be quoted.",
          "",
          P.wrap alternate
        ]

-- | Reinterprets `"` in the expected way, combining tokens until reaching
-- the closing quote.
-- Example: `untokenize ["\"uno", "dos\""]` becomes `["uno dos"]`.
untokenize :: [String] -> [String]
untokenize words = go (unwords words)
  where
    go words = case words of
      [] -> []
      '"' : quoted -> takeWhile (/= '"') quoted : go (drop 1 . dropWhile (/= '"') $ quoted)
      unquoted -> case span ok unquoted of
        ("", rem) -> go (dropWhile isSpace rem)
        (tok, rem) -> tok : go (dropWhile isSpace rem)
        where
          ok ch = ch /= '"' && not (isSpace ch)

sfind :: InputPattern
sfind =
  InputPattern "rewrite.find" ["sfind"] I.Visible [("rewrite-rule definition", Required, definitionQueryArg)] msg parse
  where
    parse = \case
      [q] -> Input.StructuredFindI (Input.FindLocal Path.relativeEmpty') <$> handleHashQualifiedNameArg q
      args -> wrongArgsLength "exactly one argument" args
    msg =
      P.lines
        [ P.wrap $
            makeExample sfind ["rule1"]
              <> " finds definitions that match any of the left side(s) of `rule`"
              <> "in the current namespace.",
          "",
          P.wrap $
            "The argument `rule1` must refer to a `@rewrite` block or a function that immediately returns"
              <> "a `@rewrite` block."
              <> "It can be in the codebase or scratch file. An example:",
          "",
          "    -- right of ==> is ignored by this command",
          "    rule1 x = @rewrite term x + 1 ==> ()",
          "",
          P.wrap $
            "Here, `x` will stand in for any expression,"
              <> "so this rule will match "
              <> P.backticked' "(42+10+11) + 1" ".",
          "",
          "See https://unison-lang.org/learn/structured-find to learn more.",
          "",
          P.wrap ("Also see the related command" <> makeExample sfindReplace [])
        ]

sfindReplace :: InputPattern
sfindReplace =
  InputPattern "rewrite" ["sfind.replace"] I.Visible [("rewrite-rule definition", Required, definitionQueryArg)] msg parse
  where
    parse [q] = Input.StructuredFindReplaceI <$> handleHashQualifiedNameArg q
    parse args = wrongArgsLength "exactly one argument" args
    msg :: P.Pretty CT.ColorText
    msg =
      P.lines
        [ makeExample sfindReplace ["rule1"] <> " rewrites definitions in the latest scratch file.",
          "",
          P.wrap $
            "The argument `rule1` must refer to a `@rewrite` block or a function that immediately returns"
              <> "a `@rewrite` block."
              <> "It can be in the codebase or scratch file. An example:",
          "",
          "    rule1 x = @rewrite term x + 1 ==> Nat.increment x",
          "",
          P.wrap $
            "Here, `x` will stand in for any expression wherever this rewrite is applied,"
              <> "so this rule will match "
              <> P.backticked "(42+10+11) + 1"
              <> "and replace it with"
              <> P.backticked' "Nat.increment (42+10+11)" ".",
          "",
          "See https://unison-lang.org/learn/structured-find to learn more.",
          "",
          P.wrap ("Also see the related command" <> makeExample sfind [])
        ]

find :: InputPattern
find = find' "find" (Input.FindLocal Path.relativeEmpty')

findAll :: InputPattern
findAll = find' "find.all" (Input.FindLocalAndDeps Path.relativeEmpty')

findGlobal :: InputPattern
findGlobal = find' "debug.find.global" Input.FindGlobal

findIn, findInAll :: InputPattern
findIn = findIn' "find-in" Input.FindLocal
findInAll = findIn' "find-in.all" Input.FindLocalAndDeps

findIn' :: String -> (Path' -> Input.FindScope) -> InputPattern
findIn' cmd mkfscope =
  InputPattern
    cmd
    []
    I.Visible
    [("namespace", Required, namespaceArg), ("query", ZeroPlus, exactDefinitionArg)]
    findHelp
    \case
      p : args -> Input.FindI False . mkfscope <$> handlePath'Arg p <*> pure (unifyArgument <$> args)
      args -> wrongArgsLength "at least one argument" args

findHelp :: P.Pretty CT.ColorText
findHelp =
  ( P.wrapColumn2
      [ ("`find`", "lists all definitions in the current namespace."),
        ( "`find foo`",
          "lists all definitions with a name similar to 'foo' in the current "
            <> "namespace (excluding those under 'lib')."
        ),
        ( "`find foo bar`",
          "lists all definitions with a name similar to 'foo' or 'bar' in the "
            <> "current namespace (excluding those under 'lib')."
        ),
        ( "`find-in namespace`",
          "lists all definitions in the specified subnamespace."
        ),
        ( "`find-in namespace foo bar`",
          "lists all definitions with a name similar to 'foo' or 'bar' in the "
            <> "specified subnamespace."
        ),
        ( "find.all foo",
          "lists all definitions with a name similar to 'foo' in the current "
            <> "namespace (including one level of 'lib')."
        ),
        ( "`find-in.all namespace`",
          "lists all definitions in the specified subnamespace (including one level of its 'lib')."
        ),
        ( "`find-in.all namespace foo bar`",
          "lists all definitions with a name similar to 'foo' or 'bar' in the "
            <> "specified subnamespace (including one level of its 'lib')."
        ),
        ( "debug.find.global foo",
          "Iteratively searches all projects and branches and lists all definitions with a name similar to 'foo'. Note that this is a very slow operation."
        )
      ]
  )

find' :: String -> Input.FindScope -> InputPattern
find' cmd fscope =
  InputPattern
    cmd
    []
    I.Visible
    [("query", ZeroPlus, exactDefinitionArg)]
    findHelp
    (pure . Input.FindI False fscope . fmap unifyArgument)

findShallow :: InputPattern
findShallow =
  InputPattern
    "list"
    ["ls", "dir"]
    I.Visible
    [("namespace", Optional, namespaceArg)]
    ( P.wrapColumn2
        [ ("`list`", "lists definitions and namespaces at the current level of the current namespace."),
          ("`list foo`", "lists the 'foo' namespace."),
          ("`list .foo`", "lists the '.foo' namespace.")
        ]
    )
    ( fmap Input.FindShallowI . \case
        [] -> pure Path.relativeEmpty'
        [path] -> handlePath'Arg path
        args -> wrongArgsLength "no more than one argument" args
    )

findVerbose :: InputPattern
findVerbose =
  InputPattern
    "find.verbose"
    []
    I.Visible
    [("query", ZeroPlus, exactDefinitionArg)]
    ( "`find.verbose` searches for definitions like `find`, but includes hashes "
        <> "and aliases in the results."
    )
    (pure . Input.FindI True (Input.FindLocal Path.relativeEmpty') . fmap unifyArgument)

findVerboseAll :: InputPattern
findVerboseAll =
  InputPattern
    "find.all.verbose"
    []
    I.Visible
    [("query", ZeroPlus, exactDefinitionArg)]
    ( "`find.all.verbose` searches for definitions like `find.all`, but includes hashes "
        <> "and aliases in the results."
    )
    (pure . Input.FindI True (Input.FindLocalAndDeps Path.relativeEmpty') . fmap unifyArgument)

renameTerm :: InputPattern
renameTerm =
  InputPattern
    "move.term"
    ["rename.term"]
    I.Visible
    [ ("definition to move", Required, exactDefinitionTermQueryArg),
      ("new location", Required, newNameArg)
    ]
    "`move.term foo bar` renames `foo` to `bar`."
    \case
      [oldName, newName] -> Input.MoveTermI <$> handleHashQualifiedSplit'Arg oldName <*> handleNewName newName
      _ -> Left $ P.wrap "`rename.term` takes two arguments, like `rename.term oldname newname`."

moveAll :: InputPattern
moveAll =
  InputPattern
    "move"
    ["rename"]
    I.Visible
    [ ("definition to move", Required, namespaceOrDefinitionArg),
      ("new location", Required, newNameArg)
    ]
    "`move foo bar` renames the term, type, and namespace foo to bar."
    \case
      [oldName, newName] -> Input.MoveAllI <$> handlePath'Arg oldName <*> handleNewPath newName
      _ -> Left $ P.wrap "`move` takes two arguments, like `move oldname newname`."

renameType :: InputPattern
renameType =
  InputPattern
    "move.type"
    ["rename.type"]
    I.Visible
    [ ("type to move", Required, exactDefinitionTypeQueryArg),
      ("new location", Required, newNameArg)
    ]
    "`move.type foo bar` renames `foo` to `bar`."
    \case
      [oldName, newName] -> Input.MoveTypeI <$> handleHashQualifiedSplit'Arg oldName <*> handleNewName newName
      _ ->
        Left $ P.wrap "`rename.type` takes two arguments, like `rename.type oldname newname`."

deleteGen :: Maybe String -> ArgumentType -> String -> ([Path.HQSplit'] -> DeleteTarget) -> InputPattern
deleteGen suffix queryCompletionArg target mkTarget =
  let cmd = maybe "delete" ("delete." <>) suffix
      info =
        P.wrapColumn2
          [ ( P.sep
                " "
                [ backtick (P.sep " " [P.string cmd, "foo"]),
                  "removes the",
                  P.string target,
                  "name `foo` from the namespace."
                ],
              ""
            ),
            ( P.sep
                " "
                [ backtick (P.sep " " [P.string cmd, "foo bar"]),
                  "removes the",
                  P.string target,
                  "name `foo` and `bar` from the namespace."
                ],
              ""
            )
          ]
      warning =
        P.sep
          " "
          [ backtick (P.string cmd),
            "takes an argument, like",
            backtick (P.sep " " [P.string cmd, "name"]) <> "."
          ]
   in InputPattern
        cmd
        []
        I.Visible
        [("definition to delete", OnePlus, queryCompletionArg)]
        info
        \case
          [] -> Left $ P.wrap warning
          queries -> Input.DeleteI . mkTarget <$> traverse handleHashQualifiedSplit'Arg queries

delete :: InputPattern
delete = deleteGen Nothing exactDefinitionTypeOrTermQueryArg "term or type" (DeleteTarget'TermOrType DeleteOutput'NoDiff)

deleteVerbose :: InputPattern
deleteVerbose = deleteGen (Just "verbose") exactDefinitionTypeOrTermQueryArg "term or type" (DeleteTarget'TermOrType DeleteOutput'Diff)

deleteTerm :: InputPattern
deleteTerm = deleteGen (Just "term") exactDefinitionTermQueryArg "term" (DeleteTarget'Term DeleteOutput'NoDiff)

deleteTermVerbose :: InputPattern
deleteTermVerbose = deleteGen (Just "term.verbose") exactDefinitionTermQueryArg "term" (DeleteTarget'Term DeleteOutput'Diff)

deleteType :: InputPattern
deleteType = deleteGen (Just "type") exactDefinitionTypeQueryArg "type" (DeleteTarget'Type DeleteOutput'NoDiff)

deleteTypeVerbose :: InputPattern
deleteTypeVerbose = deleteGen (Just "type.verbose") exactDefinitionTypeQueryArg "type" (DeleteTarget'Type DeleteOutput'Diff)

deleteProject :: InputPattern
deleteProject =
  InputPattern
    { patternName = "delete.project",
      aliases = ["project.delete"],
      visibility = I.Visible,
      args = [("project to delete", Required, projectNameArg)],
      help =
        P.wrapColumn2
          [ ("`delete.project foo`", "deletes the local project `foo`")
          ],
      parse = \case
        [name] -> Input.DeleteI . DeleteTarget'Project <$> handleProjectArg name
        args -> wrongArgsLength "exactly one argument" args
    }

deleteBranch :: InputPattern
deleteBranch =
  InputPattern
    { patternName = "delete.branch",
      aliases = ["branch.delete"],
      visibility = I.Visible,
      args = [("branch to delete", Required, projectBranchNameArg suggestionsConfig)],
      help =
        P.wrapColumn2
          [ ("`delete.branch foo/bar`", "deletes the branch `bar` in the project `foo`"),
            ("`delete.branch /bar`", "deletes the branch `bar` in the current project")
          ],
      parse = \case
        [name] -> Input.DeleteI . DeleteTarget'ProjectBranch <$> handleMaybeProjectBranchArg name
        args -> wrongArgsLength "exactly one argument" args
    }
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = OnlyWithinCurrentProject,
          branchInclusion = AllBranches
        }

aliasTerm :: InputPattern
aliasTerm =
  InputPattern
    { patternName = "alias.term",
      aliases = [],
      visibility = I.Visible,
      args = [("term to alias", Required, exactDefinitionTermQueryArg), ("alias name", Required, newNameArg)],
      help = "`alias.term foo bar` introduces `bar` with the same definition as `foo`.",
      parse = \case
        [oldName, newName] -> Input.AliasTermI False <$> handleShortHashOrHQSplit'Arg oldName <*> handleSplit'Arg newName
        _ -> Left $ P.wrap "`alias.term` takes two arguments, like `alias.term oldname newname`."
    }

debugAliasTermForce :: InputPattern
debugAliasTermForce =
  InputPattern
    { patternName = "debug.alias.term.force",
      aliases = [],
      visibility = I.Hidden,
      args = [("term to alias", Required, exactDefinitionTermQueryArg), ("alias name", Required, newNameArg)],
      help = "`debug.alias.term.force foo bar` introduces `bar` with the same definition as `foo`.",
      parse = \case
        [oldName, newName] -> Input.AliasTermI True <$> handleShortHashOrHQSplit'Arg oldName <*> handleSplit'Arg newName
        _ ->
          Left $
            P.wrap "`debug.alias.term.force` takes two arguments, like `debug.alias.term.force oldname newname`."
    }

aliasType :: InputPattern
aliasType =
  InputPattern
    "alias.type"
    []
    I.Visible
    [("type to alias", Required, exactDefinitionTypeQueryArg), ("alias name", Required, newNameArg)]
    "`alias.type Foo Bar` introduces `Bar` with the same definition as `Foo`."
    \case
      [oldName, newName] -> Input.AliasTypeI False <$> handleShortHashOrHQSplit'Arg oldName <*> handleSplit'Arg newName
      _ -> Left $ P.wrap "`alias.type` takes two arguments, like `alias.type oldname newname`."

debugAliasTypeForce :: InputPattern
debugAliasTypeForce =
  InputPattern
    { patternName = "debug.alias.type.force",
      aliases = [],
      visibility = I.Hidden,
      args = [("type to alias", Required, exactDefinitionTypeQueryArg), ("alias name", Required, newNameArg)],
      help = "`debug.alias.type.force Foo Bar` introduces `Bar` with the same definition as `Foo`.",
      parse = \case
        [oldName, newName] -> Input.AliasTypeI True <$> handleShortHashOrHQSplit'Arg oldName <*> handleSplit'Arg newName
        _ ->
          Left $
            P.wrap "`debug.alias.type.force` takes two arguments, like `debug.alias.type.force oldname newname`."
    }

aliasMany :: InputPattern
aliasMany =
  InputPattern
    "alias.many"
    ["copy"]
    I.Visible
    [("definition to alias", Required, definitionQueryArg), ("alias names", OnePlus, exactDefinitionArg)]
    ( P.group . P.lines $
        [ P.wrap $
            P.group (makeExample aliasMany ["<relative1>", "[relative2...]", "<namespace>"])
              <> "creates aliases `relative1`, `relative2`, ... in the namespace `namespace`.",
          P.wrap $
            P.group (makeExample aliasMany ["foo.foo", "bar.bar", ".quux"])
              <> "creates aliases `.quux.foo.foo` and `.quux.bar.bar`."
        ]
    )
    \case
      srcs@(_ : _) Cons.:> dest ->
        Input.AliasManyI <$> traverse handleHashQualifiedSplitArg srcs <*> handlePath'Arg dest
      args -> wrongArgsLength "at least two arguments" args

up :: InputPattern
up =
  InputPattern
    "deprecated.up"
    []
    I.Hidden
    []
    (P.wrapColumn2 [(makeExample up [], "move current path up one level (deprecated)")])
    \case
      [] -> Right Input.UpI
      args -> wrongArgsLength "no arguments" args

cd :: InputPattern
cd =
  InputPattern
    "deprecated.cd"
    ["deprecated.namespace"]
    I.Visible
    [("namespace", Required, namespaceArg)]
    ( P.lines
        [ "Moves your perspective to a different namespace. Deprecated for now because too many important things depend on your perspective selection.",
          "",
          P.wrapColumn2
            [ ( makeExample cd ["foo.bar"],
                "descends into foo.bar from the current namespace."
              ),
              ( makeExample cd [".cat.dog"],
                "sets the current namespace to the absolute namespace .cat.dog."
              ),
              ( makeExample cd [".."],
                "moves to the parent of the current namespace. E.g. moves from '.cat.dog' to '.cat'"
              ),
              ( makeExample cd [],
                "invokes a search to select which namespace to move to, which requires that `fzf` can be found within your PATH."
              )
            ]
        ]
    )
    \case
      [Left ".."] -> Right Input.UpI
      [p] -> Input.SwitchBranchI <$> handlePath'Arg p
      args -> wrongArgsLength "exactly one argument" args

back :: InputPattern
back =
  InputPattern
    "back"
    ["popd"]
    I.Visible
    []
    ( P.wrapColumn2
        [ ( makeExample back [],
            "undoes the last" <> makeExample' projectSwitch <> "command."
          )
        ]
    )
    \case
      [] -> pure Input.PopBranchI
      args -> wrongArgsLength "no arguments" args

deleteNamespace :: InputPattern
deleteNamespace =
  InputPattern
    "delete.namespace"
    []
    I.Visible
    [("namespace to delete", Required, namespaceArg)]
    "`delete.namespace <foo>` deletes the namespace `foo`"
    (deleteNamespaceParser Input.Try)

deleteNamespaceForce :: InputPattern
deleteNamespaceForce =
  InputPattern
    "delete.namespace.force"
    []
    I.Visible
    [("namespace to delete", Required, namespaceArg)]
    ( "`delete.namespace.force <foo>` deletes the namespace `foo`,"
        <> "deletion will proceed even if other code depends on definitions in foo."
    )
    (deleteNamespaceParser Input.Force)

deleteNamespaceParser :: Input.Insistence -> I.Arguments -> Either (P.Pretty CT.ColorText) Input
deleteNamespaceParser insistence = \case
  [Left "."] -> first fromString . pure $ Input.DeleteI (DeleteTarget'Namespace insistence Nothing)
  [p] -> Input.DeleteI . DeleteTarget'Namespace insistence <$> (Just <$> handleSplitArg p)
  args -> wrongArgsLength "exactly one argument" args

renameBranch :: InputPattern
renameBranch =
  InputPattern
    "move.namespace"
    ["rename.namespace"]
    I.Visible
    [("namespace to move", Required, namespaceArg), ("new location", Required, newNameArg)]
    "`move.namespace foo bar` renames the path `foo` to `bar`."
    \case
      [src, dest] -> Input.MoveBranchI <$> handlePath'Arg src <*> handlePath'Arg dest
      args -> wrongArgsLength "exactly two arguments" args

history :: InputPattern
history =
  InputPattern
    "history"
    []
    I.Visible
    [("namespace", Optional, namespaceArg)]
    ( P.wrapColumn2
        [ (makeExample history [], "Shows the history of the current path."),
          (makeExample history [".foo"], "Shows history of the path .foo."),
          ( makeExample history ["#9dndk3kbsk13nbpeu"],
            "Shows the history of the namespace with the given hash."
              <> "The full hash must be provided."
          )
        ]
    )
    \case
      [src] -> Input.HistoryI (Just 10) (Just 10) <$> handleBranchIdArg src
      [] -> pure $ Input.HistoryI (Just 10) (Just 10) (BranchAtPath Path.currentPath)
      args -> wrongArgsLength "no more than one argument" args

forkLocal :: InputPattern
forkLocal =
  InputPattern
    "fork"
    ["copy.namespace"]
    I.Visible
    [ ("source location", Required, branchRelativePathArg),
      ("dest location", Required, branchRelativePathArg)
    ]
    ( P.wrapColumn2
        [ ( makeExample forkLocal ["src", "dest"],
            "creates the namespace `dest` as a copy of `src`."
          ),
          ( makeExample forkLocal ["project0/branch0:a.path", "project1/branch1:foo"],
            "creates the namespace `foo` in `branch1` of `project1` as a copy of `a.path` in `project0/branch0`."
          ),
          ( makeExample forkLocal ["srcproject/srcbranch", "dest"],
            "creates the namespace `dest` as a copy of the branch `srcbranch` of `srcproject`."
          )
        ]
    )
    \case
      [src, dest] -> Input.ForkLocalBranchI <$> handleBranchId2Arg src <*> handleBranchRelativePathArg dest
      args -> wrongArgsLength "exactly two arguments" args

libInstallInputPattern :: InputPattern
libInstallInputPattern =
  InputPattern
    { patternName = "lib.install",
      aliases = ["install.lib"],
      visibility = I.Visible,
      args = [],
      help =
        P.lines
          [ P.wrap $
              "The"
                <> makeExample' libInstallInputPattern
                <> "command installs a dependency into the `lib` namespace.",
            "",
            P.wrapColumn2
              [ ( makeExample libInstallInputPattern ["@unison/base/releases/latest"],
                  "installs the latest release of `@unison/base`"
                ),
                ( makeExample libInstallInputPattern ["@unison/base/releases/3.0.0"],
                  "installs version 3.0.0 of `@unison/base`"
                ),
                ( makeExample libInstallInputPattern ["@unison/base/topic"],
                  "installs the `topic` branch of `@unison/base`"
                )
              ]
          ],
      parse = \case
        [arg] -> Input.LibInstallI False <$> handleProjectMaybeBranchArg arg
        args -> wrongArgsLength "exactly one argument" args
    }

reset :: InputPattern
reset =
  InputPattern
    "reset"
    []
    I.Visible
    [ ("namespace, hash, or branch to reset to", Required, namespaceOrProjectBranchArg config),
      ("namespace to be reset", Optional, namespaceOrProjectBranchArg config)
    ]
    ( P.lines
        [ P.wrapColumn2
            [ ("`reset #pvfd222s8n`", "reset the current namespace to the hash `#pvfd222s8n`"),
              ("`reset foo`", "reset the current namespace to the state of the `foo` namespace."),
              ("`reset #pvfd222s8n /topic`", "reset the branch `topic` of the current project to the causal `#pvfd222s8n`.")
            ],
          "",
          P.wrap $ "If you make a mistake using reset, consult the " <> makeExample' branchReflog <> " command and use another " <> makeExample' reset <> " command to return to a previous state."
        ]
    )
    \case
      [resetTo] -> Input.ResetI <$> handleBranchId2Arg resetTo <*> pure Nothing
      [resetTo, branchToReset] -> Input.ResetI <$> handleBranchId2Arg resetTo <*> fmap pure (handleMaybeProjectBranchArg branchToReset)
      args -> wrongArgsLength "one or two arguments" args
  where
    config =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

pull :: InputPattern
pull =
  pullImpl "pull" [] Input.PullWithHistory ""

pullWithoutHistory :: InputPattern
pullWithoutHistory =
  pullImpl
    "pull.without-history"
    []
    Input.PullWithoutHistory
    "without including the remote's history. This usually results in smaller codebase sizes."

pullImpl :: String -> [String] -> Input.PullMode -> P.Pretty CT.ColorText -> InputPattern
pullImpl name aliases pullMode addendum = do
  self
  where
    self =
      InputPattern
        { patternName = name,
          aliases = aliases,
          visibility = I.Visible,
          args =
            [ ("remote namespace to pull", Optional, remoteNamespaceArg),
              ( "destination branch",
                Optional,
                projectBranchNameArg
                  ProjectBranchSuggestionsConfig
                    { showProjectCompletions = False,
                      projectInclusion = AllProjects,
                      branchInclusion = AllBranches
                    }
              )
            ],
          help =
            P.lines
              [ P.wrap $
                  "The"
                    <> makeExample' self
                    <> "command merges a remote namespace into a local branch"
                    <> addendum,
                "",
                P.wrapColumn2
                  [ ( makeExample self ["@unison/base/main"],
                      "merges the branch `main`"
                        <> "of the Unison Share hosted project `@unison/base`"
                        <> "into the current branch"
                    ),
                    ( makeExample self ["@unison/base/main", "my-base/topic"],
                      "merges the branch `main`"
                        <> "of the Unison Share hosted project `@unison/base`"
                        <> "into the branch `topic` of the local `my-base` project"
                    )
                  ],
                "",
                explainRemote Pull
              ],
          parse = \case
            [] -> pure $ Input.PullI Input.PullSourceTarget0 pullMode
            [sourceArg] -> do
              source <- handlePullSourceArg sourceArg
              pure (Input.PullI (Input.PullSourceTarget1 source) pullMode)
            [sourceArg, targetArg] ->
              -- You used to be able to pull into a path, so this arg parser is a little complicated, because
              -- we want to provide helpful suggestions if you are doing a deprecated or invalid thing.
              case ( handlePullSourceArg sourceArg,
                     handleMaybeProjectBranchArg targetArg,
                     handlePath'Arg targetArg
                   ) of
                (Right source, Right target, _) -> Right (Input.PullI (Input.PullSourceTarget2 source target) pullMode)
                (Left err, _, _) -> Left err
                -- Parsing as a path didn't work either; just show the branch parse error
                (Right _, Left err, Left _) -> Left err
                -- The user is trying to pull a branch into `lib`, but you can't do that anymore. We will ignore
                -- the name they've chosed (e.g. "lib.base"), and instead run `lib.install` (which picks a
                -- name), with a reminder message that `lib.install` is the new way.
                --
                -- Oops we're ignoring the "pull mode" but `pull.without-history` shouldn't really be a `pull` anyway...
                ( Right (RemoteRepo.ReadShare'ProjectBranch source),
                  Left _,
                  Right (Path.RelativePath' (Path.Relative (Path.toList -> NameSegment.LibSegment : _)))
                  ) ->
                    case source of
                      This sourceProject -> Right (Input.LibInstallI True (ProjectAndBranch sourceProject Nothing))
                      -- Nice, since we can `pull /branch` but can't `lib.install /branch`, we fail here after all.
                      That _sourceBranch ->
                        Left $
                          P.wrap
                            ( "The use of"
                                <> makeExample' pull
                                <> "to install libraries is now deprecated. Going forward, you can use"
                                <> P.group (makeExample libInstallInputPattern ["@user/project/branch-or-release"] <> ".")
                            )
                      These sourceProject sourceBranch ->
                        Right (Input.LibInstallI True (ProjectAndBranch sourceProject (Just sourceBranch)))
                (Right source, Left _, Right path) ->
                  Left . P.wrap $
                    "I think you want to merge "
                      <> case source of
                        RemoteRepo.ReadShare'LooseCode _sourcePath -> "some non-project code"
                        RemoteRepo.ReadShare'ProjectBranch (This sourceProject) ->
                          prettyProjectNameSlash sourceProject
                        RemoteRepo.ReadShare'ProjectBranch (That ProjectBranchNameOrLatestRelease'LatestRelease) ->
                          "the latest release"
                        RemoteRepo.ReadShare'ProjectBranch (That (ProjectBranchNameOrLatestRelease'Name sourceBranch)) ->
                          prettySlashProjectBranchName sourceBranch
                        RemoteRepo.ReadShare'ProjectBranch (These sourceProject ProjectBranchNameOrLatestRelease'LatestRelease) ->
                          "the latest release of" <> prettyProjectName sourceProject
                        RemoteRepo.ReadShare'ProjectBranch (These sourceProject (ProjectBranchNameOrLatestRelease'Name sourceBranch)) ->
                          prettyProjectAndBranchName (ProjectAndBranch sourceProject sourceBranch)
                      <> " into the "
                      <> prettyPath' path
                      <> " namespace, but the "
                      <> makeExample' pull
                      <> " command only supports merging into the top level of a local project branch."
            args -> wrongArgsLength "no more than two arguments" args
        }

debugTabCompletion :: InputPattern
debugTabCompletion =
  InputPattern
    "debug.tab-complete"
    []
    I.Hidden
    [("command arguments", ZeroPlus, noCompletionsArg)]
    ( P.lines
        [ P.wrap $ "This command can be used to test and debug ucm's tab-completion within transcripts.",
          P.wrap $ "Completions which are finished are prefixed with a * represent finished completions."
        ]
    )
    (fmap Input.DebugTabCompletionI . traverse (unsupportedStructuredArgument debugTabCompletion "text"))

debugLspNameCompletion :: InputPattern
debugLspNameCompletion =
  InputPattern
    "debug.lsp-name-completion"
    []
    I.Hidden
    [("Completion prefix", OnePlus, noCompletionsArg)]
    ( P.lines
        [ P.wrap $ "This command can be used to test and debug ucm's LSP name-completion within transcripts."
        ]
    )
    \case
      [prefix] -> Input.DebugLSPNameCompletionI . Text.pack <$> unsupportedStructuredArgument debugLspNameCompletion "text" prefix
      args -> wrongArgsLength "exactly one argument" args

debugFuzzyOptions :: InputPattern
debugFuzzyOptions =
  InputPattern
    "debug.fuzzy-options"
    []
    I.Hidden
    [("command arguments", OnePlus, noCompletionsArg)]
    ( P.lines
        [ P.wrap $ "This command can be used to test and debug ucm's fuzzy-options within transcripts.",
          P.wrap $ "Write a command invocation with _ for any args you'd like to see completion options for.",
          P.wrap $ "We use _ instead of ! because ! will be expanded by the input parser before it hits the command itself.",
          P.wrap $ "E.g. `debug.fuzzy-options view _`",
          P.wrap $ "or `debug.fuzzy-options merge - _`"
        ]
    )
    \case
      (cmd : args) ->
        Input.DebugFuzzyOptionsI
          <$> unsupportedStructuredArgument debugFuzzyOptions "a command" cmd
          <*> traverse (unsupportedStructuredArgument debugFuzzyOptions "text") args
      args -> wrongArgsLength "at least one argument" args

debugFormat :: InputPattern
debugFormat =
  InputPattern
    "debug.format"
    []
    I.Hidden
    [("source-file", Optional, filePathArg)]
    ( P.lines
        [ P.wrap $ "This command can be used to test ucm's file formatter on the latest typechecked file.",
          makeExample' debugFormat
        ]
    )
    ( \case
        [] -> Right Input.DebugFormatI
        args -> wrongArgsLength "no arguments" args
    )

push :: InputPattern
push =
  InputPattern
    "push"
    []
    I.Visible
    [("remote destination", Optional, remoteNamespaceArg), ("local target", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
    ( P.lines
        [ P.wrap
            "The `push` command merges a local project or namespace into a remote project or namespace.",
          "",
          P.wrapColumn2
            [ ( "`push <remote> <local>`",
                "publishes the contents of a local namespace or branch"
                  <> "into a remote namespace or branch."
              ),
              ( "`push <remote>`",
                "publishes the current namespace or branch into a remote namespace or branch"
              ),
              ( "`push`",
                "publishes the current namespace or branch. Remote mappings for namespaces are configured in"
                  <> "your `.unisonConfig` at the key `RemoteMappings.<namespace>` where `<namespace>` is the "
                  <> "current namespace. Remote mappings for branches default to the branch that you cloned from"
                  <> "or pushed to initially. Otherwise, it is pushed to"
                  <> P.group "@<user handle>/<local project name>"
              )
            ],
          "",
          explainRemote Push
        ]
    )
    $ fmap
      ( \sourceTarget ->
          Input.PushRemoteBranchI
            Input.PushRemoteBranchInput
              { sourceTarget,
                pushBehavior = PushBehavior.RequireNonEmpty
              }
      )
      . \case
        [] -> pure Input.PushSourceTarget0
        [targetStr] -> Input.PushSourceTarget1 <$> handlePushTargetArg targetStr
        [targetStr, sourceStr] ->
          Input.PushSourceTarget2 <$> handlePushSourceArg sourceStr <*> handlePushTargetArg targetStr
        args -> wrongArgsLength "no more than two arguments" args
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

pushCreate :: InputPattern
pushCreate =
  InputPattern
    "push.create"
    []
    I.Visible
    [("remote destination", Optional, remoteNamespaceArg), ("local target", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
    ( P.lines
        [ P.wrap
            "The `push.create` command pushes a local namespace to an empty remote namespace.",
          "",
          P.wrapColumn2
            [ ( "`push.create remote local`",
                "pushes the contents of the local namespace `local`"
                  <> "into the empty remote namespace `remote`."
              ),
              ( "`push.create remote`",
                "publishes the current namespace into the empty remote namespace `remote`"
              ),
              ( "`push.create`",
                "publishes the current namespace into the remote namespace configured in your `.unisonConfig`"
                  <> "at the key `RemoteMappings.<namespace>` where `<namespace>` is the current namespace,"
                  <> "then publishes the current namespace to that location."
              )
            ],
          "",
          explainRemote Push
        ]
    )
    $ fmap
      ( \sourceTarget ->
          Input.PushRemoteBranchI
            Input.PushRemoteBranchInput
              { sourceTarget,
                pushBehavior = PushBehavior.RequireEmpty
              }
      )
      . \case
        [] -> pure Input.PushSourceTarget0
        [targetStr] -> Input.PushSourceTarget1 <$> handlePushTargetArg targetStr
        [targetStr, sourceStr] ->
          Input.PushSourceTarget2 <$> handlePushSourceArg sourceStr <*> handlePushTargetArg targetStr
        args -> wrongArgsLength "no more than two arguments" args
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

pushForce :: InputPattern
pushForce =
  InputPattern
    "unsafe.force-push"
    []
    I.Hidden
    [("remote destination", Optional, remoteNamespaceArg), ("local source", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
    (P.wrap "Like `push`, but overwrites any remote namespace.")
    $ fmap
      ( \sourceTarget ->
          Input.PushRemoteBranchI
            Input.PushRemoteBranchInput
              { sourceTarget,
                pushBehavior = PushBehavior.ForcePush
              }
      )
      . \case
        [] -> pure Input.PushSourceTarget0
        [targetStr] -> Input.PushSourceTarget1 <$> handlePushTargetArg targetStr
        [targetStr, sourceStr] ->
          Input.PushSourceTarget2 <$> handlePushSourceArg sourceStr <*> handlePushTargetArg targetStr
        args -> wrongArgsLength "no more than two arguments" args
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

pushExhaustive :: InputPattern
pushExhaustive =
  InputPattern
    "debug.push-exhaustive"
    []
    I.Hidden
    [("remote destination", Optional, remoteNamespaceArg), ("local target", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
    ( P.lines
        [ P.wrap $
            "The "
              <> makeExample' pushExhaustive
              <> "command can be used in place of"
              <> makeExample' push
              <> "to repair remote namespaces"
              <> "which were pushed incompletely due to a bug in UCM"
              <> "versions M1l and earlier. It may be extra slow!"
        ]
    )
    $ fmap
      ( \sourceTarget ->
          Input.PushRemoteBranchI
            Input.PushRemoteBranchInput
              { sourceTarget,
                pushBehavior = PushBehavior.RequireNonEmpty
              }
      )
      . \case
        [] -> pure Input.PushSourceTarget0
        [targetStr] -> Input.PushSourceTarget1 <$> handlePushTargetArg targetStr
        [targetStr, sourceStr] ->
          Input.PushSourceTarget2 <$> handlePushSourceArg sourceStr <*> handlePushTargetArg targetStr
        args -> wrongArgsLength "no more than two arguments" args
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

mergeOldSquashInputPattern :: InputPattern
mergeOldSquashInputPattern =
  InputPattern
    { patternName = "merge.old.squash",
      aliases = ["squash.old"],
      visibility = I.Hidden,
      args =
        [ ("namespace or branch to be squashed", Required, namespaceOrProjectBranchArg suggestionsConfig),
          ("merge destination", Required, namespaceOrProjectBranchArg suggestionsConfig)
        ],
      help =
        P.wrap $
          makeExample mergeOldSquashInputPattern ["src", "dest"]
            <> "merges `src` namespace or branch into the `dest` namespace or branch,"
            <> "discarding the history of `src` in the process."
            <> "The resulting `dest` will have (at most) 1"
            <> "additional history entry.",
      parse = \case
        [src] ->
          Input.MergeLocalBranchI
            <$> handleBranchRelativePathArg src
            <*> pure Nothing
            <*> pure Branch.SquashMerge
        [src, dest] ->
          Input.MergeLocalBranchI
            <$> handleBranchRelativePathArg src
            <*> (Just <$> handleBranchRelativePathArg dest)
            <*> pure Branch.SquashMerge
        args -> wrongArgsLength "exactly two arguments" args
    }
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

mergeOldInputPattern :: InputPattern
mergeOldInputPattern =
  InputPattern
    "merge.old"
    []
    I.Hidden
    [ ("branch or namespace to merge", Required, namespaceOrProjectBranchArg config),
      ("merge destination", Optional, namespaceOrProjectBranchArg config)
    ]
    ( P.column2
        [ ( makeExample mergeOldInputPattern ["foo/bar", "baz/qux"],
            "merges the `foo/bar` branch into the `baz/qux` branch"
          ),
          ( makeExample mergeOldInputPattern ["/topic", "/main"],
            "merges the branch `topic` of the current project into the `main` branch of the current project"
          ),
          ( makeExample mergeOldInputPattern ["foo/topic", "/main"],
            "merges the branch `topic` of the project `foo` into the `main` branch of the current project"
          ),
          ( makeExample mergeOldInputPattern ["/topic", "foo/main"],
            "merges the branch `topic` of the current project into the `main` branch of the project 'foo`"
          )
        ]
    )
    ( \case
        [src] ->
          Input.MergeLocalBranchI
            <$> handleBranchRelativePathArg src
            <*> pure Nothing
            <*> pure Branch.RegularMerge
        [src, dest] ->
          Input.MergeLocalBranchI
            <$> handleBranchRelativePathArg src
            <*> (Just <$> handleBranchRelativePathArg dest)
            <*> pure Branch.RegularMerge
        args -> wrongArgsLength "one or two arguments" args
    )
  where
    config =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

mergeInputPattern :: InputPattern
mergeInputPattern =
  InputPattern
    { patternName = "merge",
      aliases = [],
      visibility = I.Visible,
      args =
        [ ( "branch to merge",
            Required,
            projectBranchNameArg
              ProjectBranchSuggestionsConfig
                { showProjectCompletions = True,
                  projectInclusion = AllProjects,
                  branchInclusion = ExcludeCurrentBranch
                }
          )
        ],
      help = P.wrap $ makeExample mergeInputPattern ["/branch"] <> "merges `branch` into the current branch",
      parse =
        \case
          [branchString] -> Input.MergeI <$> handleMaybeProjectBranchArg branchString
          args -> wrongArgsLength "exactly one argument" args
    }

mergeCommitInputPattern :: InputPattern
mergeCommitInputPattern =
  InputPattern
    { patternName = "merge.commit",
      aliases = ["commit.merge"],
      visibility = I.Visible,
      args = [],
      help =
        let mainBranch = UnsafeProjectBranchName "main"
            tempBranch = UnsafeProjectBranchName "merge-topic-into-main"
         in P.wrap
              ( makeExample' mergeCommitInputPattern
                  <> "merges a temporary branch created by the"
                  <> makeExample' mergeInputPattern
                  <> "command back into its parent branch, and removes the temporary branch."
              )
              <> P.newline
              <> P.newline
              <> P.wrap
                ( "For example, if you've done"
                    <> makeExample mergeInputPattern ["topic"]
                    <> "from"
                    <> P.group (prettyProjectBranchName mainBranch <> ",")
                    <> "then"
                    <> makeExample' mergeCommitInputPattern
                    <> "is equivalent to doing"
                )
              <> P.newline
              <> P.newline
              <> P.indentN
                2
                ( P.bulleted
                    [ makeExampleNoBackticks projectSwitch [prettySlashProjectBranchName mainBranch],
                      makeExampleNoBackticks mergeInputPattern [prettySlashProjectBranchName tempBranch],
                      makeExampleNoBackticks deleteBranch [prettySlashProjectBranchName tempBranch]
                    ]
                ),
      parse = \case
        [] -> Right Input.MergeCommitI
        args -> wrongArgsLength "no arguments" args
    }

diffNamespace :: InputPattern
diffNamespace =
  InputPattern
    "diff.namespace"
    []
    I.Visible
    [("before namespace", Required, namespaceOrProjectBranchArg suggestionsConfig), ("after namespace", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
    ( P.column2
        [ ( "`diff.namespace before after`",
            P.wrap "shows how the namespace `after` differs from the namespace `before`"
          ),
          ( "`diff.namespace before`",
            P.wrap "shows how the current namespace differs from the namespace `before`"
          )
        ]
    )
    ( \case
        [before, after] -> Input.DiffNamespaceI <$> handleBranchId2Arg before <*> handleBranchId2Arg after
        [before] -> Input.DiffNamespaceI <$> handleBranchId2Arg before <*> pure (Right . UnqualifiedPath $ Path.currentPath)
        args -> wrongArgsLength "one or two arguments" args
    )
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

mergeOldPreviewInputPattern :: InputPattern
mergeOldPreviewInputPattern =
  InputPattern
    "merge.old.preview"
    []
    I.Hidden
    [("branch or namespace to merge", Required, namespaceOrProjectBranchArg suggestionsConfig), ("merge destination", Optional, namespaceOrProjectBranchArg suggestionsConfig)]
    ( P.column2
        [ ( makeExample mergeOldPreviewInputPattern ["src"],
            "shows how the current namespace will change after a " <> makeExample mergeOldInputPattern ["src"]
          ),
          ( makeExample mergeOldPreviewInputPattern ["src", "dest"],
            "shows how `dest` namespace will change after a " <> makeExample mergeOldInputPattern ["src", "dest"]
          )
        ]
    )
    ( \case
        [src] -> Input.PreviewMergeLocalBranchI <$> handleBranchRelativePathArg src <*> pure Nothing
        [src, dest] ->
          Input.PreviewMergeLocalBranchI <$> handleBranchRelativePathArg src <*> (Just <$> handleBranchRelativePathArg dest)
        args -> wrongArgsLength "one or two arguments" args
    )
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

deprecatedViewRootReflog :: InputPattern
deprecatedViewRootReflog =
  InputPattern
    "deprecated.root-reflog"
    []
    I.Visible
    []
    ( "`deprecated.root-reflog` lists the changes that have affected the root namespace. This has been deprecated in favor of "
        <> makeExample branchReflog []
        <> " which shows the reflog for the current project."
    )
    ( \case
        [] -> pure Input.ShowRootReflogI
        _ ->
          Left . P.string $
            I.patternName deprecatedViewRootReflog ++ " doesn't take any arguments."
    )

branchReflog :: InputPattern
branchReflog =
  InputPattern
    "reflog"
    ["reflog.branch", "branch.reflog"]
    I.Visible
    []
    ( P.lines
        [ "`reflog` lists all the changes that have affected the current branch.",
          "`reflog /mybranch` lists all the changes that have affected /mybranch."
        ]
    )
    ( \case
        [] -> pure $ Input.ShowProjectBranchReflogI Nothing
        [branchRef] -> Input.ShowProjectBranchReflogI <$> (Just <$> handleMaybeProjectBranchArg branchRef)
        _ -> Left (I.help branchReflog)
    )

projectReflog :: InputPattern
projectReflog =
  InputPattern
    "project.reflog"
    ["reflog.project"]
    I.Visible
    []
    ( P.lines
        [ "`project.reflog` lists all the changes that have affected any branches in the current project.",
          "`project.reflog myproject` lists all the changes that have affected any branches in myproject."
        ]
    )
    ( \case
        [] -> pure $ Input.ShowProjectReflogI Nothing
        [projectRef] -> Input.ShowProjectReflogI <$> (Just <$> handleProjectArg projectRef)
        _ -> Left (I.help projectReflog)
    )

globalReflog :: InputPattern
globalReflog =
  InputPattern
    "reflog.global"
    []
    I.Visible
    []
    ( P.lines
        [ "`reflog.global` lists all recent changes across all projects and branches."
        ]
    )
    ( \case
        [] -> pure $ Input.ShowGlobalReflogI
        _ -> Left (I.help globalReflog)
    )

edit :: InputPattern
edit =
  InputPattern
    { patternName = "edit",
      aliases = [],
      visibility = I.Visible,
      args = [("definition to edit", OnePlus, definitionQueryArg)],
      help =
        P.lines
          [ "`edit foo` prepends the definition of `foo` to the top of the most "
              <> "recently saved file.",
            "`edit` without arguments invokes a search to select a definition for editing, which requires that `fzf` can be found within your PATH."
          ],
      parse =
        maybe
          (wrongArgsLength "at least one argument" [])
          ( fmap (Input.ShowDefinitionI (Input.LatestFileLocation Input.WithinFold) Input.ShowDefinitionLocal)
              . traverse handleHashQualifiedNameArg
          )
          . NE.nonEmpty
    }

editNew :: InputPattern
editNew =
  InputPattern
    { patternName = "edit.new",
      aliases = [],
      visibility = I.Visible,
      args = [("definition to edit", OnePlus, definitionQueryArg)],
      help = "Like `edit`, but adds a new fold line below the definitions.",
      parse =
        maybe
          (wrongArgsLength "at least one argument" [])
          ( fmap (Input.ShowDefinitionI (Input.LatestFileLocation Input.AboveFold) Input.ShowDefinitionLocal)
              . traverse handleHashQualifiedNameArg
          )
          . NE.nonEmpty
    }

editNamespace :: InputPattern
editNamespace =
  InputPattern
    { patternName = "edit.namespace",
      aliases = [],
      visibility = I.Visible,
      args = [("namespace to load definitions from", ZeroPlus, namespaceArg)],
      help =
        P.lines
          [ "`edit.namespace` will load all terms and types contained within the current namespace into your scratch file. This includes definitions in namespaces, but excludes libraries.",
            "`edit.namespace ns1 ns2 ...` loads the terms and types contained within the provided namespaces."
          ],
      parse = fmap Input.EditNamespaceI . traverse handlePathArg
    }

topicNameArg :: ArgumentType
topicNameArg =
  let topics = Map.keys helpTopicsMap
   in ArgumentType
        { typeName = "topic",
          suggestions = \q _ _ _ -> pure (exactComplete q topics),
          fzfResolver = Just $ Resolvers.fuzzySelectFromList (Text.pack <$> topics)
        }

helpTopics :: InputPattern
helpTopics =
  InputPattern
    "help-topics"
    ["help-topic"]
    I.Visible
    [("topic", Optional, topicNameArg)]
    ("`help-topics` lists all topics and `help-topics <topic>` shows an explanation of that topic.")
    ( \case
        [] -> Right $ Input.CreateMessage topics
        [topic] -> do
          topic <- unsupportedStructuredArgument helpTopics "a help topic" topic
          case Map.lookup topic helpTopicsMap of
            Nothing -> Left $ "I don't know of that topic. Try `help-topics`."
            Just t -> Right $ Input.CreateMessage t
        _ -> Left $ "Use `help-topics <topic>` or `help-topics`."
    )
  where
    topics =
      P.callout "🌻" $
        P.lines
          [ "Here's a list of topics I can tell you more about: ",
            "",
            P.indentN 2 $ P.sep "\n" (P.string <$> Map.keys helpTopicsMap),
            "",
            aside "Example" "use `help-topics filestatus` to learn more about that topic."
          ]

helpTopicsMap :: Map String (P.Pretty P.ColorText)
helpTopicsMap =
  Map.fromList
    [ ("testcache", testCacheMsg),
      ("filestatus", fileStatusMsg),
      ("messages.disallowedAbsolute", disallowedAbsoluteMsg),
      ("remotes", remotesMsg),
      ("namespaces", pathnamesMsg),
      ("projects", projectsMsg)
    ]
  where
    blankline = ("", "")
    fileStatusMsg =
      P.callout "📓" . P.lines $
        [ P.wrap $
            "Here's a list of possible status messages you might see"
              <> "for definitions in a .u file.",
          "",
          P.wrapColumn2
            [ ( P.bold $ SR.prettyStatus SR.Collision,
                "A definition with the same name as an existing definition. Doing"
                  <> "`update` instead of `add` will turn this failure into a successful"
                  <> "update."
              ),
              blankline,
              ( P.bold $ SR.prettyStatus SR.TermExistingConstructorCollision,
                "A definition with the same name as an existing constructor for "
                  <> "some data type. Rename your definition or the data type before"
                  <> "trying again to `add` or `update`."
              ),
              blankline,
              ( P.bold $ SR.prettyStatus SR.ConstructorExistingTermCollision,
                "A type defined in the file has a constructor that's named the"
                  <> "same as an existing term. Rename that term or your constructor"
                  <> "before trying again to `add` or `update`."
              ),
              blankline,
              ( P.bold $ SR.prettyStatus SR.BlockedDependency,
                "This definition was blocked because it dependended on "
                  <> "a definition with a failed status."
              ),
              blankline,
              ( P.bold $ SR.prettyStatus SR.ExtraDefinition,
                "This definition was added because it was a dependency of"
                  <> "a definition explicitly selected."
              )
            ]
        ]
    testCacheMsg =
      P.callout "🎈" . P.lines $
        [ P.wrap $
            "Unison caches the results of "
              <> P.blue "test>"
              <> "watch expressions. Since these expressions are pure and"
              <> "always yield the same result when evaluated, there's no need"
              <> "to run them more than once!",
          "",
          P.wrap $
            "A test is rerun only if it has changed, or if one"
              <> "of the definitions it depends on has changed."
        ]
    pathnamesMsg =
      P.callout "\129488" . P.lines $
        [ P.wrap $
            "There are two kinds of namespaces,"
              <> P.group (P.blue "absolute" <> ",")
              <> "such as"
              <> P.group ("(" <> P.blue ".foo.bar")
              <> "or"
              <> P.group (P.blue ".base.math.+" <> ")")
              <> "and"
              <> P.group (P.green "relative" <> ",")
              <> "such as"
              <> P.group ("(" <> P.green "math.sqrt")
              <> "or"
              <> P.group (P.green "util.List.++" <> ")."),
          "",
          P.wrap $
            "Relative names are converted to absolute names by prepending the current namespace."
              <> "For example, if your Unison prompt reads:",
          "",
          P.indentN 2 $ P.blue ".foo.bar>",
          "",
          "and your .u file looks like:",
          "",
          P.indentN 2 $ P.green "x" <> " = 41",
          "",
          P.wrap $
            "then doing an"
              <> P.blue "add"
              <> "will create the definition with the absolute name"
              <> P.group (P.blue ".foo.bar.x" <> " = 41"),
          "",
          P.wrap $
            "and you can refer to"
              <> P.green "x"
              <> "by its absolute name "
              <> P.blue ".foo.bar.x"
              <> "elsewhere"
              <> "in your code. For instance:",
          "",
          P.indentN 2 $
            "answerToLifeTheUniverseAndEverything = " <> P.blue ".foo.bar.x" <> " + 1"
        ]

    disallowedAbsoluteMsg =
      P.callout "\129302" . P.lines $
        [ P.wrap $
            "Although I can understand absolute (ex: .foo.bar) or"
              <> "relative (ex: util.math.sqrt) references to existing definitions"
              <> P.group ("(" <> P.blue "help namespaces")
              <> "to learn more),"
              <> "I can't yet handle giving new definitions with absolute names in a .u file.",
          "",
          P.wrap $
            "As a workaround, you can give definitions with a relative name"
              <> "temporarily (like `exports.blah.foo`) and then use `move.*`."
        ]
    remotesMsg =
      P.callout "\129302" . P.lines $
        [ P.wrap $
            "Local projects may be associated with at most one remote project on Unison Share."
              <> "When this relationship is established, it becomes the default argument for a"
              <> "number of share commands. For example, running `push` or `pull` in a project"
              <> "with no arguments will push to or pull from the associated remote, if it exists.",
          "",
          P.wrap $
            "This association is created automatically on when a project is created by `clone`."
              <> "If the project was created locally then the relationship will be established on"
              <> "the first `push`."
        ]
    projectsMsg =
      P.lines $
        [ P.wrap $
            "A project is a versioned collection of code that can be edited, published, and depended on other projects."
              <> "Unison projects are analogous to Git repositories.",
          "",
          P.column2
            [ (patternName projectCreate, "create a new project"),
              (patternName projectsInputPattern, "list all your projects"),
              (patternName branchInputPattern, "create a new workstream"),
              (patternName branchesInputPattern, "list all your branches"),
              (patternName mergeInputPattern, "merge one branch into another"),
              (patternName projectSwitch, "switch to a project or branch"),
              (patternName push, "upload your changes to Unison Share"),
              (patternName pull, "download code(/changes/updates) from Unison Share"),
              (patternName clone, "download a Unison Share project or branch for contribution")
            ],
          "",
          tip ("Use" <> makeExample help [patternName projectCreate] <> "to learn more."),
          "",
          P.wrap $
            "For full documentation, see"
              <> prettyURI (fromJust (URI.parseURI "https://unison-lang.org/learn/projects"))
        ]

help :: InputPattern
help =
  InputPattern
    "help"
    ["?"]
    I.Visible
    [("command", Optional, commandNameArg)]
    "`help` shows general help and `help <cmd>` shows help for one command."
    $ \case
      [] ->
        Right . Input.CreateMessage $
          intercalateMap
            "\n\n"
            showPatternHelp
            visibleInputs
      [cmd] -> do
        cmd <- unsupportedStructuredArgument help "a command" cmd
        case (Map.lookup cmd commandsByName, isHelp cmd) of
          (Nothing, Just msg) -> Right $ Input.CreateMessage msg
          (Nothing, Nothing) -> Left $ "I don't know of that command. Try" <> makeExampleEOS help []
          (Just pat, Nothing) -> Right . Input.CreateMessage $ showPatternHelp pat
          -- If we have a command and a help topic with the same name (like "projects"), then append a tip to the
          -- command's help that suggests running `help-topic command`
          (Just pat, Just _) ->
            Right . Input.CreateMessage $
              showPatternHelp pat
                <> P.newline
                <> P.newline
                <> ( tip $
                       "To read more about"
                         <> P.group (P.string cmd <> ",")
                         <> "use"
                         <> makeExample helpTopics [P.string cmd]
                   )
      _ -> Left "Use `help <cmd>` or `help`."
  where
    commandsByName =
      Map.fromList $ do
        input@I.InputPattern {I.patternName, I.aliases} <- validInputs
        name <- patternName : aliases
        pure (name, input)
    isHelp s = Map.lookup s helpTopicsMap

quit :: InputPattern
quit =
  InputPattern
    "quit"
    ["exit", ":q"]
    I.Visible
    []
    "Exits the Unison command line interface."
    \case
      [] -> pure Input.QuitI
      _ -> Left "Use `quit`, `exit`, or <Ctrl-D> to quit."

names :: Input.IsGlobal -> InputPattern
names isGlobal =
  InputPattern
    cmdName
    []
    I.Visible
    [("name or hash", Required, definitionQueryArg)]
    (P.wrap $ makeExample (names isGlobal) ["foo"] <> description)
    $ \case
      [thing] -> Input.NamesI isGlobal <$> handleHashQualifiedNameArg thing
      args -> wrongArgsLength "exactly one argument" args
  where
    description
      | isGlobal = "Iteratively search across all projects and branches for names matching `foo`. Note that this is expected to be quite slow and is primarily for debugging issues with your codebase."
      | otherwise = "List all known names for `foo` in the current branch."
    cmdName = if isGlobal then "debug.names.global" else "names"

dependents, dependencies :: InputPattern
dependents =
  InputPattern
    "dependents"
    []
    I.Visible
    [("definition", Required, definitionQueryArg)]
    "List the named dependents of the specified definition."
    $ \case
      [thing] -> Input.ListDependentsI <$> handleHashQualifiedNameArg thing
      args -> wrongArgsLength "exactly one argument" args
dependencies =
  InputPattern
    "dependencies"
    []
    I.Visible
    [("definition", Required, definitionQueryArg)]
    "List the dependencies of the specified definition."
    $ \case
      [thing] -> Input.ListDependenciesI <$> handleHashQualifiedNameArg thing
      args -> wrongArgsLength "exactly one argument" args

namespaceDependencies :: InputPattern
namespaceDependencies =
  InputPattern
    "namespace.dependencies"
    []
    I.Visible
    [("namespace", Optional, namespaceArg)]
    "List the external dependencies of the specified namespace."
    $ \case
      [p] -> Input.NamespaceDependenciesI . pure <$> handlePath'Arg p
      [] -> pure (Input.NamespaceDependenciesI Nothing)
      args -> wrongArgsLength "no more than one argument" args

debugNumberedArgs :: InputPattern
debugNumberedArgs =
  InputPattern
    "debug.numberedArgs"
    []
    I.Visible
    []
    "Dump the contents of the numbered args state."
    (const $ Right Input.DebugNumberedArgsI)

debugFileHashes :: InputPattern
debugFileHashes =
  InputPattern
    "debug.file"
    []
    I.Visible
    []
    "View details about the most recent successfully typechecked file."
    (const $ Right Input.DebugTypecheckedUnisonFileI)

debugDumpNamespace :: InputPattern
debugDumpNamespace =
  InputPattern
    "debug.dump-namespace"
    []
    I.Visible
    []
    "Dump the namespace to a text file"
    (const $ Right Input.DebugDumpNamespacesI)

debugDumpNamespaceSimple :: InputPattern
debugDumpNamespaceSimple =
  InputPattern
    "debug.dump-namespace-simple"
    []
    I.Visible
    []
    "Dump the namespace to a text file"
    (const $ Right Input.DebugDumpNamespaceSimpleI)

debugTerm :: InputPattern
debugTerm =
  InputPattern
    "debug.term.abt"
    []
    I.Hidden
    [("term", Required, exactDefinitionTermQueryArg)]
    "View debugging information for a given term."
    ( \case
        [thing] -> Input.DebugTermI False <$> handleHashQualifiedNameArg thing
        args -> wrongArgsLength "exactly one argument" args
    )

debugTermVerbose :: InputPattern
debugTermVerbose =
  InputPattern
    "debug.term.abt.verbose"
    []
    I.Hidden
    [("term", Required, exactDefinitionTermQueryArg)]
    "View verbose debugging information for a given term."
    ( \case
        [thing] -> Input.DebugTermI True <$> handleHashQualifiedNameArg thing
        args -> wrongArgsLength "exactly one argument" args
    )

debugType :: InputPattern
debugType =
  InputPattern
    "debug.type.abt"
    []
    I.Hidden
    [("type", Required, exactDefinitionTypeQueryArg)]
    "View debugging information for a given type."
    ( \case
        [thing] -> Input.DebugTypeI <$> handleHashQualifiedNameArg thing
        args -> wrongArgsLength "exactly one argument" args
    )

debugLSPFoldRanges :: InputPattern
debugLSPFoldRanges =
  InputPattern
    "debug.lsp.fold-ranges"
    []
    I.Hidden
    []
    "Output the source from the most recently parsed file, but annotated with the computed fold ranges."
    (const $ Right Input.DebugLSPFoldRangesI)

debugClearWatchCache :: InputPattern
debugClearWatchCache =
  InputPattern
    "debug.clear-cache"
    []
    I.Visible
    []
    "Clear the watch expression cache"
    (const $ Right Input.DebugClearWatchI)

debugDoctor :: InputPattern
debugDoctor =
  InputPattern
    "debug.doctor"
    []
    I.Visible
    []
    ( P.wrap "Analyze your codebase for errors and inconsistencies."
    )
    ( \case
        [] -> Right $ Input.DebugDoctorI
        args -> wrongArgsLength "no arguments" args
    )

debugNameDiff :: InputPattern
debugNameDiff =
  InputPattern
    { patternName = "debug.name-diff",
      aliases = [],
      visibility = I.Hidden,
      args = [("before namespace", Required, namespaceArg), ("after namespace", Required, namespaceArg)],
      help = P.wrap "List all name changes between two causal hashes. Does not detect patch changes.",
      parse = \case
        [from, to] -> Input.DebugNameDiffI <$> handleShortCausalHashArg from <*> handleShortCausalHashArg to
        args -> wrongArgsLength "exactly two arguments" args
    }

test :: InputPattern
test =
  InputPattern
    { patternName = "test",
      aliases = [],
      visibility = I.Visible,
      args = [("namespace", Optional, namespaceArg)],
      help =
        P.wrapColumn2
          [ ("`test`", "runs unit tests for the current branch"),
            ("`test foo`", "runs unit tests for the current branch defined in namespace `foo`")
          ],
      parse =
        fmap
          ( \path ->
              Input.TestI
                False
                Input.TestInput
                  { includeLibNamespace = False,
                    path,
                    showFailures = True,
                    showSuccesses = True
                  }
          )
          . \case
            [] -> pure Path.empty
            [pathString] -> handlePathArg pathString
            args -> wrongArgsLength "no more than one argument" args
    }

testNative :: InputPattern
testNative =
  InputPattern
    { patternName = "test.native",
      aliases = [],
      visibility = I.Hidden,
      args = [("namespace", Optional, namespaceArg)],
      help =
        P.wrapColumn2
          [ ( "`test.native`",
              "runs unit tests for the current branch on the native runtime"
            ),
            ("`test foo`", "runs unit tests for the current branch defined in namespace `foo` on the native runtime")
          ],
      parse =
        fmap
          ( \path ->
              Input.TestI
                True
                Input.TestInput
                  { includeLibNamespace = False,
                    path,
                    showFailures = True,
                    showSuccesses = True
                  }
          )
          . \case
            [] -> pure Path.empty
            [pathString] -> handlePathArg pathString
            args -> wrongArgsLength "no more than one argument" args
    }

testAll :: InputPattern
testAll =
  InputPattern
    "test.all"
    []
    I.Visible
    []
    "`test.all` runs unit tests for the current branch (including the `lib` namespace)."
    ( const $
        pure $
          Input.TestI
            False
            Input.TestInput
              { includeLibNamespace = True,
                path = Path.empty,
                showFailures = True,
                showSuccesses = True
              }
    )

testAllNative :: InputPattern
testAllNative =
  InputPattern
    "test.native.all"
    ["test.all.native"]
    I.Hidden
    []
    "`test.native.all` runs unit tests for the current branch (including the `lib` namespace) on the native runtime."
    ( const $
        pure $
          Input.TestI
            True
            Input.TestInput
              { includeLibNamespace = True,
                path = Path.empty,
                showFailures = True,
                showSuccesses = True
              }
    )

docsToHtml :: InputPattern
docsToHtml =
  InputPattern
    "docs.to-html"
    []
    I.Visible
    [("namespace", Required, branchRelativePathArg), ("", Required, filePathArg)]
    ( P.wrapColumn2
        [ ( makeExample docsToHtml [".path.to.ns", "doc-dir"],
            "Render all docs contained within the namespace `.path.to.ns`, no matter how deep, to html files in `doc-dir` in the directory UCM was run from."
          ),
          ( makeExample docsToHtml ["project0/branch0:a.path", "/tmp/doc-dir"],
            "Renders all docs anywhere in the namespace `a.path` from `branch0` of `project0` to html in `/tmp/doc-dir`."
          )
        ]
    )
    \case
      [namespacePath, destinationFilePath] ->
        Input.DocsToHtmlI
          <$> handleBranchRelativePathArg namespacePath
          <*> unsupportedStructuredArgument docsToHtml "a file name" destinationFilePath
      args -> wrongArgsLength "exactly two arguments" args

docToMarkdown :: InputPattern
docToMarkdown =
  InputPattern
    "debug.doc-to-markdown"
    []
    I.Visible
    [("doc to render", Required, exactDefinitionTermQueryArg)]
    ( P.wrapColumn2
        [ ( "`debug.doc-to-markdown term.doc`",
            "Render a doc to markdown."
          )
        ]
    )
    \case
      [docNameText] -> Input.DocToMarkdownI <$> handleNameArg docNameText
      args -> wrongArgsLength "exactly one argument" args

execute :: InputPattern
execute =
  InputPattern
    "run"
    []
    I.Visible
    [("definition to execute", Required, exactDefinitionTermQueryArg), ("argument", ZeroPlus, noCompletionsArg)]
    ( P.wrapColumn2
        [ ( "`run mymain args...`",
            "Runs `!mymain`, where `mymain` is searched for in the most recent"
              <> "typechecked file, or in the codebase."
              <> "Any provided arguments will be passed as program arguments as though they were"
              <> "provided at the command line when running mymain as an executable."
          )
        ]
    )
    $ \case
      main : args ->
        Input.ExecuteI
          <$> handleHashQualifiedNameArg main
          <*> traverse (unsupportedStructuredArgument execute "a command-line argument") args
      [] -> wrongArgsLength "at least one argument" []

saveExecuteResult :: InputPattern
saveExecuteResult =
  InputPattern
    "add.run"
    []
    I.Visible
    [("new name", Required, newNameArg)]
    ( "`add.run name` adds to the codebase the result of the most recent `run` command"
        <> " as `name`."
    )
    $ \case
      [w] -> Input.SaveExecuteResultI <$> handleNameArg w
      args -> wrongArgsLength "exactly one argument" args

ioTest :: InputPattern
ioTest =
  InputPattern
    { patternName = "io.test",
      aliases = ["test.io"],
      visibility = I.Visible,
      args = [("test to run", Required, exactDefinitionTermQueryArg)],
      help =
        P.wrapColumn2
          [ ( "`io.test mytest`",
              "Runs `!mytest`, where `mytest` is a delayed test that can use the `IO` and `Exception` abilities."
            )
          ],
      parse = \case
        [thing] -> Input.IOTestI False <$> handleHashQualifiedNameArg thing
        args -> wrongArgsLength "exactly one argument" args
    }

ioTestNative :: InputPattern
ioTestNative =
  InputPattern
    { patternName = "io.test.native",
      aliases = ["test.io.native", "test.native.io"],
      visibility = I.Hidden,
      args = [("test to run", Required, exactDefinitionTermQueryArg)],
      help =
        P.wrapColumn2
          [ ( "`io.test.native mytest`",
              "Runs `!mytest` on the native runtime, where `mytest` "
                <> "is a delayed test that can use the `IO` and "
                <> "`Exception` abilities."
            )
          ],
      parse = \case
        [thing] -> Input.IOTestI True <$> handleHashQualifiedNameArg thing
        args -> wrongArgsLength "exactly one argument" args
    }

ioTestAll :: InputPattern
ioTestAll =
  InputPattern
    { patternName = "io.test.all",
      aliases = ["test.io.all"],
      visibility = I.Visible,
      args = [],
      help =
        P.wrapColumn2
          [ ( "`io.test.all`",
              "runs unit tests for the current branch that use IO"
            )
          ],
      parse = \case
        [] -> Right (Input.IOTestAllI False)
        args -> wrongArgsLength "no arguments" args
    }

ioTestAllNative :: InputPattern
ioTestAllNative =
  InputPattern
    { patternName = "io.test.native.all",
      aliases = ["test.io.native.all", "test.native.io.all"],
      visibility = I.Hidden,
      args = [],
      help =
        P.wrapColumn2
          [ ( "`io.test.native.all`",
              "runs unit tests for the current branch that use IO"
            )
          ],
      parse = \case
        [] -> Right (Input.IOTestAllI True)
        args -> wrongArgsLength "no arguments" args
    }

makeStandalone :: InputPattern
makeStandalone =
  InputPattern
    "compile"
    ["compile.output"]
    I.Visible
    [("definition to compile", Required, exactDefinitionTermQueryArg), ("output file", Required, filePathArg)]
    ( P.wrapColumn2
        [ ( "`compile main file`",
            "Outputs a stand alone file that can be directly loaded and"
              <> "executed by unison. Said execution will have the effect of"
              <> "running `!main`."
          )
        ]
    )
    $ \case
      [main, file] ->
        Input.MakeStandaloneI
          <$> unsupportedStructuredArgument makeStandalone "a file name" file
          <*> handleHashQualifiedNameArg main
      args -> wrongArgsLength "exactly two arguments" args

runScheme :: InputPattern
runScheme =
  InputPattern
    "run.native"
    []
    I.Visible
    [("definition to run", Required, exactDefinitionTermQueryArg), ("arguments", ZeroPlus, noCompletionsArg)]
    ( P.wrapColumn2
        [ ( makeExample runScheme ["main", "args"],
            "Executes !main using native compilation via scheme."
          )
        ]
    )
    $ \case
      main : args ->
        Input.ExecuteSchemeI
          <$> handleHashQualifiedNameArg main
          <*> traverse (unsupportedStructuredArgument runScheme "a command-line argument") args
      [] -> wrongArgsLength "at least one argument" []

compileScheme :: InputPattern
compileScheme =
  InputPattern
    "compile.native"
    []
    I.Hidden
    [ ("definition to compile", Required, exactDefinitionTermQueryArg),
      ("output file", Required, filePathArg),
      ("profile", Optional, profileArg)
    ]
    ( P.wrapColumn2
        [ ( makeExample compileScheme ["main", "file", "profile"],
            "Creates stand alone executable via compilation to"
              <> "scheme. The created executable will have the effect"
              <> "of running `!main`. Providing `profile` as a third"
              <> "argument will enable profiling."
          )
        ]
    )
    $ \case
      [main, file] -> mkCompileScheme False file main
      [main, file, prof] -> do
        unsupportedStructuredArgument compileScheme "profile" prof
          >>= \case
            "profile" -> mkCompileScheme True file main
            parg ->
              Left . P.text $
                "I expected the third argument to be `profile`, but"
                  <> " instead recieved `"
                  <> Text.pack parg
                  <> "`."
      args -> wrongArgsLength "two or three arguments" args
  where
    mkCompileScheme pf fn mn =
      Input.CompileSchemeI pf . Text.pack
        <$> unsupportedStructuredArgument compileScheme "a file name" fn
        <*> handleHashQualifiedNameArg mn

createAuthor :: InputPattern
createAuthor =
  InputPattern
    "create.author"
    []
    I.Visible
    [("definition name", Required, noCompletionsArg), ("author name", Required, noCompletionsArg)]
    ( makeExample createAuthor ["alicecoder", "\"Alice McGee\""]
        <> " "
        <> P.wrap
          ( " creates "
              <> backtick "alicecoder"
              <> "values in"
              <> backtick "metadata.authors"
              <> "and"
              <> backtick (P.group ("metadata.copyrightHolders" <> "."))
          )
    )
    \case
      symbolStr : authorStr@(_ : _) ->
        Input.CreateAuthorI
          <$> handleRelativeNameSegmentArg symbolStr
          <*> fmap
            (parseAuthorName . unwords)
            (traverse (unsupportedStructuredArgument createAuthor "text") authorStr)
      args -> wrongArgsLength "at least two arguments" args
  where
    -- let's have a real parser in not too long
    parseAuthorName :: String -> Text
    parseAuthorName =
      Text.pack . \case
        ('"' : quoted) -> init quoted
        bare -> bare

authLogin :: InputPattern
authLogin =
  InputPattern
    "auth.login"
    []
    I.Visible
    []
    ( P.lines
        [ P.wrap "Obtain an authentication session with Unison Share.",
          makeExample authLogin []
            <> "authenticates ucm with Unison Share."
        ]
    )
    ( \case
        [] -> Right $ Input.AuthLoginI
        args -> wrongArgsLength "no arguments" args
    )

printVersion :: InputPattern
printVersion =
  InputPattern
    "version"
    []
    I.Visible
    []
    ( P.wrap "Print the version of unison you're running"
    )
    ( \case
        [] -> Right $ Input.VersionI
        args -> wrongArgsLength "no arguments" args
    )

projectCreate :: InputPattern
projectCreate =
  InputPattern
    { patternName = "project.create",
      aliases = ["create.project"],
      visibility = I.Visible,
      args = [],
      help =
        P.wrapColumn2
          [ ("`project.create`", "creates a project with a random name"),
            ("`project.create foo`", "creates a project named `foo`")
          ],
      parse = \case
        [] -> pure $ Input.ProjectCreateI True Nothing
        [name] -> Input.ProjectCreateI True . pure <$> handleProjectArg name
        args -> wrongArgsLength "no more than one argument" args
    }

projectCreateEmptyInputPattern :: InputPattern
projectCreateEmptyInputPattern =
  InputPattern
    { patternName = "project.create-empty",
      aliases = ["create.empty-project"],
      visibility = I.Hidden,
      args = [],
      help =
        P.wrapColumn2
          [ ("`project.create-empty`", "creates an empty project with a random name"),
            ("`project.create-empty foo`", "creates an empty project named `foo`")
          ],
      parse = \case
        [] -> pure $ Input.ProjectCreateI False Nothing
        [name] -> Input.ProjectCreateI False . pure <$> handleProjectArg name
        args -> wrongArgsLength "no more than one argument" args
    }

projectRenameInputPattern :: InputPattern
projectRenameInputPattern =
  InputPattern
    { patternName = "project.rename",
      aliases = ["rename.project"],
      visibility = I.Visible,
      args = [("new name", Required, projectNameArg)],
      help =
        P.wrapColumn2
          [ ("`project.rename foo`", "renames the current project to `foo`")
          ],
      parse = \case
        [nameString] -> Input.ProjectRenameI <$> handleProjectArg nameString
        args -> wrongArgsLength "exactly one argument" args
    }

projectSwitch :: InputPattern
projectSwitch =
  InputPattern
    { patternName = "switch",
      aliases = [],
      visibility = I.Visible,
      args = [("project or branch to switch to", Required, projectAndBranchNamesArg suggestionsConfig)],
      help =
        P.wrapColumn2
          [ ("`switch`", "opens an interactive selector to pick a project and branch"),
            ("`switch foo/bar`", "switches to the branch `bar` in the project `foo`"),
            ("`switch foo/`", "switches to the last branch you visited in the project `foo`"),
            ("`switch /bar`", "switches to the branch `bar` in the current project")
          ],
      parse = \case
        [name] -> Input.ProjectSwitchI <$> handleProjectAndBranchNamesArg name
        args -> wrongArgsLength "exactly one argument" args
    }
  where
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = True,
          projectInclusion = AllProjects,
          branchInclusion = ExcludeCurrentBranch
        }

projectsInputPattern :: InputPattern
projectsInputPattern =
  InputPattern
    { patternName = "projects",
      aliases = ["list.project", "ls.project", "project.list"],
      visibility = I.Visible,
      args = [],
      help = P.wrap "List projects.",
      parse = \_ -> Right Input.ProjectsI
    }

branchesInputPattern :: InputPattern
branchesInputPattern =
  InputPattern
    { patternName = "branches",
      aliases = ["list.branch", "ls.branch", "branch.list"],
      visibility = I.Visible,
      args = [("project", Optional, projectNameArg)],
      help =
        P.wrapColumn2
          [ ("`branches`", "lists all branches in the current project"),
            ("`branches foo`", "lists all branches in the project `foo`")
          ],
      parse = \case
        [] -> Right (Input.BranchesI Nothing)
        [nameString] -> Input.BranchesI . pure <$> handleProjectArg nameString
        args -> wrongArgsLength "no more than one argument" args
    }

branchInputPattern :: InputPattern
branchInputPattern =
  InputPattern
    { patternName = "branch",
      aliases = ["branch.create", "create.branch"],
      visibility = I.Visible,
      args =
        [ ("branch", Required, projectBranchNameArg suggestionsConfig),
          ("branch", Optional, newBranchNameArg)
        ],
      help =
        P.wrapColumn2
          [ ("`branch foo`", "forks the current project branch to a new branch `foo`"),
            ("`branch /bar foo`", "forks the branch `bar` of the current project to a new branch `foo`")
          ],
      parse = \case
        [source0, name] ->
          Input.BranchI . Input.BranchSourceI'UnresolvedProjectBranch
            <$> handleMaybeProjectBranchArg source0
            <*> handleMaybeProjectBranchArg name
        [name] -> Input.BranchI Input.BranchSourceI'CurrentContext <$> handleMaybeProjectBranchArg name
        args -> wrongArgsLength "one or two arguments" args
    }
  where
    newBranchNameArg =
      ArgumentType
        { typeName = "new-branch",
          suggestions = \_ _ _ _ -> pure [],
          fzfResolver = Nothing
        }
    suggestionsConfig =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = False,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

branchEmptyInputPattern :: InputPattern
branchEmptyInputPattern =
  InputPattern
    { patternName = "branch.empty",
      aliases = ["branch.create-empty", "create.empty-branch"],
      visibility = I.Visible,
      args = [],
      help = P.wrap "Create a new empty branch.",
      parse = \case
        [name] ->
          Input.BranchI Input.BranchSourceI'Empty
            <$> handleMaybeProjectBranchArg name
        args -> wrongArgsLength "exactly one argument" args
    }

branchRenameInputPattern :: InputPattern
branchRenameInputPattern =
  InputPattern
    { patternName = "branch.rename",
      aliases = ["rename.branch"],
      visibility = I.Visible,
      args = [],
      help =
        P.wrapColumn2
          [("`branch.rename foo`", "renames the current branch to `foo`")],
      parse = \case
        [name] -> Input.BranchRenameI <$> handleProjectBranchNameArg name
        args -> wrongArgsLength "exactly one argument" args
    }

clone :: InputPattern
clone =
  InputPattern
    { patternName = "clone",
      aliases = [],
      visibility = I.Visible,
      args = [],
      help =
        P.wrapColumn2
          [ ( "`clone @unison/json/topic json/my-topic`",
              "creates `json/my-topic` from the remote branch `@unison/json/topic`"
            ),
            ( "`clone @unison/base base/`",
              "creates `base/main` from the remote branch `@unison/base/main`"
            ),
            ( "`clone @unison/base /main2`",
              "creates the branch `main2` in the current project from the remote branch `@unison/base/main`"
            ),
            ( "`clone /main /main2`",
              "creates the branch `main2` in the current project from the remote branch `main` of the current project's associated remote"
                <> "(see"
                <> P.group (makeExample helpTopics ["remotes"] <> ")")
            ),
            ( "`clone /main my-fork/`",
              "creates `my-fork/main` from the branch `main` of the current project's associated remote"
                <> "(see"
                <> P.group (makeExample helpTopics ["remotes"] <> ")")
            )
          ],
      parse = \case
        [remoteNames] -> Input.CloneI <$> handleProjectAndBranchNamesArg remoteNames <*> pure Nothing
        [remoteNames, localNames] ->
          Input.CloneI
            <$> handleProjectAndBranchNamesArg remoteNames
            <*> fmap pure (handleProjectAndBranchNamesArg localNames)
        args -> wrongArgsLength "one or two arguments" args
    }

releaseDraft :: InputPattern
releaseDraft =
  InputPattern
    { patternName = "release.draft",
      aliases = ["draft.release"],
      visibility = I.Visible,
      args = [],
      help = P.wrap "Draft a release.",
      parse = \case
        [semverString] ->
          bimap (const "Couldn’t parse version number") Input.ReleaseDraftI
            . tryInto @Semver
            . Text.pack
            =<< unsupportedStructuredArgument releaseDraft "a version number" semverString
        args -> wrongArgsLength "exactly one argument" args
    }

upgrade :: InputPattern
upgrade =
  InputPattern
    { patternName = "upgrade",
      aliases = [],
      visibility = I.Visible,
      args = [("dependency to upgrade", Required, dependencyArg), ("dependency to upgrade to", Required, dependencyArg)],
      help =
        P.wrap $
          "`upgrade old new` upgrades library dependency `lib.old` to `lib.new`, and, if successful, deletes `lib.old`.",
      parse = \case
        [oldString, newString] ->
          Input.UpgradeI <$> handleRelativeNameSegmentArg oldString <*> handleRelativeNameSegmentArg newString
        args -> wrongArgsLength "exactly two arguments" args
    }

upgradeCommitInputPattern :: InputPattern
upgradeCommitInputPattern =
  InputPattern
    { patternName = "upgrade.commit",
      aliases = ["commit.upgrade"],
      visibility = I.Visible,
      args = [],
      help =
        let mainBranch = UnsafeProjectBranchName "main"
            tempBranch = UnsafeProjectBranchName "upgrade-foo-to-bar"
         in P.wrap
              ( makeExample' upgradeCommitInputPattern
                  <> "merges a temporary branch created by the"
                  <> makeExample' upgrade
                  <> "command back into its parent branch, and removes the temporary branch."
              )
              <> P.newline
              <> P.newline
              <> P.wrap
                ( "For example, if you've done"
                    <> makeExample upgrade ["foo", "bar"]
                    <> "from"
                    <> P.group (prettyProjectBranchName mainBranch <> ",")
                    <> "then"
                    <> makeExample' upgradeCommitInputPattern
                    <> "is equivalent to doing"
                )
              <> P.newline
              <> P.newline
              <> P.indentN
                2
                ( P.bulleted
                    [ makeExampleNoBackticks projectSwitch [prettySlashProjectBranchName mainBranch],
                      makeExampleNoBackticks mergeInputPattern [prettySlashProjectBranchName tempBranch],
                      makeExampleNoBackticks deleteBranch [prettySlashProjectBranchName tempBranch]
                    ]
                ),
      parse = \case
        [] -> Right Input.UpgradeCommitI
        args -> wrongArgsLength "no arguments" args
    }

debugSynhashTermInputPattern :: InputPattern
debugSynhashTermInputPattern =
  InputPattern
    { patternName = "debug.synhash.term",
      aliases = [],
      visibility = I.Hidden,
      args = [("term", Required, exactDefinitionTermQueryArg)],
      help = mempty,
      parse = \case
        [arg] -> Input.DebugSynhashTermI <$> handleNameArg arg
        args -> wrongArgsLength "exactly one argument" args
    }

validInputs :: [InputPattern]
validInputs =
  sortOn
    I.patternName
    [ add,
      aliasMany,
      aliasTerm,
      aliasType,
      api,
      authLogin,
      back,
      branchEmptyInputPattern,
      branchInputPattern,
      branchRenameInputPattern,
      branchesInputPattern,
      cd,
      clear,
      clone,
      compileScheme,
      createAuthor,
      debugAliasTermForce,
      debugAliasTypeForce,
      debugClearWatchCache,
      debugDoctor,
      debugDumpNamespace,
      debugDumpNamespaceSimple,
      debugSynhashTermInputPattern,
      debugTerm,
      debugTermVerbose,
      debugType,
      debugLSPFoldRanges,
      debugFileHashes,
      debugNameDiff,
      debugNumberedArgs,
      debugTabCompletion,
      debugLspNameCompletion,
      debugFuzzyOptions,
      debugFormat,
      delete,
      deleteBranch,
      deleteProject,
      deleteNamespace,
      deleteNamespaceForce,
      deleteTerm,
      deleteTermVerbose,
      deleteType,
      deleteTypeVerbose,
      deleteVerbose,
      dependencies,
      dependents,
      diffNamespace,
      display,
      displayTo,
      docToMarkdown,
      docs,
      docsToHtml,
      edit,
      editNamespace,
      editNew,
      execute,
      find,
      findIn,
      findAll,
      findInAll,
      findGlobal,
      findShallow,
      findVerbose,
      findVerboseAll,
      sfind,
      sfindReplace,
      textfind False,
      textfind True,
      forkLocal,
      help,
      helpTopics,
      history,
      ioTest,
      ioTestNative,
      ioTestAll,
      ioTestAllNative,
      libInstallInputPattern,
      load,
      makeStandalone,
      mergeBuiltins,
      mergeIOBuiltins,
      mergeOldInputPattern,
      mergeOldPreviewInputPattern,
      mergeOldSquashInputPattern,
      mergeInputPattern,
      mergeCommitInputPattern,
      names False, -- names
      names True, -- debug.names.global
      namespaceDependencies,
      previewAdd,
      previewUpdate,
      printVersion,
      projectCreate,
      projectCreateEmptyInputPattern,
      projectRenameInputPattern,
      projectSwitch,
      projectsInputPattern,
      pull,
      pullWithoutHistory,
      push,
      pushCreate,
      pushExhaustive,
      pushForce,
      quit,
      releaseDraft,
      renameBranch,
      renameTerm,
      renameType,
      moveAll,
      reset,
      runScheme,
      saveExecuteResult,
      test,
      testNative,
      testAll,
      testAllNative,
      todo,
      ui,
      undo,
      up,
      update,
      updateBuiltins,
      updateOld,
      updateOldNoPatch,
      upgrade,
      upgradeCommitInputPattern,
      view,
      viewGlobal,
      deprecatedViewRootReflog,
      branchReflog,
      projectReflog,
      globalReflog
    ]

-- | A map of all command patterns by pattern name or alias.
patternMap :: Map String InputPattern
patternMap =
  Map.fromList $
    validInputs
      >>= (\p -> (I.patternName p, p) : ((,p) <$> I.aliases p))

visibleInputs :: [InputPattern]
visibleInputs = filter ((== I.Visible) . I.visibility) validInputs

commandNames :: [String]
commandNames = visibleInputs >>= \i -> I.patternName i : I.aliases i

commandNameArg :: ArgumentType
commandNameArg =
  let options = commandNames <> Map.keys helpTopicsMap
   in ArgumentType
        { typeName = "command",
          suggestions = \q _ _ _ -> pure (exactComplete q options),
          fzfResolver = Just $ Resolvers.fuzzySelectFromList (Text.pack <$> options)
        }

exactDefinitionArg :: ArgumentType
exactDefinitionArg =
  ArgumentType
    { typeName = "definition",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteTermOrType q p),
      fzfResolver = Just Resolvers.definitionResolver
    }

definitionQueryArg :: ArgumentType
definitionQueryArg = exactDefinitionArg {typeName = "definition query"}

exactDefinitionTypeQueryArg :: ArgumentType
exactDefinitionTypeQueryArg =
  ArgumentType
    { typeName = "type definition query",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteType q p),
      fzfResolver = Just Resolvers.typeDefinitionResolver
    }

exactDefinitionTypeOrTermQueryArg :: ArgumentType
exactDefinitionTypeOrTermQueryArg =
  ArgumentType
    { typeName = "type or term definition query",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteTermOrType q p),
      fzfResolver = Just Resolvers.definitionResolver
    }

exactDefinitionTermQueryArg :: ArgumentType
exactDefinitionTermQueryArg =
  ArgumentType
    { typeName = "term definition query",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteTerm q p),
      fzfResolver = Just Resolvers.termDefinitionResolver
    }

patchArg :: ArgumentType
patchArg =
  ArgumentType
    { typeName = "patch",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompletePatch q p),
      fzfResolver = Nothing
    }

namespaceArg :: ArgumentType
namespaceArg =
  ArgumentType
    { typeName = "namespace",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteNamespace q p),
      fzfResolver = Just Resolvers.namespaceResolver
    }

-- | Usually you'll want one or the other, but some commands support both right now.
namespaceOrProjectBranchArg :: ProjectBranchSuggestionsConfig -> ArgumentType
namespaceOrProjectBranchArg config =
  ArgumentType
    { typeName = "namespace or branch",
      suggestions =
        let namespaceSuggestions = \q cb _http pp -> Codebase.runTransaction cb (prefixCompleteNamespace q pp)
         in unionSuggestions
              [ projectAndOrBranchSuggestions config,
                namespaceSuggestions
              ],
      fzfResolver = Just Resolvers.projectOrBranchResolver
    }

namespaceOrDefinitionArg :: ArgumentType
namespaceOrDefinitionArg =
  ArgumentType
    { typeName = "term, type, or namespace",
      suggestions = \q cb _http p -> Codebase.runTransaction cb do
        namespaces <- prefixCompleteNamespace q p
        termsTypes <- prefixCompleteTermOrType q p
        pure (List.nubOrd $ namespaces <> termsTypes),
      fzfResolver =
        Just Resolvers.namespaceOrDefinitionResolver
    }

-- | A dependency name. E.g. if your project has `lib.base`, `base` would be a dependency
-- name.
dependencyArg :: ArgumentType
dependencyArg =
  ArgumentType
    { typeName = "project dependency",
      suggestions = \q cb _http pp -> Codebase.runTransaction cb do
        prefixCompleteNamespace q (pp & PP.path_ .~ Path.singleton NameSegment.libSegment),
      fzfResolver = Just Resolvers.projectDependencyResolver
    }

newNameArg :: ArgumentType
newNameArg =
  ArgumentType
    { typeName = "new-name",
      suggestions = \q cb _http p -> Codebase.runTransaction cb (prefixCompleteNamespace q p),
      fzfResolver = Nothing
    }

noCompletionsArg :: ArgumentType
noCompletionsArg =
  ArgumentType
    { typeName = "word",
      suggestions = noCompletions,
      fzfResolver = Nothing
    }

filePathArg :: ArgumentType
filePathArg =
  ArgumentType
    { typeName = "file-path",
      suggestions = noCompletions,
      fzfResolver = Nothing
    }

-- | Refers to a namespace on some remote code host.
remoteNamespaceArg :: ArgumentType
remoteNamespaceArg =
  ArgumentType
    { typeName = "remote-namespace",
      suggestions = \input _cb http _p -> sharePathCompletion http input,
      fzfResolver = Nothing
    }

profileArg :: ArgumentType
profileArg =
  ArgumentType
    { typeName = "profile",
      suggestions = \_input _cb _http _p ->
        pure [Line.simpleCompletion "profile"],
      fzfResolver = Nothing
    }

data ProjectInclusion = OnlyWithinCurrentProject | OnlyOutsideCurrentProject | AllProjects
  deriving stock (Eq, Ord, Show)

data BranchInclusion = ExcludeCurrentBranch | AllBranches
  deriving stock (Eq, Ord, Show)

data ProjectBranchSuggestionsConfig = ProjectBranchSuggestionsConfig
  { -- Whether projects (without branches) should be considered possible completions.
    showProjectCompletions :: Bool,
    -- Whether to include projects/branches within the current project, only outside the
    -- current project, or either.
    projectInclusion :: ProjectInclusion,
    -- Whether to include the current branch as a possible completion.
    branchInclusion :: BranchInclusion
  }

projectAndOrBranchSuggestions ::
  (MonadIO m) =>
  ProjectBranchSuggestionsConfig ->
  String ->
  Codebase m v a ->
  AuthenticatedHttpClient ->
  ProjectPath ->
  m [Line.Completion]
projectAndOrBranchSuggestions config inputStr codebase _httpClient pp = do
  case Text.uncons input of
    -- Things like "/foo" would be parsed as unambiguous branches in the logic below, except we also want to
    -- handle "/<TAB>" and "/@<TAB>" inputs, which aren't valid branch names, but are valid branch prefixes. So,
    -- if the input begins with a forward slash, just rip it off and treat the rest as the branch prefix.
    Just ('/', input1) -> handleBranchesComplete input1 codebase pp
    _ ->
      case tryInto @ProjectAndBranchNames input of
        -- This case handles inputs like "", "@", and possibly other things that don't look like a valid project
        -- or branch, but are a valid prefix of one
        Left _err -> handleAmbiguousComplete input codebase
        Right (ProjectAndBranchNames'Ambiguous _ _) -> handleAmbiguousComplete input codebase
        -- Here we assume that if we've unambiguously parsed a project, it ended in a forward slash, so we're ready
        -- to suggest branches in that project as autocompletions.
        --
        -- Conceivably, with some other syntax, it may be possible to unambiguously parse a project name, while
        -- still wanting to suggest full project names (e.g. I type "PROJECT=foo<tab>" to get a list of projects
        -- that begin with "foo"), but because that's not how our syntax works today, we don't inspect the input
        -- string for a trailing forward slash.
        Right (ProjectAndBranchNames'Unambiguous (This projectName)) -> do
          branches <-
            Codebase.runTransaction codebase do
              Queries.loadProjectByName projectName >>= \case
                Nothing -> pure []
                Just project -> do
                  let projectId = project ^. #projectId
                  fmap (filterBranches config pp) do
                    Queries.loadAllProjectBranchesBeginningWith projectId Nothing
          pure (map (projectBranchToCompletion projectName) branches)
        -- This branch is probably dead due to intercepting inputs that begin with "/" above
        Right (ProjectAndBranchNames'Unambiguous (That branchName)) ->
          handleBranchesComplete (into @Text branchName) codebase pp
        Right (ProjectAndBranchNames'Unambiguous (These projectName branchName)) -> do
          branches <-
            Codebase.runTransaction codebase do
              Queries.loadProjectByName projectName >>= \case
                Nothing -> pure []
                Just project -> do
                  let projectId = project ^. #projectId
                  fmap (filterBranches config pp) do
                    Queries.loadAllProjectBranchesBeginningWith projectId (Just $ into @Text branchName)
          pure (map (projectBranchToCompletion projectName) branches)
  where
    input = Text.strip . Text.pack $ inputStr

    handleAmbiguousComplete ::
      (MonadIO m) =>
      Text ->
      Codebase m v a ->
      m [Completion]
    handleAmbiguousComplete input codebase = do
      (branches, projects) <-
        Codebase.runTransaction codebase do
          branches <-
            fmap (filterBranches config pp) do
              Queries.loadAllProjectBranchesBeginningWith currentProjectId (Just input)
          projects <- case projectInclusion config of
            OnlyWithinCurrentProject -> Queries.loadProject currentProjectId <&> maybeToList
            _ -> Queries.loadAllProjectsBeginningWith (Just input) <&> filterProjects
          pure (branches, projects)
      let branchCompletions = map currentProjectBranchToCompletion branches
      let projectCompletions = map projectToCompletion projects
      -- There's one final wibble to deal with here at the eleventh hour. You might think we can just append
      -- branchCompletions to projectCompletions and call it a day, *however*...!
      --
      -- Say we have two branches "bar" and "biz". These branches are rendered (and completed) with leading forward
      -- slashes.
      --
      --   > switch b<TAB>
      --   /bar /biz
      --
      --   > switch ba<TAB>
      --   > switch /bar -- the completion
      --
      -- Now say we repeat the above, but with a project "bongo".
      --
      --   > switch <TAB>
      --   /bar /biz bongo
      --
      -- If the user types a prefix that's common to both a branch and a project, like "b", their input will simply
      -- disappear. Wtf, haskeline?!
      --
      --   > switch b<TAB>
      --   > switch -- the completion
      --
      -- Well, it makes sense: we tell haskeline that we have three completions, "/bar", "/biz", and "bongo", with
      -- partial input "b". The longest common prefix here is the empty string "".
      --
      -- So, we have this final check. If there are indeed matching projects *and* matching branches, and the user
      -- has input at least one character (i.e. they aren't just tab-completing like "switch <TAB>" to see
      -- everything), then we pretend (for the sake of tab-completion) that there are only matching projects. This
      -- makes the back-and-forth with the tab completer much more intuitive:
      --
      --   > switch <TAB>
      --   /bar /biz bongo
      --   > switch b<TAB>
      --   > switch bongo -- the completion
      --
      -- A more optimal interface would not hide branches at all, even though their tab-completions end up prefixing
      -- a forward-slash:
      --
      --   > switch <TAB>
      --   /bar /biz bongo
      --   > switch b<TAB>
      --   /bar /biz bongo
      --   > switch ba<TAB>
      --   > switch /bar -- the completion
      --
      -- However, that simly doesn't seem possible with haskeline. Another sub-optimal point in the design space
      -- would be to *not* actually tab-complete branch names with leading forward slashes, even though they are
      -- rendered as such in the tab-completion options. For example,
      --
      --   > switch <TAB>
      --   /bar /biz
      --   > switch ba<TAB>
      --   > switch bar -- the completion
      --
      -- However, this has the unfortunate disadvantage of tab-completing a possibly ambiguous thing for the user,
      -- as in the case when there's both a branch and project with the same name:
      --
      --   > switch <TAB>
      --   /bar /biz bar
      --   > switch ba<TAB>
      --   > switch bar -- the completion
      --
      --   Ambiguous! Try `switch /bar` or `switch bar/`
      pure
        if not (null branchCompletions) && not (null projectCompletions) && not (Text.null input)
          then projectCompletions
          else branchCompletions ++ projectCompletions

    -- Complete the text into a branch name within the provided project
    handleBranchesComplete :: (MonadIO m) => Text -> Codebase m v a -> PP.ProjectPath -> m [Completion]
    handleBranchesComplete branchName codebase pp = do
      let projId = pp ^. #project . #projectId
      branches <-
        Codebase.runTransaction codebase do
          fmap (filterBranches config pp) do
            Queries.loadAllProjectBranchesBeginningWith projId (Just branchName)
      pure (map currentProjectBranchToCompletion branches)

    filterProjects :: [Sqlite.Project] -> [Sqlite.Project]
    filterProjects projects =
      case (projectInclusion config) of
        AllProjects -> projects
        OnlyOutsideCurrentProject -> projects & filter (\Sqlite.Project {projectId} -> projectId /= currentProjectId)
        OnlyWithinCurrentProject ->
          projects
            & List.find (\Sqlite.Project {projectId} -> projectId == currentProjectId)
            & maybeToList

    PP.ProjectPath currentProjectId _currentBranchId _currentPath = PP.toIds pp

projectToCompletion :: Sqlite.Project -> Completion
projectToCompletion project =
  Completion
    { replacement = stringProjectName,
      display = P.toAnsiUnbroken (prettyProjectNameSlash (project ^. #name)),
      isFinished = False
    }
  where
    stringProjectName = Text.unpack (into @Text (project ^. #name) <> "/")

projectBranchToCompletion :: ProjectName -> (ProjectBranchId, ProjectBranchName) -> Completion
projectBranchToCompletion projectName (_, branchName) =
  Completion
    { replacement = Text.unpack (into @Text (ProjectAndBranch projectName branchName)),
      display = P.toAnsiUnbroken (prettySlashProjectBranchName branchName),
      isFinished = False
    }

handleBranchesComplete ::
  (MonadIO m) =>
  ProjectBranchSuggestionsConfig ->
  Text ->
  Codebase m v a ->
  PP.ProjectPath ->
  m [Completion]
handleBranchesComplete config branchName codebase pp = do
  branches <-
    Codebase.runTransaction codebase do
      fmap (filterBranches config pp) do
        Queries.loadAllProjectBranchesBeginningWith (pp ^. #project . #projectId) (Just branchName)
  pure (map currentProjectBranchToCompletion branches)

filterBranches :: ProjectBranchSuggestionsConfig -> PP.ProjectPath -> [(ProjectBranchId, a)] -> [(ProjectBranchId, a)]
filterBranches config pp branches =
  case (branchInclusion config) of
    AllBranches -> branches
    ExcludeCurrentBranch -> branches & filter (\(branchId, _) -> branchId /= currentBranchId)
  where
    currentBranchId = pp ^. #branch . #branchId

currentProjectBranchToCompletion :: (ProjectBranchId, ProjectBranchName) -> Completion
currentProjectBranchToCompletion (_, branchName) =
  Completion
    { replacement = '/' : Text.unpack (into @Text branchName),
      display = P.toAnsiUnbroken (prettySlashProjectBranchName branchName),
      isFinished = False
    }

branchRelativePathSuggestions ::
  (MonadIO m) =>
  ProjectBranchSuggestionsConfig ->
  String ->
  Codebase m v a ->
  AuthenticatedHttpClient ->
  PP.ProjectPath ->
  m [Line.Completion]
branchRelativePathSuggestions config inputStr codebase _httpClient pp = do
  case parseIncrementalBranchRelativePath inputStr of
    Left _ -> pure []
    Right ibrp -> case ibrp of
      BranchRelativePath.ProjectOrPath' _txt _path -> do
        namespaceSuggestions <- Codebase.runTransaction codebase (prefixCompleteNamespace inputStr pp)
        projectSuggestions <- projectNameSuggestions WithSlash inputStr codebase
        pure (namespaceSuggestions ++ projectSuggestions)
      BranchRelativePath.OnlyPath' _path ->
        Codebase.runTransaction codebase (prefixCompleteNamespace inputStr pp)
      BranchRelativePath.IncompleteProject _proj ->
        projectNameSuggestions WithSlash inputStr codebase
      BranchRelativePath.IncompleteBranch mproj mbranch -> case mproj of
        Nothing -> map suffixPathSep <$> handleBranchesComplete config (maybe "" into mbranch) codebase pp
        Just projectName -> do
          branches <-
            Codebase.runTransaction codebase do
              Queries.loadProjectByName projectName >>= \case
                Nothing -> pure []
                Just project -> do
                  let projectId = project ^. #projectId
                  fmap (filterBranches config pp) do
                    Queries.loadAllProjectBranchesBeginningWith projectId (into @Text <$> mbranch)
          pure (map (projectBranchToCompletionWithSep projectName) branches)
      BranchRelativePath.PathRelativeToCurrentBranch absPath -> Codebase.runTransaction codebase do
        map prefixPathSep <$> prefixCompleteNamespace (Text.unpack . Path.toText' $ Path.AbsolutePath' absPath) pp
      BranchRelativePath.IncompletePath projStuff mpath -> do
        Codebase.runTransaction codebase do
          map (addBranchPrefix projStuff) <$> prefixCompleteNamespace (maybe "" (Text.unpack . Path.toText' . Path.AbsolutePath') mpath) pp
  where
    projectBranchToCompletionWithSep :: ProjectName -> (ProjectBranchId, ProjectBranchName) -> Completion
    projectBranchToCompletionWithSep projectName (_, branchName) =
      Completion
        { replacement = Text.unpack (into @Text (ProjectAndBranch projectName branchName) <> branchPathSep),
          display = P.toAnsiUnbroken (prettySlashProjectBranchName branchName <> branchPathSepPretty),
          isFinished = False
        }

    prefixPathSep :: Completion -> Completion
    prefixPathSep c =
      c
        { Line.replacement = branchPathSep <> Line.replacement c,
          Line.display = P.toAnsiUnbroken branchPathSepPretty <> Line.display c
        }

    suffixPathSep :: Completion -> Completion
    suffixPathSep c =
      c
        { Line.replacement = Line.replacement c <> branchPathSep,
          Line.display = Line.display c <> P.toAnsiUnbroken branchPathSepPretty
        }

    addBranchPrefix ::
      Either (ProjectAndBranch ProjectName ProjectBranchName) ProjectBranchName ->
      Completion ->
      Completion
    addBranchPrefix eproj =
      let (prefixText, prefixPretty) = case eproj of
            Left pb ->
              ( into @Text pb,
                prettyProjectAndBranchName pb
              )
            Right branch ->
              ( "/" <> into @Text branch,
                prettySlashProjectBranchName branch
              )
       in \c ->
            c
              { Line.replacement = Text.unpack prefixText <> branchPathSep <> Line.replacement c,
                Line.display = P.toAnsiUnbroken (prefixPretty <> branchPathSepPretty) <> Line.display c
              }

    branchPathSepPretty = P.hiBlack branchPathSep

    branchPathSep :: (IsString s) => s
    branchPathSep = ":"

-- | A project name, branch name, or both.
projectAndBranchNamesArg :: ProjectBranchSuggestionsConfig -> ArgumentType
projectAndBranchNamesArg config =
  ArgumentType
    { typeName = "project-and-branch-names",
      suggestions = projectAndOrBranchSuggestions config,
      fzfResolver = Just Resolvers.projectAndOrBranchArg
    }

-- | A project branch name.
projectBranchNameArg :: ProjectBranchSuggestionsConfig -> ArgumentType
projectBranchNameArg config =
  ArgumentType
    { typeName = "project-branch-name",
      suggestions = projectAndOrBranchSuggestions config,
      fzfResolver = Just Resolvers.projectBranchResolver
    }

branchRelativePathArg :: ArgumentType
branchRelativePathArg =
  ArgumentType
    { typeName = "branch-relative-path",
      suggestions = branchRelativePathSuggestions config,
      fzfResolver = Nothing
    }
  where
    config =
      ProjectBranchSuggestionsConfig
        { showProjectCompletions = True,
          projectInclusion = AllProjects,
          branchInclusion = AllBranches
        }

-- | A project name.
projectNameArg :: ArgumentType
projectNameArg =
  ArgumentType
    { typeName = "project-name",
      suggestions = \input codebase _httpClient _path -> projectNameSuggestions NoSlash input codebase,
      fzfResolver = Just $ Resolvers.multiResolver [Resolvers.projectNameOptions]
    }

data OptionalSlash
  = WithSlash
  | NoSlash

projectNameSuggestions ::
  (MonadIO m) =>
  OptionalSlash ->
  String ->
  Codebase m v a ->
  m [Line.Completion]
projectNameSuggestions slash (Text.strip . Text.pack -> input) codebase = do
  projects <-
    Codebase.runTransaction codebase do
      Queries.loadAllProjectsBeginningWith (Just input)
  pure $ map projectToCompletion projects
  where
    projectToCompletion :: Sqlite.Project -> Completion
    projectToCompletion =
      let toPretty = case slash of
            NoSlash -> prettyProjectName
            WithSlash -> prettyProjectNameSlash
          toText project = case slash of
            NoSlash -> into @Text (project ^. #name)
            WithSlash -> Text.snoc (into @Text (project ^. #name)) '/'
       in \project ->
            Completion
              { replacement = Text.unpack (toText project),
                display = P.toAnsiUnbroken (toPretty (project ^. #name)),
                isFinished = False
              }

-- | Parse a 'Input.PushSource'.
parsePushSource :: String -> Maybe Input.PushSource
parsePushSource sourceStr =
  fixup Input.ProjySource (tryFrom $ Text.pack sourceStr)
  where
    fixup = either (const Nothing) . (pure .)

-- | Parse a push target.
parsePushTarget :: String -> Maybe (These ProjectName ProjectBranchName)
parsePushTarget = Megaparsec.parseMaybe UriParser.writeRemoteNamespace . Text.pack

parseHashQualifiedName ::
  String -> Either (P.Pretty CT.ColorText) (HQ.HashQualified Name)
parseHashQualifiedName s =
  maybe
    ( Left
        . P.wrap
        $ P.string s
          <> " is not a well-formed name, hash, or hash-qualified name. "
          <> "I expected something like `foo`, `#abc123`, or `foo#abc123`."
    )
    Right
    $ HQ.parseText (Text.pack s)

explainRemote :: PushPull -> P.Pretty CT.ColorText
explainRemote pushPull =
  P.group $
    P.lines
      [ P.wrap $ "where `remote` is a project or project branch, such as:",
        P.indentN 2 . P.column2 $
          [ ("Project (defaults to the /main branch)", P.backticked "@unison/base"),
            ("Project Branch", P.backticked "@unison/base/feature"),
            ("Contributor Branch", P.backticked "@unison/base/@johnsmith/feature")
          ]
            <> Monoid.whenM (pushPull == Pull) [("Project Release", P.backticked "@unison/base/releases/1.0.0")]
      ]

megaparse :: Megaparsec.Parsec Void Text a -> Text -> Either (P.Pretty P.ColorText) a
megaparse parser input =
  input
    & Megaparsec.parse (parser <* Megaparsec.eof) ""
    & mapLeft (prettyPrintParseError (Text.unpack input))
