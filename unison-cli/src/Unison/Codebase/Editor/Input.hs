module Unison.Codebase.Editor.Input
  ( Input (..),
    BranchSourceI (..),
    PullSourceTarget (..),
    PushRemoteBranchInput (..),
    PushSourceTarget (..),
    PushSource (..),
    TestInput (..),
    Event (..),
    OutputLocation (..),
    RelativeToFold (..),
    BranchIdG (..),
    BranchId,
    BranchId2,
    AbsBranchId,
    UnresolvedProjectBranch,
    parseBranchId,
    parseBranchId2,
    parseShortCausalHash,
    Insistence (..),
    PullMode (..),
    OptionalPatch (..),
    FindScope (..),
    ShowDefinitionScope (..),
    IsGlobal,
    DeleteOutput (..),
    DeleteTarget (..),

    -- * Type aliases
    ErrorMessageOrName,
    RawQuery,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as Text
import Data.These (These)
import Unison.Codebase.Branch.Merge qualified as Branch
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace)
import Unison.Codebase.Path (Path, Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Codebase.PushBehavior (PushBehavior)
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.CommandLine.BranchRelativePath (BranchRelativePath, parseBranchRelativePath)
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Project (ProjectAndBranch, ProjectAndBranchNames, ProjectBranchName, ProjectBranchNameOrLatestRelease, ProjectName, Semver)
import Unison.Util.Pretty qualified as P

data Event
  = UnisonFileChanged SourceName Source
  deriving stock (Show)

type Source = Text -- "id x = x\nconst a b = a"

type SourceName = Text -- "foo.u" or "buffer 7"

type PatchPath = Path.Split Path'

type ErrorMessageOrValue a = Either (P.Pretty P.ColorText) a

type ErrorMessageOrName = ErrorMessageOrValue (HQ.HashQualified Name)

type RawQuery = String

data OptionalPatch = NoPatch | DefaultPatch | UsePatch PatchPath
  deriving (Eq, Ord, Show)

data BranchIdG p
  = BranchAtSCH ShortCausalHash
  | BranchAtPath p
  | BranchAtProjectPath ProjectPath
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance (From p Text) => From (BranchIdG p) Text where
  from = \case
    BranchAtSCH h -> "#" <> SCH.toText h
    BranchAtPath p -> from p
    BranchAtProjectPath pp -> from pp

type BranchId = BranchIdG Path'

type BranchId2 = Either ShortCausalHash BranchRelativePath

type AbsBranchId = BranchIdG Path.Absolute

-- | An unambiguous project branch name, use the current project name if not provided.
type UnresolvedProjectBranch = ProjectAndBranch (Maybe ProjectName) ProjectBranchName

-- | Should we force the operation or not?
data Insistence = Force | Try
  deriving (Show, Eq)

parseBranchId :: String -> Either Text BranchId
parseBranchId ('#' : s) = case SCH.fromText (Text.pack s) of
  Nothing -> Left "Invalid hash, expected a base32hex string."
  Just h -> pure $ BranchAtSCH h
parseBranchId s = BranchAtPath <$> Path.parsePath' s

parseBranchId2 :: String -> Either (P.Pretty P.ColorText) (Either ShortCausalHash BranchRelativePath)
parseBranchId2 ('#' : s) = case SCH.fromText (Text.pack s) of
  Nothing -> Left "Invalid hash, expected a base32hex string."
  Just h -> Right (Left h)
parseBranchId2 s = Right <$> parseBranchRelativePath s

parseShortCausalHash :: String -> Either String ShortCausalHash
parseShortCausalHash ('#' : s) | Just sch <- SCH.fromText (Text.pack s) = Right sch
parseShortCausalHash _ = Left "Invalid hash, expected a base32hex string."

data PullMode
  = PullWithHistory
  | PullWithoutHistory
  deriving (Eq, Show)

type IsGlobal = Bool

data Input
  = -- names stuff:
    -- directory ops
    -- `Link` must describe a repo and a source path within that repo.
    -- clone w/o merge, error if would clobber
    ForkLocalBranchI (Either ShortCausalHash BranchRelativePath) BranchRelativePath
  | -- merge first causal into destination
    MergeLocalBranchI BranchRelativePath (Maybe BranchRelativePath) Branch.MergeMode
  | PreviewMergeLocalBranchI BranchRelativePath (Maybe BranchRelativePath)
  | DiffNamespaceI BranchId2 BranchId2 -- old new
  | PullI !PullSourceTarget !PullMode
  | PushRemoteBranchI PushRemoteBranchInput
  | SyncToFileI FilePath (ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName))
  | SyncFromFileI FilePath UnresolvedProjectBranch
  | -- | Sync from a codebase project branch to this codebase's project branch
    SyncFromCodebaseI FilePath (ProjectAndBranch ProjectName ProjectBranchName) UnresolvedProjectBranch
  | ResetI (BranchId2 {- namespace to reset it to -}) (Maybe UnresolvedProjectBranch {- ProjectBranch to reset -})
  | -- | used in Welcome module to give directions to user
    --
    -- todo: Q: Does it make sense to publish to not-the-root of a Github repo?
    --          Does it make sense to fork from not-the-root of a Github repo?
    CreateMessage (P.Pretty P.ColorText)
  | -- Change directory.
    SwitchBranchI Path'
  | UpI
  | PopBranchI
  | -- > names foo
    -- > names foo.bar
    -- > names .foo.bar
    -- > names .foo.bar#asdflkjsdf
    -- > names #sdflkjsdfhsdf
    -- > names foo.bar foo.baz #sdflkjsdfhsdf
    NamesI IsGlobal [(RawQuery, ErrorMessageOrName)]
  | AliasTermI !Bool (HQ'.HashOrHQ (Path.Split Path')) (Path.Split Path') -- bool = force?
  | AliasTypeI !Bool (HQ'.HashOrHQ (Path.Split Path')) (Path.Split Path') -- bool = force?
  | AliasManyI [HQ'.HashQualified (Path.Split Path)] Path'
  | MoveAllI Path.Path' Path.Path'
  | MoveTermI (HQ'.HashQualified (Path.Split Path')) (Path.Split Path')
  | MoveTypeI (HQ'.HashQualified (Path.Split Path')) (Path.Split Path')
  | MoveBranchI Path.Path' Path.Path'
  | -- delete = unname
    DeleteI DeleteTarget
  | -- edits stuff:
    LoadI (Maybe FilePath)
  | ClearI
  | AddI (Set Name)
  | PreviewAddI (Set Name)
  | UpdateI OptionalPatch (Set Name)
  | Update2I
  | PreviewUpdateI (Set Name)
  | TodoI
  | UndoI
  | -- First `Maybe Int` is cap on number of results, if any
    -- Second `Maybe Int` is cap on diff elements shown, if any
    HistoryI (Maybe Int) (Maybe Int) BranchId
  | -- execute an IO thunk with args
    ExecuteI (HQ.HashQualified Name) [String]
  | -- save the result of a previous Execute
    SaveExecuteResultI Name
  | -- execute an IO [Result], bool selects runtime
    IOTestI Bool (HQ.HashQualified Name)
  | -- execute all in-scope IO tests, interpreter or native
    IOTestAllI Bool
  | -- make a standalone binary file
    MakeStandaloneI String (HQ.HashQualified Name)
  | -- execute an IO thunk using scheme
    ExecuteSchemeI (HQ.HashQualified Name) [String]
  | -- compile to a scheme file; profiling flag
    CompileSchemeI Bool Text (HQ.HashQualified Name)
  | TestI Bool TestInput
  | CreateAuthorI NameSegment {- identifier -} Text {- name -}
  | -- Display provided definitions.
    DisplayI OutputLocation (NonEmpty (HQ.HashQualified Name))
  | -- Display docs for provided terms.
    DocsI (NonEmpty Name)
  | -- other
    FindI Bool FindScope [String] -- FindI isVerbose findScope query
  | FindShallowI Path'
  | StructuredFindI FindScope (HQ.HashQualified Name) -- sfind findScope query
  | StructuredFindReplaceI (HQ.HashQualified Name) -- sfind.replace rewriteQuery
  | TextFindI Bool [String] -- TextFindI allowLib tokens
  | -- Show provided definitions.
    ShowDefinitionI OutputLocation ShowDefinitionScope (NonEmpty (HQ.HashQualified Name))
  | ShowRootReflogI {- Deprecated -}
  | ShowGlobalReflogI
  | ShowProjectReflogI (Maybe ProjectName)
  | ShowProjectBranchReflogI (Maybe (ProjectAndBranch (Maybe ProjectName) ProjectBranchName))
  | UpdateBuiltinsI
  | MergeBuiltinsI (Maybe Path.Relative)
  | MergeIOBuiltinsI (Maybe Path.Relative)
  | ListDependenciesI (HQ.HashQualified Name)
  | ListDependentsI (HQ.HashQualified Name)
  | -- | List all external dependencies of a given namespace, or the current namespace if
    -- no path is provided.
    NamespaceDependenciesI (Maybe Path')
  | DebugTabCompletionI [String] -- The raw arguments provided
  | DebugLSPNameCompletionI Text -- The raw arguments provided
  | DebugFuzzyOptionsI String [String] -- cmd and arguments
  | DebugFormatI
  | DebugNumberedArgsI
  | DebugTypecheckedUnisonFileI
  | DebugDumpNamespacesI
  | DebugDumpNamespaceSimpleI
  | DebugTermI (Bool {- Verbose mode -}) (HQ.HashQualified Name)
  | DebugTypeI (HQ.HashQualified Name)
  | DebugLSPFoldRangesI
  | DebugClearWatchI
  | DebugDoctorI
  | DebugNameDiffI ShortCausalHash ShortCausalHash
  | QuitI
  | ApiI
  | UiI Path'
  | DocToMarkdownI Name
  | DocsToHtmlI BranchRelativePath FilePath
  | AuthLoginI
  | VersionI
  | ProjectCreateI Bool {- try downloading base? -} (Maybe ProjectName)
  | ProjectRenameI ProjectName
  | ProjectSwitchI ProjectAndBranchNames
  | ProjectsI
  | BranchI BranchSourceI (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | BranchRenameI ProjectBranchName
  | BranchesI (Maybe ProjectName)
  | CloneI ProjectAndBranchNames (Maybe ProjectAndBranchNames)
  | ReleaseDraftI Semver
  | UpgradeI !NameSegment !NameSegment
  | EditNamespaceI [Path.Path]
  | -- New merge algorithm: merge the given project branch into the current one.
    MergeI (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | LibInstallI
      !Bool -- Remind the user to use `lib.install` next time, not `pull`?
      !(ProjectAndBranch ProjectName (Maybe ProjectBranchNameOrLatestRelease))
  | UpgradeCommitI
  | MergeCommitI
  | DebugSynhashTermI !Name
  | EditDependentsI !(HQ.HashQualified Name)
  deriving (Eq, Show)

-- | The source of a `branch` command: what to make the new branch from.
data BranchSourceI
  = -- | Create a branch from the current context
    BranchSourceI'CurrentContext
  | -- | Create an empty branch
    BranchSourceI'Empty
  | -- | Create a branch from this other branch
    BranchSourceI'UnresolvedProjectBranch UnresolvedProjectBranch
  deriving stock (Eq, Show)

-- | Pull source and target: either neither is specified, or only a source, or both.
data PullSourceTarget
  = PullSourceTarget0
  | PullSourceTarget1 (ReadRemoteNamespace (These ProjectName ProjectBranchNameOrLatestRelease))
  | PullSourceTarget2 (ReadRemoteNamespace (These ProjectName ProjectBranchNameOrLatestRelease)) (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  deriving stock (Eq, Show)

data PushSource
  = ProjySource (These ProjectName ProjectBranchName)
  deriving stock (Eq, Show)

-- | Push source and target: either neither is specified, or only a target, or both.
data PushSourceTarget
  = PushSourceTarget0
  | PushSourceTarget1 (These ProjectName ProjectBranchName)
  | PushSourceTarget2 PushSource (These ProjectName ProjectBranchName)
  deriving stock (Eq, Show)

data PushRemoteBranchInput = PushRemoteBranchInput
  { sourceTarget :: PushSourceTarget,
    pushBehavior :: PushBehavior
  }
  deriving stock (Eq, Show)

data TestInput = TestInput
  { -- | Should we run tests in the `lib` namespace?
    includeLibNamespace :: Bool,
    -- | Relative path to run the tests in. Ignore if `includeLibNamespace` is True - that means test everything.
    path :: Path.Relative,
    showFailures :: Bool,
    showSuccesses :: Bool
  }
  deriving stock (Eq, Show)

-- Some commands, like `view`, can dump output to either console or a file.
data OutputLocation
  = ConsoleLocation
  | LatestFileLocation RelativeToFold
  | FileLocation FilePath RelativeToFold
  -- ClipboardLocation
  deriving (Eq, Show)

-- | Above a new fold, or within the topmost fold?
data RelativeToFold
  = AboveFold
  | WithinFold
  deriving stock (Eq, Show)

data FindScope
  = FindLocal Path'
  | FindLocalAndDeps Path'
  | FindGlobal
  deriving stock (Eq, Show)

data ShowDefinitionScope
  = ShowDefinitionLocal
  | ShowDefinitionGlobal
  deriving stock (Eq, Show)

data DeleteOutput
  = DeleteOutput'Diff
  | DeleteOutput'NoDiff
  deriving stock (Eq, Show)

data DeleteTarget
  = DeleteTarget'TermOrType DeleteOutput [HQ'.HashQualified (Path.Split Path')]
  | DeleteTarget'Term DeleteOutput [HQ'.HashQualified (Path.Split Path')]
  | DeleteTarget'Type DeleteOutput [HQ'.HashQualified (Path.Split Path')]
  | DeleteTarget'Namespace Insistence (Maybe (Path.Split Path.Relative))
  | DeleteTarget'ProjectBranch (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | DeleteTarget'Project ProjectName
  deriving stock (Eq, Show)
