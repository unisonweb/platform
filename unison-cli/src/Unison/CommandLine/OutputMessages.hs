{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Unison.CommandLine.OutputMessages where

import Control.Lens hiding (at)
import Control.Monad.State.Strict qualified as State
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable qualified as Foldable
import Data.List (stripPrefix)
import Data.List qualified as List
import Data.List.Extra (notNull, nubOrd, nubOrdOn)
import Data.List.NonEmpty qualified as NEList
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime, getCurrentTime)
import Data.Tuple (swap)
import Data.Tuple.Extra (dupe)
import Data.Void (absurd)
import Debug.RecoverRTTI qualified as RTTI
import Network.HTTP.Types qualified as Http
import Servant.Client qualified as Servant
import System.Console.ANSI qualified as ANSI
import System.Console.Haskeline.Completion qualified as Completion
import System.Directory (canonicalizePath, getHomeDirectory)
import System.Exit (ExitCode (..))
import Text.Pretty.Simple (pShowNoColor, pStringNoColor)
import U.Codebase.Branch (NamespaceStats (..))
import U.Codebase.Branch.Diff (NameChanges (..))
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.ProjectReflog qualified as ProjectReflog
import Unison.ABT qualified as ABT
import Unison.Auth.Types qualified as Auth
import Unison.Builtin.Decls qualified as DD
import Unison.Cli.MergeTypes (MergeSourceAndTarget (..))
import Unison.Cli.Pretty
import Unison.Cli.ServantClientUtils qualified as ServantClientUtils
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Codebase.Editor.Input (BranchIdG (..))
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output
  ( CreatedProjectBranchFrom (..),
    NumberedArgs,
    NumberedOutput (..),
    Output (..),
    ShareError (..),
    TestReportStats (CachedTests, NewlyComputed),
    TodoOutput,
    UndoFailureReason (CantUndoPastMerge, CantUndoPastStart),
    todoOutputIsEmpty,
  )
import Unison.Codebase.Editor.Output qualified as E
import Unison.Codebase.Editor.Output.BranchDiff qualified as OBD
import Unison.Codebase.Editor.Output.PushPull qualified as PushPull
import Unison.Codebase.Editor.SlurpResult qualified as SlurpResult
import Unison.Codebase.Editor.StructuredArgument (StructuredArgument)
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.Codebase.IntegrityCheck (IntegrityResult (..), prettyPrintIntegrityErrors)
import Unison.Codebase.Patch qualified as Patch
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.CommandLine.FZFResolvers qualified as FZFResolvers
import Unison.CommandLine.Helpers (bigproblem, note, tip)
import Unison.CommandLine.InputPattern (InputPattern)
import Unison.CommandLine.InputPatterns (makeExample')
import Unison.CommandLine.InputPatterns qualified as IP
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.Core.Project (ProjectBranchName (UnsafeProjectBranchName))
import Unison.DataDeclaration qualified as DD
import Unison.Hash qualified as Hash
import Unison.Hash32 (Hash32)
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency as LD
import Unison.Merge.DeclCoherencyCheck (IncoherentDeclReason (..), IncoherentDeclReasons (..))
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann, startingLine)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Util qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyTerminal
  ( clearCurrentLine,
    putPretty',
  )
import Unison.PrintError
  ( prettyParseError,
    prettyResolutionFailures,
    prettyVar,
    printNoteWithSource,
    renderCompilerBug,
  )
import Unison.Project (ProjectAndBranch (..))
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Referent
import Unison.Result qualified as Result
import Unison.Server.Backend (ShallowListEntry (..), TypeEntry (..))
import Unison.Server.Backend qualified as Backend
import Unison.Server.SearchResultPrime qualified as SR'
import Unison.Share.Sync qualified as Share
import Unison.Share.Sync.Types (CodeserverTransportError (..))
import Unison.Sync.Types qualified as Share
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.HashQualified qualified as HQ (toText, unsafeFromVar)
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Syntax.NamePrinter
  ( prettyHashQualified,
    prettyHashQualified',
    prettyName,
    prettyNamedReference,
    prettyNamedReferent,
    prettyReference,
    prettyReferent,
    prettyShortHash,
  )
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.UnisonFile qualified as UF
import Unison.Util.Conflicted (Conflicted (..))
import Unison.Util.Defn (Defn (..))
import Unison.Util.Defns (Defns (..))
import Unison.Util.List qualified as List
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty qualified as P
import Unison.Util.Relation qualified as R
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind qualified as WK
import Witch (unsafeFrom)

reportBugURL :: Pretty
reportBugURL = "https://github.com/unisonweb/unison/issues/new"

type Pretty = P.Pretty P.ColorText

shortenDirectory :: FilePath -> IO FilePath
shortenDirectory dir = do
  home <- getHomeDirectory
  pure $ case stripPrefix home dir of
    Just d -> "~" <> d
    Nothing -> dir

renderFileName :: FilePath -> IO Pretty
renderFileName dir = P.group . P.blue . fromString <$> shortenDirectory dir

notifyNumbered :: NumberedOutput -> (Pretty, NumberedArgs)
notifyNumbered = \case
  ShowDiffNamespace oldPrefix newPrefix ppe diffOutput ->
    showDiffNamespace ShowNumbers ppe (either BranchAtSCH BranchAtProjectPath oldPrefix) (either BranchAtSCH BranchAtProjectPath newPrefix) diffOutput
  ShowDiffAfterDeleteDefinitions ppe diff ->
    first
      ( \p ->
          P.lines
            [ p,
              "",
              undoTip
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId Path.absoluteEmpty) (absPathToBranchId Path.absoluteEmpty) diff)
  ShowDiffAfterDeleteBranch bAbs ppe diff ->
    first
      ( \p ->
          P.lines
            [ p,
              "",
              undoTip
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId bAbs) (absPathToBranchId bAbs) diff)
  ShowDiffAfterModifyBranch b' _ _ (OBD.isEmpty -> True) ->
    (P.wrap $ "Nothing changed in" <> prettyPath' b' <> ".", mempty)
  ShowDiffAfterModifyBranch b' bAbs ppe diff ->
    first
      ( \p ->
          P.lines
            [ P.wrap $ "Here's what changed in" <> prettyPath' b' <> ":",
              "",
              p,
              "",
              undoTip
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId bAbs) (absPathToBranchId bAbs) diff)
  ShowDiffAfterMerge _ _ _ (OBD.isEmpty -> True) ->
    (P.wrap $ "Nothing changed as a result of the merge.", mempty)
  ShowDiffAfterMerge dest' destAbs ppe diffOutput ->
    first
      ( \p ->
          P.lines
            [ P.wrap $ "Here's what's changed in " <> prettyNamespaceKey dest' <> "after the merge:",
              "",
              p,
              "",
              tip $
                "You can use "
                  <> IP.makeExample' IP.todo
                  <> "to see if this generated any work to do in this namespace"
                  <> "and "
                  <> IP.makeExample' IP.test
                  <> "to run the tests."
                  <> "Or you can use"
                  <> IP.makeExample' IP.undo
                  <> " or use a hash from "
                  <> IP.makeExample' IP.branchReflog
                  <> " with "
                  <> IP.makeExample' IP.reset
                  <> " to reset to a previous state."
            ]
      )
      (showDiffNamespace ShowNumbers ppe (BranchAtProjectPath destAbs) (BranchAtProjectPath destAbs) diffOutput)
  ShowDiffAfterMergePropagate dest' destAbs patchPath' ppe diffOutput ->
    first
      ( \p ->
          P.lines
            [ P.wrap $
                "Here's what's changed in "
                  <> prettyNamespaceKey dest'
                  <> "after applying the patch at "
                  <> P.group (prettyPath' patchPath' <> ":"),
              "",
              p,
              "",
              tip $
                "You can use "
                  <> IP.makeExample IP.todo [prettyPath' patchPath', prettyNamespaceKey dest']
                  <> "to see if this generated any work to do in this namespace"
                  <> "and "
                  <> IP.makeExample' IP.test
                  <> "to run the tests."
                  <> "Or you can use"
                  <> IP.makeExample' IP.undo
                  <> " or use a hash from "
                  <> IP.makeExample' IP.branchReflog
                  <> " to undo the results of this merge."
            ]
      )
      (showDiffNamespace ShowNumbers ppe (BranchAtProjectPath destAbs) (BranchAtProjectPath destAbs) diffOutput)
  ShowDiffAfterMergePreview dest' destAbs ppe diffOutput ->
    first
      ( \p ->
          P.lines
            [ P.wrap $ "Here's what would change in " <> prettyNamespaceKey dest' <> "after the merge:",
              "",
              p
            ]
      )
      (showDiffNamespace ShowNumbers ppe (BranchAtProjectPath destAbs) (BranchAtProjectPath destAbs) diffOutput)
  ShowDiffAfterUndo ppe diffOutput ->
    first
      (\p -> P.lines ["Here are the changes I undid", "", p])
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId Path.absoluteEmpty) (absPathToBranchId Path.absoluteEmpty) diffOutput)
  ShowDiffAfterPull dest' destAbs ppe diff ->
    if OBD.isEmpty diff
      then ("✅  Looks like " <> prettyPath' dest' <> " is up to date.", mempty)
      else
        first
          ( \p ->
              P.lines
                [ P.wrap $ "Here's what's changed in " <> prettyPath' dest' <> "after the pull:",
                  "",
                  p,
                  "",
                  undoTip
                ]
          )
          (showDiffNamespace ShowNumbers ppe (absPathToBranchId destAbs) (absPathToBranchId destAbs) diff)
  -- todo: these numbers aren't going to work,
  --  since the content isn't necessarily here.
  -- Should we have a mode with no numbers? :P

  ShowDiffAfterCreateAuthor authorNS authorPath' bAbs ppe diff ->
    first
      ( \p ->
          P.lines
            [ p,
              "",
              tip $
                "Add"
                  <> prettyName (Name.fromSegment NameSegment.licenseSegment)
                  <> "values for"
                  <> prettyName (Name.fromSegment authorNS)
                  <> "under"
                  <> P.group (prettyPath' authorPath' <> ".")
            ]
      )
      (showDiffNamespace ShowNumbers ppe (absPathToBranchId bAbs) (absPathToBranchId bAbs) diff)
  TestResults stats ppe _showSuccess _showFailures oksUnsorted failsUnsorted ->
    let oks = Name.sortByText (HQ.toText . fst) [(name r, msgs) | (r, msgs) <- Map.toList oksUnsorted]
        fails = Name.sortByText (HQ.toText . fst) [(name r, msgs) | (r, msgs) <- Map.toList failsUnsorted]
        name r = PPE.termName ppe (Referent.fromTermReferenceId r)
     in ( case stats of
            CachedTests 0 _ -> P.callout "😶" $ "No tests to run."
            CachedTests n n' | n == n' -> P.lines [cache, "", displayTestResults True oks fails]
            CachedTests _n m ->
              if m == 0
                then "✅  "
                else
                  P.indentN 2 $
                    P.lines ["", cache, "", displayTestResults False oks fails, "", "✅  "]
            NewlyComputed ->
              P.lines
                [ "  " <> P.bold "New test results:",
                  "",
                  displayTestResults True oks fails
                ],
          fmap (SA.HashQualified . fst) $ oks <> fails
        )
    where
      cache = P.bold "Cached test results " <> "(`help testcache` to learn more)"
  Output'Todo todoOutput -> runNumbered (handleTodoOutput todoOutput)
  CantDeleteDefinitions ppeDecl endangerments ->
    ( P.warnCallout $
        P.lines
          [ P.wrap "I didn't delete the following definitions because they are still in use:",
            "",
            endangeredDependentsTable ppeDecl endangerments
          ],
      numberedArgsForEndangerments ppeDecl endangerments
    )
  CantDeleteNamespace ppeDecl endangerments ->
    ( P.warnCallout $
        P.lines
          [ P.wrap "I didn't delete the namespace because the following definitions are still in use.",
            "",
            endangeredDependentsTable ppeDecl endangerments,
            "",
            P.wrap ("If you want to proceed anyways and leave those definitions without names, use " <> IP.patternName IP.deleteNamespaceForce)
          ],
      numberedArgsForEndangerments ppeDecl endangerments
    )
  History _cap schLength history tail ->
    let (tailMsg, tailHashes) = handleTail (length history + 1)
        msg :: Pretty
        msg =
          P.lines
            [ note $ "The most recent namespace hash is immediately below this message.",
              "",
              P.sep "\n\n" [go i (toSCH h) diff | (i, (h, diff)) <- zip [1 ..] reversedHistory],
              "",
              tailMsg
            ]
        branchHashes :: [CausalHash]
        branchHashes = (fst <$> reversedHistory) <> tailHashes
     in (msg, SA.Namespace <$> branchHashes)
    where
      toSCH :: CausalHash -> ShortCausalHash
      toSCH h = SCH.fromHash schLength h
      reversedHistory = reverse history
      showNum :: Int -> Pretty
      showNum n = P.shown n <> ". "
      handleTail :: Int -> (Pretty, [CausalHash])
      handleTail n = case tail of
        E.EndOfLog h ->
          ( P.lines
              [ "□ " <> showNum n <> prettySCH (toSCH h) <> " (start of history)"
              ],
            [h]
          )
        E.MergeTail h hs ->
          ( P.lines
              [ P.wrap $ "This segment of history starts with a merge." <> ex,
                "",
                "⊙ " <> showNum n <> prettySCH (toSCH h),
                "⑃",
                P.lines (hs & imap \i h -> showNum (n + 1 + i) <> prettySCH (toSCH h))
              ],
            h : hs
          )
        E.PageEnd h _n ->
          ( P.lines
              [ P.wrap $ "There's more history before the versions shown here." <> ex,
                "",
                dots,
                "",
                "⊙ " <> showNum n <> prettySCH (toSCH h),
                ""
              ],
            [h]
          )
      dots = "⠇"
      go i sch diff =
        P.lines
          [ "⊙ " <> showNum i <> prettySCH sch,
            "",
            P.indentN 2 $ prettyDiff diff
          ]
      ex =
        "Use"
          <> IP.makeExample IP.history ["#som3n4m3space"]
          <> "to view history starting from a given namespace hash."
  DeletedDespiteDependents ppeDecl endangerments ->
    ( P.warnCallout $
        P.lines
          [ P.wrap "Of the things I deleted, the following are still used in the following definitions. They now contain un-named references.",
            "",
            endangeredDependentsTable ppeDecl endangerments
          ],
      numberedArgsForEndangerments ppeDecl endangerments
    )
  ListProjects projects ->
    ( P.numberedList (map (prettyProjectName . view #name) projects),
      map (SA.Project . view #name) projects
    )
  ListBranches projectName branches ->
    ( P.columnNHeader
        ["", "Branch", "Remote branch"]
        ( do
            (i, (branchName, remoteBranches0)) <- zip [(1 :: Int) ..] branches
            case uncons remoteBranches0 of
              Nothing -> pure [P.hiBlack (P.shown i <> "."), prettyProjectBranchName branchName, ""]
              Just (firstRemoteBranch, remoteBranches) ->
                [ P.hiBlack (P.shown i <> "."),
                  prettyProjectBranchName branchName,
                  prettyRemoteBranchInfo firstRemoteBranch
                ]
                  : map (\branch -> ["", "", prettyRemoteBranchInfo branch]) remoteBranches
        ),
      map
        (SA.ProjectBranch . ProjectAndBranch (pure projectName) . fst)
        branches
    )
  AmbiguousSwitch project (ProjectAndBranch currentProject branch) ->
    ( P.wrap
        ( "I'm not sure if you wanted to switch to the branch"
            <> prettyProjectAndBranchName (ProjectAndBranch currentProject branch)
            <> "or the project"
            <> P.group (prettyProjectName project <> ".")
            <> "Could you be more specific?"
        )
        <> P.newline
        <> P.newline
        <> P.numberedList
          [ prettySlashProjectBranchName branch <> " (the branch " <> prettyProjectBranchName branch <> " in the current project)",
            prettyProjectNameSlash project <> " (the project " <> prettyProjectName project <> ", with the branch left unspecified)"
          ]
        <> P.newline
        <> P.newline
        <> tip
          ( "use "
              <> switch ["1"]
              <> " or "
              <> switch ["2"]
              <> " to pick one of these."
          ),
      [ SA.ProjectBranch $ ProjectAndBranch Nothing branch,
        SA.ProjectBranch . ProjectAndBranch (pure project) $
          UnsafeProjectBranchName "main"
      ]
    )
    where
      switch = IP.makeExample IP.projectSwitch
  AmbiguousReset sourceOfAmbiguity (ProjectAndBranch _pn0 _bn0, path) (ProjectAndBranch currentProject branch) ->
    ( P.wrap
        ( openingLine
            <> prettyProjectAndBranchName (ProjectAndBranch currentProject branch)
            <> orTheNamespace
            <> relPath0
            <> "in the current branch."
            <> "Could you be more specific?"
        )
        <> P.newline
        <> P.newline
        <> P.numberedList
          [ prettySlashProjectBranchName branch <> " (the branch " <> prettyProjectBranchName branch <> " in the current project)",
            relPath0 <> " (the relative path " <> relPath0 <> " in the current branch)"
          ]
        <> P.newline
        <> P.newline
        <> tip
          ( "use "
              <> reset (resetArgs ["1"])
              <> " or "
              <> reset (resetArgs ["2"])
              <> " to pick one of these."
          ),
      [ SA.ProjectBranch $ ProjectAndBranch Nothing branch,
        SA.AbsolutePath absPath0
      ]
    )
    where
      openingLine = case sourceOfAmbiguity of
        E.AmbiguousReset'Hash -> "I'm not sure if you wanted to reset to the branch"
        E.AmbiguousReset'Target -> "I'm not sure if you wanted to reset the branch"
      orTheNamespace = case sourceOfAmbiguity of
        E.AmbiguousReset'Hash -> "or to the namespace"
        E.AmbiguousReset'Target -> "or the namespace"
      resetArgs = case sourceOfAmbiguity of
        E.AmbiguousReset'Hash -> \xs -> xs
        E.AmbiguousReset'Target -> \xs -> "<some hash>" : xs
      reset = IP.makeExample IP.reset
      relPath0 = prettyPath path
      absPath0 = Path.Absolute path
  ListNamespaceDependencies _ppe _path Empty -> ("This namespace has no external dependencies.", mempty)
  ListNamespaceDependencies ppe path' externalDependencies ->
    ( P.column2Header (P.hiBlack "External dependency") ("Dependents in " <> prettyProjectPath path') $
        List.intersperse spacer (externalDepsTable externalDependencies),
      numberedArgs
    )
    where
      spacer = ("", "")
      (nameNumbers, numberedArgs) = numberedDependents externalDependencies
      getNameNumber name = fromMaybe (error "ListNamespaceDependencies: name is missing number") (Map.lookup name nameNumbers)
      numberedDependents :: Map LabeledDependency (Set Name) -> (Map Name Int, NumberedArgs)
      numberedDependents deps =
        deps
          & Map.elems
          & List.foldl'
            ( \(nextNum, (nameToNum, args)) names ->
                let unnumberedNames = Set.toList $ Set.difference names (Map.keysSet nameToNum)
                    newNextNum = nextNum + length unnumberedNames
                 in ( newNextNum,
                      ( nameToNum <> (Map.fromList (zip unnumberedNames [nextNum ..])),
                        args <> unnumberedNames
                      )
                    )
            )
            (1, (mempty, mempty))
          & snd
          & over (_2 . mapped) SA.Name
      externalDepsTable :: Map LabeledDependency (Set Name) -> [(P.Pretty P.ColorText, P.Pretty P.ColorText)]
      externalDepsTable = ifoldMap $ \ld dependents ->
        [(prettyLD ld, prettyDependents dependents)]
      prettyLD :: LabeledDependency -> P.Pretty P.ColorText
      prettyLD =
        P.syntaxToColor
          . prettyHashQualified
          . LD.fold
            (PPE.typeName ppe)
            (PPE.termName ppe)
      prettyDependents :: Set Name -> P.Pretty P.ColorText
      prettyDependents refs =
        refs
          & Set.toList
          & fmap (\name -> formatNum (getNameNumber name) <> prettyName name)
          & P.lines
  ShowProjectBranchReflog now moreToShow entries -> displayProjectBranchReflogEntries now moreToShow entries
  where
    absPathToBranchId = BranchAtPath

undoTip :: P.Pretty P.ColorText
undoTip =
  tip $
    "You can use"
      <> IP.makeExample' IP.undo
      <> " or use a hash from "
      <> IP.makeExample' IP.branchReflog
      <> "to undo this change."

notifyUser :: FilePath -> Output -> IO Pretty
notifyUser dir = \case
  SaveTermNameConflict name ->
    pure
      . P.warnCallout
      . P.wrap
      $ "Cannot save the last run result into"
        <> P.backticked (P.text (Name.toText name))
        <> "because that name conflicts with a name in the scratch file."
  NoLastRunResult ->
    pure
      . P.warnCallout
      . P.wrap
      $ "There is no previous evaluation to save."
        <> "Use"
        <> P.backticked "run"
        <> "to evaluate something before attempting"
        <> "to save it."
  Success -> pure $ P.bold "Done."
  PrintMessage pretty -> do
    pure pretty
  CouldntLoadBranch h ->
    pure . P.fatalCallout . P.wrap $
      "I have reason to believe that"
        <> P.shown h
        <> "exists in the codebase, but there was a failure"
        <> "when I tried to load it."
  NamespaceEmpty p ->
    case p of
      (p0 NEList.:| []) ->
        pure
          . P.warnCallout
          $ "The namespace "
            <> either prettySCH prettyProjectPath p0
            <> " is empty. Was there a typo?"
      ps ->
        pure
          . P.warnCallout
          $ "The namespaces "
            <> P.commas (either prettySCH prettyProjectPath <$> ps)
            <> " are empty. Was there a typo?"
  LoadPullRequest baseNS headNS basePath headPath mergedPath squashedPath ->
    pure $
      P.lines
        [ P.wrap $ "I checked out" <> prettyReadRemoteNamespaceWith absurd baseNS <> "to" <> P.group (prettyPath' basePath <> "."),
          P.wrap $ "I checked out" <> prettyReadRemoteNamespaceWith absurd headNS <> "to" <> P.group (prettyPath' headPath <> "."),
          "",
          P.wrap $ "The merged result is in" <> P.group (prettyPath' mergedPath <> "."),
          P.wrap $ "The (squashed) merged result is in" <> P.group (prettyPath' squashedPath <> "."),
          P.wrap $
            "Use"
              <> IP.makeExample
                IP.diffNamespace
                [prettyPath' basePath, prettyPath' mergedPath]
              <> "or"
              <> IP.makeExample
                IP.diffNamespace
                [prettyPath' basePath, prettyPath' squashedPath]
              <> "to see what's been updated.",
          P.wrap $
            "Use"
              <> IP.makeExample
                IP.todo
                [ prettyPath' (snoc mergedPath NameSegment.defaultPatchSegment),
                  prettyPath' mergedPath
                ]
              <> "to see what work is remaining for the merge.",
          P.wrap $
            "Use"
              <> IP.makeExample
                IP.push
                [prettyReadRemoteNamespaceWith absurd baseNS, prettyPath' mergedPath]
              <> "or"
              <> IP.makeExample
                IP.push
                [prettyReadRemoteNamespaceWith absurd baseNS, prettyPath' squashedPath]
              <> "to push the changes."
        ]
  LoadedDefinitionsToSourceFile fp numDefinitions ->
    pure $
      P.callout "☝️" $
        P.lines
          [ P.wrap $ "I added " <> P.shown @Int numDefinitions <> " definitions to the top of " <> fromString fp,
            "",
            P.wrap $
              "You can edit them there, then run"
                <> makeExample' IP.update
                <> "to replace the definitions currently in this namespace."
          ]
  DisplayDefinitions code -> pure code
  OutputRewrittenFile dest vs -> displayOutputRewrittenFile dest vs
  DisplayRendered outputLoc pp ->
    displayRendered outputLoc pp
  TestIncrementalOutputStart ppe (n, total) r -> do
    putPretty' $
      P.shown (total - n)
        <> " tests left to run, current test: "
        <> P.syntaxToColor (prettyHashQualified (PPE.termName ppe $ Referent.fromTermReferenceId r))
    pure mempty
  TestIncrementalOutputEnd _ppe (_n, _total) _r isOk -> do
    clearCurrentLine
    if isOk
      then putPretty' "  ✅  "
      else putPretty' "  🚫  "
    pure mempty
  TermMissingType ref ->
    pure . P.fatalCallout . P.lines $
      [ P.wrap $
          "The type signature for reference "
            <> P.blue (P.text (Reference.toText ref))
            <> " is missing from the codebase! This means something might be wrong "
            <> " with the codebase, or the term was deleted just now "
            <> " by someone else. Trying your command again might fix it."
      ]
  EvaluationFailure err -> pure err
  SearchTermsNotFound hqs | null hqs -> pure mempty
  SearchTermsNotFound hqs ->
    pure $
      P.warnCallout "The following names were not found in the codebase. Check your spelling."
        <> P.newline
        <> (P.syntaxToColor $ P.indent "  " (P.lines (prettyHashQualified <$> hqs)))
  SearchTermsNotFoundDetailed wasTerm hqMisses otherHits ->
    pure (missMsg <> hitMsg)
    where
      typeOrTermMsg =
        if wasTerm
          then "I was expecting the following names to be terms, though I found types instead."
          else "I was expecting the following names to be types, though I found terms instead."
      missMsg = case null hqMisses of
        True -> mempty
        False ->
          P.warnCallout "The following names were not found in the codebase. Check your spelling."
            <> P.newline
            <> P.syntaxToColor (P.indent "  " (P.lines (prettyHashQualified <$> hqMisses)))
      hitMsg = case null otherHits of
        True -> mempty
        False ->
          P.warnCallout typeOrTermMsg
            <> P.newline
            <> P.syntaxToColor (P.indent "  " (P.lines (prettyHashQualified <$> otherHits)))
  NameNotFound _ ->
    pure . P.warnCallout $ "I don't know about that name."
  NamesNotFound hqs ->
    pure $
      P.warnCallout "The following names were not found in the codebase. Check your spelling."
        <> P.newline
        <> (P.syntaxToColor $ P.indent "  " (P.lines (fmap prettyName hqs)))
  TermNotFound _ ->
    pure . P.warnCallout $ "I don't know about that term."
  TypeNotFound _ ->
    pure . P.warnCallout $ "I don't know about that type."
  MoveNothingFound p ->
    pure . P.warnCallout $ "There is no term, type, or namespace at " <> prettyPath' p <> "."
  TermAlreadyExists _ _ ->
    pure . P.warnCallout $ "A term by that name already exists."
  TypeAlreadyExists _ _ ->
    pure . P.warnCallout $ "A type by that name already exists."
  BranchEmpty b ->
    pure . P.warnCallout . P.wrap $
      P.group (prettyWhichBranchEmpty b) <> "is an empty namespace."
  CantUndo reason -> case reason of
    CantUndoPastStart -> pure . P.warnCallout $ "Nothing more to undo."
    CantUndoPastMerge -> pure . P.warnCallout $ "Sorry, I can't undo a merge (not implemented yet)."
  NoMainFunction main ppe ts ->
    pure . P.callout "😶" $
      P.lines
        [ P.wrap $
            "I looked for a function"
              <> P.backticked (P.text $ HQ.toText main)
              <> "in the most recently typechecked file and codebase but couldn't find one. It has to have the type:",
          "",
          P.indentN 2 $ P.lines [P.text (HQ.toText main) <> " : " <> TypePrinter.pretty ppe t | t <- ts]
        ]
  BadMainFunction what main ty ppe ts ->
    pure . P.callout "😶" $
      P.lines
        [ P.string "I found this function:",
          "",
          P.indentN 2 $ P.text (HQ.toText main) <> " : " <> TypePrinter.pretty ppe ty,
          "",
          P.wrap $ P.string "but in order for me to" <> P.backticked (P.text what) <> "it needs to be a subtype of:",
          "",
          P.indentN 2 $ P.lines [P.text (HQ.toText main) <> " : " <> TypePrinter.pretty ppe t | t <- ts]
        ]
  NoUnisonFile -> do
    dir' <- canonicalizePath dir
    fileName <- renderFileName dir'
    pure . P.callout "😶" $
      P.lines
        [ P.wrap "There's nothing for me to add right now.",
          "",
          P.column2 [(P.bold "Hint:", msg fileName)]
        ]
    where
      msg dir =
        P.wrap $
          "I'm currently watching for definitions in .u files under the"
            <> dir
            <> "directory. Make sure you've updated something there before using the"
            <> makeExample' IP.add
            <> "or"
            <> makeExample' IP.update
            <> "commands, or use"
            <> makeExample' IP.load
            <> "to load a file explicitly."
  InvalidSourceName name ->
    pure . P.callout "😶" $
      P.wrap $
        "The file "
          <> P.blue (P.shown name)
          <> " does not exist or is not a valid source file."
  InvalidStructuredFindReplace _sym ->
    pure . P.callout "😶" $ IP.helpFor IP.sfindReplace
  InvalidStructuredFind _sym ->
    pure . P.callout "😶" $ IP.helpFor IP.sfind
  SourceLoadFailed name ->
    pure . P.callout "😶" $
      P.wrap $
        "The file "
          <> P.blue (P.shown name)
          <> " could not be loaded."
  BadNamespace msg path ->
    pure . P.warnCallout $ "Invalid namespace " <> P.blue (P.string path) <> ", " <> P.string msg
  BranchNotFound b ->
    pure . P.warnCallout $ "The namespace " <> P.blue (P.shown b) <> " doesn't exist."
  EmptyLooseCodePush b ->
    pure . P.warnCallout $ "The namespace " <> P.blue (P.shown b) <> " is empty. There is nothing to push."
  EmptyProjectBranchPush projectAndBranch ->
    pure . P.warnCallout . P.wrap $
      prettyProjectAndBranchName projectAndBranch <> "is empty. There is nothing to push."
  CreatedNewBranch path ->
    pure $
      "☝️  The namespace " <> prettyAbsolute path <> " is empty."
  -- RenameOutput rootPath oldName newName r -> do
  --   nameChange "rename" "renamed" oldName newName r
  -- AliasOutput rootPath existingName newName r -> do
  --   nameChange "alias" "aliased" existingName newName r
  DeletedEverything ->
    pure . P.wrap . P.lines $
      [ "Okay, I deleted everything except the history.",
        "Use "
          <> IP.makeExample' IP.undo
          <> " to undo, or "
          <> IP.makeExample' IP.mergeBuiltins
          <> " to restore the absolute "
          <> "basics to the current path."
      ]
  DeleteEverythingConfirmation ->
    pure . P.warnCallout . P.lines $
      [ "Are you sure you want to clear away everything?",
        P.wrap
          ( "You could use "
              <> IP.makeExample' IP.projectCreate
              <> " to switch to a new project instead,"
              <> " or delete the current branch with "
              <> IP.makeExample' IP.deleteBranch
          )
      ]
  DeleteBranchConfirmation _uniqueDeletions -> error "todo"
  -- let
  --   pretty (branchName, (ppe, results)) =
  --     header $ listOfDefinitions' ppe False results
  --     where
  --     header = plural uniqueDeletions id ((P.text branchName <> ":") `P.hang`)
  --
  -- in putPrettyLn . P.warnCallout
  --   $ P.wrap ("The"
  --   <> plural uniqueDeletions "namespace contains" "namespaces contain"
  --   <> "definitions that don't exist in any other branches:")
  --   <> P.border 2 (mconcat (fmap pretty uniqueDeletions))
  --   <> P.newline
  --   <> P.wrap "Please repeat the same command to confirm the deletion."
  MoveRootBranchConfirmation ->
    pure . P.warnCallout . P.lines $
      [ "Moves which affect the root branch cannot be undone, are you sure?",
        "Re-run the same command to proceed."
      ]
  MovedOverExistingBranch dest' ->
    pure . P.warnCallout . P.lines $
      [ P.wrap $ "A branch existed at the destination:" <> prettyPath' dest' <> "so I over-wrote it.",
        "",
        undoTip
      ]
  ListOfDefinitions fscope ppe detailed results ->
    listOfDefinitions fscope ppe detailed results
  GlobalFindBranchResults projBranchName ppe detailed results -> do
    output <- listOfDefinitions Input.FindGlobal ppe detailed results
    pure $
      P.lines
        [ P.wrap $ "Found results in " <> P.text (into @Text projBranchName),
          "",
          output
        ]
  ListNames len types terms ->
    listOfNames len types terms
  GlobalListNames projectBranchName len types terms -> do
    output <- listOfNames len types terms
    pure $
      P.lines
        [ P.wrap $ "Found results in " <> P.text (into @Text projectBranchName),
          "",
          output
        ]
  -- > names foo
  --   Terms:
  --     Hash: #asdflkjasdflkjasdf
  --     Names: .util.frobnicate foo blarg.mcgee
  --
  --   Term (with hash #asldfkjsdlfkjsdf): .util.frobnicate, foo, blarg.mcgee
  --   Types (with hash #hsdflkjsdfsldkfj): Optional, Maybe, foo
  ListShallow buildPPE entries -> do
    let needPPE =
          entries
            & any \case
              ShallowTermEntry {} -> True
              _ -> False
    ppe <-
      if needPPE
        then buildPPE
        else pure PPE.empty
    -- todo: make a version of prettyNumberedResult to support 3-columns
    pure $
      if null entries
        then P.lit "nothing to show"
        else numberedEntries ppe entries
    where
      numberedEntries :: (Var v) => PPE.PrettyPrintEnv -> [ShallowListEntry v a] -> Pretty
      numberedEntries ppe entries =
        (P.column3 . fmap f) ([(1 :: Integer) ..] `zip` fmap (formatEntry ppe) entries)
        where
          f (i, (p1, p2)) = (P.hiBlack . fromString $ show i <> ".", p1, p2)
      formatEntry :: (Var v) => PPE.PrettyPrintEnv -> ShallowListEntry v a -> (Pretty, Pretty)
      formatEntry ppe = \case
        ShallowTermEntry termEntry ->
          ( P.syntaxToColor . prettyHashQualified' . Backend.termEntryHQName $ termEntry,
            P.lit "(" <> maybe "type missing" (TypePrinter.pretty ppe) (Backend.termEntryType termEntry) <> P.lit ")"
          )
        ShallowTypeEntry typeEntry ->
          ( P.syntaxToColor . prettyHashQualified' . Backend.typeEntryHQName $ typeEntry,
            isBuiltin (typeEntryReference typeEntry)
          )
        ShallowBranchEntry ns _ (NamespaceStats {numContainedTerms, numContainedTypes}) ->
          ( (P.syntaxToColor . prettyName . Name.fromSegment) ns <> "/",
            case catMaybes [formatCount "term" numContainedTerms, formatCount "type" numContainedTypes] of
              [] -> ""
              counts -> P.hiBlack $ "(" <> intercalateMap ", " id counts <> ")"
          )
        ShallowPatchEntry ns ->
          ( (P.syntaxToColor . prettyName . Name.fromSegment) ns,
            P.lit "(patch)"
          )
      formatCount :: Pretty -> Int -> Maybe Pretty
      formatCount _thing 0 = Nothing
      formatCount thing 1 = Just $ "1 " <> thing
      formatCount thing n = Just $ P.shown n <> " " <> thing <> "s"
      isBuiltin = \case
        Reference.Builtin {} -> P.lit "(builtin type)"
        Reference.DerivedId {} -> P.lit "(type)"
  SlurpOutput input ppe s ->
    let isPast = case input of
          Input.AddI {} -> True
          Input.UpdateI {} -> True
          Input.SaveExecuteResultI {} -> True
          _ -> False
     in pure $ SlurpResult.pretty isPast ppe s
  FindNoLocalMatches ->
    pure . P.callout "☝️" $ P.wrap "I couldn't find matches in this namespace, searching in 'lib'..."
  NoExactTypeMatches ->
    pure . P.callout "☝️" $ P.wrap "I couldn't find exact type matches, resorting to fuzzy matching..."
  TypeParseError src e ->
    pure . P.fatalCallout $
      P.lines
        [ P.wrap "I couldn't parse the type you supplied:",
          "",
          prettyParseError src e
        ]
  ParseResolutionFailures src es ->
    pure $
      prettyResolutionFailures src es
  TypeHasFreeVars typ ->
    pure . P.warnCallout $
      P.lines
        [ P.wrap "The type uses these names, but I'm not sure what they are:",
          P.sep ", " (map (P.text . Var.name) . toList $ ABT.freeVars typ)
        ]
  ParseErrors src es ->
    pure . P.sep "\n\n" $ prettyParseError (Text.unpack src) <$> es
  TypeErrors _curPath src ppenv notes -> do
    let showNote =
          intercalateMap "\n\n" (printNoteWithSource ppenv (Text.unpack src))
            . map Result.TypeError
    pure $ showNote notes
  CompilerBugs src env bugs -> pure $ intercalateMap "\n\n" bug bugs
    where
      bug = renderCompilerBug env (Text.unpack src)
  Evaluated fileContents ppe bindings watches ->
    if null watches
      then pure "\n"
      else -- todo: hashqualify binding names if necessary to distinguish them from
      --       defs in the codebase.  In some cases it's fine for bindings to
      --       shadow codebase names, but you don't want it to capture them in
      --       the decompiled output.

        let prettyBindings =
              P.bracket . P.lines $
                P.wrap "The watch expression(s) reference these definitions:"
                  : ""
                  : [ P.syntaxToColor $ TermPrinter.prettyBinding ppe (HQ.unsafeFromVar v) b
                      | (v, b) <- bindings
                    ]
            prettyWatches =
              P.sep
                "\n\n"
                [ watchPrinter fileContents ppe ann kind evald isCacheHit
                  | (ann, kind, evald, isCacheHit) <-
                      sortOn (\(a, _, _, _) -> a) . toList $ watches
                ]
         in -- todo: use P.nonempty
            pure $
              if null bindings
                then prettyWatches
                else prettyBindings <> "\n" <> prettyWatches
  RunResult ppe term -> pure (TermPrinter.pretty ppe term)
  DisplayConflicts termNamespace typeNamespace ->
    pure $
      P.sepNonEmpty
        "\n\n"
        [ showConflicts "terms" terms,
          showConflicts "types" types
        ]
    where
      terms = R.dom termNamespace
      types = R.dom typeNamespace
      showConflicts :: (Foldable f) => Pretty -> f Name -> Pretty
      showConflicts thingsName things =
        if (null things)
          then mempty
          else
            P.lines
              [ "These " <> thingsName <> " have conflicts: ",
                "",
                P.lines [("  " <> prettyName x) | x <- toList things]
              ]
  LoadingFile sourceName -> do
    fileName <- renderFileName $ Text.unpack sourceName
    pure $ P.wrap $ "Loading changes detected in " <> P.group (fileName <> ".")
  Typechecked sourceName ppe slurpResult uf -> do
    let fileStatusMsg = SlurpResult.pretty False ppe slurpResult
    let containsWatchExpressions = notNull $ UF.watchComponents uf
    if UF.nonEmpty uf
      then do
        fileName <- renderFileName $ Text.unpack sourceName
        pure $
          P.linesNonEmpty
            ( [ if fileStatusMsg == mempty
                  then P.okCallout $ fileName <> " changed."
                  else
                    if SlurpResult.isAllDuplicates slurpResult
                      then
                        P.wrap $
                          "I found and"
                            <> P.bold "typechecked"
                            <> "the definitions in "
                            <> P.group (fileName <> ".")
                            <> "This file "
                            <> P.bold "has been previously added"
                            <> "to the codebase."
                      else
                        P.linesSpaced $
                          [ P.wrap $
                              "I found and"
                                <> P.bold "typechecked"
                                <> "these definitions in "
                                <> P.group (fileName <> ".")
                                <> "If you do an "
                                <> IP.makeExample' IP.add
                                <> " or "
                                <> P.group (IP.makeExample' IP.update <> ",")
                                <> "here's how your codebase would change:",
                            P.indentN 2 $ SlurpResult.pretty False ppe slurpResult
                          ]
              ]
                ++ if containsWatchExpressions
                  then
                    [ "",
                      P.wrap $
                        "Now evaluating any watch expressions"
                          <> "(lines starting with `>`)... "
                          <> P.group (P.hiBlack "Ctrl+C cancels.")
                    ]
                  else []
            )
      else
        if (null $ UF.watchComponents uf)
          then
            pure . P.wrap $
              "I loaded " <> P.text sourceName <> " and didn't find anything."
          else pure mempty
  BustedBuiltins (Set.toList -> new) (Set.toList -> old) ->
    -- todo: this could be prettier!  Have a nice list like `find` gives, but
    -- that requires querying the codebase to determine term types.  Probably
    -- the only built-in types will be primitive types like `Int`, so no need
    -- to look up decl types.
    -- When we add builtin terms, they may depend on new derived types, so
    -- these derived types should be added to the branch too; but not
    -- necessarily ever be automatically deprecated.  (A library curator might
    -- deprecate them; more work needs to go into the idea of sharing deprecations and stuff.
    pure . P.warnCallout . P.lines $
      case (new, old) of
        ([], []) -> error "BustedBuiltins busted, as there were no busted builtins."
        ([], old) ->
          P.wrap ("This codebase includes some builtins that are considered deprecated. Use the " <> makeExample' IP.updateBuiltins <> " command when you're ready to work on eliminating them from your codebase:")
            : ""
            : fmap (P.text . Reference.toText) old
        (new, []) ->
          P.wrap ("This version of Unison provides builtins that are not part of your codebase. Use " <> makeExample' IP.updateBuiltins <> " to add them:")
            : ""
            : fmap (P.text . Reference.toText) new
        (new@(_ : _), old@(_ : _)) ->
          [ P.wrap
              ( "Sorry and/or good news!  This version of Unison supports a different set of builtins than this codebase uses.  You can use "
                  <> makeExample' IP.updateBuiltins
                  <> " to add the ones you're missing and deprecate the ones I'm missing. 😉"
              ),
            "You're missing:" `P.hang` P.lines (fmap (P.text . Reference.toText) new),
            "I'm missing:" `P.hang` P.lines (fmap (P.text . Reference.toText) old)
          ]
  NoConfiguredRemoteMapping pp p -> do
    let (localPathExample, sharePathExample) =
          if Path.isRoot p
            then ("myproject", "myuser.public.myproject")
            else (Path.toText (Path.unabsolute p), "myuser.public." <> Path.toText (Path.unabsolute p))
    pure . P.fatalCallout $
      P.lines
        [ "I don't know where to " <> PushPull.fold "push to." "pull from." pp,
          "Add a `RemoteMapping` configuration to your .unisonConfig file. E.g.",
          "",
          "```",
          "RemoteMapping {",
          P.text ("  " <> localPathExample <> " = \"" <> sharePathExample <> "\""),
          "}",
          "```",
          "",
          "Type `help " <> PushPull.fold "push" "pull" pp <> "` for more information."
        ]

  --  | ConfiguredRemoteMappingParseError PushPull Path' Text String
  ConfiguredRemoteMappingParseError pp p url err ->
    pure . P.fatalCallout . P.lines $
      [ P.wrap $
          "I couldn't understand the RemoteMapping that's set for"
            <> prettyAbsolute p
            <> "in .unisonConfig",
        P.wrap $
          "The value I found was"
            <> (P.backticked . P.blue . P.text) url
            <> "but I encountered the following error when trying to parse it:",
        "",
        P.string err,
        "",
        P.wrap $
          "Type"
            <> P.backticked ("help " <> PushPull.fold "push" "pull" pp)
            <> "for more information."
      ]
  NoBranchWithHash _h ->
    pure . P.callout "😶" $
      P.wrap $
        "I don't know of a namespace with that hash."
  NotImplemented -> pure $ P.wrap "That's not implemented yet. Sorry! 😬"
  BranchAlreadyExists p ->
    pure . P.wrap $
      "The namespace" <> prettyPath' p <> "already exists."
  LabeledReferenceNotFound hq ->
    pure . P.callout "\129300" . P.wrap . P.syntaxToColor $
      "Sorry, I couldn't find anything named" <> prettyHashQualified hq <> "."
  LabeledReferenceAmbiguous hashLen hq (LD.partition -> (tps, tms)) ->
    pure . P.callout "\129300" . P.lines $
      [ P.wrap "That name is ambiguous. It could refer to any of the following definitions:",
        "",
        P.indentN 2 (P.lines (map qualifyTerm tms ++ map qualifyType tps))
      ]
    where
      qualifyTerm :: Referent -> Pretty
      qualifyTerm =
        P.syntaxToColor . case hq of
          HQ.NameOnly n -> prettyNamedReferent hashLen n
          HQ.HashQualified n _ -> prettyNamedReferent hashLen n
          HQ.HashOnly _ -> prettyReferent hashLen
      qualifyType :: Reference -> Pretty
      qualifyType =
        P.syntaxToColor . case hq of
          HQ.NameOnly n -> prettyNamedReference hashLen n
          HQ.HashQualified n _ -> prettyNamedReference hashLen n
          HQ.HashOnly _ -> prettyReference hashLen
  DeleteNameAmbiguous hashLen p tms tys ->
    pure . P.callout "\129300" . P.lines $
      [ P.wrap "I wasn't sure which of these you meant to delete:",
        "",
        P.indentN 2 (P.lines (map qualifyTerm (Set.toList tms) ++ map qualifyType (Set.toList tys))),
        "",
        P.wrap "You may:",
        "",
        P.indentN 2 . P.bulleted $
          [ P.wrap "Delete one by an unambiguous name, given above.",
            P.wrap "Delete them all by re-issuing the previous command."
          ]
      ]
    where
      name :: Name
      name = HQ'.toName $ Path.nameFromHQSplit' p
      qualifyTerm :: Referent -> Pretty
      qualifyTerm = P.syntaxToColor . prettyNamedReferent hashLen name
      qualifyType :: Reference -> Pretty
      qualifyType = P.syntaxToColor . prettyNamedReference hashLen name
  TermAmbiguous _ _ tms | Set.null tms -> pure "I couldn't find any term by that name."
  TermAmbiguous ppe _n tms ->
    pure . P.callout "🤔" . P.lines $
      [ P.wrap "I wasn't sure which of these you meant:",
        "",
        P.indentN 2 (P.lines (map (phq . PPE.termNameOrHashOnly ppe) (Set.toList tms))),
        "",
        tip "Try again, using one of the unambiguous choices above."
      ]
    where
      phq = P.syntaxToColor . prettyHashQualified
  HashAmbiguous h rs ->
    pure . P.callout "\129300" . P.lines $
      [ P.wrap $
          "The hash"
            <> prettyShortHash h
            <> "is ambiguous."
            <> "Did you mean one of these hashes?",
        "",
        P.indentN 2 $ P.lines (P.shown <$> Set.toList rs),
        "",
        P.wrap "Try again with a few more hash characters to disambiguate."
      ]
  BranchHashAmbiguous h rs ->
    pure . P.callout "\129300" . P.lines $
      [ P.wrap $
          "The namespace hash"
            <> prettySCH h
            <> "is ambiguous."
            <> "Did you mean one of these hashes?",
        "",
        P.indentN 2 $ P.lines (prettySCH <$> Set.toList rs),
        "",
        P.wrap "Try again with a few more hash characters to disambiguate."
      ]
  BadName n -> pure . P.wrap $ P.text n <> " is not a kind of name I understand."
  TermNotFound' sh ->
    pure $
      "I could't find a term with hash "
        <> (prettyShortHash sh)
  TypeNotFound' sh ->
    pure $
      "I could't find a type with hash "
        <> (prettyShortHash sh)
  AboutToPropagatePatch -> pure "Applying changes from patch..."
  PatchNeedsToBeConflictFree ->
    pure . P.wrap $
      "I tried to auto-apply the patch, but couldn't because it contained"
        <> "contradictory entries."
  PatchInvolvesExternalDependents _ _ ->
    pure "That patch involves external dependents."
  ShowReflog [] -> pure . P.warnCallout $ "The reflog is empty"
  ShowReflog entries -> do
    now <- getCurrentTime
    pure $
      P.lines
        [ header,
          P.numberedColumnNHeader ["When", "Root Hash", "Action"] $ entries <&> renderEntry3Column now,
          "",
          tip $ "Use " <> IP.makeExample IP.diffNamespace ["1", "7"] <> " to compare namespaces between two points in history."
        ]
    where
      header =
        case entries of
          (_head : (_, prevSCH, _) : _) ->
            P.lines
              [ P.wrap $
                  "Here is a log of the root namespace hashes,"
                    <> "starting with the most recent,"
                    <> "along with the command that got us there."
                    <> "Try:",
                "",
                ( P.indentN 2 . P.wrapColumn2 $
                    [ ( IP.makeExample IP.forkLocal ["2", ".old"],
                        ""
                      ),
                      ( IP.makeExample IP.forkLocal [prettySCH prevSCH, ".old"],
                        "to make an old namespace accessible again,"
                      ),
                      (mempty, mempty),
                      ( IP.makeExample IP.reset [prettySCH prevSCH],
                        "to reset the current namespace and its history to that of the specified"
                          <> "namespace."
                      )
                    ]
                ),
                ""
              ]
          _ -> mempty
      renderEntry3Column :: UTCTime -> (Maybe UTCTime, SCH.ShortCausalHash, Text) -> [Pretty]
      renderEntry3Column now (mayTime, sch, reason) =
        [maybe "" (prettyHumanReadableTime now) mayTime, P.blue (prettySCH sch), P.text $ truncateReason reason]
      truncateReason :: Text -> Text
      truncateReason txt = case Text.splitAt 60 txt of
        (short, "") -> short
        (short, _) -> short <> "..."
  StartOfCurrentPathHistory ->
    pure $
      P.wrap "You're already at the very beginning! 🙂"
  PullAlreadyUpToDate ns dest ->
    pure . P.callout "😶" $
      P.wrap $
        prettyProjectAndBranchName (ProjectAndBranch dest.project.name dest.branch.name)
          <> "was already up-to-date with"
          <> P.group (prettyReadRemoteNamespace ns <> ".")
  PullSuccessful ns dest ->
    pure . P.okCallout $
      P.wrap $
        "Successfully updated"
          <> prettyProjectAndBranchName (ProjectAndBranch dest.project.name dest.branch.name)
          <> "from"
          <> P.group (prettyReadRemoteNamespace ns <> ".")
  AboutToMerge -> pure "Merging..."
  MergeOverEmpty dest ->
    pure . P.okCallout $
      P.wrap $
        "Successfully pulled into "
          <> P.group
            ( prettyProjectAndBranchName (ProjectAndBranch dest.project.name dest.branch.name)
                <> ", which was empty."
            )
  MergeAlreadyUpToDate src dest ->
    pure . P.callout "😶" $
      P.wrap $
        prettyBranchRelativePath dest
          <> "was already up-to-date with"
          <> P.group (prettyBranchRelativePath src <> ".")
  MergeAlreadyUpToDate2 aliceAndBob ->
    pure . P.callout "😶" $
      P.wrap $
        prettyProjectAndBranchName aliceAndBob.alice
          <> "was already up-to-date with"
          <> P.group (prettyMergeSource aliceAndBob.bob <> ".")
  MergeConflictedAliases aliceOrBob defn ->
    pure $
      P.wrap "Sorry, I wasn't able to perform the merge:"
        <> P.newline
        <> P.newline
        <> P.wrap
          ( "On the merge ancestor,"
              <> ( let (isTerm, name1, name2) =
                         case defn of
                           TermDefn (n1, n2) -> (True, n1, n2)
                           TypeDefn (n1, n2) -> (False, n1, n2)
                    in prettyName name1
                         <> "and"
                         <> prettyName name2
                         <> "were aliases for the same"
                         <> P.group ((if isTerm then "term" else "type") <> ",")
                 )
              <> "but on"
              <> prettyMergeSourceOrTarget aliceOrBob
              <> "the names have different definitions currently. I'd need just a single new definition to use in their"
              <> "dependents when I merge."
          )
        <> P.newline
        <> P.newline
        <> P.wrap ("Please fix up" <> prettyMergeSourceOrTarget aliceOrBob <> "to resolve this. For example,")
        <> P.newline
        <> P.newline
        <> P.indentN
          2
          ( P.bulleted
              [ P.wrap
                  ( IP.makeExample' IP.update
                      <> "the definitions to be the same again, so that there's nothing for me to decide."
                  ),
                P.wrap
                  ( IP.makeExample' IP.moveAll
                      <> "or"
                      <> IP.makeExample' IP.delete
                      <> "all but one of the definitions; I'll use the remaining name when propagating updates."
                      <> "(You can"
                      <> IP.makeExample' IP.moveAll
                      <> "it back after the merge.)"
                  )
              ]
          )
        <> P.newline
        <> P.newline
        <> P.wrap "and then try merging again."
  MergeConflictInvolvingBuiltin defn ->
    let (isTerm, name) =
          case defn of
            TermDefn n -> (True, n)
            TypeDefn n -> (False, n)
     in pure . P.lines $
          [ P.wrap "Sorry, I wasn't able to perform the merge:",
            "",
            P.wrap
              ( "There's a merge conflict on"
                  <> (if isTerm then "term" else "type")
                  <> P.group (prettyName name <> ",")
                  <> "but it's a builtin on one or both branches. I can't yet handle merge conflicts involving builtins."
              ),
            "",
            P.wrap
              ( "Please eliminate this conflict by updating one branch or the other, making"
                  <> prettyName name
                  <> "the same on both branches, or making neither of them a builtin, and then try the merge again."
              )
          ]
  -- Note [DefnsInLibMessage] If you change this, also change the other similar one
  MergeDefnsInLib aliceOrBob ->
    pure . P.lines $
      [ P.wrap "Sorry, I wasn't able to perform the merge:",
        "",
        P.wrap $
          "On"
            <> P.group (prettyMergeSourceOrTarget aliceOrBob <> ",")
            <> "there's a type or term at the top level of the `lib` namespace, where I only expect to find"
            <> "subnamespaces representing library dependencies.",
        "",
        P.wrap "Please move or remove it and then try merging again."
      ]
  PreviewMergeAlreadyUpToDate src dest ->
    pure . P.callout "😶" $
      P.wrap $
        prettyProjectPath dest
          <> "is already up-to-date with"
          <> P.group (prettyProjectPath src)
  DumpNumberedArgs schLength args ->
    pure . P.numberedList $ fmap (P.text . IP.formatStructuredArgument (pure schLength)) args
  HelpMessage pat -> pure $ IP.showPatternHelp pat
  NoOp -> pure $ P.string "I didn't make any changes."
  DumpBitBooster head map ->
    let go output [] = output
        go output (head : queue) = case Map.lookup head map of
          Nothing -> go (renderLine head [] : output) queue
          Just tails -> go (renderLine head tails : output) (queue ++ tails)
          where
            renderHash = take 10 . Text.unpack . Hash.toBase32HexText . unCausalHash
            renderLine head tail =
              (renderHash head)
                ++ "|"
                ++ intercalateMap " " renderHash tail
                ++ case Map.lookup (Hash.toBase32HexText . unCausalHash $ head) tags of
                  Just t -> "|tag: " ++ t
                  Nothing -> ""
            -- some specific hashes that we want to label in the output
            tags :: Map Text String
            tags =
              Map.fromList . fmap swap $
                [ ("unisonbase 2019/8/6", "54s9qjhaonotuo4sp6ujanq7brngk32f30qt5uj61jb461h9fcca6vv5levnoo498bavne4p65lut6k6a7rekaruruh9fsl19agu8j8"),
                  ("unisonbase 2019/8/5", "focmbmg7ca7ht7opvjaqen58fobu3lijfa9adqp7a1l1rlkactd7okoimpfmd0ftfmlch8gucleh54t3rd1e7f13fgei86hnsr6dt1g"),
                  ("unisonbase 2019/7/31", "jm2ltsg8hh2b3c3re7aru6e71oepkqlc3skr2v7bqm4h1qgl3srucnmjcl1nb8c9ltdv56dpsgpdur1jhpfs6n5h43kig5bs4vs50co"),
                  ("unisonbase 2019/7/25", "an1kuqsa9ca8tqll92m20tvrmdfk0eksplgjbda13evdlngbcn5q72h8u6nb86ojr7cvnemjp70h8cq1n95osgid1koraq3uk377g7g"),
                  ("ucm m1b", "o6qocrqcqht2djicb1gcmm5ct4nr45f8g10m86bidjt8meqablp0070qae2tvutnvk4m9l7o1bkakg49c74gduo9eati20ojf0bendo"),
                  ("ucm m1, m1a", "auheev8io1fns2pdcnpf85edsddj27crpo9ajdujum78dsncvfdcdu5o7qt186bob417dgmbd26m8idod86080bfivng1edminu3hug")
                ]
     in pure $
          P.lines
            [ P.lines (fmap fromString . reverse . nubOrd $ go [] [head]),
              "",
              "Paste that output into http://bit-booster.com/graph.html"
            ]
  ListDependents ppe lds types terms ->
    pure $ listDependentsOrDependencies ppe "Dependents" "dependents" lds types terms
  ListDependencies ppe lds types terms ->
    pure $ listDependentsOrDependencies ppe "Dependencies" "dependencies" lds types terms
  ListStructuredFind terms ->
    pure $ listFind False Nothing terms
  ListTextFind True terms ->
    pure $ listFind True Nothing terms
  ListTextFind False terms ->
    pure $ listFind False (Just tip) terms
    where
      tip = (IP.makeExample (IP.textfind True) [] <> " will search `lib` as well.")
  DumpUnisonFileHashes hqLength datas effects terms ->
    pure . P.syntaxToColor . P.lines $
      ( effects <&> \(n, r) ->
          "ability "
            <> prettyHashQualified' (HQ'.take hqLength . HQ'.fromNamedReference n $ Reference.DerivedId r)
      )
        <> ( datas <&> \(n, r) ->
               "type "
                 <> prettyHashQualified' (HQ'.take hqLength . HQ'.fromNamedReference n $ Reference.DerivedId r)
           )
        <> ( terms <&> \(n, r) ->
               prettyHashQualified' (HQ'.take hqLength . HQ'.fromNamedReference n $ Reference.DerivedId r)
           )
  GistCreated remoteNamespace ->
    pure $
      P.lines
        [ "Gist created. Pull via:",
          "",
          P.indentN 2 (IP.patternName IP.pull <> " " <> prettyReadRemoteNamespaceWith absurd remoteNamespace)
        ]
  InitiateAuthFlow authURI -> do
    pure $
      P.wrap $
        "Please navigate to " <> prettyURI authURI <> " to authorize UCM with the codebase server."
  UnknownCodeServer codeServerName -> do
    pure $
      P.lines
        [ P.wrap $ "No host configured for code server " <> P.red (P.text codeServerName) <> ".",
          "You can configure code server hosts in your .unisonConfig file."
        ]
  CredentialFailureMsg err -> pure $ case err of
    Auth.ReauthRequired host ->
      P.lines
        [ "Authentication for host " <> P.red (P.shown host) <> " is required.",
          "Run "
            <> IP.makeExample IP.help [IP.patternName IP.authLogin]
            <> " to learn how."
        ]
    Auth.CredentialParseFailure fp txt ->
      P.lines
        [ "Failed to parse the credentials file at " <> prettyFilePath fp <> ", with error: " <> P.text txt <> ".",
          "You can attempt to fix the issue, or may simply delete the credentials file and run " <> IP.makeExample IP.authLogin [] <> "."
        ]
    Auth.InvalidDiscoveryDocument uri txt ->
      P.lines
        [ "Failed to parse the discover document from " <> prettyURI uri <> ", with error: " <> P.text txt <> "."
        ]
    Auth.InvalidJWT txt ->
      P.lines
        [ "Failed to validate JWT from authentication server: " <> P.text txt
        ]
    Auth.RefreshFailure txt ->
      P.lines
        [ "Failed to refresh access token with authentication server: " <> P.text txt
        ]
    Auth.InvalidTokenResponse uri txt ->
      P.lines
        [ "Failed to parse token response from authentication server: " <> prettyURI uri,
          "The error was: " <> P.text txt
        ]
    Auth.InvalidHost host ->
      P.lines
        [ "Failed to parse a URI from the hostname: " <> P.shown host <> ".",
          "Host names should NOT include a schema or path."
        ]
    Auth.FailedToFetchUserInfo userInfoEndpoint err ->
      P.lines
        [ "Failed to parse the response from user info endpoint: " <> P.shown userInfoEndpoint,
          P.text err,
          "Please `auth.login` then try again, if this error persists please file a bug report and include the above error message."
        ]
  PrintVersion ucmVersion -> pure (P.text ucmVersion)
  ShareError shareError -> pure (prettyShareError shareError)
  ViewOnShare shareRef ->
    pure $ "View it here: " <> prettyRemoteBranchInfo shareRef
  IntegrityCheck result -> pure $ case result of
    NoIntegrityErrors -> "🎉 No issues detected 🎉"
    IntegrityErrorDetected ns -> prettyPrintIntegrityErrors ns
  DebugTerm verbose builtinOrTerm -> pure $ case builtinOrTerm of
    Left builtin -> "Builtin term: ##" <> P.text builtin
    Right trm ->
      if verbose
        then P.text . TL.toStrict . pStringNoColor $ RTTI.anythingToString trm
        else P.shown trm
  DebugDecl typ mayConId -> do
    let constructorMsg = case mayConId of
          Nothing -> ""
          Just conId -> "Constructor #" <> P.shown conId <> " of the following type:\n"
    pure $
      constructorMsg
        <> case typ of
          Left builtinTxt -> "Builtin type: ##" <> P.text builtinTxt
          Right decl -> either (P.text . TL.toStrict . pShowNoColor) (P.text . TL.toStrict . pShowNoColor) decl
  AnnotatedFoldRanges txt -> pure $ P.text txt
  DisplayDebugNameDiff NameChanges {termNameAdds, termNameRemovals, typeNameAdds, typeNameRemovals} -> do
    let referentText =
          -- We don't use the constructor type in the actual output here, so there's no
          -- point in looking up the correct one.
          P.text . Referent.toText . runIdentity . Cv.referent2to1 (\_ref -> Identity CT.Data)
    let referenceText = P.text . Reference.toText . Cv.reference2to1
    pure $
      P.columnNHeader
        ["Kind", "Name", "Change", "Ref"]
        ( (termNameAdds <&> \(n, ref) -> ["Term", prettyName n, "Added", referentText ref])
            <> (termNameRemovals <&> \(n, ref) -> ["Term", prettyName n, "Removed", referentText ref])
            <> (typeNameAdds <&> \(n, ref) -> ["Type", prettyName n, "Added", referenceText ref])
            <> (typeNameRemovals <&> \(n, ref) -> ["Type", prettyName n, "Removed", referenceText ref])
        )
  DisplayDebugCompletions completions ->
    pure $
      P.column2
        ( completions <&> \comp ->
            let isCompleteTxt =
                  if Completion.isFinished comp
                    then "*"
                    else ""
             in (isCompleteTxt, P.string (Completion.replacement comp))
        )
  DisplayDebugLSPNameCompletions completions ->
    pure $
      P.columnNHeader
        ["Matching Path", "Name", "Hash"]
        ( completions <&> \(pathText, fqn, ld) ->
            let ldRef = case ld of
                  LD.TermReferent ref -> prettyReferent 10 ref
                  LD.TypeReference ref -> prettyReference 10 ref
             in [P.text pathText, prettyName fqn, P.syntaxToColor ldRef]
        )
  DebugDisplayFuzzyOptions argDesc fuzzyOptions ->
    pure $
      P.lines
        [P.text (FZFResolvers.fuzzySelectHeader argDesc), P.indentN 2 $ P.bulleted (P.string <$> fuzzyOptions)]
  DebugFuzzyOptionsNoResolver -> pure "No resolver found for fuzzy options in this slot."
  ClearScreen -> do
    ANSI.clearScreen
    ANSI.setCursorPosition 0 0
    pure mempty
  PulledEmptyBranch remote ->
    pure . P.warnCallout . P.wrap $
      P.group (prettyReadRemoteNamespace remote) <> "has some history, but is currently empty."
  CreatedProject nameWasRandomlyGenerated projectName ->
    pure $
      if nameWasRandomlyGenerated
        then
          P.wrap $
            "🎉 I've created the project with the randomly-chosen name"
              <> prettyProjectName projectName
              <> "(use"
              <> IP.makeExample IP.projectRenameInputPattern ["<new-name>"]
              <> "to change it)."
        else
          P.wrap $
            "🎉 I've created the project" <> P.group (prettyProjectName projectName <> ".")
  CreatedProjectBranch from projectAndBranch ->
    case from of
      CreatedProjectBranchFrom'LooseCode path ->
        pure $
          P.wrap
            ( "Done. I've created the"
                <> prettyProjectAndBranchName projectAndBranch
                <> "branch from the namespace"
                <> prettyAbsolute path
            )
            <> "."
      CreatedProjectBranchFrom'Nothingness ->
        pure $
          P.wrap ("Done. I've created an empty branch" <> prettyProjectAndBranchName projectAndBranch)
            <> "."
            <> P.newline
            <> P.newline
            <> tip
              ( "Use"
                  <> IP.makeExample IP.mergeInputPattern [prettySlashProjectBranchName (UnsafeProjectBranchName "somebranch")]
                  <> "to initialize this branch."
              )
      CreatedProjectBranchFrom'OtherBranch (ProjectAndBranch otherProject otherBranch) ->
        pure $
          P.wrap
            ( "Done. I've created the"
                <> prettyProjectAndBranchName projectAndBranch
                <> "branch based off"
                <> prettyProjectAndBranchName (ProjectAndBranch (otherProject ^. #name) (otherBranch ^. #name))
            )
            <> "."
      CreatedProjectBranchFrom'ParentBranch parentBranch ->
        pure $
          P.wrap
            ( "Done. I've created the"
                <> prettyProjectBranchName (projectAndBranch ^. #branch)
                <> "branch based off of"
                <> prettyProjectBranchName parentBranch
            )
            <> "."
            <> P.newline
            <> P.newline
            <> tip
              ( "To merge your work back into the"
                  <> prettyProjectBranchName parentBranch
                  <> "branch, first"
                  <> IP.makeExample IP.projectSwitch [prettySlashProjectBranchName parentBranch]
                  <> "then"
                  <> P.group (IP.makeExample IP.mergeInputPattern [prettySlashProjectBranchName (projectAndBranch ^. #branch)] <> ".")
              )
  CreatedRemoteProject host (ProjectAndBranch projectName _) ->
    pure . P.wrap $
      "I just created"
        <> prettyProjectName projectName
        <> "on"
        <> prettyShareURI host
  CreatedRemoteProjectBranch host projectAndBranch ->
    pure . P.wrap $
      "I just created" <> prettyProjectAndBranchName projectAndBranch <> "on" <> prettyShareURI host
  RemoteProjectBranchIsUpToDate host projectAndBranch ->
    pure $
      P.wrap $
        prettyProjectAndBranchName projectAndBranch
          <> "on"
          <> prettyShareURI host
          <> "is already up-to-date."
  InvalidProjectName name -> pure (P.wrap (P.text name <> "is not a valid project name."))
  InvalidProjectBranchName name -> pure (P.wrap (P.text name <> "is not a valid branch name."))
  ProjectNameAlreadyExists name ->
    pure . P.wrap $
      "Project" <> prettyProjectName name <> "already exists."
  ProjectNameRequiresUserSlug name ->
    pure . P.wrap $
      prettyProjectName name
        <> "requires a username, as in"
        <> prettyProjectName (unsafeFrom @Text "@unison/base")
  ProjectAndBranchNameAlreadyExists projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch
        <> "already exists."
        <> "You can switch to it with "
        <> IP.makeExampleEOS IP.projectSwitch [prettyProjectAndBranchName projectAndBranch]
  NotOnProjectBranch -> pure (P.wrap "You are not currently on a branch.")
  NoAssociatedRemoteProject host projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch <> "isn't associated with any project on" <> prettyShareURI host
  NoAssociatedRemoteProjectBranch host (ProjectAndBranch project branch) ->
    pure . P.wrap $
      prettyProjectAndBranchName (ProjectAndBranch (project ^. #name) (branch ^. #name))
        <> "isn't associated with any branch on"
        <> prettyShareURI host
  LocalProjectDoesntExist project ->
    pure . P.wrap $
      prettyProjectName project <> "does not exist."
  LocalProjectBranchDoesntExist projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch <> "does not exist."
  LocalProjectNorProjectBranchExist project branch ->
    pure . P.wrap $
      "Neither project"
        <> prettyProjectName project
        <> "nor branch"
        <> prettySlashProjectBranchName branch
        <> "exists."
  RemoteProjectDoesntExist host project ->
    pure . P.wrap $
      prettyProjectName project <> "does not exist on" <> prettyShareURI host
  RemoteProjectBranchDoesntExist host projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch <> "does not exist on" <> prettyShareURI host
  RemoteProjectBranchDoesntExist'Push host projectAndBranch ->
    let push = P.group . P.backticked . IP.patternName $ IP.push
     in pure . P.wrap $
          "The previous push target named"
            <> prettyProjectAndBranchName projectAndBranch
            <> "has been deleted from"
            <> P.group (prettyShareURI host <> ".")
            <> "I've deleted the invalid push target."
            <> "Run the"
            <> push
            <> "command again to push to a new target."
  RemoteProjectBranchHeadMismatch host projectAndBranch ->
    pure . P.wrap $
      prettyProjectAndBranchName projectAndBranch
        <> "on"
        <> prettyShareURI host
        <> "has some history that I don't know about."
  RemoteProjectPublishedReleaseCannotBeChanged host projectAndBranch ->
    pure . P.wrap $
      "The release"
        <> prettyProjectAndBranchName projectAndBranch
        <> "on"
        <> prettyShareURI host
        <> "has already been published and cannot be changed."
        <> "Consider making a new release instead."
  RemoteProjectReleaseIsDeprecated host projectAndBranch ->
    pure . P.wrap $
      "The release"
        <> prettyProjectAndBranchName projectAndBranch
        <> "on"
        <> prettyShareURI host
        <> "has been deprecated."
  Unauthorized message ->
    pure . P.wrap $
      P.text ("Unauthorized: " <> message)
  ServantClientError err ->
    pure case err of
      Servant.ConnectionError exception ->
        P.wrap $
          fromMaybe "Something went wrong with the connection. Try again?" do
            case ServantClientUtils.classifyConnectionError exception of
              ServantClientUtils.ConnectionError'Offline -> Just "You appear to be offline."
              ServantClientUtils.ConnectionError'SomethingElse _ -> Nothing
              ServantClientUtils.ConnectionError'SomethingEntirelyUnexpected _ -> Nothing
      Servant.DecodeFailure message response ->
        P.wrap "Huh, I failed to decode a response from the server."
          <> P.newline
          <> P.newline
          <> P.indentN 2 (P.text message)
          <> P.newline
          <> P.newline
          <> P.wrap "Here is the full response."
          <> P.newline
          <> P.newline
          <> P.indentN 2 (P.pshown response)
      Servant.FailureResponse request response ->
        P.wrap "Oops, I received an unexpected status code from the server."
          <> P.newline
          <> P.newline
          <> P.wrap "Here is the request."
          <> P.newline
          <> P.newline
          <> P.indentN 2 (P.pshown request)
          <> P.newline
          <> P.newline
          <> P.wrap "Here is the full response."
          <> P.newline
          <> P.newline
          <> P.indentN 2 (P.pshown response)
      Servant.InvalidContentTypeHeader response -> wrongContentType response
      Servant.UnsupportedContentType _mediaType response -> wrongContentType response
    where
      wrongContentType response =
        P.wrap "Huh, the server sent me the wrong content type."
          <> P.newline
          <> P.newline
          <> P.wrap "Here is the full response."
          <> P.newline
          <> P.newline
          <> P.indentN 2 (P.pshown response)
  MarkdownOut md -> pure $ P.text md
  DownloadedEntities n -> pure (P.wrap ("Downloaded" <> P.num n <> "entities."))
  UploadedEntities n -> pure (P.wrap ("Uploaded" <> P.num n <> "entities."))
  NotImplementedYet message -> pure (P.wrap ("Not implemented:" <> P.text message))
  DraftingRelease branch ver ->
    pure $
      P.wrap ("😎 Great! I've created a draft release for you at " <> prettySlashProjectBranchName branch)
        <> "."
        <> P.newline
        <> P.newline
        <> P.wrap
          ( "You can create a"
              <> P.group (P.backticked "ReleaseNotes : Doc")
              <> "in this branch to give an overview of the release."
              <> "It'll automatically show up on Unison Share when you publish."
          )
        <> P.newline
        <> P.newline
        <> P.wrap
          ( "When ready to release"
              <> prettySemver ver
              <> "to the world,"
              <> IP.makeExample' IP.push
              <> "the release to Unison Share, navigate to the release, and click \"Publish\"."
          )
        <> P.newline
        <> P.newline
        <> tip
          ( "if you get pulled away from drafting your release, you can always get back to it with "
              <> IP.makeExample IP.projectSwitch [prettySlashProjectBranchName branch]
          )
        <> "."
  CannotCreateReleaseBranchWithBranchCommand branch ver ->
    pure $
      P.wrap ("Branch names like" <> prettyProjectBranchName branch <> "are reserved for releases.")
        <> P.newline
        <> P.newline
        <> tip
          ( "to download an existing release, try "
              <> IP.makeExample IP.clone [prettySlashProjectBranchName branch]
          )
        <> "."
        <> P.newline
        <> P.newline
        <> tip ("to draft a new release, try " <> IP.makeExample IP.releaseDraft [prettySemver ver])
        <> "."
  CalculatingDiff -> pure (P.wrap "Calculating diff...")
  AmbiguousCloneLocal project branch -> do
    pure $
      P.wrap
        ( "I'm not sure if you wanted to clone as the branch"
            <> prettyProjectAndBranchName branch
            <> "or as the branch"
            <> P.group (prettyProjectAndBranchName project <> ".")
            <> "Could you be more specific?"
        )
        <> P.newline
        <> P.newline
        <> tip
          ( prettySlashProjectBranchName (branch ^. #branch)
              <> "refers to the branch"
              <> P.group (prettyProjectAndBranchName branch <> ".")
          )
        <> P.newline
        <> P.newline
        <> tip
          ( prettyProjectNameSlash (project ^. #project)
              <> "refers to"
              <> "the branch"
              <> P.group (prettyProjectAndBranchName project <> ".")
          )
  AmbiguousCloneRemote project (ProjectAndBranch currentProject branch) ->
    pure $
      P.wrap
        ( "I'm not sure if you wanted to clone the branch"
            <> prettyProjectAndBranchName (ProjectAndBranch currentProject branch)
            <> "or the project"
            <> P.group (prettyProjectName project <> ".")
            <> "Could you be more specific?"
        )
        <> P.newline
        <> P.newline
        <> tip
          ( prettySlashProjectBranchName branch
              <> "refers to the branch"
              <> P.group (prettyProjectAndBranchName (ProjectAndBranch currentProject branch) <> ".")
          )
        <> P.newline
        <> P.newline
        <> tip (prettyProjectNameSlash project <> "refers to the project" <> P.group (prettyProjectName project <> "."))
  ClonedProjectBranch remote local ->
    pure . P.wrap $
      "Cloned"
        <> if remote == local
          then P.group (prettyProjectAndBranchName remote <> ".")
          else prettyProjectAndBranchName remote <> "as" <> P.group (prettyProjectAndBranchName local <> ".")
  RenamedProject oldName newName ->
    pure . P.wrap $
      if oldName == newName
        then prettyProjectName oldName <> "is already named" <> P.group (prettyProjectName oldName <> "!") <> "😄"
        else "Ok, I renamed" <> prettyProjectName oldName <> "to" <> P.group (prettyProjectName newName <> ".")
  RenamedProjectBranch projectName oldBranchName newBranchName ->
    let oldProjectAndBranchName = prettyProjectAndBranchName (ProjectAndBranch projectName oldBranchName)
        newProjectAndBranchName = prettyProjectAndBranchName (ProjectAndBranch projectName newBranchName)
     in pure . P.wrap $
          if oldBranchName == newBranchName
            then oldProjectAndBranchName <> "is already named" <> P.group (newProjectAndBranchName <> "!") <> "😄"
            else "Ok, I renamed" <> oldProjectAndBranchName <> "to" <> P.group (newProjectAndBranchName <> ".")
  CantRenameBranchTo branch ->
    pure . P.wrap $
      "You can't rename a branch to" <> P.group (prettyProjectBranchName branch <> ".")
  FetchingLatestReleaseOfBase ->
    pure . P.wrap $
      "I'll now fetch the latest version of the base Unison library..."
  FailedToFetchLatestReleaseOfBase ->
    pure . P.wrap $ "Sorry something went wrong while fetching the library."
  HappyCoding -> do
    pure $
      P.wrap "🎨 Type `ui` to explore this project's code in your browser."
        <> P.newline
        <> P.wrap ("🔭 Discover libraries at https://share.unison-lang.org")
        <> P.newline
        <> P.wrap "📖 Use `help-topic projects` to learn more about projects."
        <> P.newline
        <> P.newline
        <> P.wrap "Write your first Unison code with UCM:"
        <> P.newline
        <> P.newline
        <> P.indentN
          2
          ( P.wrap "1. Open scratch.u."
              <> P.newline
              <> P.wrap "2. Write some Unison code and save the file."
              <> P.newline
              <> P.wrap "3. In UCM, type `add` to save it to your new project."
          )
        <> P.newline
        <> P.newline
        <> P.wrap "🎉 🥳 Happy coding!"
  ProjectHasNoReleases projectName ->
    pure . P.wrap $ prettyProjectName projectName <> "has no releases."
  UpdateLookingForDependents -> pure . P.wrap $ "Okay, I'm searching the branch for code that needs to be updated..."
  UpdateStartTypechecking -> pure . P.wrap $ "That's done. Now I'm making sure everything typechecks..."
  UpdateTypecheckingSuccess -> pure . P.wrap $ "Everything typechecks, so I'm saving the results..."
  UpdateTypecheckingFailure ->
    pure . P.wrap $
      "Typechecking failed. I've updated your scratch file with the definitions that need fixing."
        <> P.newline
        <> P.newline
        <> "Once the file is compiling, try"
        <> makeExample' IP.update
        <> "again."
  UpdateIncompleteConstructorSet operation typeName _ctorMap _expectedCount ->
    let operationName = case operation of E.UOUUpdate -> "update"; E.UOUUpgrade -> "upgrade"
     in pure $
          P.lines
            [ P.wrap $
                "I couldn't complete the"
                  <> operationName
                  <> "because the type"
                  <> prettyName typeName
                  <> "has unnamed constructors."
                  <> "(I currently need each constructor to have a name somewhere under the type name.)",
              "",
              P.wrap $
                "You can use"
                  <> P.indentNAfterNewline 2 (IP.makeExample IP.view [prettyName typeName])
                  <> "and"
                  <> P.indentNAfterNewline 2 (IP.makeExample IP.aliasTerm ["<hash>", prettyName typeName <> ".<ConstructorName>"])
                  <> "to give names to each constructor, and then try the"
                  <> operationName
                  <> "again."
            ]
  UpgradeFailure main temp path old new ->
    pure $
      P.lines
        [ P.wrap $
            "I couldn't automatically upgrade"
              <> P.text (NameSegment.toEscapedText old)
              <> "to"
              <> P.group (P.text (NameSegment.toEscapedText new) <> ".")
              <> "However, I've added the definitions that need attention to the top of"
              <> P.group (prettyFilePath path <> "."),
          "",
          P.wrap "When you're done, you can run",
          "",
          P.indentN 2 (IP.makeExampleNoBackticks IP.upgradeCommitInputPattern []),
          "",
          P.wrap $
            "to merge your changes back into"
              <> prettyProjectBranchName main
              <> "and delete the temporary branch. Or, if you decide to cancel the upgrade instead, you can run",
          "",
          P.indentN 2 (IP.makeExampleNoBackticks IP.deleteBranch [prettySlashProjectBranchName temp]),
          "",
          P.wrap $
            "to delete the temporary branch and switch back to"
              <> P.group (prettyProjectBranchName main <> ".")
        ]
  UpgradeSuccess old new ->
    pure . P.wrap $
      "I upgraded"
        <> P.text (NameSegment.toEscapedText old)
        <> "to"
        <> P.group (P.text (NameSegment.toEscapedText new) <> ",")
        <> "and removed"
        <> P.group (P.text (NameSegment.toEscapedText old) <> ".")
  MergeFailure path aliceAndBob temp ->
    pure $
      P.lines $
        [ P.wrap $
            "I couldn't automatically merge"
              <> prettyMergeSource aliceAndBob.bob
              <> "into"
              <> P.group (prettyProjectAndBranchName aliceAndBob.alice <> ".")
              <> "However, I've added the definitions that need attention to the top of"
              <> P.group (prettyFilePath path <> "."),
          "",
          P.wrap "When you're done, you can run",
          "",
          P.indentN 2 (IP.makeExampleNoBackticks IP.mergeCommitInputPattern []),
          "",
          P.wrap $
            "to merge your changes back into"
              <> prettyProjectBranchName aliceAndBob.alice.branch
              <> "and delete the temporary branch. Or, if you decide to cancel the merge instead, you can run",
          "",
          P.indentN 2 (IP.makeExampleNoBackticks IP.deleteBranch [prettySlashProjectBranchName temp]),
          "",
          P.wrap $
            "to delete the temporary branch and switch back to"
              <> P.group (prettyProjectBranchName aliceAndBob.alice.branch <> ".")
        ]
  MergeFailureWithMergetool aliceAndBob temp mergetool exitCode ->
    case exitCode of
      ExitSuccess ->
        pure $
          P.lines $
            [ P.wrap $
                "I couldn't automatically merge"
                  <> prettyMergeSource aliceAndBob.bob
                  <> "into"
                  <> P.group (prettyProjectAndBranchName aliceAndBob.alice <> ",")
                  <> "so I'm running your UCM_MERGETOOL environment variable as",
              "",
              P.indentN 2 (P.text mergetool),
              "",
              P.wrap "When you're done, you can run",
              "",
              P.indentN 2 (IP.makeExampleNoBackticks IP.mergeCommitInputPattern []),
              "",
              P.wrap $
                "to merge your changes back into"
                  <> prettyProjectBranchName aliceAndBob.alice.branch
                  <> "and delete the temporary branch. Or, if you decide to cancel the merge instead, you can run",
              "",
              P.indentN 2 (IP.makeExampleNoBackticks IP.deleteBranch [prettySlashProjectBranchName temp]),
              "",
              P.wrap $
                "to delete the temporary branch and switch back to"
                  <> P.group (prettyProjectBranchName aliceAndBob.alice.branch <> ".")
            ]
      ExitFailure code ->
        pure $
          P.lines $
            [ P.wrap $
                "I couldn't automatically merge"
                  <> prettyMergeSource aliceAndBob.bob
                  <> "into"
                  <> P.group (prettyProjectAndBranchName aliceAndBob.alice <> ",")
                  <> "so I tried to run your UCM_MERGETOOL environment variable as",
              "",
              P.indentN 2 (P.text mergetool),
              "",
              P.wrap ("but it failed with exit code" <> P.group (P.num code <> "."))
            ]
  MergeSuccess aliceAndBob ->
    pure . P.wrap $
      "I merged"
        <> prettyMergeSource aliceAndBob.bob
        <> "into"
        <> P.group (prettyProjectAndBranchName aliceAndBob.alice <> ".")
  MergeSuccessFastForward aliceAndBob ->
    pure . P.wrap $
      "I fast-forward merged"
        <> prettyMergeSource aliceAndBob.bob
        <> "into"
        <> P.group (prettyProjectAndBranchName aliceAndBob.alice <> ".")
  InstalledLibdep libdep segment ->
    pure . P.wrap $
      "I installed"
        <> prettyProjectAndBranchName libdep
        <> "as"
        <> P.group (P.text (NameSegment.toEscapedText segment) <> ".")
  NoUpgradeInProgress ->
    pure . P.wrap $ "It doesn't look like there's an upgrade in progress."
  UseLibInstallNotPull libdep ->
    pure . P.wrap $
      "The use of"
        <> IP.makeExample' IP.pull
        <> "to install libraries is now deprecated. Going forward, you can use"
        <> P.group (IP.makeExample IP.libInstallInputPattern [prettyProjectAndBranchName libdep] <> ".")
  PullIntoMissingBranch source (ProjectAndBranch maybeTargetProject targetBranch) ->
    pure . P.wrap $
      "I think you want to merge"
        <> sourcePretty
        <> "into the"
        <> targetPretty
        <> "branch, but it doesn't exist. If you want, you can create it with"
        <> P.group (IP.makeExample IP.branchEmptyInputPattern [targetPretty] <> ",")
        <> "and then"
        <> IP.makeExample' IP.pull
        <> "again."
    where
      sourcePretty = prettyReadRemoteNamespace source
      targetPretty =
        case maybeTargetProject of
          Nothing -> prettyProjectBranchName targetBranch
          Just targetProject -> prettyProjectAndBranchName (ProjectAndBranch targetProject targetBranch)
  NoMergeInProgress ->
    pure . P.wrap $ "It doesn't look like there's a merge in progress."
  Output'DebugSynhashTerm ref synhash filename ->
    pure $
      "Hash: "
        <> P.syntaxToColor (prettyReference 120 ref)
        <> P.newline
        <> "Synhash: "
        <> prettyHash synhash
        <> P.newline
        <> "Synhash tokens: "
        <> P.text filename
  ConflictedDefn operation defn ->
    pure . P.wrap $
      ( "This branch has more than one" <> case defn of
          TermDefn (Conflicted name _refs) -> "term with the name" <> P.group (P.backticked (prettyName name) <> ".")
          TypeDefn (Conflicted name _refs) -> "type with the name" <> P.group (P.backticked (prettyName name) <> ".")
      )
        <> P.newline
        <> "Please delete or rename all but one of them, then try the"
        <> P.text operation
        <> "again."
  IncoherentDeclDuringMerge aliceOrBob reason ->
    case reason of
      -- Note [ConstructorAliasMessage] If you change this, also change the other similar ones
      IncoherentDeclReason'ConstructorAlias typeName conName1 conName2 ->
        pure . P.lines $
          [ P.wrap "Sorry, I wasn't able to perform the merge:",
            "",
            P.wrap $
              "On"
                <> P.group (prettyMergeSourceOrTarget aliceOrBob <> ",")
                <> "the type"
                <> prettyName typeName
                <> "has a constructor with multiple names, and I can't perform a merge in this situation:",
            "",
            P.indentN 2 (P.bulleted [prettyName conName1, prettyName conName2]),
            "",
            P.wrap "Please delete all but one name for each constructor, and then try merging again."
          ]
      -- Note [MissingConstructorNameMessage] If you change this, also change the other similar ones
      IncoherentDeclReason'MissingConstructorName name ->
        pure . P.lines $
          [ P.wrap "Sorry, I wasn't able to perform the merge:",
            "",
            P.wrap $
              "On"
                <> P.group (prettyMergeSourceOrTarget aliceOrBob <> ",")
                <> "the type"
                <> prettyName name
                <> "has some constructors with missing names, and I can't perform a merge in this situation.",
            "",
            P.wrap $
              "You can use"
                <> IP.makeExample IP.view [prettyName name]
                <> "and"
                <> IP.makeExample IP.aliasTerm ["<hash>", prettyName name <> ".<ConstructorName>"]
                <> "to give names to each unnamed constructor, and then try the merge again."
          ]
      -- Note [NestedDeclAliasMessage] If you change this, also change the other similar ones
      IncoherentDeclReason'NestedDeclAlias shorterName longerName ->
        pure . P.wrap $
          "On"
            <> P.group (prettyMergeSourceOrTarget aliceOrBob <> ",")
            <> "the type"
            <> prettyName longerName
            <> "is an alias of"
            <> P.group (prettyName shorterName <> ".")
            <> "I'm not able to perform a merge when a type exists nested under an alias of itself. Please separate them or"
            <> "delete one copy, and then try merging again."
      -- Note [StrayConstructorMessage] If you change this, also change the other similar ones
      IncoherentDeclReason'StrayConstructor _typeRef name ->
        pure . P.lines $
          [ P.wrap $
              "Sorry, I wasn't able to perform the merge, because I need all constructor names to be nested somewhere"
                <> "beneath the corresponding type name.",
            "",
            P.wrap $
              "On"
                <> P.group (prettyMergeSourceOrTarget aliceOrBob <> ",")
                <> "the constructor"
                <> prettyName name
                <> "is not nested beneath the corresponding type name. Please either use"
                <> IP.makeExample' IP.moveAll
                <> "to move it, or if it's an extra copy, you can simply"
                <> IP.makeExample' IP.delete
                <> "it. Then try the merge again."
          ]
  IncoherentDeclDuringUpdate reason ->
    case reason of
      -- Note [ConstructorAliasMessage] If you change this, also change the other similar ones
      IncoherentDeclReason'ConstructorAlias typeName conName1 conName2 ->
        pure . P.lines $
          [ P.wrap "Sorry, I wasn't able to perform the update:",
            "",
            P.wrap $
              "The type"
                <> prettyName typeName
                <> "has a constructor with multiple names, and I can't perform an update in this situation:",
            "",
            P.indentN 2 (P.bulleted [prettyName conName1, prettyName conName2]),
            "",
            P.wrap "Please delete all but one name for each constructor, and then try updating again."
          ]
      -- Note [MissingConstructorNameMessage] If you change this, also change the other similar ones
      IncoherentDeclReason'MissingConstructorName name ->
        pure . P.lines $
          [ P.wrap "Sorry, I wasn't able to perform the update:",
            "",
            P.wrap $
              "The type"
                <> prettyName name
                <> "has some constructors with missing names, and I can't perform an update in this situation.",
            "",
            P.wrap $
              "You can use"
                <> IP.makeExample IP.view [prettyName name]
                <> "and"
                <> IP.makeExample IP.aliasTerm ["<hash>", prettyName name <> ".<ConstructorName>"]
                <> "to give names to each unnamed constructor, and then try the update again."
          ]
      -- Note [NestedDeclAliasMessage] If you change this, also change the other similar ones
      IncoherentDeclReason'NestedDeclAlias shorterName longerName ->
        pure . P.wrap $
          "The type"
            <> prettyName longerName
            <> "is an alias of"
            <> P.group (prettyName shorterName <> ".")
            <> "I'm not able to perform an update when a type exists nested under an alias of itself. Please separate"
            <> "them or delete one copy, and then try updating again."
      -- Note [StrayConstructorMessage] If you change this, also change the other similar ones
      IncoherentDeclReason'StrayConstructor _typeRef name ->
        pure . P.lines $
          [ P.wrap $
              "Sorry, I wasn't able to perform the update, because I need all constructor names to be nested somewhere"
                <> "beneath the corresponding type name.",
            "",
            P.wrap $
              "The constructor"
                <> prettyName name
                <> "is not nested beneath the corresponding type name. Please either use"
                <> IP.makeExample' IP.moveAll
                <> "to move it, or if it's an extra copy, you can simply"
                <> IP.makeExample' IP.delete
                <> "it. Then try the update again."
          ]

prettyShareError :: ShareError -> Pretty
prettyShareError =
  P.fatalCallout . \case
    ShareErrorDownloadEntities err -> prettyDownloadEntitiesError err
    ShareErrorGetCausalHashByPath err -> prettyGetCausalHashByPathError err
    ShareErrorPull err -> prettyPullError err
    ShareErrorTransport err -> prettyTransportError err
    ShareErrorUploadEntities err -> prettyUploadEntitiesError err
    ShareExpectedSquashedHead -> "The server failed to provide a squashed branch head when requested. Please report this as a bug to the Unison team."

prettyDownloadEntitiesError :: Share.DownloadEntitiesError -> Pretty
prettyDownloadEntitiesError = \case
  Share.DownloadEntitiesNoReadPermission repoInfo -> noReadPermissionForRepo repoInfo
  Share.DownloadEntitiesInvalidRepoInfo err repoInfo -> invalidRepoInfo err repoInfo
  Share.DownloadEntitiesUserNotFound userHandle -> shareUserNotFound (Share.RepoInfo userHandle)
  Share.DownloadEntitiesProjectNotFound project -> shareProjectNotFound project
  Share.DownloadEntitiesEntityValidationFailure err -> prettyEntityValidationFailure err

prettyGetCausalHashByPathError :: Share.GetCausalHashByPathError -> Pretty
prettyGetCausalHashByPathError = \case
  Share.GetCausalHashByPathErrorNoReadPermission sharePath -> noReadPermissionForPath sharePath
  Share.GetCausalHashByPathErrorInvalidRepoInfo err repoInfo -> invalidRepoInfo err repoInfo
  Share.GetCausalHashByPathErrorUserNotFound repoInfo -> shareUserNotFound repoInfo

prettyPullError :: Share.PullError -> Pretty
prettyPullError = \case
  Share.PullError'DownloadEntities err -> prettyDownloadEntitiesError err
  Share.PullError'GetCausalHash err -> prettyGetCausalHashByPathError err
  Share.PullError'NoHistoryAtPath sharePath ->
    P.wrap $ P.text "The server didn't find anything at" <> prettySharePath sharePath

prettyUploadEntitiesError :: Share.UploadEntitiesError -> Pretty
prettyUploadEntitiesError = \case
  Share.UploadEntitiesError'EntityValidationFailure validationFailureErr -> prettyEntityValidationFailure validationFailureErr
  Share.UploadEntitiesError'HashMismatchForEntity (Share.HashMismatchForEntity {supplied, computed}) ->
    hashMismatchFromShare supplied computed
  Share.UploadEntitiesError'InvalidRepoInfo err repoInfo -> invalidRepoInfo err repoInfo
  Share.UploadEntitiesError'NeedDependencies dependencies -> needDependencies dependencies
  Share.UploadEntitiesError'NoWritePermission repoInfo -> noWritePermissionForRepo repoInfo
  Share.UploadEntitiesError'ProjectNotFound project -> shareProjectNotFound project
  Share.UploadEntitiesError'UserNotFound userHandle -> shareUserNotFound (Share.RepoInfo userHandle)

prettyEntityValidationFailure :: Share.EntityValidationError -> Pretty
prettyEntityValidationFailure = \case
  Share.EntityHashMismatch entityType (Share.HashMismatchForEntity {supplied, computed}) ->
    P.lines
      [ P.wrap $ "The hash associated with the given " <> prettyEntityType entityType <> " entity is incorrect.",
        "",
        P.wrap $ "The associated hash is: " <> prettyHash32 supplied,
        P.wrap $ "The computed hash is: " <> prettyHash32 computed,
        "",
        "Please create an issue and report this to the Unison team."
      ]
  Share.UnsupportedEntityType hash32 entityType ->
    P.lines
      [ P.wrap $ "The entity with hash " <> prettyHash32 hash32 <> " of type " <> prettyEntityType entityType <> " is not supported by your version of ucm.",
        P.wrap $ "Try upgrading to the latest version of ucm."
      ]
  Share.InvalidByteEncoding hash32 entityType msg ->
    P.lines
      [ P.wrap $ "Failed to decode a " <> prettyEntityType entityType <> " entity with the hash " <> prettyHash32 hash32 <> ".",
        "Please create an issue and report this to the Unison team",
        "",
        P.wrap $ "The error was: " <> P.text msg
      ]
  Share.HashResolutionFailure hash32 ->
    -- See https://github.com/unisonweb/unison/pull/4381#discussion_r1452652087 for discussion.
    P.lines
      [ P.wrap $ "Failed to resolve a referenced hash when validating the hash for " <> prettyHash32 hash32 <> ".",
        "Please create an issue and report this to the Unison team"
      ]
  where
    prettyEntityType = \case
      Share.TermComponentType -> "term component"
      Share.DeclComponentType -> "type component"
      Share.PatchType -> "patch"
      Share.PatchDiffType -> "patch diff"
      Share.NamespaceType -> "namespace"
      Share.NamespaceDiffType -> "namespace diff"
      Share.CausalType -> "causal"

prettyTransportError :: CodeserverTransportError -> Pretty
prettyTransportError = \case
  DecodeFailure msg resp ->
    (P.lines . catMaybes)
      [ Just ("The server sent a response that we couldn't decode: " <> P.text msg),
        responseRequestId resp <&> \responseId -> P.newline <> "Request ID: " <> P.blue (P.text responseId)
      ]
  Unauthenticated codeServerURL ->
    P.wrap . P.lines $
      [ "Authentication with this code server (" <> P.string (Servant.showBaseUrl codeServerURL) <> ") is missing or expired.",
        "Please run " <> makeExample' IP.authLogin <> "."
      ]
  PermissionDenied msg -> P.hang "Permission denied:" (P.text msg)
  UnreachableCodeserver codeServerURL ->
    P.lines $
      [ P.wrap $ "Unable to reach the code server hosted at:" <> P.string (Servant.showBaseUrl codeServerURL),
        "",
        P.wrap "Please check your network, ensure you've provided the correct location, or try again later."
      ]
  RateLimitExceeded -> "Rate limit exceeded, please try again later."
  Timeout -> "The code server timed-out when responding to your request. Please try again later or report an issue if the problem persists."
  UnexpectedResponse resp ->
    (P.lines . catMaybes)
      [ Just
          ( "The server sent a "
              <> P.red (P.shown (Http.statusCode (Servant.responseStatusCode resp)))
              <> " that we didn't expect."
          ),
        let body = Text.decodeUtf8 (LazyByteString.toStrict (Servant.responseBody resp))
         in if Text.null body then Nothing else Just (P.newline <> "Response body: " <> P.text body),
        responseRequestId resp <&> \responseId -> P.newline <> "Request ID: " <> P.blue (P.text responseId)
      ]
  where
    -- Dig the request id out of a response header.
    responseRequestId :: Servant.Response -> Maybe Text
    responseRequestId =
      fmap Text.decodeUtf8 . List.lookup "X-RequestId" . Foldable.toList @Seq . Servant.responseHeaders

prettyEntityType :: Share.EntityType -> Pretty
prettyEntityType = \case
  Share.TermComponentType -> "term component"
  Share.DeclComponentType -> "type component"
  Share.PatchType -> "patch"
  Share.PatchDiffType -> "patch diff"
  Share.NamespaceType -> "namespace"
  Share.NamespaceDiffType -> "namespace diff"
  Share.CausalType -> "causal"

invalidRepoInfo :: Text -> Share.RepoInfo -> Pretty
invalidRepoInfo err repoInfo =
  P.lines
    [ P.wrap $
        "The server doesn't recognize the codebase path UCM provided. This is probably a bug in UCM.",
      P.text "",
      P.text "The invalid path is:\n"
        <> P.indentN 2 (P.text (Share.unRepoInfo repoInfo)),
      P.text err
    ]

hashMismatchFromShare :: Hash32 -> Hash32 -> Pretty
hashMismatchFromShare supplied computed =
  P.lines
    [ P.wrap "Uh oh, Share double-checked the hash of something you're uploading and it didn't match.",
      P.wrap "Don't worry, you didn't do anything wrong, this is a bug in UCM, please report it and we'll do our best to sort it out 🤞",
      reportBugURL,
      "",
      "Please include the following information in your report:",
      P.wrap $ "The hash provided by your UCM is: " <> prettyHash32 supplied,
      P.wrap $ "The hash computed by Share is: " <> prettyHash32 computed
    ]

pushPublicNote :: InputPattern -> Text -> [Text] -> Pretty
pushPublicNote cmd uname ys =
  let msg =
        mconcat
          [ "Unison Share currently only supports sharing public code. ",
            "This is done by hosting code in a public namespace under your handle.",
            "It looks like you were trying to push directly to the" <> P.backticked (P.text uname),
            "handle. Try nesting under `public` like so: "
          ]
      pushCommand = IP.makeExampleNoBackticks cmd [prettySharePath exPath]
      exPath = Share.Path (uname NEList.:| "public" : ys)
   in P.lines
        [ P.wrap msg,
          "",
          P.indentN 4 pushCommand
        ]

needDependencies :: Share.NeedDependencies Hash32 -> Pretty
needDependencies (Share.NeedDependencies hashes) =
  -- maybe todo: stuff in all the args to CheckAndSetPush
  P.lines
    [ P.wrap
        ( P.text "The server was expecting to have received some stuff from UCM during that last command, but claims to have not received it."
            <> P.text "(This is probably a bug in UCM.)"
        ),
      P.text "",
      P.text "The hashes it expected are:\n"
        <> P.indentN 2 (P.lines (map prettyHash32 (toList hashes)))
    ]

noReadPermissionForPath :: Share.Path -> Pretty
noReadPermissionForPath sharePath =
  P.wrap $ P.text "The server said you don't have permission to read" <> P.group (prettySharePath sharePath <> ".")

noReadPermissionForRepo :: Share.RepoInfo -> Pretty
noReadPermissionForRepo repoInfo =
  P.wrap $ P.text "The server said you don't have permission to read" <> P.group (prettyRepoInfo repoInfo <> ".")

noWritePermissionForPath :: Share.Path -> Pretty
noWritePermissionForPath sharePath =
  case Share.pathSegments sharePath of
    _ NEList.:| "public" : _ -> P.wrap $ P.text "The server said you don't have permission to write" <> P.group (prettySharePath sharePath <> ".")
    uname NEList.:| ys -> pushPublicNote IP.pushCreate uname ys

noWritePermissionForRepo :: Share.RepoInfo -> Pretty
noWritePermissionForRepo repoInfo =
  P.wrap $ P.text "The server said you don't have permission to write" <> P.group (prettyRepoInfo repoInfo <> ".")

notFastForward :: Share.Path -> Pretty
notFastForward path =
  P.lines $
    [ P.wrap $
        "There are some changes at" <> prettySharePath path <> "that aren't in the history you pushed.",
      "",
      P.wrap $
        "If you're sure you got the right paths, try"
          <> pull
          <> "to merge these changes locally, then"
          <> push
          <> "again."
    ]
  where
    push = P.group . P.backticked . IP.patternName $ IP.push
    pull = P.group . P.backticked . IP.patternName $ IP.pull

shareProjectNotFound :: Text -> Pretty
shareProjectNotFound projectShortHand =
  P.lines
    [ P.wrap $
        "This project does not exist: " <> P.text projectShortHand
    ]

shareUserNotFound :: Share.RepoInfo -> Pretty
shareUserNotFound repoInfo =
  P.wrap ("User" <> prettyRepoInfo repoInfo <> "does not exist.")

formatMissingStuff ::
  (Show tm, Show typ) =>
  [(HQ.HashQualified Name, tm)] ->
  [(HQ.HashQualified Name, typ)] ->
  Pretty
formatMissingStuff terms types =
  ( Monoid.unlessM (null terms) . P.fatalCallout $
      P.wrap "The following terms have a missing or corrupted type signature:"
        <> "\n\n"
        <> P.column2 [(P.syntaxToColor $ prettyHashQualified name, fromString (show ref)) | (name, ref) <- terms]
  )
    <> ( Monoid.unlessM (null types) . P.fatalCallout $
           P.wrap "The following types weren't found in the codebase:"
             <> "\n\n"
             <> P.column2 [(P.syntaxToColor $ prettyHashQualified name, fromString (show ref)) | (name, ref) <- types]
       )

displayOutputRewrittenFile :: (Var v) => FilePath -> [v] -> IO Pretty
displayOutputRewrittenFile _fp [] =
  pure "😶️ I couldn't find any matches in the file."
displayOutputRewrittenFile fp vs = do
  let modifiedDefs = P.sep " " (P.blue . prettyVar <$> vs)
  pure $
    P.callout "☝️" . P.lines $
      [ P.wrap $ "I found and replaced matches in these definitions: " <> modifiedDefs,
        "",
        "The rewritten file has been added to the top of " <> fromString fp
      ]

displayDefinitions' ::
  (Var v) =>
  (Ord a1) =>
  PPED.PrettyPrintEnvDecl ->
  Map Reference.Reference (DisplayObject () (DD.Decl v a1)) ->
  Map Reference.Reference (DisplayObject (Type v a1) (Term v a1)) ->
  Pretty
displayDefinitions' ppe0 types terms = P.syntaxToColor $ P.sep "\n\n" (prettyTypes <> prettyTerms)
  where
    ppeBody r = PPE.declarationPPE ppe0 r
    ppeDecl = PPED.unsuffixifiedPPE ppe0
    prettyTerms =
      map go . Map.toList
      -- sort by name
      $
        Map.mapKeys (first (PPE.termName ppeDecl . Referent.Ref) . dupe) terms
    prettyTypes =
      map go2 . Map.toList $
        Map.mapKeys (first (PPE.typeName ppeDecl) . dupe) types
    go ((n, r), dt) =
      case dt of
        MissingObject r -> missing n r
        BuiltinObject typ ->
          P.hang
            ("builtin " <> prettyHashQualified n <> " :")
            (TypePrinter.prettySyntax (ppeBody r) typ)
        UserObject tm -> TermPrinter.prettyBinding (ppeBody r) n tm
    go2 ((n, r), dt) =
      case dt of
        MissingObject r -> missing n r
        BuiltinObject _ -> builtin n
        UserObject decl -> DeclPrinter.prettyDecl (PPE.declarationPPEDecl ppe0 r) r n decl
    builtin n = P.wrap $ "--" <> prettyHashQualified n <> " is built-in."
    missing n r =
      P.wrap
        ( "-- The name "
            <> prettyHashQualified n
            <> " is assigned to the "
            <> "reference "
            <> fromString (show r ++ ",")
            <> "which is missing from the codebase."
        )
        <> P.newline
        <> tip "You might need to repair the codebase manually."

displayRendered :: Maybe FilePath -> Pretty -> IO Pretty
displayRendered outputLoc pp =
  pure $ maybe pp scratchMessage outputLoc
  where
    scratchMessage path =
      P.callout "☝️" $
        P.lines
          [ P.wrap $ "I added this to the top of " <> fromString path,
            "",
            P.indentN 2 pp
          ]

displayTestResults ::
  Bool -> -- whether to show the tip
  [(HQ.HashQualified Name, [Text])] ->
  [(HQ.HashQualified Name, [Text])] ->
  Pretty
displayTestResults showTip oks fails =
  let name = P.text . HQ.toText
      okMsg =
        if null oks
          then mempty
          else
            P.indentN 2 $
              P.numberedColumn2ListFrom 0 [(name r, P.lines $ P.green . ("  ◉ " <>) . P.text <$> msgs) | (r, msgs) <- oks]
      okSummary =
        if null oks
          then mempty
          else "✅ " <> P.bold (P.num (sum $ fmap (length . snd) oks)) <> P.green " test(s) passing"
      failMsg =
        if null fails
          then mempty
          else
            P.indentN 2 $
              P.numberedColumn2ListFrom
                (length oks)
                [(name r, P.lines $ P.red . ("  ✗ " <>) . P.text <$> msgs) | (r, msgs) <- fails]
      failSummary =
        if null fails
          then mempty
          else "🚫 " <> P.bold (P.num (sum $ fmap (length . snd) fails)) <> P.red " test(s) failing"
      tipMsg =
        if not showTip || (null oks && null fails)
          then mempty
          else tip $ "Use " <> P.blue "view 1" <> "to view the source of a test."
   in if null oks && null fails
        then "😶 No tests available."
        else
          P.sep "\n\n" . P.nonEmpty $
            [ okMsg,
              failMsg,
              P.sep ", " . P.nonEmpty $ [failSummary, okSummary],
              tipMsg
            ]

unsafePrettyTermResultSig' ::
  (Var v) =>
  PPE.PrettyPrintEnv ->
  SR'.TermResult' v a ->
  Pretty
unsafePrettyTermResultSig' ppe = \case
  SR'.TermResult' name (Just typ) r _aliases ->
    head (TypePrinter.prettySignaturesCT ppe [(r, name, typ)])
  _ -> error "Don't pass Nothing"

renderNameConflicts :: Int -> Names -> Numbered Pretty
renderNameConflicts hashLen conflictedNames = do
  let conflictedTypeNames :: Map Name [HQ.HashQualified Name]
      conflictedTypeNames =
        conflictedNames
          & Names.types
          & R.domain
          & Map.mapWithKey \name -> map (HQ.take hashLen . HQ.HashQualified name . Reference.toShortHash) . Set.toList
  let conflictedTermNames :: Map Name [HQ.HashQualified Name]
      conflictedTermNames =
        conflictedNames
          & Names.terms
          & R.domain
          & Map.mapWithKey \name -> map (HQ.take hashLen . HQ.HashQualified name . Referent.toShortHash) . Set.toList
  let allConflictedNames :: [Name]
      allConflictedNames = Set.toList (Map.keysSet conflictedTermNames <> Map.keysSet conflictedTypeNames)
  prettyConflictedTypes <- showConflictedNames "type" conflictedTypeNames
  prettyConflictedTerms <- showConflictedNames "term" conflictedTermNames
  pure $
    Monoid.unlessM (null allConflictedNames) $
      P.callout "❓" . P.linesSpaced . P.nonEmpty $
        [ prettyConflictedTypes,
          prettyConflictedTerms,
          tip $
            "Use "
              <> makeExample'
                ( if (not . null) conflictedTypeNames
                    then IP.renameType
                    else IP.renameTerm
                )
              <> " or "
              <> makeExample'
                ( if (not . null) conflictedTypeNames
                    then IP.deleteType
                    else IP.deleteTerm
                )
              <> "to resolve the conflicts."
        ]
  where
    showConflictedNames :: Pretty -> Map Name [HQ.HashQualified Name] -> Numbered Pretty
    showConflictedNames thingKind conflictedNames =
      P.linesSpaced <$> do
        for (Map.toList conflictedNames) \(name, hashes) -> do
          prettyConflicts <- for hashes \hash -> do
            n <- addNumberedArg $ SA.HashQualified hash
            pure $ formatNum n <> (P.blue . P.syntaxToColor . prettyHashQualified $ hash)
          pure $
            P.wrap
              ( "The "
                  <> thingKind
                  <> " "
                  <> P.green (prettyName name)
                  <> " has conflicting definitions:"
              )
              <> P.newline
              <> P.newline
              <> P.indentN 2 (P.lines prettyConflicts)

type Numbered = State.State (Int, Seq.Seq StructuredArgument)

addNumberedArg :: StructuredArgument -> Numbered Int
addNumberedArg s = do
  (n, args) <- State.get
  let !n' = n + 1
  State.put (n', args Seq.|> s)
  pure n'

formatNum :: Int -> Pretty
formatNum n = P.string (show n <> ". ")

runNumbered :: Numbered a -> (a, NumberedArgs)
runNumbered m =
  let (a, (_, args)) = State.runState m (0, mempty)
   in (a, Foldable.toList args)

handleTodoOutput :: TodoOutput -> Numbered Pretty
handleTodoOutput todo
  | todoOutputIsEmpty todo = pure "You have no pending todo items. Good work! ✅"
  | otherwise = do
      prettyDependentsOfTodo <- do
        if Set.null todo.dependentsOfTodo
          then pure mempty
          else do
            terms <-
              for (Set.toList todo.dependentsOfTodo) \term -> do
                n <- addNumberedArg (SA.HashQualified (HQ.HashOnly (Reference.idToShortHash term)))
                let name =
                      term
                        & Referent.fromTermReferenceId
                        & PPE.termName todo.ppe.suffixifiedPPE
                        & prettyHashQualified
                        & P.syntaxToColor
                pure (formatNum n <> name)
            pure $
              P.wrap "These terms call `todo`:"
                <> P.newline
                <> P.newline
                <> P.indentN 2 (P.lines terms)

      prettyDirectTermDependenciesWithoutNames <- do
        if Set.null todo.directDependenciesWithoutNames.terms
          then pure mempty
          else do
            terms <-
              for (Set.toList todo.directDependenciesWithoutNames.terms) \term -> do
                n <- addNumberedArg (SA.HashQualified (HQ.HashOnly (Reference.toShortHash term)))
                pure (formatNum n <> P.syntaxToColor (prettyReference todo.hashLen term))
            pure $
              P.wrap "These terms do not have any names in the current namespace:"
                <> P.newline
                <> P.newline
                <> P.indentN 2 (P.lines terms)

      prettyDirectTypeDependenciesWithoutNames <- do
        if Set.null todo.directDependenciesWithoutNames.types
          then pure mempty
          else do
            types <-
              for (Set.toList todo.directDependenciesWithoutNames.types) \typ -> do
                n <- addNumberedArg (SA.HashQualified (HQ.HashOnly (Reference.toShortHash typ)))
                pure (formatNum n <> P.syntaxToColor (prettyReference todo.hashLen typ))
            pure $
              P.wrap "These types do not have any names in the current namespace:"
                <> P.newline
                <> P.newline
                <> P.indentN 2 (P.lines types)

      prettyConflicts <-
        if todo.nameConflicts == mempty
          then pure mempty
          else renderNameConflicts todo.hashLen todo.nameConflicts

      let prettyDefnsInLib =
            if todo.defnsInLib
              then
                P.wrap $
                  -- Note [DefnsInLibMessage] If you change this, also change the other similar one
                  "There's a type or term at the top level of the `lib` namespace, where I only expect to find"
                    <> "subnamespaces representing library dependencies. Please move or remove it."
              else mempty

      prettyConstructorAliases <-
        let -- We want to filter out constructor aliases whose types are part of a "nested decl alias" problem, because
            -- otherwise we'd essentially be reporting those issues twice.
            --
            -- That is, if we have two nested aliases like
            --
            --   Foo = #XYZ
            --   Foo.Bar = #XYZ#0
            --
            --   Foo.inner.Alias = #XYZ
            --   Foo.inner.Alias.Constructor = #XYZ#0
            --
            -- then we'd prefer to say "oh no Foo and Foo.inner.Alias are aliases" but *not* additionally say "oh no
            -- Foo.Bar and Foo.inner.Alias.Constructor are aliases".
            notNestedDeclAlias (typeName, _, _) =
              foldr
                (\(short, long) acc -> typeName /= short && typeName /= long && acc)
                True
                todo.incoherentDeclReasons.nestedDeclAliases
         in case filter notNestedDeclAlias todo.incoherentDeclReasons.constructorAliases of
              [] -> pure mempty
              aliases -> do
                things <-
                  for aliases \(typeName, conName1, conName2) -> do
                    n1 <- addNumberedArg (SA.Name conName1)
                    n2 <- addNumberedArg (SA.Name conName2)
                    pure (typeName, formatNum n1 <> prettyName conName1, formatNum n2 <> prettyName conName2)
                pure $
                  things
                    & map
                      ( \(typeName, prettyCon1, prettyCon2) ->
                          -- Note [ConstructorAliasMessage] If you change this, also change the other similar ones
                          P.wrap ("The type" <> prettyName typeName <> "has a constructor with multiple names.")
                            <> P.newline
                            <> P.newline
                            <> P.indentN 2 (P.lines [prettyCon1, prettyCon2])
                            <> P.newline
                            <> P.newline
                            <> P.wrap "Please delete all but one name for each constructor."
                      )
                    & P.sep "\n\n"

      prettyMissingConstructorNames <-
        case NEList.nonEmpty todo.incoherentDeclReasons.missingConstructorNames of
          Nothing -> pure mempty
          Just types0 -> do
            stuff <-
              for types0 \typ -> do
                n <- addNumberedArg (SA.Name typ)
                pure (n, typ)
            -- Note [MissingConstructorNameMessage] If you change this, also change the other similar ones
            pure $
              P.wrap
                "These types have some constructors with missing names."
                <> P.newline
                <> P.newline
                <> P.indentN 2 (P.lines (fmap (\(n, typ) -> formatNum n <> prettyName typ) stuff))
                <> P.newline
                <> P.newline
                <> P.wrap
                  ( "You can use"
                      <> IP.makeExample
                        IP.view
                        [ let firstNum = fst (NEList.head stuff)
                              lastNum = fst (NEList.last stuff)
                           in if firstNum == lastNum
                                then P.string (show firstNum)
                                else P.string (show firstNum) <> "-" <> P.string (show lastNum)
                        ]
                      <> "and"
                      <> IP.makeExample IP.aliasTerm ["<hash>", "<TypeName>.<ConstructorName>"]
                      <> "to give names to each unnamed constructor."
                  )

      prettyNestedDeclAliases <-
        case todo.incoherentDeclReasons.nestedDeclAliases of
          [] -> pure mempty
          aliases0 -> do
            aliases1 <-
              for aliases0 \(short, long) -> do
                n1 <- addNumberedArg (SA.Name short)
                n2 <- addNumberedArg (SA.Name long)
                pure (formatNum n1 <> prettyName short, formatNum n2 <> prettyName long)
            -- Note [NestedDeclAliasMessage] If you change this, also change the other similar ones
            pure $
              aliases1
                & map
                  ( \(short, long) ->
                      P.wrap
                        ( "These types are aliases, but one is nested under the other. Please separate them or delete"
                            <> "one copy."
                        )
                        <> P.newline
                        <> P.newline
                        <> P.indentN 2 (P.lines [short, long])
                  )
                & P.sep "\n\n"

      prettyStrayConstructors <-
        case todo.incoherentDeclReasons.strayConstructors of
          [] -> pure mempty
          constructors -> do
            nums <-
              for constructors \(_typeRef, constructor) -> do
                addNumberedArg (SA.Name constructor)
            -- Note [StrayConstructorMessage] If you change this, also change the other similar ones
            pure $
              P.wrap "These constructors are not nested beneath their corresponding type names:"
                <> P.newline
                <> P.newline
                <> P.indentN
                  2
                  ( P.lines
                      ( zipWith
                          (\n (_typeRef, constructor) -> formatNum n <> prettyName constructor)
                          nums
                          constructors
                      )
                  )
                <> P.newline
                <> P.newline
                <> P.wrap
                  ( "For each one, please either use"
                      <> IP.makeExample' IP.moveAll
                      <> "to move if, or if it's an extra copy, you can simply"
                      <> IP.makeExample' IP.delete
                      <> "it."
                  )

      (pure . P.sep "\n\n" . P.nonEmpty)
        [ prettyDependentsOfTodo,
          prettyDirectTermDependenciesWithoutNames,
          prettyDirectTypeDependenciesWithoutNames,
          prettyConflicts,
          prettyDefnsInLib,
          prettyConstructorAliases,
          prettyMissingConstructorNames,
          prettyNestedDeclAliases,
          prettyStrayConstructors
        ]

listOfDefinitions ::
  (Var v) => Input.FindScope -> PPE.PrettyPrintEnv -> E.ListDetailed -> [SR'.SearchResult' v a] -> IO Pretty
listOfDefinitions fscope ppe detailed results =
  pure $ listOfDefinitions' fscope ppe detailed results

listOfNames :: Int -> [(Reference, [HQ'.HashQualified Name])] -> [(Referent, [HQ'.HashQualified Name])] -> IO Pretty
listOfNames len types terms = do
  if null types && null terms
    then
      pure . P.callout "😶" $
        P.sepNonEmpty "\n\n" $
          [ P.wrap "I couldn't find anything by that name."
          ]
    else
      pure . P.sepNonEmpty "\n\n" $
        [ formatTypes types,
          formatTerms terms
        ]
  where
    formatTerms tms =
      P.lines . P.nonEmpty $ P.plural tms (P.blue "Term") : List.intersperse "" (go <$> tms)
      where
        go (ref, hqs) =
          P.column2
            [ ("Hash:", P.syntaxToColor (prettyReferent len ref)),
              ( "Names: ",
                P.group $
                  P.spaced $
                    P.bold . P.syntaxToColor . prettyHashQualified' <$> List.sortBy Name.compareAlphabetical hqs
              )
            ]
    formatTypes types =
      P.lines . P.nonEmpty $ P.plural types (P.blue "Type") : List.intersperse "" (go <$> types)
      where
        go (ref, hqs) =
          P.column2
            [ ("Hash:", P.syntaxToColor (prettyReference len ref)),
              ( "Names:",
                P.group $
                  P.spaced $
                    P.bold . P.syntaxToColor . prettyHashQualified' <$> List.sortBy Name.compareAlphabetical hqs
              )
            ]

data ShowNumbers = ShowNumbers | HideNumbers

-- | `ppe` is just for rendering type signatures
--   `oldPath, newPath :: Path.Absolute` are just for producing fully-qualified
--                                       numbered args
showDiffNamespace ::
  forall v.
  (Var v) =>
  ShowNumbers ->
  PPE.PrettyPrintEnv ->
  Input.AbsBranchId ->
  Input.AbsBranchId ->
  OBD.BranchDiffOutput v Ann ->
  (Pretty, NumberedArgs)
showDiffNamespace _ _ _ _ diffOutput
  | OBD.isEmpty diffOutput =
      ("The namespaces are identical.", mempty)
showDiffNamespace sn ppe oldPath newPath OBD.BranchDiffOutput {..} =
  (P.sepNonEmpty "\n\n" p, toList args)
  where
    (p, (menuSize, args)) =
      (`State.runState` (0 :: Int, Seq.empty)) $
        sequence
          [ if (not . null) newTypeConflicts
              || (not . null) newTermConflicts
              then do
                prettyUpdatedTypes :: [Pretty] <- traverse prettyUpdateType newTypeConflicts
                prettyUpdatedTerms :: [Pretty] <- traverse prettyUpdateTerm newTermConflicts
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.red "New name conflicts:",
                      P.indentN 2 . P.sepNonEmpty "\n\n" $ prettyUpdatedTypes <> prettyUpdatedTerms
                    ]
              else pure mempty,
            if (not . null) resolvedTypeConflicts
              || (not . null) resolvedTermConflicts
              then do
                prettyUpdatedTypes :: [Pretty] <- traverse prettyUpdateType resolvedTypeConflicts
                prettyUpdatedTerms :: [Pretty] <- traverse prettyUpdateTerm resolvedTermConflicts
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.bold "Resolved name conflicts:",
                      P.indentN 2 . P.sepNonEmpty "\n\n" $ prettyUpdatedTypes <> prettyUpdatedTerms
                    ]
              else pure mempty,
            if (not . null) updatedTypes
              || (not . null) updatedTerms
              || (not . null) updatedPatches
              then do
                prettyUpdatedTypes :: [Pretty] <- traverse prettyUpdateType updatedTypes
                prettyUpdatedTerms :: [Pretty] <- traverse prettyUpdateTerm updatedTerms
                prettyUpdatedPatches :: [Pretty] <- traverse (prettySummarizePatch newPath) updatedPatches
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.bold "Updates:",
                      P.indentNonEmptyN 2 . P.sepNonEmpty "\n\n" $ prettyUpdatedTypes <> prettyUpdatedTerms,
                      P.indentNonEmptyN 2 . P.linesNonEmpty $ prettyUpdatedPatches
                    ]
              else pure mempty,
            if (not . null) addedTypes
              || (not . null) addedTerms
              || (not . null) addedPatches
              then do
                prettyAddedTypes :: Pretty <- prettyAddTypes addedTypes
                prettyAddedTerms :: Pretty <- prettyAddTerms addedTerms
                prettyAddedPatches :: [Pretty] <- traverse (prettySummarizePatch newPath) addedPatches
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.bold "Added definitions:",
                      P.indentNonEmptyN 2 $ P.linesNonEmpty [prettyAddedTypes, prettyAddedTerms],
                      P.indentNonEmptyN 2 $ P.lines prettyAddedPatches
                    ]
              else pure mempty,
            if (not . null) removedTypes
              || (not . null) removedTerms
              || (not . null) removedPatches
              then do
                prettyRemovedTypes :: Pretty <- prettyRemoveTypes removedTypes
                prettyRemovedTerms :: Pretty <- prettyRemoveTerms removedTerms
                prettyRemovedPatches :: [Pretty] <- traverse (prettyNamePatch oldPath) removedPatches
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.bold "Removed definitions:",
                      P.indentN 2 $
                        P.linesNonEmpty
                          [ prettyRemovedTypes,
                            prettyRemovedTerms,
                            P.linesNonEmpty prettyRemovedPatches
                          ]
                    ]
              else pure mempty,
            if (not . null) renamedTypes
              || (not . null) renamedTerms
              then do
                results <- prettyRenameGroups renamedTypes renamedTerms
                pure $
                  P.sepNonEmpty
                    "\n\n"
                    [ P.bold "Name changes:",
                      P.indentN 2 . P.sepNonEmpty "\n\n" $ results
                    ]
              else -- todo: change separator to just '\n' here if all the results are 1 to 1
                pure mempty
          ]

    {- new implementation
      23. X  ┐  =>  (added)   24. X'
      25. X2 ┘      (removed) 26. X2
    -}
    prettyRenameGroups ::
      [OBD.RenameTypeDisplay v a] ->
      [OBD.RenameTermDisplay v a] ->
      Numbered [Pretty]
    prettyRenameGroups types terms =
      (<>)
        <$> traverse
          (prettyGroup . (over (_1 . _1) Referent.Ref))
          (types `zip` [0 ..])
        <*> traverse prettyGroup (terms `zip` [length types ..])
      where
        leftNamePad :: P.Width =
          foldl1' max $
            map
              (foldl1' max . map (P.Width . HQ'.nameLength Name.toText) . toList . view _3)
              terms
              <> map
                (foldl1' max . map (P.Width . HQ'.nameLength Name.toText) . toList . view _3)
                types
        prettyGroup ::
          ( (Referent, b, Set (HQ'.HashQualified Name), Set (HQ'.HashQualified Name)),
            Int
          ) ->
          Numbered Pretty
        prettyGroup ((r, _, olds, news), i) =
          let -- [ "peach  ┐"
              -- , "peach' ┘"]
              olds' :: [Numbered Pretty] =
                let olds0 = List.sortBy Name.compareAlphabetical (Set.toList olds)
                 in map (\(oldhq, oldp) -> numHQ' oldPath oldhq r <&> (\n -> n <> " " <> oldp))
                      . zip olds0
                      . P.boxRight
                      . map (P.rightPad leftNamePad . phq')
                      $ olds0

              added' = toList $ Set.difference news olds
              removed' = toList $ Set.difference olds news
              -- [ "(added)   24. X'"
              -- , "(removed) 26. X2"
              -- ]

              news' :: [Numbered Pretty] =
                map (number addedLabel) added' ++ map (number removedLabel) removed'
                where
                  addedLabel = "(added)"
                  removedLabel = "(removed)"
                  number label name =
                    numHQ' newPath name r
                      <&> (\num -> num <> " " <> phq' name <> " " <> label)

              buildTable :: [Numbered Pretty] -> [Numbered Pretty] -> Numbered Pretty
              buildTable lefts rights =
                let hlefts =
                      if i == 0
                        then pure (P.bold "Original") : lefts
                        else lefts
                    hrights = if i == 0 then pure (P.bold "Changes") : rights else rights
                 in P.column2UnzippedM @Numbered mempty hlefts hrights
           in buildTable olds' news'

    prettyUpdateType :: OBD.UpdateTypeDisplay v a -> Numbered Pretty
    {-
       1. ability Foo#pqr x y
          2. - AllRightsReserved : License
          3. + MIT               : License
       4. ability Foo#abc
          5. - apiDocs : License
          6. + MIT     : License
    -}
    prettyUpdateType (OBD.UpdateTypeDisplay Nothing mdUps) =
      P.column2 <$> traverse (mdTypeLine newPath) mdUps
    {-
        1. ┌ ability Foo#pqr x y
        2. └ ability Foo#xyz a b
           ⧩
        4. ┌ ability Foo#abc
           │  5. - apiDocs : Doc
           │  6. + MIT     : License
        7. └ ability Foo#def
              8. - apiDocs : Doc
              9. + MIT     : License

        1. ┌ foo#abc : Nat -> Nat -> Poop
        2. └ foo#xyz : Nat
           ↓
        4. foo	 : Poop
             5. + foo.docs : Doc
    -}
    prettyUpdateType (OBD.UpdateTypeDisplay (Just olds) news) =
      do
        olds <- traverse (mdTypeLine oldPath) [OBD.TypeDisplay name r decl | (name, r, decl) <- olds]
        news <- traverse (mdTypeLine newPath) news
        let (oldnums, olddatas) = unzip olds
        let (newnums, newdatas) = unzip news
        pure . P.column2 $
          zip
            (oldnums <> [""] <> newnums)
            (P.boxLeft olddatas <> [downArrow] <> P.boxLeft newdatas)

    {-
    13. ┌ability Yyz
    14. └ability copies.Yyz
    -}
    prettyAddTypes :: forall a. [OBD.AddedTypeDisplay v a] -> Numbered Pretty
    prettyAddTypes = fmap P.lines . traverse prettyGroup
      where
        prettyGroup :: OBD.AddedTypeDisplay v a -> Numbered Pretty
        prettyGroup (hqs, r, odecl) = do
          pairs <- traverse (prettyLine r odecl) hqs
          let (nums, decls) = unzip pairs
          let boxLeft = case hqs of
                _ : _ : _ -> P.boxLeft
                _ -> id
          pure . P.column2 $ zip nums (boxLeft decls)
        prettyLine :: Reference -> Maybe (DD.DeclOrBuiltin v a) -> HQ'.HashQualified Name -> Numbered (Pretty, Pretty)
        prettyLine r odecl hq = do
          n <- numHQ' newPath hq (Referent.Ref r)
          pure . (n,) $ prettyDecl hq odecl

    prettyAddTerms :: forall a. [OBD.AddedTermDisplay v a] -> Numbered Pretty
    prettyAddTerms = fmap (P.column3 . mconcat) . traverse prettyGroup . reorderTerms
      where
        reorderTerms = sortOn (not . Referent.isConstructor . view _2)
        prettyGroup :: OBD.AddedTermDisplay v a -> Numbered [(Pretty, Pretty, Pretty)]
        prettyGroup (hqs, r, otype) = do
          pairs <- traverse (prettyLine r otype) hqs
          let (nums, names, decls) = unzip3 pairs
              boxLeft =
                case hqs of
                  _ : _ : _ -> P.boxLeft
                  _ -> id
          pure $ zip3 nums (boxLeft names) decls
        prettyLine ::
          Referent ->
          Maybe (Type v a) ->
          HQ'.HashQualified Name ->
          Numbered (Pretty, Pretty, Pretty)
        prettyLine r otype hq = do
          n <- numHQ' newPath hq r
          pure . (n,phq' hq,) $ ": " <> prettyType otype

    prettySummarizePatch, prettyNamePatch :: Input.AbsBranchId -> OBD.PatchDisplay -> Numbered Pretty
    --  12. patch p (added 3 updates, deleted 1)
    prettySummarizePatch prefix (name, patchDiff) = do
      n <- numPatch prefix name
      let addCount =
            (R.size . view Patch.addedTermEdits) patchDiff
              + (R.size . view Patch.addedTypeEdits) patchDiff
          delCount =
            (R.size . view Patch.removedTermEdits) patchDiff
              + (R.size . view Patch.removedTypeEdits) patchDiff
          messages =
            (if addCount > 0 then ["added " <> P.shown addCount] else [])
              ++ (if delCount > 0 then ["deleted " <> P.shown addCount] else [])
          message = case messages of
            [] -> mempty
            x : ys -> " (" <> P.commas (x <> " updates" : ys) <> ")"
      pure $ n <> P.bold " patch " <> prettyName name <> message
    --          18. patch q
    prettyNamePatch prefix (name, _patchDiff) = do
      n <- numPatch prefix name
      pure $ n <> P.bold " patch " <> prettyName name

    {-
     Removes:

       10. ┌ oldn'busted : Nat -> Nat -> Poop
       11. └ oldn'busted'
       12.  ability BadType
       13.  patch defunctThingy
    -}
    prettyRemoveTypes :: forall a. [OBD.RemovedTypeDisplay v a] -> Numbered Pretty
    prettyRemoveTypes = fmap P.lines . traverse prettyGroup
      where
        prettyGroup :: OBD.RemovedTypeDisplay v a -> Numbered Pretty
        prettyGroup (hqs, r, odecl) = do
          lines <- traverse (prettyLine r odecl) hqs
          let (nums, decls) = unzip lines
              boxLeft = case hqs of _ : _ : _ -> P.boxLeft; _ -> id
          pure . P.column2 $ zip nums (boxLeft decls)
        prettyLine :: Reference -> Maybe (DD.DeclOrBuiltin v a) -> HQ'.HashQualified Name -> Numbered (Pretty, Pretty)
        prettyLine r odecl hq = do
          n <- numHQ' newPath hq (Referent.Ref r)
          pure (n, prettyDecl hq odecl)

    prettyRemoveTerms :: forall a. [OBD.RemovedTermDisplay v a] -> Numbered Pretty
    prettyRemoveTerms = fmap (P.column3 . mconcat) . traverse prettyGroup . reorderTerms
      where
        reorderTerms = sortOn (not . Referent.isConstructor . view _2)
        prettyGroup :: OBD.RemovedTermDisplay v a -> Numbered [(Pretty, Pretty, Pretty)]
        prettyGroup ([], r, _) =
          error $ "trying to remove " <> show r <> " without any names."
        prettyGroup (hq1 : hqs, r, otype) = do
          line1 <- prettyLine1 r otype hq1
          lines <- traverse (prettyLine r) hqs
          let (nums, names, decls) = unzip3 (line1 : lines)
              boxLeft = case hqs of _ : _ -> P.boxLeft; _ -> id
          pure $ zip3 nums (boxLeft names) decls
        prettyLine1 r otype hq = do
          n <- numHQ' newPath hq r
          pure (n, phq' hq, ": " <> prettyType otype)
        prettyLine r hq = do
          n <- numHQ' newPath hq r
          pure (n, phq' hq, mempty)

    downArrow = P.bold "↓"
    mdTypeLine :: Input.AbsBranchId -> OBD.TypeDisplay v a -> Numbered (Pretty, Pretty)
    mdTypeLine p (OBD.TypeDisplay hq r odecl) = do
      n <- numHQ' p hq (Referent.Ref r)
      fmap ((n,) . P.linesNonEmpty) . sequence $
        [ pure $ prettyDecl hq odecl
        ]

    -- + 2. MIT               : License
    -- - 3. AllRightsReserved : License
    mdTermLine ::
      Input.AbsBranchId ->
      P.Width ->
      OBD.TermDisplay v a ->
      Numbered (Pretty, Pretty)
    mdTermLine p namesWidth (OBD.TermDisplay hq r otype) = do
      n <- numHQ' p hq r
      fmap ((n,) . P.linesNonEmpty)
        . sequence
        $ [ pure $ P.rightPad namesWidth (phq' hq) <> " : " <> prettyType otype
          ]

    prettyUpdateTerm :: OBD.UpdateTermDisplay v a -> Numbered Pretty
    prettyUpdateTerm (OBD.UpdateTermDisplay Nothing newTerms) =
      if null newTerms
        then error "Super invalid UpdateTermDisplay"
        else fmap P.column2 $ traverse (mdTermLine newPath namesWidth) newTerms
      where
        namesWidth = foldl1' max $ fmap (P.Width . HQ'.nameLength Name.toText . view #name) newTerms
    prettyUpdateTerm (OBD.UpdateTermDisplay (Just olds) news) = fmap P.column2 $ do
      olds <-
        traverse
          (mdTermLine oldPath namesWidth)
          [OBD.TermDisplay name r typ | (name, r, typ) <- olds]
      news <- traverse (mdTermLine newPath namesWidth) news
      let (oldnums, olddatas) = unzip olds
      let (newnums, newdatas) = unzip news
      pure $
        zip
          (oldnums <> [""] <> newnums)
          (P.boxLeft olddatas <> [downArrow] <> P.boxLeft newdatas)
      where
        namesWidth =
          foldl1' max $
            fmap (P.Width . HQ'.nameLength Name.toText . view #name) news
              <> fmap (P.Width . HQ'.nameLength Name.toText . view _1) olds

    prettyType :: Maybe (Type v a) -> Pretty
    prettyType = maybe (P.red "type not found") (TypePrinter.pretty ppe)
    prettyDecl hq =
      maybe
        (P.red "type not found")
        (P.syntaxToColor . DeclPrinter.prettyDeclOrBuiltinHeader (HQ'.toHQ hq))
    phq' :: _ -> Pretty = P.syntaxToColor . prettyHashQualified'

    -- DeclPrinter.prettyDeclHeader : HQ -> Either
    numPatch :: Input.AbsBranchId -> Name -> Numbered Pretty
    numPatch prefix name =
      addNumberedArg' $ SA.NameWithBranchPrefix prefix name

    numHQ' :: Input.AbsBranchId -> HQ'.HashQualified Name -> Referent -> Numbered Pretty
    numHQ' prefix hq r =
      addNumberedArg' . SA.HashQualifiedWithBranchPrefix prefix $ HQ'.requalify hq r

    addNumberedArg' :: StructuredArgument -> Numbered Pretty
    addNumberedArg' s = case sn of
      ShowNumbers -> do
        n <- addNumberedArg s
        pure $ padNumber n
      HideNumbers -> pure mempty

    padNumber :: Int -> Pretty
    padNumber n = P.hiBlack . P.rightPad leftNumsWidth $ P.shown n <> "."

    leftNumsWidth = P.Width $ length (show menuSize) + length ("." :: String)

noResults :: Input.FindScope -> Pretty
noResults fscope =
  P.callout "😶" $
    P.lines $
      [ P.wrap $
          "No results. Check your spelling, or try using tab completion "
            <> "to supply command arguments.",
        ""
      ]
        ++ case fscope of
          Input.FindGlobal -> []
          _ -> [suggestFindGlobal]
  where
    suggestFindGlobal =
      P.wrap $
        IP.makeExample IP.findGlobal []
          <> "can be used to search outside the current namespace."

listOfDefinitions' ::
  (Var v) =>
  Input.FindScope ->
  PPE.PrettyPrintEnv -> -- for printing types of terms :-\
  E.ListDetailed ->
  [SR'.SearchResult' v a] ->
  Pretty
listOfDefinitions' fscope ppe detailed results =
  if null results
    then noResults fscope
    else
      P.lines
        . P.nonEmpty
        $ prettyNumberedResults
          : [ formatMissingStuff termsWithMissingTypes missingTypes,
              Monoid.unlessM (null missingBuiltins)
                . bigproblem
                $ P.wrap
                  "I encountered an inconsistency in the codebase; these definitions refer to built-ins that this version of unison doesn't know about:"
                  `P.hang` P.column2
                    ( (P.bold "Name", P.bold "Built-in")
                        -- : ("-", "-")
                        : fmap
                          ( bimap
                              (P.syntaxToColor . prettyHashQualified)
                              (P.text . Referent.toText)
                          )
                          missingBuiltins
                    )
            ]
  where
    prettyNumberedResults = P.numberedList prettyResults
    -- todo: group this by namespace
    prettyResults =
      map
        (SR'.foldResult' renderTerm renderType)
        (filter (not . missingType) results)
      where
        (renderTerm, renderType) =
          if detailed
            then (unsafePrettyTermResultSigFull' ppe, prettyTypeResultHeaderFull')
            else (unsafePrettyTermResultSig' ppe, prettyTypeResultHeader')
    missingType (SR'.Tm _ Nothing _ _) = True
    missingType (SR'.Tp _ (MissingObject _) _ _) = True
    missingType _ = False
    -- termsWithTypes = [(name,t) | (name, Just t) <- sigs0 ]
    --   where sigs0 = (\(name, _, typ) -> (name, typ)) <$> terms
    termsWithMissingTypes =
      [ (name, Reference.idToShortHash r)
        | SR'.Tm name Nothing (Referent.Ref (Reference.DerivedId r)) _ <- results
      ]
    missingTypes =
      nubOrdOn snd $
        [(name, r) | SR'.Tp name (MissingObject r) _ _ <- results]
          <> [ (name, Reference.toShortHash r)
               | SR'.Tm name Nothing (Referent.toTypeReference -> Just r) _ <- results
             ]
    missingBuiltins =
      results >>= \case
        SR'.Tm name Nothing r@(Referent.Ref (Reference.Builtin _)) _ ->
          [(name, r)]
        _ -> []

watchPrinter ::
  (Var v) =>
  Text ->
  PPE.PrettyPrintEnv ->
  Ann ->
  WK.WatchKind ->
  Term v () ->
  Runtime.IsCacheHit ->
  Pretty
watchPrinter src ppe ann kind term isHit =
  P.bracket $
    let lines = Text.lines src
        lineNum = fromMaybe 1 $ startingLine ann
        lineNumWidth = length (show lineNum)
        extra = "     " <> replicate (length kind) ' ' -- for the ` | > ` after the line number
        line = lines !! (lineNum - 1)
        addCache p = if isHit then p <> " (cached)" else p
        renderTest (Term.App' (Term.Constructor' (ConstructorReference _ id)) (Term.Text' msg)) =
          "\n"
            <> if id == DD.okConstructorId
              then
                addCache
                  (P.green "✅ " <> P.bold "Passed" <> P.green (P.text msg'))
              else
                if id == DD.failConstructorId
                  then
                    addCache
                      (P.red "🚫 " <> P.bold "FAILED" <> P.red (P.text msg'))
                  else P.red "❓ " <> TermPrinter.pretty ppe term
          where
            msg' =
              if Text.take 1 msg == " "
                then msg
                else " " <> msg
        renderTest x =
          fromString $ "\n Unison bug: " <> show x <> " is not a test."
     in P.lines
          [ fromString (show lineNum) <> " | " <> P.text line,
            case (kind, term) of
              (WK.TestWatch, Term.List' tests) -> foldMap renderTest tests
              _ ->
                P.lines
                  [ fromString (replicate lineNumWidth ' ')
                      <> fromString extra
                      <> (if isHit then id else P.purple) "⧩",
                    P.indentN (P.Width (lineNumWidth + length extra))
                      . (if isHit then id else P.bold)
                      $ TermPrinter.pretty ppe term
                  ]
          ]

filestatusTip :: Pretty
filestatusTip = tip "Use `help filestatus` to learn more."

prettyDiff :: Names.Diff -> Pretty
prettyDiff diff =
  let orig = Names.originalNames diff
      adds = Names.addedNames diff
      removes = Names.removedNames diff

      addedTerms =
        [ (n, r) | (n, r) <- R.toList (Names.terms adds), not $ R.memberRan r (Names.terms removes)
        ]
      addedTypes =
        [ (n, r) | (n, r) <- R.toList (Names.types adds), not $ R.memberRan r (Names.types removes)
        ]
      added = List.sortBy Name.compareAlphabetical (hqTerms ++ hqTypes)
        where
          hqTerms = [Names.hqName adds n (Right r) | (n, r) <- addedTerms]
          hqTypes = [Names.hqName adds n (Left r) | (n, r) <- addedTypes]

      removedTerms =
        [ (n, r) | (n, r) <- R.toList (Names.terms removes), not $ R.memberRan r (Names.terms adds), Set.notMember n addedTermsSet
        ]
        where
          addedTermsSet = Set.fromList (map fst addedTerms)
      removedTypes =
        [ (n, r) | (n, r) <- R.toList (Names.types removes), not $ R.memberRan r (Names.types adds), Set.notMember n addedTypesSet
        ]
        where
          addedTypesSet = Set.fromList (map fst addedTypes)
      removed = List.sortBy Name.compareAlphabetical (hqTerms ++ hqTypes)
        where
          hqTerms = [Names.hqName removes n (Right r) | (n, r) <- removedTerms]
          hqTypes = [Names.hqName removes n (Left r) | (n, r) <- removedTypes]

      movedTerms =
        [ (n, n2) | (n, r) <- R.toList (Names.terms removes), n2 <- toList (R.lookupRan r (Names.terms adds))
        ]
      movedTypes =
        [ (n, n2) | (n, r) <- R.toList (Names.types removes), n2 <- toList (R.lookupRan r (Names.types adds))
        ]
      moved = Name.sortNamed Name.toText fst . nubOrd $ (movedTerms <> movedTypes)

      copiedTerms =
        List.multimap
          [ (n, n2) | (n2, r) <- R.toList (Names.terms adds), not (R.memberRan r (Names.terms removes)), n <- toList (R.lookupRan r (Names.terms orig))
          ]
      copiedTypes =
        List.multimap
          [ (n, n2) | (n2, r) <- R.toList (Names.types adds), not (R.memberRan r (Names.types removes)), n <- toList (R.lookupRan r (Names.types orig))
          ]
      copied =
        Name.sortNamed Name.toText fst $
          Map.toList (Map.unionWith (<>) copiedTerms copiedTypes)
   in P.sepNonEmpty
        "\n\n"
        [ if not $ null added
            then
              P.lines
                [ -- todo: split out updates
                  P.green "+ Adds / updates:",
                  "",
                  P.indentN 2 . P.wrap $
                    P.sep " " (P.syntaxToColor . prettyHashQualified' <$> added)
                ]
            else mempty,
          if not $ null removed
            then
              P.lines
                [ P.hiBlack "- Deletes:",
                  "",
                  P.indentN 2 . P.wrap $
                    P.sep " " (P.syntaxToColor . prettyHashQualified' <$> removed)
                ]
            else mempty,
          if not $ null moved
            then
              P.lines
                [ P.purple "> Moves:",
                  "",
                  P.indentN 2 $
                    P.column2 $
                      (P.hiBlack "Original name", P.hiBlack "New name")
                        : [(prettyName n, prettyName n2) | (n, n2) <- moved]
                ]
            else mempty,
          if not $ null copied
            then
              P.lines
                [ P.yellow "= Copies:",
                  "",
                  P.indentN 2 $
                    P.column2 $
                      (P.hiBlack "Original name", P.hiBlack "New name(s)")
                        : [ (prettyName n, P.sep " " (prettyName <$> ns))
                            | (n, ns) <- copied
                          ]
                ]
            else mempty
        ]

-- | Get the list of numbered args corresponding to an endangerment map, which is used by a
-- few outputs. See 'endangeredDependentsTable'.
numberedArgsForEndangerments ::
  PPED.PrettyPrintEnvDecl ->
  Map LabeledDependency (NESet LabeledDependency) ->
  NumberedArgs
numberedArgsForEndangerments (PPED.unsuffixifiedPPE -> ppe) m =
  m
    & Map.elems
    & concatMap toList
    & fmap (SA.HashQualified . PPE.labeledRefName ppe)

-- | Format and render all dependents which are endangered by references going extinct.
endangeredDependentsTable ::
  PPED.PrettyPrintEnvDecl ->
  Map LabeledDependency (NESet LabeledDependency) ->
  P.Pretty P.ColorText
endangeredDependentsTable ppeDecl m =
  m
    & Map.toList
    & fmap (second toList)
    & numberDependents
    & map
      ( \(dependency, dependents) ->
          (prettyLabeled suffixifiedEnv dependency, prettyDependents dependents)
      )
    & List.intersperse spacer
    & P.column2Header "Dependency" "Referenced In"
  where
    numberDependents :: [(x, [LabeledDependency])] -> [(x, [(Int, LabeledDependency)])]
    numberDependents xs =
      let (_acc, numbered) =
            mapAccumLOf
              (traversed . _2 . traversed)
              (\n ld -> (n + 1, (n, ld)))
              1
              xs
       in numbered
    spacer = ("", "")
    suffixifiedEnv = (PPED.suffixifiedPPE ppeDecl)
    fqnEnv = (PPED.unsuffixifiedPPE ppeDecl)
    prettyLabeled ppe = \case
      LD.TermReferent ref -> prettyTermName ppe ref
      LD.TypeReference ref -> prettyTypeName ppe ref
    numArg = (\n -> P.hiBlack . fromString $ show n <> ". ")
    prettyDependents refs =
      refs
        & fmap (\(n, dep) -> numArg n <> prettyLabeled fqnEnv dep)
        & P.lines

listFind :: Bool -> Maybe Pretty -> [HQ.HashQualified Name] -> Pretty
listFind _ Nothing [] = "😶 I couldn't find any matches."
listFind _ (Just onMissing) [] = P.lines ["😶 I couldn't find any matches.", "", tip onMissing]
listFind allowLib _ tms =
  P.callout "🔎" . P.lines $
    [ "These definitions from the current namespace " <> parenthetical <> "have matches:",
      "",
      P.indentN 2 $ P.numberedList (pnames tms),
      "",
      tip (msg (length tms))
    ]
  where
    parenthetical = if allowLib then "" else "(excluding `lib`) "
    pnames hqs = P.syntaxToColor . prettyHashQualified <$> hqs
    msg 1 = "Try " <> IP.makeExample IP.edit ["1"] <> " to bring this into your scratch file."
    msg n =
      "Try "
        <> IP.makeExample IP.edit ["1"]
        <> " or "
        <> IP.makeExample IP.edit ["1-" <> P.shown n]
        <> " to bring these into your scratch file."

listDependentsOrDependencies ::
  PPE.PrettyPrintEnv ->
  Text ->
  Text ->
  Set LabeledDependency ->
  [HQ.HashQualified Name] ->
  [HQ.HashQualified Name] ->
  Pretty
listDependentsOrDependencies ppe labelStart label lds types terms =
  if null (types <> terms)
    then prettyLabeledDependencies ppe lds <> " has no " <> P.text label <> "."
    else P.sepNonEmpty "\n\n" [hdr, typesOut, termsOut, tip msg]
  where
    msg = "Try " <> IP.makeExample IP.view args <> " to see the source of any numbered item in the above list."
    args = [P.shown (length (types <> terms))]
    hdr = P.text labelStart <> " of: " <> prettyLabeledDependencies ppe lds
    typesOut =
      if null types
        then mempty
        else
          P.lines $
            [ P.indentN 2 $ P.bold "Types:",
              "",
              P.indentN 2 . P.numberedList $ c . prettyHashQualified <$> types
            ]
    termsOut =
      if null terms
        then mempty
        else
          P.lines
            [ P.indentN 2 $ P.bold "Terms:",
              "",
              P.indentN 2 . P.numberedListFrom (length types) $ c . prettyHashQualified <$> terms
            ]
    c = P.syntaxToColor

displayProjectBranchReflogEntries ::
  Maybe UTCTime ->
  E.MoreEntriesThanShown ->
  [ProjectReflog.Entry Project ProjectBranch (CausalHash, ShortCausalHash)] ->
  (Pretty, NumberedArgs)
displayProjectBranchReflogEntries _ _ [] =
  (P.warnCallout "The reflog is empty", mempty)
displayProjectBranchReflogEntries mayNow _ entries =
  let (entryRows, numberedArgs) = foldMap renderEntry entries
      rendered =
        P.lines
          [ header,
            "",
            P.numberedColumnNHeader (["Branch"] <> Monoid.whenM (isJust mayNow) ["When"] <> ["Hash", "Description"]) entryRows
          ]
   in (rendered, numberedArgs)
  where
    header =
      P.lines
        [ P.wrap $
            "Below is a record of recent changes, you can use "
              <> IP.makeExample IP.reset ["#abcdef"]
              <> " to reset the current branch to a previous state.",
          "",
          tip $ "Use " <> IP.makeExample IP.diffNamespace ["1", "7"] <> " to compare between points in history."
        ]
    renderEntry :: ProjectReflog.Entry Project ProjectBranch (CausalHash, SCH.ShortCausalHash) -> ([[Pretty]], NumberedArgs)
    renderEntry ProjectReflog.Entry {time, project, branch, toRootCausalHash = (toCH, toSCH), reason} =
      ( [ [prettyProjectAndBranchName $ ProjectAndBranch project.name branch.name]
            <> ( mayNow
                   & foldMap (\now -> [prettyHumanReadableTime now time])
               )
            <> [P.blue (prettySCH toSCH), P.text $ truncateReason reason]
        ],
        [SA.Namespace toCH]
      )
    truncateReason :: Text -> Text
    truncateReason txt = case Text.splitAt 60 txt of
      (short, "") -> short
      (short, _) -> short <> "..."
