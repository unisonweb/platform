{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Unison.Codebase.Editor.HandleInput (loop) where

-- TODO: Don't import backend

import Control.Arrow ((&&&))
import Control.Error.Util qualified as ErrorUtil
import Control.Lens hiding (from)
import Control.Monad.Reader (ask)
import Control.Monad.State (StateT)
import Control.Monad.State qualified as State
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.Extra (nubOrd)
import Data.List.NonEmpty qualified as Nel
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Tuple.Extra (uncurry3)
import Text.Megaparsec qualified as Megaparsec
import U.Codebase.Branch.Diff qualified as V2Branch.Diff
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Reflog qualified as Reflog
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.ABT qualified as ABT
import Unison.Builtin qualified as Builtin
import Unison.Builtin.Terms qualified as Builtin
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils (getCurrentProjectBranch)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch (..), Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Merge qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Causal qualified as Causal
import Unison.Codebase.Editor.AuthorInfo (AuthorInfo (..))
import Unison.Codebase.Editor.AuthorInfo qualified as AuthorInfo
import Unison.Codebase.Editor.HandleInput.AddRun (handleAddRun)
import Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin)
import Unison.Codebase.Editor.HandleInput.Branch (handleBranch)
import Unison.Codebase.Editor.HandleInput.BranchRename (handleBranchRename)
import Unison.Codebase.Editor.HandleInput.Branches (handleBranches)
import Unison.Codebase.Editor.HandleInput.CommitMerge (handleCommitMerge)
import Unison.Codebase.Editor.HandleInput.CommitUpgrade (handleCommitUpgrade)
import Unison.Codebase.Editor.HandleInput.DebugDefinition qualified as DebugDefinition
import Unison.Codebase.Editor.HandleInput.DebugFoldRanges qualified as DebugFoldRanges
import Unison.Codebase.Editor.HandleInput.DebugSynhashTerm (handleDebugSynhashTerm)
import Unison.Codebase.Editor.HandleInput.DeleteBranch (handleDeleteBranch)
import Unison.Codebase.Editor.HandleInput.DeleteProject (handleDeleteProject)
import Unison.Codebase.Editor.HandleInput.EditNamespace (handleEditNamespace)
import Unison.Codebase.Editor.HandleInput.FindAndReplace (handleStructuredFindI, handleStructuredFindReplaceI, handleTextFindI)
import Unison.Codebase.Editor.HandleInput.FormatFile qualified as Format
import Unison.Codebase.Editor.HandleInput.Global qualified as Global
import Unison.Codebase.Editor.HandleInput.InstallLib (handleInstallLib)
import Unison.Codebase.Editor.HandleInput.LSPDebug qualified as LSPDebug
import Unison.Codebase.Editor.HandleInput.Load (EvalMode (Sandboxed), evalUnisonFile, handleLoad, loadUnisonFile)
import Unison.Codebase.Editor.HandleInput.Ls (handleLs)
import Unison.Codebase.Editor.HandleInput.Merge2 (handleMerge)
import Unison.Codebase.Editor.HandleInput.MoveAll (handleMoveAll)
import Unison.Codebase.Editor.HandleInput.MoveBranch (doMoveBranch)
import Unison.Codebase.Editor.HandleInput.MoveTerm (doMoveTerm)
import Unison.Codebase.Editor.HandleInput.MoveType (doMoveType)
import Unison.Codebase.Editor.HandleInput.NamespaceDependencies (handleNamespaceDependencies)
import Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils (diffHelper)
import Unison.Codebase.Editor.HandleInput.ProjectClone (handleClone)
import Unison.Codebase.Editor.HandleInput.ProjectCreate (projectCreate)
import Unison.Codebase.Editor.HandleInput.ProjectRename (handleProjectRename)
import Unison.Codebase.Editor.HandleInput.ProjectSwitch (projectSwitch)
import Unison.Codebase.Editor.HandleInput.Projects (handleProjects)
import Unison.Codebase.Editor.HandleInput.Pull (handlePull, mergeBranchAndPropagateDefaultPatch)
import Unison.Codebase.Editor.HandleInput.Push (handlePushRemoteBranch)
import Unison.Codebase.Editor.HandleInput.Reflogs qualified as Reflogs
import Unison.Codebase.Editor.HandleInput.ReleaseDraft (handleReleaseDraft)
import Unison.Codebase.Editor.HandleInput.Run (handleRun)
import Unison.Codebase.Editor.HandleInput.RuntimeUtils qualified as RuntimeUtils
import Unison.Codebase.Editor.HandleInput.ShowDefinition (handleShowDefinition)
import Unison.Codebase.Editor.HandleInput.TermResolution (resolveMainRef)
import Unison.Codebase.Editor.HandleInput.Tests qualified as Tests
import Unison.Codebase.Editor.HandleInput.Todo (handleTodo)
import Unison.Codebase.Editor.HandleInput.UI (openUI)
import Unison.Codebase.Editor.HandleInput.Update (doSlurpAdds, handleUpdate)
import Unison.Codebase.Editor.HandleInput.Update2 (handleUpdate2)
import Unison.Codebase.Editor.HandleInput.Upgrade (handleUpgrade)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.Output.DumpNamespace qualified as Output.DN
import Unison.Codebase.Editor.RemoteRepo qualified as RemoteRepo
import Unison.Codebase.Editor.Slurp qualified as Slurp
import Unison.Codebase.Editor.SlurpResult qualified as SlurpResult
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.Codebase.Execute qualified as Codebase
import Unison.Codebase.IntegrityCheck qualified as IntegrityCheck (integrityCheckFullCodebase)
import Unison.Codebase.Metadata qualified as Metadata
import Unison.Codebase.Path (Path, Path' (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.Runtime qualified as Runtime
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.CommandLine.BranchRelativePath (BranchRelativePath (..))
import Unison.CommandLine.Completion qualified as Completion
import Unison.CommandLine.DisplayValues qualified as DisplayValues
import Unison.CommandLine.InputPattern qualified as IP
import Unison.CommandLine.InputPatterns qualified as IP
import Unison.CommandLine.InputPatterns qualified as InputPatterns
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.LabeledDependency qualified as LabeledDependency
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names (Names))
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPE hiding (biasTo, empty)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Runtime.IOSource qualified as IOSource
import Unison.Server.Backend qualified as Backend
import Unison.Server.CodebaseServer qualified as Server
import Unison.Server.Doc.Markdown.Render qualified as Md
import Unison.Server.Doc.Markdown.Types qualified as Md
import Unison.Server.NameSearch.FromNames qualified as NameSearch
import Unison.Server.SearchResult (SearchResult)
import Unison.Server.SearchResult qualified as SR
import Unison.Share.Codeserver qualified as Codeserver
import Unison.ShortHash qualified as SH
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (parseTextWith, toText)
import Unison.Syntax.Lexer.Unison qualified as L
import Unison.Syntax.Name qualified as Name (toText, toVar, unsafeParseVar)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Type.Names qualified as Type
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Util.Find qualified as Find
import Unison.Util.List (nubOrdOn, uniqueBy)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty qualified as P
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as R
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.Star2 qualified as Star2
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind qualified as WK
import UnliftIO.Directory qualified as Directory

------------------------------------------------------------------------------------------------------------------------
-- Main loop

loop :: Either Event Input -> Cli ()
loop e = do
  case e of
    Left (UnisonFileChanged sourceName text) -> Cli.time "UnisonFileChanged" do
      -- We skip this update if it was programmatically generated
      Cli.getLatestFile >>= \case
        Just (_, True) -> (#latestFile . _Just . _2) .= False
        _ -> loadUnisonFile sourceName text
    Right input ->
      let previewResponse sourceName sr uf = do
            names <- Cli.currentNames
            let namesWithDefinitionsFromFile = UF.addNamesFromTypeCheckedUnisonFile uf names
            let filePPED = PPED.makePPED (PPE.hqNamer 10 namesWithDefinitionsFromFile) (PPE.suffixifyByHash namesWithDefinitionsFromFile)
            let suffixifiedPPE = PPE.suffixifiedPPE filePPED
            Cli.respond $ Typechecked (Text.pack sourceName) suffixifiedPPE sr uf
       in Cli.time "InputPattern" case input of
            ApiI -> do
              pp <- Cli.getCurrentProjectPath
              Cli.Env {serverBaseUrl} <- ask
              whenJust serverBaseUrl \baseUrl ->
                Cli.respond $
                  PrintMessage $
                    P.lines
                      [ "The API information is as follows:",
                        P.newline,
                        P.indentN 2 (P.hiBlue ("UI: " <> Pretty.text (Server.urlFor (Server.ProjectBranchUI (PP.toProjectAndBranch . PP.toNames $ pp) Path.absoluteEmpty Nothing) baseUrl))),
                        P.newline,
                        P.indentN 2 (P.hiBlue ("API: " <> Pretty.text (Server.urlFor Server.Api baseUrl)))
                      ]
            CreateMessage pretty ->
              Cli.respond $ PrintMessage pretty
            ShowRootReflogI -> do
              let numEntriesToShow = 500
              (schLength, entries) <-
                Cli.runTransaction $
                  (,) <$> Codebase.branchHashLength <*> Codebase.getDeprecatedRootReflog numEntriesToShow
              let moreEntriesToLoad = length entries == numEntriesToShow
              let expandedEntries = List.unfoldr expandEntries (entries, Nothing, moreEntriesToLoad)
              let (shortEntries, numberedEntries) =
                    unzip $
                      expandedEntries <&> \(time, hash, reason) ->
                        let (exp, sa) = (SCH.fromHash schLength &&& SA.Namespace) hash
                         in ((time, exp, reason), sa)
              Cli.setNumberedArgs numberedEntries
              Cli.respond $ ShowReflog shortEntries
              where
                expandEntries ::
                  ([Reflog.Entry CausalHash Text], Maybe CausalHash, Bool) ->
                  Maybe ((Maybe UTCTime, CausalHash, Text), ([Reflog.Entry CausalHash Text], Maybe CausalHash, Bool))
                expandEntries ([], Just expectedHash, moreEntriesToLoad) =
                  if moreEntriesToLoad
                    then Nothing
                    else Just ((Nothing, expectedHash, "history starts here"), ([], Nothing, moreEntriesToLoad))
                expandEntries ([], Nothing, _moreEntriesToLoad) = Nothing
                expandEntries (entries@(Reflog.Entry {time, fromRootCausalHash, toRootCausalHash, reason} : rest), mayExpectedHash, moreEntriesToLoad) =
                  Just $
                    case mayExpectedHash of
                      Just expectedHash
                        | expectedHash == toRootCausalHash -> ((Just time, toRootCausalHash, reason), (rest, Just fromRootCausalHash, moreEntriesToLoad))
                        -- Historical discontinuity, insert a synthetic entry
                        | otherwise -> ((Nothing, toRootCausalHash, "(external change)"), (entries, Nothing, moreEntriesToLoad))
                      -- No expectation, either because this is the most recent entry or
                      -- because we're recovering from a discontinuity
                      Nothing -> ((Just time, toRootCausalHash, reason), (rest, Just fromRootCausalHash, moreEntriesToLoad))
            ShowProjectBranchReflogI mayProjBranch -> do
              Reflogs.showProjectBranchReflog mayProjBranch
            ShowGlobalReflogI -> do
              Reflogs.showGlobalReflog
            ShowProjectReflogI mayProj -> do
              Reflogs.showProjectReflog mayProj
            ResetI newRoot mtarget -> do
              newRoot <- resolveBranchId2 newRoot
              target <-
                case mtarget of
                  Nothing -> Cli.getCurrentProjectPath
                  Just unresolvedProjectAndBranch -> do
                    targetProjectAndBranch <- ProjectUtils.resolveProjectBranch (second Just unresolvedProjectAndBranch)
                    pure $ PP.projectBranchRoot targetProjectAndBranch
              description <- inputDescription input
              _ <- Cli.updateAt description target (const newRoot)
              Cli.respond Success
            ForkLocalBranchI src0 dest0 -> do
              (srcb, branchEmpty) <-
                case src0 of
                  Left hash -> (,WhichBranchEmptyHash hash) <$> Cli.resolveShortCausalHash hash
                  Right path' -> do
                    srcPP <- ProjectUtils.resolveBranchRelativePath path'
                    srcb <- Cli.getBranchFromProjectPath srcPP
                    pure (srcb, WhichBranchEmptyPath srcPP)
              description <- inputDescription input
              dest <- ProjectUtils.resolveBranchRelativePath dest0
              ok <- Cli.updateAtM description dest (const $ pure srcb)
              Cli.respond
                if ok
                  then Success
                  else BranchEmpty branchEmpty
            MergeI branch -> handleMerge branch
            MergeCommitI -> handleCommitMerge
            MergeLocalBranchI unresolvedSrc mayUnresolvedDest mergeMode -> do
              description <- inputDescription input
              srcPP <- ProjectUtils.resolveBranchRelativePath unresolvedSrc
              (destPP, destBRP) <- case mayUnresolvedDest of
                Nothing -> Cli.getCurrentProjectPath <&> \pp -> (pp, QualifiedBranchPath (pp ^. #project . #name) (pp ^. #branch . #name) (pp ^. PP.absPath_))
                Just unresolvedDest -> do
                  ProjectUtils.resolveBranchRelativePath unresolvedDest <&> \pp -> (pp, unresolvedDest)
              srcBranch <- Cli.getProjectBranchRoot srcPP.branch
              let err = Just $ MergeAlreadyUpToDate unresolvedSrc destBRP
              mergeBranchAndPropagateDefaultPatch mergeMode description err srcBranch (Just $ Left destPP) destPP
            PreviewMergeLocalBranchI unresolvedSrc mayUnresolvedDest -> do
              Cli.Env {codebase} <- ask
              srcPP <- ProjectUtils.resolveBranchRelativePath unresolvedSrc
              destPP <- case mayUnresolvedDest of
                Nothing -> Cli.getCurrentProjectPath
                Just unresolvedDest -> do
                  ProjectUtils.resolveBranchRelativePath unresolvedDest
              srcBranch <- Cli.getProjectBranchRoot srcPP.branch
              destBranch <- Cli.getProjectBranchRoot destPP.branch
              merged <- liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge srcBranch destBranch)
              if merged == destBranch
                then Cli.respond (PreviewMergeAlreadyUpToDate srcPP destPP)
                else do
                  (ppe, diff) <- diffHelper (Branch.head destBranch) (Branch.head merged)
                  Cli.respondNumbered (ShowDiffAfterMergePreview (Left destPP) destPP ppe diff)
            DiffNamespaceI before after -> do
              beforeLoc <- traverse ProjectUtils.resolveBranchRelativePath before
              beforeBranch0 <- Branch.head <$> resolveBranchId2 before
              afterLoc <- traverse ProjectUtils.resolveBranchRelativePath after
              afterBranch0 <- Branch.head <$> resolveBranchId2 after
              case (Branch.isEmpty0 beforeBranch0, Branch.isEmpty0 afterBranch0) of
                (True, True) -> Cli.returnEarly . NamespaceEmpty $ (beforeLoc Nel.:| [afterLoc])
                (True, False) -> Cli.returnEarly . NamespaceEmpty $ (beforeLoc Nel.:| [])
                (False, True) -> Cli.returnEarly . NamespaceEmpty $ (afterLoc Nel.:| [])
                (False, False) -> pure ()
              (ppe, diff) <- diffHelper beforeBranch0 afterBranch0
              Cli.respondNumbered (ShowDiffNamespace beforeLoc afterLoc ppe diff)
            MoveBranchI src' dest' -> do
              hasConfirmed <- confirmedCommand input
              description <- inputDescription input
              doMoveBranch description hasConfirmed src' dest'
            SwitchBranchI path' -> do
              path <- Cli.resolvePath' path'
              branchExists <- Cli.branchExistsAtPath' path'
              when (not branchExists) (Cli.respond $ CreatedNewBranch (path ^. PP.absPath_))
              Cli.cd (path ^. PP.absPath_)
            UpI -> do
              path0 <- Cli.getCurrentPath
              whenJust (unsnoc path0) \(path, _) ->
                Cli.cd path
            PopBranchI -> do
              success <- Cli.popd
              when (not success) (Cli.respond StartOfCurrentPathHistory)
            HistoryI resultsCap diffCap from -> do
              branch <-
                case from of
                  BranchAtSCH hash -> Cli.resolveShortCausalHash hash
                  BranchAtPath path' -> do
                    pp <- Cli.resolvePath' path'
                    Cli.getBranchFromProjectPath pp
                  BranchAtProjectPath pp -> Cli.getBranchFromProjectPath pp
              schLength <- Cli.runTransaction Codebase.branchHashLength
              history <- liftIO (doHistory schLength 0 branch [])
              Cli.respondNumbered history
              where
                doHistory :: Int -> Int -> Branch IO -> [(CausalHash, Names.Diff)] -> IO NumberedOutput
                doHistory schLength !n b acc =
                  if maybe False (n >=) resultsCap
                    then pure (History diffCap schLength acc (PageEnd (Branch.headHash b) n))
                    else case Branch._history b of
                      Causal.One {} -> pure (History diffCap schLength acc (EndOfLog $ Branch.headHash b))
                      Causal.Merge _ _ _ tails ->
                        pure (History diffCap schLength acc (MergeTail (Branch.headHash b) $ Map.keys tails))
                      Causal.Cons _ _ _ tail -> do
                        b' <- fmap Branch.Branch $ snd tail
                        let elem = (Branch.headHash b, Branch.namesDiff b' b)
                        doHistory schLength (n + 1) b' (elem : acc)
            UndoI -> do
              rootBranch <- Cli.getCurrentProjectRoot
              (_, prev) <-
                liftIO (Branch.uncons rootBranch) & onNothingM do
                  Cli.returnEarly . CantUndo $
                    if Branch.isOne rootBranch
                      then CantUndoPastStart
                      else CantUndoPastMerge
              description <- inputDescription input
              pb <- getCurrentProjectBranch
              Cli.updateProjectBranchRoot_ pb description (const prev)
              (ppe, diff) <- diffHelper (Branch.head prev) (Branch.head rootBranch)
              Cli.respondNumbered (Output.ShowDiffAfterUndo ppe diff)
            UiI path' -> openUI path'
            DocToMarkdownI docName -> do
              names <- Cli.currentNames
              let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
              Cli.Env {codebase, runtime} <- ask
              docRefs <- Cli.runTransaction do
                hqLength <- Codebase.hashLength
                let nameSearch = NameSearch.makeNameSearch hqLength names
                Backend.docsForDefinitionName codebase nameSearch Names.IncludeSuffixes docName
              mdText <- liftIO $ do
                for docRefs \docRef -> do
                  Identity (_, _, doc, _evalErrs) <- Backend.renderDocRefs pped (Pretty.Width 80) codebase runtime (Identity docRef)
                  pure . Md.toText $ Md.toMarkdown doc
              Cli.respond $ Output.MarkdownOut (Text.intercalate "\n---\n" mdText)
            DocsToHtmlI namespacePath' sourceDirectory -> do
              Cli.Env {codebase, sandboxedRuntime} <- ask
              projPath <- ProjectUtils.resolveBranchRelativePath namespacePath'
              branch <- Cli.getBranchFromProjectPath projPath
              _evalErrs <- liftIO $ (Backend.docsInBranchToHtmlFiles sandboxedRuntime codebase branch sourceDirectory)
              pure ()
            AliasTermI force src' dest' -> do
              Cli.Env {codebase} <- ask
              src <- traverseOf _Right Cli.resolveSplit' src'
              srcTerms <-
                either
                  (Cli.runTransaction . Backend.termReferentsByShortHash codebase)
                  Cli.getTermsAt
                  src
              srcTerm <-
                Set.asSingleton srcTerms & onNothing do
                  Cli.returnEarly =<< case (Set.null srcTerms, src') of
                    (True, Left hash) -> pure (TermNotFound' hash)
                    (True, Right name) -> pure (TermNotFound name)
                    (False, Left hash) -> pure (HashAmbiguous hash srcTerms)
                    (False, Right name) -> do
                      hqLength <- Cli.runTransaction Codebase.hashLength
                      pure (DeleteNameAmbiguous hqLength name srcTerms Set.empty)
              dest <- Cli.resolveSplit' dest'
              destTerms <- Cli.getTermsAt (HQ'.NameOnly <$> dest)
              when (not force && not (Set.null destTerms)) do
                Cli.returnEarly (TermAlreadyExists dest' destTerms)
              description <- inputDescription input
              Cli.stepAt description (BranchUtil.makeAddTermName dest srcTerm)
              Cli.respond Success
            AliasTypeI force src' dest' -> do
              src <- traverseOf _Right Cli.resolveSplit' src'
              srcTypes <-
                either
                  (Cli.runTransaction . Backend.typeReferencesByShortHash)
                  Cli.getTypesAt
                  src
              srcType <-
                Set.asSingleton srcTypes & onNothing do
                  Cli.returnEarly =<< case (Set.null srcTypes, src') of
                    (True, Left hash) -> pure (TypeNotFound' hash)
                    (True, Right name) -> pure (TypeNotFound name)
                    (False, Left hash) -> pure (HashAmbiguous hash (Set.map Referent.Ref srcTypes))
                    (False, Right name) -> do
                      hqLength <- Cli.runTransaction Codebase.hashLength
                      pure (DeleteNameAmbiguous hqLength name Set.empty srcTypes)
              dest <- Cli.resolveSplit' dest'
              destTypes <- Cli.getTypesAt (HQ'.NameOnly <$> dest)
              when (not force && not (Set.null destTypes)) do
                Cli.returnEarly (TypeAlreadyExists dest' destTypes)
              description <- inputDescription input
              Cli.stepAt description (BranchUtil.makeAddTypeName dest srcType)
              Cli.respond Success

            -- this implementation will happily produce name conflicts,
            -- but will surface them in a normal diff at the end of the operation.
            AliasManyI srcs dest' -> do
              root0 <- Cli.getCurrentProjectRoot0
              currentBranch0 <- Cli.getCurrentBranch0
              destPP <- Cli.resolvePath' dest'
              old <- Cli.getBranch0FromProjectPath destPP
              description <- inputDescription input
              let (unknown, actions) = foldl' (go root0 currentBranch0 (PP.absPath destPP)) mempty srcs
              Cli.stepManyAt destPP.branch description actions
              new <- Cli.getBranch0FromProjectPath destPP
              (ppe, diff) <- diffHelper old new
              Cli.respondNumbered (ShowDiffAfterModifyBranch dest' (destPP.absPath) ppe diff)
              when (not (null unknown)) do
                Cli.respond . SearchTermsNotFound . fmap fixupOutput $ unknown
              where
                -- a list of missing sources (if any) and the actions that do the work
                go ::
                  Branch0 IO ->
                  Branch0 IO ->
                  Path.Absolute ->
                  ([Path.HQSplit], [(Path.Absolute, Branch0 m -> Branch0 m)]) ->
                  Path.HQSplit ->
                  ([Path.HQSplit], [(Path.Absolute, Branch0 m -> Branch0 m)])
                go root0 currentBranch0 dest (missingSrcs, actions) hqsrc =
                  let proposedDest :: Path.AbsSplit
                      proposedDest = second HQ'.toName hqProposedDest
                      hqProposedDest :: Path.HQSplitAbsolute
                      hqProposedDest = Path.resolve dest hqsrc
                      -- `Nothing` if src doesn't exist
                      doType :: Maybe [(Path.Absolute, Branch0 m -> Branch0 m)]
                      doType = case ( BranchUtil.getType hqsrc currentBranch0,
                                      BranchUtil.getType (first Path.unabsolute hqProposedDest) root0
                                    ) of
                        (null -> True, _) -> Nothing -- missing src
                        (rsrcs, existing) ->
                          -- happy path
                          Just . map addAlias . toList $ Set.difference rsrcs existing
                          where
                            addAlias :: Reference -> (Path.Absolute, Branch0 m -> Branch0 m)
                            addAlias r = BranchUtil.makeAddTypeName proposedDest r
                      doTerm :: Maybe [(Path.Absolute, Branch0 m -> Branch0 m)]
                      doTerm = case ( BranchUtil.getTerm hqsrc currentBranch0,
                                      BranchUtil.getTerm (first Path.unabsolute hqProposedDest) root0
                                    ) of
                        (null -> True, _) -> Nothing -- missing src
                        (rsrcs, existing) ->
                          Just . map addAlias . toList $ Set.difference rsrcs existing
                          where
                            addAlias r = BranchUtil.makeAddTermName proposedDest r
                   in case (doType, doTerm) of
                        (Nothing, Nothing) -> (missingSrcs :> hqsrc, actions)
                        (Just as, Nothing) -> (missingSrcs, actions ++ as)
                        (Nothing, Just as) -> (missingSrcs, actions ++ as)
                        (Just as1, Just as2) -> (missingSrcs, actions ++ as1 ++ as2)

                fixupOutput :: Path.HQSplit -> HQ.HashQualified Name
                fixupOutput = HQ'.toHQ . Path.nameFromHQSplit
            NamesI global query -> do
              hqLength <- Cli.runTransaction Codebase.hashLength
              let searchNames names = do
                    let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
                        unsuffixifiedPPE = PPED.unsuffixifiedPPE pped
                        terms = Names.lookupHQTerm Names.IncludeSuffixes query names
                        types = Names.lookupHQType Names.IncludeSuffixes query names
                        terms' :: [(Referent, [HQ'.HashQualified Name])]
                        terms' = map (\r -> (r, PPE.allTermNames unsuffixifiedPPE r)) (Set.toList terms)
                        types' :: [(Reference, [HQ'.HashQualified Name])]
                        types' = map (\r -> (r, PPE.allTypeNames unsuffixifiedPPE r)) (Set.toList types)
                    pure (terms', types')
              if global
                then do
                  Global.forAllProjectBranches \(projBranchNames, _ids) branch -> do
                    let names = Branch.toNames . Branch.head $ branch
                    (terms, types) <- searchNames names
                    when (not (null terms) || not (null types)) do
                      Cli.respond $ GlobalListNames projBranchNames hqLength types terms
                else do
                  names <- Cli.currentNames
                  (terms, types) <- searchNames names
                  Cli.respond $ ListNames hqLength types terms
            DocsI srcs -> do
              for_ srcs docsI
            CreateAuthorI authorNameSegment authorFullName -> do
              Cli.Env {codebase} <- ask
              initialBranch <- Cli.getCurrentBranch
              AuthorInfo
                guid@(guidRef, _, _)
                author@(authorRef, _, _)
                copyrightHolder@(copyrightHolderRef, _, _) <-
                AuthorInfo.createAuthorInfo Ann.External authorFullName
              description <- inputDescription input
              -- add the new definitions to the codebase and to the namespace
              Cli.runTransaction (traverse_ (uncurry3 (Codebase.putTerm codebase)) [guid, author, copyrightHolder])
              authorPath <- Cli.resolveSplit' authorPath'
              copyrightHolderPath <- Cli.resolveSplit' (base |> NameSegment.copyrightHoldersSegment |> authorNameSegment)
              guidPath <- Cli.resolveSplit' (authorPath' |> NameSegment.guidSegment)
              pb <- Cli.getCurrentProjectBranch
              Cli.stepManyAt
                pb
                description
                [ BranchUtil.makeAddTermName (first PP.absPath authorPath) (d authorRef),
                  BranchUtil.makeAddTermName (first PP.absPath copyrightHolderPath) (d copyrightHolderRef),
                  BranchUtil.makeAddTermName (first PP.absPath guidPath) (d guidRef)
                ]
              currentPath <- Cli.getCurrentPath
              finalBranch <- Cli.getCurrentBranch0
              (ppe, diff) <- diffHelper (Branch.head initialBranch) finalBranch
              Cli.respondNumbered $
                ShowDiffAfterCreateAuthor
                  authorNameSegment
                  (Path.unsplit' base)
                  currentPath
                  ppe
                  diff
              where
                d :: Reference.Id -> Referent
                d = Referent.Ref . Reference.DerivedId
                base :: Path.Split' = (Path.relativeEmpty', NameSegment.metadataSegment)
                authorPath' = base |> NameSegment.authorsSegment |> authorNameSegment
            MoveTermI src' dest' -> doMoveTerm src' dest' =<< inputDescription input
            MoveTypeI src' dest' -> doMoveType src' dest' =<< inputDescription input
            MoveAllI src' dest' -> do
              hasConfirmed <- confirmedCommand input
              desc <- inputDescription input
              handleMoveAll hasConfirmed src' dest' desc
            DeleteI dtarget -> do
              pp <- Cli.getCurrentProjectPath
              let getTerms (absPath, seg) = Cli.getTermsAt (set PP.absPath_ absPath pp, seg)
              let getTypes (absPath, seg) = Cli.getTypesAt (set PP.absPath_ absPath pp, seg)
              case dtarget of
                DeleteTarget'TermOrType doutput hqs -> do
                  delete input doutput getTerms getTypes hqs
                DeleteTarget'Type doutput hqs -> delete input doutput (const (pure Set.empty)) getTypes hqs
                DeleteTarget'Term doutput hqs -> delete input doutput getTerms (const (pure Set.empty)) hqs
                DeleteTarget'Namespace insistence Nothing -> do
                  hasConfirmed <- confirmedCommand input
                  if hasConfirmed || insistence == Force
                    then do
                      description <- inputDescription input
                      pp <- Cli.getCurrentProjectPath
                      _ <- Cli.updateAt description pp (const Branch.empty)
                      Cli.respond DeletedEverything
                    else Cli.respond DeleteEverythingConfirmation
                DeleteTarget'Namespace insistence (Just p@(parentPath, childName)) -> do
                  branch <- Cli.expectBranchAtPath (Path.unsplit p)
                  description <- inputDescription input
                  let toDelete =
                        Names.prefix0
                          (Path.nameFromSplit' $ first (Path.RelativePath' . Path.Relative) p)
                          (Branch.toNames (Branch.head branch))
                  afterDelete <- do
                    names <- Cli.currentNames
                    endangerments <- Cli.runTransaction (getEndangeredDependents toDelete Set.empty names)
                    case (null endangerments, insistence) of
                      (True, _) -> pure (Cli.respond Success)
                      (False, Force) -> do
                        let ppeDecl = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
                        pure do
                          Cli.respond Success
                          Cli.respondNumbered $ DeletedDespiteDependents ppeDecl endangerments
                      (False, Try) -> do
                        let ppeDecl = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
                        Cli.respondNumbered $ CantDeleteNamespace ppeDecl endangerments
                        Cli.returnEarlyWithoutOutput
                  parentPathAbs <- Cli.resolvePath parentPath
                  -- We have to modify the parent in order to also wipe out the history at the
                  -- child.
                  Cli.updateAt description parentPathAbs \parentBranch ->
                    parentBranch
                      & Branch.modifyAt (Path.singleton childName) \_ -> Branch.empty
                  afterDelete
                DeleteTarget'ProjectBranch name -> handleDeleteBranch name
                DeleteTarget'Project name -> handleDeleteProject name
            DisplayI outputLoc namesToDisplay -> do
              traverse_ (displayI outputLoc) namesToDisplay
            ShowDefinitionI outputLoc showDefinitionScope query -> handleShowDefinition outputLoc showDefinitionScope query
            EditNamespaceI paths -> handleEditNamespace (LatestFileLocation AboveFold) paths
            FindShallowI pathArg -> handleLs pathArg
            FindI isVerbose fscope ws -> handleFindI isVerbose fscope ws input
            StructuredFindI _fscope ws -> handleStructuredFindI ws
            StructuredFindReplaceI ws -> handleStructuredFindReplaceI ws
            TextFindI allowLib ws -> handleTextFindI allowLib ws
            LoadI maybePath -> handleLoad maybePath
            ClearI -> Cli.respond ClearScreen
            AddI requestedNames -> do
              description <- inputDescription input
              let vars = Set.map Name.toVar requestedNames
              uf <- Cli.expectLatestTypecheckedFile
              Cli.Env {codebase} <- ask
              currentNames <- Branch.toNames <$> Cli.getCurrentBranch0
              let sr = Slurp.slurpFile uf vars Slurp.AddOp currentNames
              let adds = SlurpResult.adds sr
              Cli.runTransaction . Codebase.addDefsToCodebase codebase . SlurpResult.filterUnisonFile sr $ uf
              pp <- Cli.getCurrentProjectPath
              Cli.stepAt description (pp, doSlurpAdds adds uf)
              let pped =
                    let names = UF.addNamesFromTypeCheckedUnisonFile uf currentNames
                     in PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
              let suffixifiedPPE = PPED.suffixifiedPPE pped
              Cli.respond $ SlurpOutput input suffixifiedPPE sr
            SaveExecuteResultI resultName -> handleAddRun input resultName
            PreviewAddI requestedNames -> do
              (sourceName, _) <- Cli.expectLatestFile
              uf <- Cli.expectLatestTypecheckedFile
              let vars = Set.map Name.toVar requestedNames
              currentNames <- Branch.toNames <$> Cli.getCurrentBranch0
              let sr = Slurp.slurpFile uf vars Slurp.AddOp currentNames
              previewResponse sourceName sr uf
            UpdateI optionalPatch requestedNames -> handleUpdate input optionalPatch requestedNames
            Update2I -> handleUpdate2
            PreviewUpdateI requestedNames -> do
              (sourceName, _) <- Cli.expectLatestFile
              uf <- Cli.expectLatestTypecheckedFile
              let vars = Set.map Name.toVar requestedNames
              currentNames <- Branch.toNames <$> Cli.getCurrentBranch0
              let sr = Slurp.slurpFile uf vars Slurp.UpdateOp currentNames
              previewResponse sourceName sr uf
            TodoI -> handleTodo
            TestI testInput -> Tests.handleTest testInput
            ExecuteI main args -> handleRun False main args
            MakeStandaloneI output main ->
              doCompile False False output main
            CompileSchemeI prof output main ->
              doCompile prof True (Text.unpack output) main
            ExecuteSchemeI main args -> handleRun True main args
            IOTestI main -> Tests.handleIOTest main
            IOTestAllI -> Tests.handleAllIOTests
            -- UpdateBuiltinsI -> do
            --   stepAt updateBuiltins
            --   checkTodo

            MergeBuiltinsI opath -> do
              Cli.Env {codebase} <- ask
              description <- inputDescription input
              -- these were added once, but maybe they've changed and need to be
              -- added again.
              let uf =
                    UF.typecheckedUnisonFile
                      (Map.fromList Builtin.builtinDataDecls)
                      (Map.fromList Builtin.builtinEffectDecls)
                      [Builtin.builtinTermsSrc Intrinsic]
                      mempty
              Cli.runTransaction (Codebase.addDefsToCodebase codebase uf)
              -- add the names; note, there are more names than definitions
              -- due to builtin terms; so we don't just reuse `uf` above.
              let srcb = BranchUtil.fromNames Builtin.names
              currentPath <- Cli.getCurrentPath
              let destPath = case opath of
                    Just path -> Path.resolve currentPath (Path.Relative path)
                    Nothing -> currentPath `snoc` NameSegment.builtinSegment
              pp <- set PP.absPath_ destPath <$> Cli.getCurrentProjectPath
              _ <- Cli.updateAtM description pp \destb ->
                liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge srcb destb)
              Cli.respond Success
            MergeIOBuiltinsI opath -> do
              Cli.Env {codebase} <- ask
              description <- inputDescription input
              -- these were added once, but maybe they've changed and need to be
              -- added again.
              let uf =
                    UF.typecheckedUnisonFile
                      (Map.fromList Builtin.builtinDataDecls)
                      (Map.fromList Builtin.builtinEffectDecls)
                      [Builtin.builtinTermsSrc Intrinsic]
                      mempty
              Cli.runTransaction do
                Codebase.addDefsToCodebase codebase uf
                -- these have not necessarily been added yet
                Codebase.addDefsToCodebase codebase IOSource.typecheckedFile'

              -- add the names; note, there are more names than definitions
              -- due to builtin terms; so we don't just reuse `uf` above.
              let names0 = Builtin.names <> UF.typecheckedToNames IOSource.typecheckedFile'
              let srcb = BranchUtil.fromNames names0
              currentPath <- Cli.getCurrentPath
              let destPath = case opath of
                    Just path -> Path.resolve currentPath (Path.Relative path)
                    Nothing -> currentPath `snoc` NameSegment.builtinSegment
              pp <- set PP.absPath_ destPath <$> Cli.getCurrentProjectPath
              _ <- Cli.updateAtM description pp \destb ->
                liftIO (Branch.merge'' (Codebase.lca codebase) Branch.RegularMerge srcb destb)
              Cli.respond Success
            PullI sourceTarget pullMode -> handlePull sourceTarget pullMode
            PushRemoteBranchI pushRemoteBranchInput -> handlePushRemoteBranch pushRemoteBranchInput
            ListDependentsI hq -> handleDependents hq
            ListDependenciesI hq -> handleDependencies hq
            NamespaceDependenciesI path -> handleNamespaceDependencies path
            DebugNumberedArgsI -> do
              schLength <- Cli.runTransaction Codebase.branchHashLength
              numArgs <- use #numberedArgs
              Cli.respond (DumpNumberedArgs schLength numArgs)
            DebugTypecheckedUnisonFileI -> do
              hqLength <- Cli.runTransaction Codebase.hashLength
              uf <- Cli.expectLatestTypecheckedFile
              let datas, effects, terms :: [(Name, Reference.Id)]
                  datas = [(Name.unsafeParseVar v, r) | (v, (r, _d)) <- Map.toList $ UF.dataDeclarationsId' uf]
                  effects = [(Name.unsafeParseVar v, r) | (v, (r, _e)) <- Map.toList $ UF.effectDeclarationsId' uf]
                  terms = [(Name.unsafeParseVar v, r) | (v, (_, r, _wk, _tm, _tp)) <- Map.toList $ UF.hashTermsId uf]
              Cli.respond $ DumpUnisonFileHashes hqLength datas effects terms
            DebugTabCompletionI inputs -> do
              Cli.Env {authHTTPClient, codebase} <- ask
              pp <- Cli.getCurrentProjectPath
              let completionFunc = Completion.haskelineTabComplete IP.patternMap codebase authHTTPClient pp
              (_, completions) <- liftIO $ completionFunc (reverse (unwords inputs), "")
              Cli.respond (DisplayDebugCompletions completions)
            DebugLSPNameCompletionI prefix -> do
              LSPDebug.debugLspNameCompletion prefix
            DebugFuzzyOptionsI command args -> do
              Cli.Env {codebase} <- ask
              currentBranch <- Branch.withoutTransitiveLibs <$> Cli.getCurrentBranch0
              case Map.lookup command InputPatterns.patternMap of
                Just (IP.InputPattern {args = argTypes}) -> do
                  zip argTypes args & Monoid.foldMapM \case
                    ((argName, _, IP.ArgumentType {fzfResolver = Just IP.FZFResolver {getOptions}}), "_") -> do
                      pp <- Cli.getCurrentProjectPath
                      results <- liftIO $ getOptions codebase pp currentBranch
                      Cli.respond (DebugDisplayFuzzyOptions argName (Text.unpack <$> results))
                    ((_, _, IP.ArgumentType {fzfResolver = Nothing}), "_") -> do
                      Cli.respond DebugFuzzyOptionsNoResolver
                    _ -> pure ()
                Nothing -> do
                  Cli.respond DebugFuzzyOptionsNoResolver
            DebugFormatI -> do
              env <- ask
              void $ runMaybeT do
                (filePath, _) <- MaybeT Cli.getLatestFile
                pf <- lift Cli.getLatestParsedFile
                tf <- lift Cli.getLatestTypecheckedFile
                names <- lift Cli.currentNames
                let buildPPED uf tf =
                      let names' = (fromMaybe mempty $ (UF.typecheckedToNames <$> tf) <|> (UF.toNames <$> uf)) `Names.shadowing` names
                       in pure (PPED.makePPED (PPE.hqNamer 10 names') (PPE.suffixifyByHashName names'))
                let formatWidth = 80
                currentPath <- lift $ Cli.getCurrentPath
                updates <- MaybeT $ Format.formatFile buildPPED formatWidth currentPath pf tf Nothing
                source <-
                  liftIO (env.loadSource (Text.pack filePath)) >>= \case
                    Cli.InvalidSourceNameError -> lift $ Cli.returnEarly $ Output.InvalidSourceName filePath
                    Cli.LoadError -> lift $ Cli.returnEarly $ Output.SourceLoadFailed filePath
                    Cli.LoadSuccess contents -> pure contents
                let updatedSource = Format.applyTextReplacements updates source
                liftIO $ env.writeSource (Text.pack filePath) updatedSource True
            DebugDumpNamespacesI -> do
              let seen h = State.gets (Set.member h)
                  set h = State.modify (Set.insert h)
                  getCausal b = (Branch.headHash b, pure $ Branch._history b)
                  goCausal :: forall m. (Monad m) => [(CausalHash, m (Branch.UnwrappedBranch m))] -> StateT (Set CausalHash) m ()
                  goCausal [] = pure ()
                  goCausal ((h, mc) : queue) = do
                    ifM (seen h) (goCausal queue) do
                      lift mc >>= \case
                        Causal.One h _bh b -> goBranch h b mempty queue
                        Causal.Cons h _bh b tail -> goBranch h b [fst tail] (tail : queue)
                        Causal.Merge h _bh b (Map.toList -> tails) -> goBranch h b (map fst tails) (tails ++ queue)
                  goBranch :: forall m. (Monad m) => CausalHash -> Branch0 m -> [CausalHash] -> [(CausalHash, m (Branch.UnwrappedBranch m))] -> StateT (Set CausalHash) m ()
                  goBranch h b (Set.fromList -> causalParents) queue =
                    let ignoreMetadata :: (Ord r, Ord n) => Metadata.Star r n -> r -> (r, Set n)
                        ignoreMetadata s r =
                          (r, R.lookupDom r $ Star2.d1 s)
                        terms = Map.fromList . map (ignoreMetadata (b ^. Branch.terms)) . Foldable.toList $ Star2.fact (b ^. Branch.terms)
                        types = Map.fromList . map (ignoreMetadata (b ^. Branch.types)) . Foldable.toList $ Star2.fact (b ^. Branch.types)
                        patches = fmap fst (b ^. Branch.edits)
                        children = fmap Branch.headHash (b ^. Branch.children)
                     in do
                          let d = Output.DN.DumpNamespace terms types patches children causalParents
                          -- the alternate implementation that doesn't rely on `traceM` blows up
                          traceM $ P.toPlain 200 (prettyDump (h, d))
                          set h
                          goCausal (map getCausal (Foldable.toList (b ^. Branch.children)) ++ queue)
                  prettyDump (h, Output.DN.DumpNamespace terms types patches children causalParents) =
                    P.lit "Namespace "
                      <> P.shown h
                      <> P.newline
                      <> ( P.indentN 2 $
                             P.linesNonEmpty
                               [ Monoid.unlessM (null causalParents) $ P.lit "Causal Parents:" <> P.newline <> P.indentN 2 (P.lines (map P.shown $ Set.toList causalParents)),
                                 Monoid.unlessM (null terms) $ P.lit "Terms:" <> P.newline <> P.indentN 2 (P.lines (map (prettyDefn Referent.toText) $ Map.toList terms)),
                                 Monoid.unlessM (null types) $ P.lit "Types:" <> P.newline <> P.indentN 2 (P.lines (map (prettyDefn Reference.toText) $ Map.toList types)),
                                 Monoid.unlessM (null patches) $ P.lit "Patches:" <> P.newline <> P.indentN 2 (P.column2 (map (bimap (P.text . NameSegment.toEscapedText) P.shown) $ Map.toList patches)),
                                 Monoid.unlessM (null children) $ P.lit "Children:" <> P.newline <> P.indentN 2 (P.column2 (map (bimap (P.text . NameSegment.toEscapedText) P.shown) $ Map.toList children))
                               ]
                         )
                    where
                      prettyRef renderR r = P.indentN 2 $ P.text (renderR r)
                      prettyDefn renderR (r, Foldable.toList -> names) =
                        P.lines (P.text <$> if null names then ["<unnamed>"] else NameSegment.toEscapedText <$> names) <> P.newline <> prettyRef renderR r
              projectRoot <- Cli.getCurrentProjectRoot
              void . liftIO . flip State.execStateT mempty $ goCausal [getCausal projectRoot]
            DebugDumpNamespaceSimpleI -> do
              projectRootBranch0 <- Cli.getCurrentProjectRoot0
              for_ (Relation.toList . Branch.deepTypes $ projectRootBranch0) \(r, name) ->
                traceM $ show name ++ ",Type," ++ Text.unpack (Reference.toText r)
              for_ (Relation.toList . Branch.deepTerms $ projectRootBranch0) \(r, name) ->
                traceM $ show name ++ ",Term," ++ Text.unpack (Referent.toText r)
            DebugTermI isVerbose hqName -> DebugDefinition.debugTerm isVerbose hqName
            DebugLSPFoldRangesI -> do
              DebugFoldRanges.debugFoldRanges
            DebugTypeI hqName -> DebugDefinition.debugDecl hqName
            DebugClearWatchI {} ->
              Cli.runTransaction Codebase.clearWatches
            DebugDoctorI {} -> do
              r <- Cli.runTransaction IntegrityCheck.integrityCheckFullCodebase
              Cli.respond (IntegrityCheck r)
            DebugNameDiffI fromSCH toSCH -> do
              (schLen, fromCHs, toCHs) <-
                Cli.runTransaction do
                  schLen <- Codebase.branchHashLength
                  fromCHs <- Codebase.causalHashesByPrefix fromSCH
                  toCHs <- Codebase.causalHashesByPrefix toSCH
                  pure (schLen, fromCHs, toCHs)
              (fromCH, toCH) <- case (Set.toList fromCHs, Set.toList toCHs) of
                ((_ : _ : _), _) -> Cli.returnEarly $ Output.BranchHashAmbiguous fromSCH (Set.map (SCH.fromHash schLen) fromCHs)
                ([], _) -> Cli.returnEarly $ Output.NoBranchWithHash fromSCH
                (_, []) -> Cli.returnEarly $ Output.NoBranchWithHash toSCH
                (_, (_ : _ : _)) -> Cli.returnEarly $ Output.BranchHashAmbiguous toSCH (Set.map (SCH.fromHash schLen) toCHs)
                ([fromCH], [toCH]) -> pure (fromCH, toCH)
              output <-
                Cli.runTransaction do
                  fromBranch <- Codebase.expectCausalBranchByCausalHash fromCH >>= V2Causal.value
                  toBranch <- Codebase.expectCausalBranchByCausalHash toCH >>= V2Causal.value
                  treeDiff <- V2Branch.Diff.diffBranches fromBranch toBranch
                  nameChanges <- V2Branch.Diff.allNameChanges Nothing treeDiff
                  pure (DisplayDebugNameDiff nameChanges)
              Cli.respond output
            UpdateBuiltinsI -> Cli.respond NotImplemented
            QuitI -> Cli.haltRepl
            AuthLoginI -> void $ authLogin (Codeserver.resolveCodeserver RemoteRepo.DefaultCodeserver)
            VersionI -> do
              Cli.Env {ucmVersion} <- ask
              Cli.respond $ PrintVersion ucmVersion
            ProjectRenameI name -> handleProjectRename name
            ProjectSwitchI name -> projectSwitch name
            ProjectCreateI tryDownloadingBase name -> void $ projectCreate tryDownloadingBase name
            ProjectsI -> handleProjects
            BranchI source name -> handleBranch source name
            BranchRenameI name -> handleBranchRename name
            BranchesI name -> handleBranches name
            CloneI remoteNames localNames -> handleClone remoteNames localNames
            ReleaseDraftI semver -> handleReleaseDraft semver
            UpgradeI old new -> handleUpgrade old new
            UpgradeCommitI -> handleCommitUpgrade
            LibInstallI remind libdep -> handleInstallLib remind libdep
            DebugSynhashTermI name -> handleDebugSynhashTerm name

inputDescription :: Input -> Cli Text
inputDescription input =
  case input of
    SaveExecuteResultI _str -> pure "save-execute-result"
    ForkLocalBranchI src0 dest0 -> do
      src <- either (pure . Text.pack . show) brp src0
      dest <- brp dest0
      pure ("fork " <> src <> " " <> dest)
    MergeLocalBranchI src0 dest0 mode -> do
      let src = into @Text src0
      let dest = maybe "" (into @Text) dest0
      let command =
            case mode of
              Branch.RegularMerge -> "merge"
              Branch.SquashMerge -> "merge.squash"
      pure (command <> " " <> src <> " " <> dest)
    ResetI newRoot tgt -> do
      hashTxt <- bid2 newRoot
      tgt <- case tgt of
        Nothing -> pure ""
        Just tgt -> do
          let tgtText = into @Text tgt
          pure (" " <> tgtText)
      pure ("reset " <> hashTxt <> tgt)
    AliasTermI force src0 dest0 -> do
      src <- hhqs' src0
      dest <- ps' dest0
      pure ((if force then "debug.alias.term.force " else "alias.term ") <> src <> " " <> dest)
    AliasTypeI force src0 dest0 -> do
      src <- hhqs' src0
      dest <- ps' dest0
      pure ((if force then "debug.alias.type.force " else "alias.term ") <> src <> " " <> dest)
    AliasManyI srcs0 dest0 -> do
      srcs <- traverse hqs srcs0
      dest <- p' dest0
      pure ("alias.many " <> Text.intercalate " " srcs <> " " <> dest)
    MoveTermI src0 dest0 -> do
      src <- hqs' src0
      dest <- ps' dest0
      pure ("move.term " <> src <> " " <> dest)
    MoveTypeI src0 dest0 -> do
      src <- hqs' src0
      dest <- ps' dest0
      pure ("move.type " <> src <> " " <> dest)
    MoveBranchI src0 dest0 -> do
      src <- p' src0
      dest <- p' dest0
      pure ("move.namespace " <> src <> " " <> dest)
    MoveAllI src0 dest0 -> do
      src <- p' src0
      dest <- p' dest0
      pure ("move " <> src <> " " <> dest)
    DeleteI dtarget -> do
      case dtarget of
        DeleteTarget'TermOrType DeleteOutput'NoDiff things0 -> do
          thing <- traverse hqs' things0
          pure ("delete " <> Text.intercalate " " thing)
        DeleteTarget'TermOrType DeleteOutput'Diff things0 -> do
          thing <- traverse hqs' things0
          pure ("delete.verbose " <> Text.intercalate " " thing)
        DeleteTarget'Term DeleteOutput'NoDiff things0 -> do
          thing <- traverse hqs' things0
          pure ("delete.term " <> Text.intercalate " " thing)
        DeleteTarget'Term DeleteOutput'Diff things0 -> do
          thing <- traverse hqs' things0
          pure ("delete.term.verbose " <> Text.intercalate " " thing)
        DeleteTarget'Type DeleteOutput'NoDiff thing0 -> do
          thing <- traverse hqs' thing0
          pure ("delete.type " <> Text.intercalate " " thing)
        DeleteTarget'Type DeleteOutput'Diff thing0 -> do
          thing <- traverse hqs' thing0
          pure ("delete.type.verbose " <> Text.intercalate " " thing)
        DeleteTarget'Namespace Try opath0 -> do
          opath <- ops opath0
          pure ("delete.namespace " <> opath)
        DeleteTarget'Namespace Force opath0 -> do
          opath <- ops opath0
          pure ("delete.namespace.force " <> opath)
        DeleteTarget'ProjectBranch _ -> wat
        DeleteTarget'Project _ -> wat
    AddI _selection -> pure "add"
    UpdateI p0 _selection -> do
      p <-
        case p0 of
          NoPatch -> pure ".nopatch"
          DefaultPatch -> (" " <>) <$> ps' Cli.defaultPatchPath
          UsePatch p0 -> (" " <>) <$> ps' p0
      pure ("update.old" <> p)
    Update2I -> pure ("update")
    UndoI {} -> pure "undo"
    ExecuteI s args -> pure ("execute " <> Text.unwords (HQ.toText s : fmap Text.pack args))
    IOTestI hq -> pure ("io.test " <> HQ.toText hq)
    IOTestAllI -> pure "io.test.all"
    UpdateBuiltinsI -> pure "builtins.update"
    MergeBuiltinsI Nothing -> pure "builtins.merge"
    MergeBuiltinsI (Just path) -> ("builtins.merge " <>) <$> p path
    MergeIOBuiltinsI Nothing -> pure "builtins.mergeio"
    MergeIOBuiltinsI (Just path) -> ("builtins.mergeio " <>) <$> p path
    MakeStandaloneI out nm -> pure ("compile " <> Text.pack out <> " " <> HQ.toText nm)
    ExecuteSchemeI nm args ->
      pure $ "run.native " <> Text.unwords (HQ.toText nm : fmap Text.pack args)
    CompileSchemeI pr fi nm ->
      pure ("compile.native " <> HQ.toText nm <> " " <> fi <> if pr then " profile" else "")
    CreateAuthorI id name -> pure ("create.author " <> NameSegment.toEscapedText id <> " " <> name)
    ClearI {} -> pure "clear"
    DocToMarkdownI name -> pure ("debug.doc-to-markdown " <> Name.toText name)
    DebugTermI verbose hqName ->
      if verbose
        then pure ("debug.term.verbose " <> HQ.toText hqName)
        else pure ("debug.term " <> HQ.toText hqName)
    DebugTypeI hqName -> pure ("debug.type " <> HQ.toText hqName)
    DebugLSPFoldRangesI -> pure "debug.lsp.fold-ranges"
    DebugFuzzyOptionsI cmd input -> pure . Text.pack $ "debug.fuzzy-completions " <> unwords (cmd : toList input)
    DebugFormatI -> pure "debug.format"
    EditNamespaceI paths ->
      pure $ Text.unwords ("edit.namespace" : (Path.toText <$> paths))
    -- wat land
    ApiI -> wat
    AuthLoginI {} -> wat
    BranchI {} -> wat
    BranchRenameI {} -> wat
    BranchesI {} -> wat
    CloneI {} -> wat
    CreateMessage {} -> wat
    DebugClearWatchI {} -> wat
    DebugDoctorI {} -> wat
    DebugDumpNamespaceSimpleI {} -> wat
    DebugDumpNamespacesI {} -> wat
    DebugLSPNameCompletionI {} -> wat
    DebugNameDiffI {} -> wat
    DebugNumberedArgsI {} -> wat
    DebugSynhashTermI {} -> wat
    DebugTabCompletionI {} -> wat
    DebugTypecheckedUnisonFileI {} -> wat
    DiffNamespaceI {} -> wat
    DisplayI {} -> wat
    DocsI {} -> wat
    DocsToHtmlI {} -> wat
    FindI {} -> wat
    FindShallowI {} -> wat
    HistoryI {} -> wat
    LibInstallI {} -> wat
    ListDependenciesI {} -> wat
    ListDependentsI {} -> wat
    LoadI {} -> wat
    MergeCommitI {} -> wat
    MergeI {} -> wat
    NamesI {} -> wat
    NamespaceDependenciesI {} -> wat
    PopBranchI {} -> wat
    PreviewAddI {} -> wat
    PreviewMergeLocalBranchI {} -> wat
    PreviewUpdateI {} -> wat
    ProjectCreateI {} -> wat
    ProjectRenameI {} -> wat
    ProjectSwitchI {} -> wat
    ProjectsI -> wat
    PullI {} -> wat
    PushRemoteBranchI {} -> wat
    QuitI {} -> wat
    ReleaseDraftI {} -> wat
    ShowDefinitionI {} -> wat
    StructuredFindI {} -> wat
    StructuredFindReplaceI {} -> wat
    TextFindI {} -> wat
    ShowRootReflogI {} -> pure "deprecated.root-reflog"
    ShowGlobalReflogI {} -> pure "reflog.global"
    ShowProjectReflogI mayProjName -> do
      case mayProjName of
        Nothing -> pure "project.reflog"
        Just projName -> pure $ "project.reflog" <> into @Text projName
    ShowProjectBranchReflogI mayProjBranch -> do
      case mayProjBranch of
        Nothing -> pure "branch.reflog"
        Just (PP.ProjectAndBranch Nothing branchName) -> pure $ "branch.reflog" <> into @Text branchName
        Just (PP.ProjectAndBranch (Just projName) branchName) -> pure $ "branch.reflog" <> into @Text (PP.ProjectAndBranch projName branchName)
    SwitchBranchI {} -> wat
    TestI {} -> wat
    TodoI {} -> wat
    UiI {} -> wat
    UpI {} -> wat
    UpgradeCommitI {} -> wat
    UpgradeI {} -> wat
    VersionI -> wat
  where
    p :: Path -> Cli Text
    p = fmap (into @Text) . Cli.resolvePath
    p' :: Path' -> Cli Text
    p' = fmap (into @Text) . Cli.resolvePath'
    brp :: BranchRelativePath -> Cli Text
    brp = fmap (into @Text) . ProjectUtils.resolveBranchRelativePath
    ops :: Maybe Path.Split -> Cli Text
    ops = maybe (pure ".") ps
    wat = error $ show input ++ " is not expected to alter the branch"
    hhqs' :: Either SH.ShortHash Path.HQSplit' -> Cli Text
    hhqs' = \case
      Left sh -> pure (SH.toText sh)
      Right x -> hqs' x
    hqs' :: Path.HQSplit' -> Cli Text
    hqs' (p0, hq) = do
      p <- if Path.isRoot' p0 then pure mempty else p' p0
      pure (p <> "." <> HQ'.toTextWith NameSegment.toEscapedText hq)
    hqs (p, hq) = hqs' (Path' . Right . Path.Relative $ p, hq)
    ps' = p' . Path.unsplit'
    ps = p . Path.unsplit
    bid2 :: BranchId2 -> Cli Text
    bid2 = \case
      Left sch -> pure $ into @Text sch
      Right p -> brp p

handleFindI ::
  Bool ->
  FindScope ->
  [String] ->
  Input ->
  Cli ()
handleFindI isVerbose fscope ws input = do
  Cli.Env {codebase} <- ask
  case fscope of
    FindLocal p -> do
      searchRoot <- Cli.resolvePath' p
      branch0 <- Cli.getBranch0FromProjectPath searchRoot
      let names = Branch.toNames (Branch.withoutLib branch0)
      -- Don't exclude anything from the pretty printer, since the type signatures we print for
      -- results may contain things in lib.
      currentNames <- Cli.currentNames
      let pped = PPED.makePPED (PPE.hqNamer 10 currentNames) (PPE.suffixifyByHash currentNames)
      let suffixifiedPPE = PPED.suffixifiedPPE pped
      results <- searchBranch0 codebase branch0 names
      if (null results)
        then do
          Cli.respond FindNoLocalMatches
          -- We've already searched everything else, so now we search JUST the
          -- names in lib.
          let mayOnlyLibBranch = branch0 & Branch.children %%~ \cs -> Map.singleton NameSegment.libSegment <$> Map.lookup NameSegment.libSegment cs
          case mayOnlyLibBranch of
            Nothing -> respondResults codebase suffixifiedPPE (Just p) []
            Just onlyLibBranch -> do
              let onlyLibNames = Branch.toNames onlyLibBranch
              results <- searchBranch0 codebase branch0 onlyLibNames
              respondResults codebase suffixifiedPPE (Just p) results
        else respondResults codebase suffixifiedPPE (Just p) results
    FindLocalAndDeps p -> do
      searchRoot <- Cli.resolvePath' p
      branch0 <- Cli.getBranch0FromProjectPath searchRoot
      let names = Branch.toNames (Branch.withoutTransitiveLibs branch0)
      -- Don't exclude anything from the pretty printer, since the type signatures we print for
      -- results may contain things in lib.
      currentNames <- Cli.currentNames
      let pped = PPED.makePPED (PPE.hqNamer 10 currentNames) (PPE.suffixifyByHash currentNames)
      let suffixifiedPPE = PPED.suffixifiedPPE pped
      results <- searchBranch0 codebase branch0 names
      respondResults codebase suffixifiedPPE (Just p) results
    FindGlobal -> do
      Global.forAllProjectBranches \(projAndBranchNames, _ids) branch -> do
        let branch0 = Branch.head branch
        let projectRootNames = Names.makeAbsolute . Branch.toNames $ branch0
        let pped = PPED.makePPED (PPE.hqNamer 10 projectRootNames) (PPE.suffixifyByHash projectRootNames)
        results <- searchBranch0 codebase branch0 projectRootNames
        when (not $ null results) do
          Cli.setNumberedArgs $ fmap (SA.SearchResult Nothing) results
          results' <- Cli.runTransaction (Backend.loadSearchResults codebase results)
          Cli.respond $ GlobalFindBranchResults projAndBranchNames (PPED.suffixifiedPPE pped) isVerbose results'
  where
    searchBranch0 :: Codebase.Codebase m Symbol Ann -> Branch0 IO -> Names -> Cli [SearchResult]
    searchBranch0 codebase branch0 names =
      case ws of
        [] -> pure (List.sortBy SR.compareByName (SR.fromNames names))
        -- type query
        ":" : ws -> do
          typ <- parseSearchType (show input) (unwords ws)
          let keepNamed = Set.intersection (Branch.deepReferents branch0)
          (noExactTypeMatches, matches) <- do
            Cli.runTransaction do
              matches <- keepNamed <$> Codebase.termsOfType codebase typ
              if null matches
                then (True,) . keepNamed <$> Codebase.termsMentioningType codebase typ
                else pure (False, matches)
          when noExactTypeMatches (Cli.respond NoExactTypeMatches)
          pure $
            -- in verbose mode, aliases are shown, so we collapse all
            -- aliases to a single search result; in non-verbose mode,
            -- a separate result may be shown for each alias
            (if isVerbose then uniqueBy SR.toReferent else id) $
              searchResultsFor names (Set.toList matches) []

        -- name query
        qs -> do
          let anythingBeforeHash :: Megaparsec.Parsec (L.Token Text) [Char] Text
              anythingBeforeHash = Text.pack <$> Megaparsec.takeWhileP Nothing (/= '#')
          let srs =
                searchBranchScored
                  names
                  Find.simpleFuzzyScore
                  (mapMaybe (HQ.parseTextWith anythingBeforeHash . Text.pack) qs)
          pure $ uniqueBy SR.toReferent srs
    respondResults :: Codebase.Codebase m Symbol Ann -> PPE.PrettyPrintEnv -> Maybe Path' -> [SearchResult] -> Cli ()
    respondResults codebase ppe searchRoot results = do
      Cli.setNumberedArgs $ fmap (SA.SearchResult searchRoot) results
      results' <- Cli.runTransaction (Backend.loadSearchResults codebase results)
      Cli.respond $ ListOfDefinitions fscope ppe isVerbose results'

handleDependencies :: HQ.HashQualified Name -> Cli ()
handleDependencies hq = do
  Cli.Env {codebase} <- ask
  -- todo: add flag to handle transitive efficiently
  lds <- resolveHQToLabeledDependencies hq
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  when (null lds) do
    Cli.returnEarly (LabeledReferenceNotFound hq)
  results <- for (toList lds) \ld -> do
    dependencies :: Set LabeledDependency <-
      Cli.runTransaction do
        let tp r@(Reference.DerivedId i) =
              Codebase.getTypeDeclaration codebase i <&> \case
                Nothing -> error $ "What happened to " ++ show i ++ "?"
                Just decl ->
                  Set.map LabeledDependency.TypeReference . Set.delete r . DD.typeDependencies $
                    DD.asDataDecl decl
            tp _ = pure mempty
            tm r@(Referent.Ref (Reference.DerivedId i)) =
              Codebase.getTerm codebase i <&> \case
                Nothing -> error $ "What happened to " ++ show i ++ "?"
                Just tm -> Set.delete (LabeledDependency.TermReferent r) (Term.labeledDependencies tm)
            tm con@(Referent.Con (ConstructorReference (Reference.DerivedId i) cid) _ct) =
              Codebase.getTypeDeclaration codebase i <&> \case
                Nothing -> error $ "What happened to " ++ show i ++ "?"
                Just decl -> case DD.typeOfConstructor (DD.asDataDecl decl) cid of
                  Nothing -> error $ "What happened to " ++ show con ++ "?"
                  Just tp -> Type.labeledDependencies tp
            tm _ = pure mempty
         in LD.fold tp tm ld
    let types = [(PPE.typeName suffixifiedPPE r, r) | LabeledDependency.TypeReference r <- toList dependencies]
    let terms = [(PPE.termName suffixifiedPPE r, r) | LabeledDependency.TermReferent r <- toList dependencies]
    pure (types, terms)
  let types = fmap fst . nubOrdOn snd . Name.sortByText (HQ.toText . fst) . join $ fst <$> results
  let terms = fmap fst . nubOrdOn snd . Name.sortByText (HQ.toText . fst) . join $ snd <$> results
  Cli.setNumberedArgs . map SA.HashQualified $ types <> terms
  Cli.respond $ ListDependencies suffixifiedPPE lds types terms

handleDependents :: HQ.HashQualified Name -> Cli ()
handleDependents hq = do
  -- todo: add flag to handle transitive efficiently
  lds <- resolveHQToLabeledDependencies hq
  -- Use an unsuffixified PPE here, so we display full names (relative to the current path),
  -- rather than the shortest possible unambiguous name.
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let fqppe = PPE.unsuffixifiedPPE pped
  let ppe = PPE.suffixifiedPPE pped
  when (null lds) do
    Cli.returnEarly (LabeledReferenceNotFound hq)

  results <- for (toList lds) \ld -> do
    -- The full set of dependent references, any number of which may not have names in the current namespace.
    dependents <-
      let tp = Codebase.dependents Queries.ExcludeOwnComponent
          tm = \case
            Referent.Ref r -> Codebase.dependents Queries.ExcludeOwnComponent r
            Referent.Con (ConstructorReference r _cid) _ct ->
              Codebase.dependents Queries.ExcludeOwnComponent r
       in Cli.runTransaction (LD.fold tp tm ld)
    let -- True is term names, False is type names
        results :: [(Bool, HQ.HashQualified Name, Reference)]
        results = do
          r <- Set.toList dependents
          Just (isTerm, hq) <- [(True,) <$> PPE.terms fqppe (Referent.Ref r), (False,) <$> PPE.types fqppe r]
          fullName <- [HQ'.toName hq]
          guard (not (Name.beginsWithSegment fullName NameSegment.libSegment))
          Just shortName <- pure $ PPE.terms ppe (Referent.Ref r) <|> PPE.types ppe r
          pure (isTerm, HQ'.toHQ shortName, r)
    pure results
  let sort = fmap fst . nubOrdOn snd . Name.sortByText (HQ.toText . fst)
  let types = sort [(n, r) | (False, n, r) <- join results]
  let terms = sort [(n, r) | (True, n, r) <- join results]
  Cli.setNumberedArgs . map SA.HashQualified $ types <> terms
  Cli.respond (ListDependents ppe lds types terms)

-- todo: compare to `getHQTerms` / `getHQTypes`.  Is one universally better?
resolveHQToLabeledDependencies :: HQ.HashQualified Name -> Cli (Set LabeledDependency)
resolveHQToLabeledDependencies = \case
  HQ.NameOnly n -> do
    names <- Cli.currentNames
    let terms, types :: Set LabeledDependency
        terms = Set.map LD.referent . Name.searchBySuffix n $ Names.terms names
        types = Set.map LD.typeRef . Name.searchBySuffix n $ Names.types names
    pure $ terms <> types
  -- rationale: the hash should be unique enough that the name never helps
  HQ.HashQualified _n sh -> resolveHashOnly sh
  HQ.HashOnly sh -> resolveHashOnly sh
  where
    resolveHashOnly sh = do
      Cli.Env {codebase} <- ask
      (terms, types) <-
        Cli.runTransaction do
          terms <- Backend.termReferentsByShortHash codebase sh
          types <- Backend.typeReferencesByShortHash sh
          pure (terms, types)
      pure $ Set.map LD.referent terms <> Set.map LD.typeRef types

doDisplay :: OutputLocation -> Names -> Term Symbol () -> Cli ()
doDisplay outputLoc names tm = do
  Cli.Env {codebase} <- ask
  loopState <- State.get
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  (tms, typs) <- maybe mempty UF.indexByReference <$> Cli.getLatestTypecheckedFile
  let useCache = True
      evalTerm tm =
        fmap ErrorUtil.hush . fmap (fmap Term.unannotate) $
          RuntimeUtils.evalUnisonTermE True suffixifiedPPE useCache (Term.amap (const External) tm)
      loadTerm (Reference.DerivedId r) = case Map.lookup r tms of
        Nothing -> fmap (fmap Term.unannotate) $ Cli.runTransaction (Codebase.getTerm codebase r)
        Just (_, tm, _) -> pure (Just $ Term.unannotate tm)
      loadTerm _ = pure Nothing
      loadDecl (Reference.DerivedId r) = case Map.lookup r typs of
        Nothing -> fmap (fmap $ DD.amap (const ())) $ Cli.runTransaction $ Codebase.getTypeDeclaration codebase r
        Just decl -> pure (Just $ DD.amap (const ()) decl)
      loadDecl _ = pure Nothing
      loadTypeOfTerm' (Referent.Ref (Reference.DerivedId r))
        | Just (_, _, ty) <- Map.lookup r tms = pure $ Just (void ty)
      loadTypeOfTerm' r = fmap (fmap void) . Cli.runTransaction . Codebase.getTypeOfReferent codebase $ r
  rendered <- DisplayValues.displayTerm pped loadTerm loadTypeOfTerm' evalTerm loadDecl tm
  mayFP <- case outputLoc of
    ConsoleLocation -> pure Nothing
    FileLocation path _ -> Just <$> Directory.canonicalizePath path
    LatestFileLocation _ -> traverse Directory.canonicalizePath $ fmap fst (loopState ^. #latestFile) <|> Just "scratch.u"
  whenJust mayFP \fp -> do
    liftIO $ prependFile fp (Text.pack . P.toPlain 80 $ rendered)
  Cli.respond $ DisplayRendered mayFP rendered
  where
    suffixify =
      case outputLoc of
        ConsoleLocation -> PPE.suffixifyByHash
        FileLocation _ _ -> PPE.suffixifyByHashName
        LatestFileLocation _ -> PPE.suffixifyByHashName

    prependFile :: FilePath -> Text -> IO ()
    prependFile filePath txt = do
      exists <- Directory.doesFileExist filePath
      if exists
        then do
          existing <- readUtf8 filePath
          writeUtf8 filePath (txt <> "\n\n" <> existing)
        else do
          writeUtf8 filePath txt

confirmedCommand :: Input -> Cli Bool
confirmedCommand i = do
  loopState <- State.get
  pure $ Just i == (loopState ^. #lastInput)

-- return `name` and `name.<everything>...`
_searchBranchPrefix :: Branch m -> Name -> [SearchResult]
_searchBranchPrefix b n = case Path.unsnoc (Path.fromName n) of
  Nothing -> []
  Just (init, last) -> case Branch.getAt init b of
    Nothing -> []
    Just b -> SR.fromNames . Names.prefix0 n $ names0
      where
        lastName = Name.fromSegment last
        subnames =
          Branch.toNames . Branch.head $
            Branch.getAt' (Path.singleton last) b
        rootnames =
          Names.filter (== lastName)
            . Branch.toNames
            . set Branch.children mempty
            $ Branch.head b
        names0 = rootnames <> Names.prefix0 lastName subnames

searchResultsFor :: Names -> [Referent] -> [Reference] -> [SearchResult]
searchResultsFor ns terms types =
  [ SR.termSearchResult ns name ref
    | ref <- terms,
      name <- toList (Names.namesForReferent ns ref)
  ]
    <> [ SR.typeSearchResult ns name ref
         | ref <- types,
           name <- toList (Names.namesForReference ns ref)
       ]

searchBranchScored ::
  forall score.
  (Ord score) =>
  Names ->
  (Text -> Text -> Maybe score) ->
  [HQ.HashQualified Text] ->
  [SearchResult]
searchBranchScored names0 score queries =
  nubOrd
    . fmap snd
    . List.sortBy (\(s0, r0) (s1, r1) -> compare s0 s1 <> SR.compareByName r0 r1)
    $ searchTermNamespace <> searchTypeNamespace
  where
    searchTermNamespace = queries >>= do1query
      where
        do1query :: HQ.HashQualified Text -> [(Maybe score, SearchResult)]
        do1query q = mapMaybe (score1hq q) (R.toList . Names.terms $ names0)
        score1hq :: HQ.HashQualified Text -> (Name, Referent) -> Maybe (Maybe score, SearchResult)
        score1hq query (name, ref) = case query of
          HQ.NameOnly qn ->
            pair qn
          HQ.HashQualified qn h
            | h `SH.isPrefixOf` Referent.toShortHash ref ->
                pair qn
          HQ.HashOnly h
            | h `SH.isPrefixOf` Referent.toShortHash ref ->
                Just (Nothing, result)
          _ -> Nothing
          where
            result = SR.termSearchResult names0 name ref
            pair qn =
              (\score -> (Just score, result)) <$> score qn (Name.toText name)
    searchTypeNamespace = queries >>= do1query
      where
        do1query :: HQ.HashQualified Text -> [(Maybe score, SearchResult)]
        do1query q = mapMaybe (score1hq q) (R.toList . Names.types $ names0)
        score1hq :: HQ.HashQualified Text -> (Name, Reference) -> Maybe (Maybe score, SearchResult)
        score1hq query (name, ref) = case query of
          HQ.NameOnly qn ->
            pair qn
          HQ.HashQualified qn h
            | h `SH.isPrefixOf` Reference.toShortHash ref ->
                pair qn
          HQ.HashOnly h
            | h `SH.isPrefixOf` Reference.toShortHash ref ->
                Just (Nothing, result)
          _ -> Nothing
          where
            result = SR.typeSearchResult names0 name ref
            pair qn =
              (\score -> (Just score, result)) <$> score qn (Name.toText name)

doCompile :: Bool -> Bool -> String -> HQ.HashQualified Name -> Cli ()
doCompile profile native output main = do
  Cli.Env {codebase, runtime, nativeRuntime} <- ask
  let theRuntime
        | native = nativeRuntime
        | otherwise = runtime
  (ref, ppe) <- resolveMainRef main
  let codeLookup = () <$ Codebase.codebaseToCodeLookup codebase
      outf
        | native = output
        | otherwise = output <> ".uc"
      copts = Runtime.defaultCompileOpts {Runtime.profile = profile}
  whenJustM
    ( liftIO $
        Runtime.compileTo theRuntime copts codeLookup ppe ref outf
    )
    (Cli.returnEarly . EvaluationFailure)

delete ::
  Input ->
  DeleteOutput ->
  ((Path.Absolute, HQ'.HQSegment) -> Cli (Set Referent)) -> -- compute matching terms
  ((Path.Absolute, HQ'.HQSegment) -> Cli (Set Reference)) -> -- compute matching types
  [Path.HQSplit'] -> -- targets for deletion
  Cli ()
delete input doutput getTerms getTypes hqs' = do
  -- persists the original hash qualified entity for error reporting
  typesTermsTuple <-
    traverse
      ( \hq -> do
          absolute <- Cli.resolveSplit' hq
          types <- getTypes (first PP.absPath absolute)
          terms <- getTerms (first PP.absPath absolute)
          return (hq, types, terms)
      )
      hqs'
  let notFounds = List.filter (\(_, types, terms) -> Set.null terms && Set.null types) typesTermsTuple
  -- if there are any entities which cannot be deleted because they don't exist, short circuit.
  if not $ null notFounds
    then do
      let toName :: [(Path.HQSplit', Set Reference, Set referent)] -> [Name]
          toName notFounds =
            map (\(split, _, _) -> HQ'.toName $ Path.nameFromHQSplit' split) notFounds
      Cli.returnEarly $ NamesNotFound (toName notFounds)
    else do
      checkDeletes typesTermsTuple doutput input

checkDeletes :: [(Path.HQSplit', Set Reference, Set Referent)] -> DeleteOutput -> Input -> Cli ()
checkDeletes typesTermsTuples doutput inputs = do
  let toSplitName ::
        (Path.HQSplit', Set Reference, Set Referent) ->
        Cli (Path.AbsSplit, Name, Set Reference, Set Referent)
      toSplitName hq = do
        (pp, ns) <- Cli.resolveSplit' (HQ'.toName <$> hq ^. _1)
        let resolvedSplit = (pp.absPath, ns)
        return
          (resolvedSplit, Path.nameFromSplit' $ first (Path.RelativePath' . Path.Relative . Path.unabsolute) resolvedSplit, hq ^. _2, hq ^. _3)

  -- get the splits and names with terms and types
  splitsNames <- traverse toSplitName typesTermsTuples
  let toRel :: (Ord ref) => Set ref -> Name -> R.Relation Name ref
      toRel setRef name = R.fromList (fmap (name,) (toList setRef))
  let toDelete = fmap (\(_, names, types, terms) -> Names (toRel terms names) (toRel types names)) splitsNames
  -- make sure endangered is compeletely contained in paths
  projectNames <- Branch.toNames <$> Cli.getCurrentProjectRoot0
  -- get only once for the entire deletion set
  let allTermsToDelete :: Set LabeledDependency
      allTermsToDelete = Set.unions (fmap Names.labeledReferences toDelete)
  -- get the endangered dependencies for each entity to delete
  endangered <-
    Cli.runTransaction $
      traverse
        ( \targetToDelete ->
            getEndangeredDependents targetToDelete (allTermsToDelete) projectNames
        )
        toDelete
  -- If the overall dependency map is not completely empty, abort deletion
  let endangeredDeletions = List.filter (\m -> not $ null m || Map.foldr (\s b -> null s || b) False m) endangered
  if null endangeredDeletions
    then do
      let deleteTypesTerms =
            splitsNames
              >>= ( \(split, _, types, terms) ->
                      (map (BranchUtil.makeDeleteTypeName split) . Set.toList $ types)
                        ++ (map (BranchUtil.makeDeleteTermName split) . Set.toList $ terms)
                  )
      before <- Cli.getCurrentBranch0
      description <- inputDescription inputs
      pb <- Cli.getCurrentProjectBranch
      Cli.stepManyAt pb description deleteTypesTerms
      case doutput of
        DeleteOutput'Diff -> do
          after <- Cli.getCurrentBranch0
          (ppe, diff) <- diffHelper before after
          Cli.respondNumbered (ShowDiffAfterDeleteDefinitions ppe diff)
        DeleteOutput'NoDiff -> do
          Cli.respond Success
    else do
      let ppeDecl = PPED.makePPED (PPE.hqNamer 10 projectNames) (PPE.suffixifyByHash projectNames)
      let combineRefs = List.foldl (Map.unionWith NESet.union) Map.empty endangeredDeletions
      Cli.respondNumbered (CantDeleteDefinitions ppeDecl combineRefs)

-- | Goal: When deleting, we might be removing the last name of a given definition (i.e. the
-- definition is going "extinct"). In this case we may wish to take some action or warn the
-- user about these "endangered" definitions which would now contain unnamed references.
-- The argument `otherDesiredDeletions` is included in this function because the user might want to
-- delete a term and all its dependencies in one command, so we give this function access to
-- the full set of entities that the user wishes to delete.
getEndangeredDependents ::
  -- | Prospective target for deletion
  Names ->
  -- | All entities we want to delete (including the target)
  Set LabeledDependency ->
  -- | Names from the current branch
  Names ->
  -- | map from references going extinct to the set of endangered dependents
  Sqlite.Transaction (Map LabeledDependency (NESet LabeledDependency))
getEndangeredDependents targetToDelete otherDesiredDeletions rootNames = do
  -- names of terms left over after target deletion
  let remainingNames :: Names
      remainingNames = rootNames `Names.difference` targetToDelete
  -- target refs for deletion
  let refsToDelete :: Set LabeledDependency
      refsToDelete = Names.labeledReferences targetToDelete
  -- refs left over after deleting target
  let remainingRefs :: Set LabeledDependency
      remainingRefs = Names.labeledReferences remainingNames
  -- remove the other targets for deletion from the remaining terms
  let remainingRefsWithoutOtherTargets :: Set LabeledDependency
      remainingRefsWithoutOtherTargets = Set.difference remainingRefs otherDesiredDeletions
  -- deleting and not left over
  let extinct :: Set LabeledDependency
      extinct = refsToDelete `Set.difference` remainingRefs
  let accumulateDependents :: LabeledDependency -> Sqlite.Transaction (Map LabeledDependency (Set LabeledDependency))
      accumulateDependents ld =
        let ref = LD.fold id Referent.toReference ld
         in Map.singleton ld . Set.map LD.termRef <$> Codebase.dependents Queries.ExcludeOwnComponent ref
  -- All dependents of extinct, including terms which might themselves be in the process of being deleted.
  allDependentsOfExtinct :: Map LabeledDependency (Set LabeledDependency) <-
    Map.unionsWith (<>) <$> for (Set.toList extinct) accumulateDependents

  -- Filtered to only include dependencies which are not being deleted, but depend one which
  -- is going extinct.
  let extinctToEndangered :: Map LabeledDependency (NESet LabeledDependency)
      extinctToEndangered =
        allDependentsOfExtinct & Map.mapMaybe \endangeredDeps ->
          let remainingEndangered = endangeredDeps `Set.intersection` remainingRefsWithoutOtherTargets
           in NESet.nonEmptySet remainingEndangered
  pure extinctToEndangered

displayI ::
  OutputLocation ->
  HQ.HashQualified Name ->
  Cli ()
displayI outputLoc hq = do
  let useRoot = any Name.isAbsolute hq
  (names, pped) <-
    if useRoot
      then do
        root <- Cli.getCurrentProjectRoot
        let root0 = Branch.head root
        let names = Names.makeAbsolute $ Branch.toNames root0
        let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
        pure (names, pped)
      else do
        names <- Cli.currentNames
        let pped = PPED.makePPED (PPE.hqNamer 10 names) (suffixify names)
        pure (names, pped)
  let suffixifiedPPE = PPE.suffixifiedPPE pped
  let bias = maybeToList $ HQ.toName hq
  latestTypecheckedFile <- Cli.getLatestTypecheckedFile
  case addWatch (Text.unpack (HQ.toText hq)) latestTypecheckedFile of
    Nothing -> do
      let results = Names.lookupHQTerm Names.IncludeSuffixes hq names
      ref <-
        Set.asSingleton results & onNothing do
          Cli.returnEarly
            if Set.null results
              then SearchTermsNotFound [hq]
              else TermAmbiguous suffixifiedPPE hq results
      let tm = Term.fromReferent External ref
      tm <- RuntimeUtils.evalUnisonTerm True (PPE.biasTo bias $ suffixifiedPPE) True tm
      doDisplay outputLoc names (Term.unannotate tm)
    Just (toDisplay, unisonFile) -> do
      let namesWithDefinitionsFromFile = UF.addNamesFromTypeCheckedUnisonFile unisonFile names
      let filePPED = PPED.makePPED (PPE.hqNamer 10 namesWithDefinitionsFromFile) (suffixify namesWithDefinitionsFromFile)

      let suffixifiedFilePPE = PPE.biasTo bias $ PPE.suffixifiedPPE filePPED
      (_, watches) <- evalUnisonFile Sandboxed suffixifiedFilePPE unisonFile []
      (_, _, _, _, tm, _) <-
        Map.lookup toDisplay watches & onNothing (error $ "Evaluation dropped a watch expression: " <> Text.unpack (HQ.toText hq))
      let ns = UF.addNamesFromTypeCheckedUnisonFile unisonFile names
      doDisplay outputLoc ns tm
  where
    suffixify =
      case outputLoc of
        ConsoleLocation -> PPE.suffixifyByHash
        FileLocation _ _ -> PPE.suffixifyByHashName
        LatestFileLocation _ -> PPE.suffixifyByHashName

docsI :: Name -> Cli ()
docsI src = do
  findInScratchfileByName
  where
    {- Given `docs foo`, we look for docs in 3 places, in this order:
       (fileByName) First check the file for `foo.doc`, and if found do `display foo.doc`
       (codebaseByName) Lastly check for `foo.doc` in the codebase and if found do `display foo.doc`
    -}
    dotDoc :: HQ.HashQualified Name
    dotDoc = HQ.NameOnly . Name.joinDot src $ Name.fromSegment NameSegment.docSegment

    findInScratchfileByName :: Cli ()
    findInScratchfileByName = do
      namesInFile <- Cli.getNamesFromLatestFile
      case Names.lookupHQTerm Names.IncludeSuffixes dotDoc namesInFile of
        s | Set.size s == 1 -> do
          -- the displayI command expects full term names, so we resolve
          -- the hash back to its full name in the file
          displayI ConsoleLocation (Names.longestTermName 10 (Set.findMin s) namesInFile)
        _ -> displayI ConsoleLocation dotDoc

lexedSource :: Text -> Text -> Cli (Text, [L.Token L.Lexeme])
lexedSource name src = do
  let tokens = L.lexer (Text.unpack name) (Text.unpack src)
  pure (src, tokens)

parseSearchType :: SrcLoc -> String -> Cli (Type Symbol Ann)
parseSearchType srcLoc typ = Type.removeAllEffectVars <$> parseType srcLoc typ

-- | A description of where the given parse was triggered from, for error messaging purposes.
type SrcLoc = String

parseType :: SrcLoc -> String -> Cli (Type Symbol Ann)
parseType input src = do
  -- `show Input` is the name of the "file" being lexed
  lexed <- lexedSource (Text.pack input) (Text.pack src)
  names <- Cli.currentNames
  let parsingEnv =
        Parser.ParsingEnv
          { uniqueNames = mempty,
            uniqueTypeGuid = \_ -> pure Nothing,
            names,
            maybeNamespace = Nothing,
            localNamespacePrefixedTypesAndConstructors = mempty
          }
  typ <-
    Parsers.parseType (Text.unpack (fst lexed)) parsingEnv & onLeftM \err ->
      Cli.returnEarly (TypeParseError src err)

  Type.bindNames Name.unsafeParseVar Name.toVar Set.empty names (Type.generalizeLowercase mempty typ) & onLeft \errs ->
    Cli.returnEarly (ParseResolutionFailures src (toList errs))

-- Adds a watch expression of the given name to the file, if
-- it would resolve to a TLD in the file. Returns the freshened
-- variable name and the new typechecked file.
--
-- Otherwise, returns `Nothing`.
addWatch ::
  (Var v) =>
  String ->
  Maybe (TypecheckedUnisonFile v Ann) ->
  Maybe (v, TypecheckedUnisonFile v Ann)
addWatch _watchName Nothing = Nothing
addWatch watchName (Just uf) = do
  let components = join $ UF.topLevelComponents uf
  let mainComponent = filter ((\v -> Var.nameStr v == watchName) . view _1) components
  case mainComponent of
    [(v, ann, tm, ty)] ->
      Just $
        let v2 = Var.freshIn (Set.fromList [v]) v
            a = ABT.annotation tm
         in ( v2,
              UF.typecheckedUnisonFile
                (UF.dataDeclarationsId' uf)
                (UF.effectDeclarationsId' uf)
                (UF.topLevelComponents' uf)
                (UF.watchComponents uf <> [(WK.RegularWatch, [(v2, ann, Term.var a v, ty)])])
            )
    _ -> addWatch watchName Nothing

resolveBranchId2 :: BranchId2 -> Cli (Branch IO)
resolveBranchId2 = \case
  Left sch -> Cli.resolveShortCausalHash sch
  Right brp -> do
    pp <- ProjectUtils.resolveBranchRelativePath brp
    Cli.Env {codebase} <- ask
    fromMaybe Branch.empty <$> liftIO (Codebase.getBranchAtProjectPath codebase pp)
