-- | @merge@ input handler.
module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,

    -- * API exported for @pull@
    MergeInfo (..),
    AliceMergeInfo (..),
    BobMergeInfo (..),
    LcaMergeInfo (..),
    doMerge,
    doMergeLocalBranch,

    -- * API exported for @todo@
    hasDefnsInLib,
  )
where

import Control.Monad.Reader (ask)
import Data.Algorithm.Diff qualified as Diff
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Semialign (zipWith)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import System.Directory (canonicalizePath, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp qualified as Temporary
import System.Process qualified as Process
import Text.ANSI qualified as Text
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import U.Codebase.Branch qualified as V2 (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified as V2.Causal
import U.Codebase.HashTags (CausalHash, unCausalHash)
import U.Codebase.Reference (TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Sqlite.DbId (ProjectId)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.MergeTypes (MergeSource (..), MergeSourceAndTarget (..), MergeSourceOrTarget (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.UpdateUtils
  ( getNamespaceDependentsOf3,
    hydrateDefns,
    loadNamespaceDefinitions,
  )
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.RemoteRepo (ReadShareLooseCode (..))
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.Codebase.SqliteCodebase.Operations qualified as Operations
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.Debug qualified as Debug
import Unison.Hash qualified as Hash
import Unison.Merge qualified as Merge
import Unison.Merge.EitherWayI qualified as EitherWayI
import Unison.Merge.Synhashed qualified as Synhashed
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Project
  ( ProjectAndBranch (..),
    ProjectBranchName,
    ProjectBranchNameKind (..),
    ProjectName,
    Semver (..),
    classifyProjectBranchName,
  )
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Referent'
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Conflicted (Conflicted)
import Unison.Util.Defn (Defn)
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3, alignDefnsWith)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Nametree (Nametree (..), unflattenNametree)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Star2 (Star2)
import Unison.Util.Star2 qualified as Star2
import Unison.WatchKind qualified as WatchKind
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip, zipWith)

handleMerge :: ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleMerge (ProjectAndBranch maybeBobProjectName bobBranchName) = do
  -- Assert that Alice (us) is on a project branch, and grab the causal hash.
  ProjectPath aliceProject aliceProjectBranch _path <- Cli.getCurrentProjectPath
  let aliceProjectAndBranch = ProjectAndBranch aliceProject aliceProjectBranch

  -- Resolve Bob's maybe-project-name + branch-name to the info the merge algorithm needs: the project name, branch
  -- name, and causal hash.
  bobProject <-
    case maybeBobProjectName of
      Nothing -> pure aliceProjectAndBranch.project
      Just bobProjectName
        | bobProjectName == aliceProjectAndBranch.project.name -> pure aliceProjectAndBranch.project
        | otherwise -> do
            Cli.runTransaction (Queries.loadProjectByName bobProjectName)
              & onNothingM (Cli.returnEarly (Output.LocalProjectDoesntExist bobProjectName))
  bobProjectBranch <- ProjectUtils.expectProjectBranchByName bobProject bobBranchName
  let bobProjectAndBranch = ProjectAndBranch bobProject bobProjectBranch

  doMergeLocalBranch
    Merge.TwoWay
      { alice = aliceProjectAndBranch,
        bob = bobProjectAndBranch
      }

data MergeInfo = MergeInfo
  { alice :: !AliceMergeInfo,
    bob :: !BobMergeInfo,
    lca :: !LcaMergeInfo,
    -- | How should we describe this merge in the reflog?
    description :: !Text
  }

data AliceMergeInfo = AliceMergeInfo
  { causalHash :: !CausalHash,
    projectAndBranch :: !(ProjectAndBranch Project ProjectBranch)
  }

data BobMergeInfo = BobMergeInfo
  { causalHash :: !CausalHash,
    source :: !MergeSource
  }

newtype LcaMergeInfo = LcaMergeInfo
  { causalHash :: Maybe CausalHash
  }

doMerge :: MergeInfo -> Cli ()
doMerge info = do
  let debugFunctions =
        if Debug.shouldDebug Debug.Merge
          then realDebugFunctions
          else fakeDebugFunctions

  let aliceBranchNames = ProjectUtils.justTheNames info.alice.projectAndBranch
  let mergeSource = MergeSourceOrTarget'Source info.bob.source
  let mergeTarget = MergeSourceOrTarget'Target aliceBranchNames
  let mergeSourceAndTarget = MergeSourceAndTarget {alice = aliceBranchNames, bob = info.bob.source}

  env <- ask

  finalOutput <-
    Cli.label \done -> do
      -- If alice == bob, or LCA == bob (so alice is ahead of bob), then we are done.
      when (info.alice.causalHash == info.bob.causalHash || info.lca.causalHash == Just info.bob.causalHash) do
        done (Output.MergeAlreadyUpToDate2 mergeSourceAndTarget)

      -- Otherwise, if LCA == alice (so alice is behind bob), then we could fast forward to bob, so we're done.
      when (info.lca.causalHash == Just info.alice.causalHash) do
        bobBranch <- liftIO (Codebase.expectBranchForHash env.codebase info.bob.causalHash)
        _ <- Cli.updateAt info.description (PP.projectBranchRoot info.alice.projectAndBranch) (\_aliceBranch -> bobBranch)
        done (Output.MergeSuccessFastForward mergeSourceAndTarget)

      Cli.withRespondRegion \respondRegion -> do
        respondRegion (Output.Literal "Loading branches...")

        -- Load Alice/Bob/LCA causals
        causals <-
          Cli.runTransaction do
            traverse
              Operations.expectCausalBranchByCausalHash
              Merge.TwoOrThreeWay
                { alice = info.alice.causalHash,
                  bob = info.bob.causalHash,
                  lca = info.lca.causalHash
                }

        liftIO (debugFunctions.debugCausals causals)

        -- Load Alice/Bob/LCA branches
        branches <-
          Cli.runTransaction do
            alice <- causals.alice.value
            bob <- causals.bob.value
            lca <- for causals.lca \causal -> causal.value
            pure Merge.TwoOrThreeWay {lca, alice, bob}

        -- Assert that neither Alice nor Bob have defns in lib
        for_ [(mergeTarget, branches.alice), (mergeSource, branches.bob)] \(who, branch) -> do
          whenM (Cli.runTransaction (hasDefnsInLib branch)) do
            done (Output.MergeDefnsInLib who)

        -- Load Alice/Bob/LCA definitions
        --
        -- FIXME: Oops, if this fails due to a conflicted name, we don't actually say where the conflicted name came from.
        -- We should have a better error message (even though you can't do anything about conflicted names in the LCA).
        nametrees3 :: Merge.ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) <- do
          let referent2to1 = Conversions.referent2to1 (Codebase.getDeclType env.codebase)
          let action ::
                (forall a. Defn (Conflicted Name Referent) (Conflicted Name TypeReference) -> Transaction a) ->
                Transaction (Merge.ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)))
              action rollback = do
                alice <- loadNamespaceDefinitions referent2to1 branches.alice & onLeftM rollback
                bob <- loadNamespaceDefinitions referent2to1 branches.bob & onLeftM rollback
                lca <-
                  case branches.lca of
                    Nothing -> pure Nametree {value = Defns Map.empty Map.empty, children = Map.empty}
                    Just lca -> loadNamespaceDefinitions referent2to1 lca & onLeftM rollback
                pure Merge.ThreeWay {alice, bob, lca}
          Cli.runTransactionWithRollback2 (\rollback -> Right <$> action (rollback . Left))
            & onLeftM (done . Output.ConflictedDefn "merge")

        libdeps3 <- Cli.runTransaction (loadLibdeps branches)

        let blob0 = Merge.makeMergeblob0 nametrees3 libdeps3

        -- Hydrate
        hydratedDefns ::
          Merge.ThreeWay
            ( DefnsF
                (Map Name)
                (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
                (TypeReferenceId, Decl Symbol Ann)
            ) <-
          Cli.runTransaction $
            traverse
              ( hydrateDefns
                  (Codebase.unsafeGetTermComponent env.codebase)
                  Operations.expectDeclComponent
              )
              ( let f = Map.mapMaybe Referent.toTermReferenceId . BiMultimap.range
                    g = Map.mapMaybe Reference.toId . BiMultimap.range
                 in bimap f g <$> blob0.defns
              )

        respondRegion (Output.Literal "Computing diff between branches...")

        blob1 <-
          Merge.makeMergeblob1 blob0 hydratedDefns & onLeft \case
            Merge.Alice reason -> done (Output.IncoherentDeclDuringMerge mergeTarget reason)
            Merge.Bob reason -> done (Output.IncoherentDeclDuringMerge mergeSource reason)

        liftIO (debugFunctions.debugDiffs blob1.diffs)

        liftIO (debugFunctions.debugCombinedDiff blob1.diff)

        blob2 <-
          Merge.makeMergeblob2 blob1 & onLeft \err ->
            done case err of
              Merge.Mergeblob2Error'ConflictedAlias defn0 ->
                case defn0 of
                  Merge.Alice defn -> Output.MergeConflictedAliases mergeTarget defn
                  Merge.Bob defn -> Output.MergeConflictedAliases mergeSource defn
              Merge.Mergeblob2Error'ConflictedBuiltin defn -> Output.MergeConflictInvolvingBuiltin defn

        liftIO (debugFunctions.debugPartitionedDiff blob2.conflicts blob2.unconflicts)

        respondRegion (Output.Literal "Loading dependents of changes...")

        dependents0 <-
          Cli.runTransaction $
            for ((,) <$> ThreeWay.forgetLca blob2.defns <*> blob2.coreDependencies) \(defns, deps) ->
              getNamespaceDependentsOf3 defns deps

        respondRegion (Output.Literal "Loading and merging library dependencies...")

        -- Load libdeps
        (mergedLibdeps, lcaLibdeps) <- do
          -- We make a fresh branch cache to load the branch of libdeps.
          -- It would probably be better to reuse the codebase's branch cache.
          -- FIXME how slow/bad is this without that branch cache?
          Cli.runTransaction do
            branchCache <- Sqlite.unsafeIO newBranchCache
            let load children =
                  Conversions.branch2to1
                    branchCache
                    (Codebase.getDeclType env.codebase)
                    V2.Branch {terms = Map.empty, types = Map.empty, patches = Map.empty, children}
            mergedLibdeps <- load blob2.libdeps
            lcaLibdeps <- load blob2.lcaLibdeps
            pure (mergedLibdeps, lcaLibdeps)

        let hasConflicts =
              blob2.hasConflicts

        respondRegion (Output.Literal "Rendering Unison file...")

        let blob3 =
              Merge.makeMergeblob3
                blob2
                dependents0
                (Branch.toNames mergedLibdeps)
                (Branch.toNames lcaLibdeps)
                Merge.TwoWay
                  { alice = into @Text aliceBranchNames,
                    bob =
                      case info.bob.source of
                        MergeSource'LocalProjectBranch bobBranchNames -> into @Text bobBranchNames
                        MergeSource'RemoteProjectBranch bobBranchNames
                          | aliceBranchNames == bobBranchNames -> "remote " <> into @Text bobBranchNames
                          | otherwise -> into @Text bobBranchNames
                        MergeSource'RemoteLooseCode info ->
                          case Path.toName info.path of
                            Nothing -> "<root>"
                            Just name -> Name.toText name
                  }

        maybeBlob5 <-
          if hasConflicts
            then pure Nothing
            else case Merge.makeMergeblob4 blob3 of
              Left _parseErr -> pure Nothing
              Right blob4 -> do
                respondRegion (Output.Literal "Typechecking Unison file...")
                typeLookup <- Cli.runTransaction (Codebase.typeLookupForDependencies env.codebase blob4.dependencies)
                pure case Merge.makeMergeblob5 blob4 typeLookup of
                  Left _typecheckErr -> Nothing
                  Right blob5 -> Just blob5

        let parents =
              causals <&> \causal -> (causal.causalHash, Codebase.expectBranchForHash env.codebase causal.causalHash)

        blob5 <-
          maybeBlob5 & onNothing do
            env <- ask
            (_temporaryBranchId, temporaryBranchName) <-
              HandleInput.Branch.createBranch
                info.description
                ( HandleInput.Branch.CreateFrom'NamespaceWithParent
                    info.alice.projectAndBranch.branch
                    ( Branch.mergeNode
                        (defnsAndLibdepsToBranch0 env.codebase blob3.stageTwo mergedLibdeps)
                        parents.alice
                        parents.bob
                    )
                )
                info.alice.projectAndBranch.project
                (findTemporaryBranchName info.alice.projectAndBranch.project.projectId mergeSourceAndTarget)

            --   Merge conflicts?    Have UCM_MERGETOOL?    Result
            --   ----------------    -------------------    ------------------------------------------------------------
            --                 No                     No           Put code that doesn't parse or typecheck in scratch.u
            --                 No                    Yes           Put code that doesn't parse or typecheck in scratch.u
            --                Yes                     No    Put code that doesn't parse (because conflicts) in scratch.u
            --                Yes                    Yes                                              Run that cool tool

            maybeMergetool <-
              if hasConflicts
                then liftIO (lookupEnv "UCM_MERGETOOL")
                else pure Nothing

            case maybeMergetool of
              Nothing -> do
                scratchFilePath <-
                  Cli.getLatestFile <&> \case
                    Nothing -> "scratch.u"
                    Just (file, _) -> file
                liftIO $ env.writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 blob3.unparsedFile) True
                done (Output.MergeFailure scratchFilePath mergeSourceAndTarget temporaryBranchName)
              Just mergetool0 -> do
                let aliceFilenameSlug = mangleBranchName mergeSourceAndTarget.alice.branch
                let bobFilenameSlug = mangleMergeSource mergeSourceAndTarget.bob
                makeTempFilename <-
                  liftIO do
                    tmpdir0 <- getTemporaryDirectory
                    tmpdir1 <- canonicalizePath tmpdir0
                    tmpdir2 <- Temporary.createTempDirectory tmpdir1 "unison-merge"
                    pure \filename -> Text.pack (tmpdir2 </> Text.unpack (Text.Builder.run filename))
                let lcaFilename = makeTempFilename (aliceFilenameSlug <> "-" <> bobFilenameSlug <> "-base.u")
                let aliceFilename = makeTempFilename (aliceFilenameSlug <> ".u")
                let bobFilename = makeTempFilename (bobFilenameSlug <> ".u")
                let mergedFilename = Text.Builder.run (aliceFilenameSlug <> "-" <> bobFilenameSlug <> "-merged.u")
                let mergetool =
                      mergetool0
                        & Text.pack
                        & Text.replace "$BASE" lcaFilename
                        & Text.replace "$LOCAL" aliceFilename
                        & Text.replace "$MERGED" mergedFilename
                        & Text.replace "$REMOTE" bobFilename
                exitCode <-
                  liftIO do
                    let aliceFileContents = Text.pack (Pretty.toPlain 80 blob3.unparsedSoloFiles.alice)
                    let bobFileContents = Text.pack (Pretty.toPlain 80 blob3.unparsedSoloFiles.bob)
                    removeFile (Text.unpack mergedFilename) <|> pure ()
                    env.writeSource lcaFilename (Text.pack (Pretty.toPlain 80 blob3.unparsedSoloFiles.lca)) True
                    env.writeSource aliceFilename aliceFileContents True
                    env.writeSource bobFilename bobFileContents True
                    env.writeSource
                      mergedFilename
                      ( makeMergedFileContents
                          mergeSourceAndTarget
                          aliceFileContents
                          bobFileContents
                      )
                      True
                    let createProcess = (Process.shell (Text.unpack mergetool)) {Process.delegate_ctlc = True}
                    Process.withCreateProcess createProcess \_ _ _ -> Process.waitForProcess
                done (Output.MergeFailureWithMergetool mergeSourceAndTarget temporaryBranchName mergetool exitCode)

        Cli.runTransaction (Codebase.addDefsToCodebase env.codebase blob5.file)
        Cli.updateProjectBranchRoot_
          info.alice.projectAndBranch.branch
          info.description
          ( \_aliceBranch ->
              Branch.mergeNode
                ( Branch.batchUpdates
                    (typecheckedUnisonFileToBranchAdds blob5.file)
                    (defnsAndLibdepsToBranch0 env.codebase blob3.stageOne mergedLibdeps)
                )
                parents.alice
                parents.bob
          )
        pure (Output.MergeSuccess mergeSourceAndTarget)

  Cli.respond finalOutput

doMergeLocalBranch :: Merge.TwoWay (ProjectAndBranch Project ProjectBranch) -> Cli ()
doMergeLocalBranch branches = do
  (aliceCausalHash, bobCausalHash, lcaCausalHash) <-
    Cli.runTransaction do
      aliceCausalHash <- ProjectUtils.getProjectBranchCausalHash (branches.alice ^. #branch)
      bobCausalHash <- ProjectUtils.getProjectBranchCausalHash (branches.bob ^. #branch)
      -- Using Alice and Bob's causal hashes, find the LCA (if it exists)
      lcaCausalHash <- Operations.lca aliceCausalHash bobCausalHash
      pure (aliceCausalHash, bobCausalHash, lcaCausalHash)

  -- Do the merge!
  doMerge
    MergeInfo
      { alice =
          AliceMergeInfo
            { causalHash = aliceCausalHash,
              projectAndBranch = branches.alice
            },
        bob =
          BobMergeInfo
            { causalHash = bobCausalHash,
              source = MergeSource'LocalProjectBranch (ProjectUtils.justTheNames branches.bob)
            },
        lca =
          LcaMergeInfo
            { causalHash = lcaCausalHash
            },
        description = "merge " <> into @Text (ProjectUtils.justTheNames branches.bob)
      }

------------------------------------------------------------------------------------------------------------------------
-- Loading basic info out of the database

loadLibdeps ::
  Merge.TwoOrThreeWay (V2.Branch Transaction) ->
  Transaction (Merge.ThreeWay (Map NameSegment (V2.CausalBranch Transaction)))
loadLibdeps branches = do
  lca <-
    case branches.lca of
      Nothing -> pure Map.empty
      Just lcaBranch -> load lcaBranch
  alice <- load branches.alice
  bob <- load branches.bob
  pure Merge.ThreeWay {lca, alice, bob}
  where
    load :: V2.Branch Transaction -> Transaction (Map NameSegment (V2.CausalBranch Transaction))
    load branch =
      case Map.lookup NameSegment.libSegment branch.children of
        Nothing -> pure Map.empty
        Just libdepsCausal -> do
          libdepsBranch <- libdepsCausal.value
          pure libdepsBranch.children

------------------------------------------------------------------------------------------------------------------------
-- Merge precondition violation checks

hasDefnsInLib :: (Applicative m) => V2.Branch m -> m Bool
hasDefnsInLib branch = do
  libdeps <-
    case Map.lookup NameSegment.libSegment branch.children of
      Nothing -> pure V2.Branch.empty
      Just libdeps -> libdeps.value
  pure (not (Map.null libdeps.terms) || not (Map.null libdeps.types))

------------------------------------------------------------------------------------------------------------------------
--

defnsAndLibdepsToBranch0 ::
  Codebase IO v a ->
  DefnsF (Map Name) Referent TypeReference ->
  Branch0 Transaction ->
  Branch0 IO
defnsAndLibdepsToBranch0 codebase defns libdeps =
  let -- Unflatten the collection of terms into tree, ditto for types
      nametrees :: DefnsF2 Nametree (Map NameSegment) Referent TypeReference
      nametrees =
        bimap unflattenNametree unflattenNametree defns

      -- Align the tree of terms and tree of types into one tree
      nametree :: Nametree (DefnsF (Map NameSegment) Referent TypeReference)
      nametree =
        nametrees & alignDefnsWith \case
          This terms -> Defns {terms, types = Map.empty}
          That types -> Defns {terms = Map.empty, types}
          These terms types -> Defns terms types

      -- Convert the tree to a branch0
      branch0 = nametreeToBranch0 nametree

      -- Add back the libdeps branch at path "lib"
      branch1 = Branch.setChildBranch NameSegment.libSegment (Branch.one libdeps) branch0

      -- Awkward: we have a Branch Transaction but we need a Branch IO (because reasons)
      branch2 = Branch.transform0 (Codebase.runTransaction codebase) branch1
   in branch2

nametreeToBranch0 :: Nametree (DefnsF (Map NameSegment) Referent TypeReference) -> Branch0 m
nametreeToBranch0 nametree =
  Branch.branch0
    (rel2star defns.terms)
    (rel2star defns.types)
    (Branch.one . nametreeToBranch0 <$> nametree.children)
    Map.empty
  where
    defns :: Defns (Relation Referent NameSegment) (Relation TypeReference NameSegment)
    defns =
      bimap (Relation.swap . Relation.fromMap) (Relation.swap . Relation.fromMap) nametree.value

    rel2star :: Relation ref name -> Star2 ref name metadata
    rel2star rel =
      Star2.Star2 {fact = Relation.dom rel, d1 = rel, d2 = Relation.empty}

findTemporaryBranchName :: ProjectId -> MergeSourceAndTarget -> Transaction ProjectBranchName
findTemporaryBranchName projectId mergeSourceAndTarget = do
  ProjectUtils.findTemporaryBranchName projectId preferred
  where
    preferred :: ProjectBranchName
    preferred =
      unsafeFrom @Text $
        Text.Builder.run $
          "merge-"
            <> mangleMergeSource mergeSourceAndTarget.bob
            <> "-into-"
            <> mangleBranchName mergeSourceAndTarget.alice.branch

mangleMergeSource :: MergeSource -> Text.Builder
mangleMergeSource = \case
  MergeSource'LocalProjectBranch (ProjectAndBranch _project branch) -> mangleBranchName branch
  MergeSource'RemoteProjectBranch (ProjectAndBranch _project branch) -> "remote-" <> mangleBranchName branch
  MergeSource'RemoteLooseCode info -> manglePath info.path
  where
    manglePath :: Path -> Text.Builder
    manglePath =
      Monoid.intercalateMap "-" (Text.Builder.text . NameSegment.toUnescapedText) . Path.toList

mangleBranchName :: ProjectBranchName -> Text.Builder
mangleBranchName name =
  case classifyProjectBranchName name of
    ProjectBranchNameKind'Contributor user name1 ->
      Text.Builder.text user
        <> Text.Builder.char '-'
        <> mangleBranchName name1
    ProjectBranchNameKind'DraftRelease semver -> "releases-drafts-" <> mangleSemver semver
    ProjectBranchNameKind'Release semver -> "releases-" <> mangleSemver semver
    ProjectBranchNameKind'NothingSpecial -> Text.Builder.text (into @Text name)
  where
    mangleSemver :: Semver -> Text.Builder
    mangleSemver (Semver x y z) =
      Text.Builder.decimal x
        <> Text.Builder.char '.'
        <> Text.Builder.decimal y
        <> Text.Builder.char '.'
        <> Text.Builder.decimal z

typecheckedUnisonFileToBranchAdds :: TypecheckedUnisonFile Symbol Ann -> [(Path, Branch0 m -> Branch0 m)]
typecheckedUnisonFileToBranchAdds tuf = do
  declAdds ++ termAdds
  where
    declAdds :: [(Path, Branch0 m -> Branch0 m)]
    declAdds = do
      foldMap makeDataDeclAdds (Map.toList (UnisonFile.dataDeclarationsId' tuf))
        ++ foldMap makeEffectDeclUpdates (Map.toList (UnisonFile.effectDeclarationsId' tuf))
      where
        makeDataDeclAdds (symbol, (typeRefId, dataDecl)) = makeDeclAdds (symbol, (typeRefId, Right dataDecl))
        makeEffectDeclUpdates (symbol, (typeRefId, effectDecl)) = makeDeclAdds (symbol, (typeRefId, Left effectDecl))

        makeDeclAdds :: (Symbol, (TypeReferenceId, Decl Symbol Ann)) -> [(Path, Branch0 m -> Branch0 m)]
        makeDeclAdds (symbol, (typeRefId, decl)) =
          let insertTypeAction = BranchUtil.makeAddTypeName (splitVar symbol) (Reference.fromId typeRefId)
              insertTypeConstructorActions =
                zipWith
                  (\sym rid -> BranchUtil.makeAddTermName (splitVar sym) (Reference.fromId <$> rid))
                  (DataDeclaration.constructorVars (DataDeclaration.asDataDecl decl))
                  (DataDeclaration.declConstructorReferents typeRefId decl)
           in insertTypeAction : insertTypeConstructorActions

    termAdds :: [(Path, Branch0 m -> Branch0 m)]
    termAdds =
      tuf
        & UnisonFile.hashTermsId
        & Map.toList
        & mapMaybe \(var, (_, ref, wk, _, _)) -> do
          guard (WatchKind.watchKindShouldBeStoredInDatabase wk)
          Just (BranchUtil.makeAddTermName (splitVar var) (Referent.fromTermReferenceId ref))

    splitVar :: Symbol -> Path.Split Path
    splitVar = Path.splitFromName . Name.unsafeParseVar

------------------------------------------------------------------------------------------------------------------------
-- Making file with conflict markers

makeMergedFileContents :: MergeSourceAndTarget -> Text -> Text -> Text
makeMergedFileContents sourceAndTarget aliceContents bobContents =
  let f :: (Text.Builder, Diff.Diff Text) -> Diff.Diff Text -> (Text.Builder, Diff.Diff Text)
      f (acc, previous) line =
        case (previous, line) of
          (Diff.Both {}, Diff.Both bothLine _) -> go (Text.Builder.text bothLine)
          (Diff.Both {}, Diff.First aliceLine) -> go (aliceSlug <> Text.Builder.text aliceLine)
          (Diff.Both {}, Diff.Second bobLine) -> go (aliceSlug <> middleSlug <> Text.Builder.text bobLine)
          (Diff.First {}, Diff.Both bothLine _) -> go (middleSlug <> bobSlug <> Text.Builder.text bothLine)
          (Diff.First {}, Diff.First aliceLine) -> go (Text.Builder.text aliceLine)
          (Diff.First {}, Diff.Second bobLine) -> go (middleSlug <> Text.Builder.text bobLine)
          (Diff.Second {}, Diff.Both bothLine _) -> go (bobSlug <> Text.Builder.text bothLine)
          (Diff.Second {}, Diff.First aliceLine) -> go (bobSlug <> aliceSlug <> Text.Builder.text aliceLine)
          (Diff.Second {}, Diff.Second bobLine) -> go (Text.Builder.text bobLine)
        where
          go content =
            let !acc1 = acc <> content <> newline
             in (acc1, line)
   in Diff.getDiff (Text.lines aliceContents) (Text.lines bobContents)
        & List.foldl' f (mempty @Text.Builder, Diff.Both Text.empty Text.empty)
        & fst
        & Text.Builder.run
  where
    aliceSlug :: Text.Builder
    aliceSlug =
      "<<<<<<< " <> Text.Builder.text (into @Text sourceAndTarget.alice.branch) <> newline

    middleSlug :: Text.Builder
    middleSlug = "=======\n"

    bobSlug :: Text.Builder
    bobSlug =
      ">>>>>>> "
        <> ( case sourceAndTarget.bob of
               MergeSource'LocalProjectBranch bobProjectAndBranch ->
                 Text.Builder.text (into @Text bobProjectAndBranch.branch)
               MergeSource'RemoteProjectBranch bobProjectAndBranch ->
                 "remote " <> Text.Builder.text (into @Text bobProjectAndBranch.branch)
               MergeSource'RemoteLooseCode info ->
                 case Path.toName info.path of
                   Nothing -> "<root>"
                   Just name -> Text.Builder.text (Name.toText name)
           )
        <> newline

    newline :: Text.Builder
    newline = "\n"

------------------------------------------------------------------------------------------------------------------------
-- Debugging by printing a bunch of stuff out

data DebugFunctions = DebugFunctions
  { debugCausals :: Merge.TwoOrThreeWay (V2.CausalBranch Transaction) -> IO (),
    debugDiffs :: Merge.TwoWay (DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference) -> IO (),
    debugCombinedDiff :: DefnsF2 (Map Name) Merge.CombinedDiffOp Referent TypeReference -> IO (),
    debugPartitionedDiff ::
      Merge.TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
      DefnsF Merge.Unconflicts Referent TypeReference ->
      IO ()
  }

realDebugFunctions :: DebugFunctions
realDebugFunctions =
  DebugFunctions
    { debugCausals = realDebugCausals,
      debugDiffs = realDebugDiffs,
      debugCombinedDiff = realDebugCombinedDiff,
      debugPartitionedDiff = realDebugPartitionedDiff
    }

fakeDebugFunctions :: DebugFunctions
fakeDebugFunctions =
  DebugFunctions mempty mempty mempty mempty

realDebugCausals :: Merge.TwoOrThreeWay (V2.CausalBranch Transaction) -> IO ()
realDebugCausals causals = do
  Text.putStrLn (Text.bold "\n=== Alice causal hash ===")
  Text.putStrLn (Hash.toBase32HexText (unCausalHash causals.alice.causalHash))
  Text.putStrLn (Text.bold "\n=== Bob causal hash ===")
  Text.putStrLn (Hash.toBase32HexText (unCausalHash causals.bob.causalHash))
  Text.putStrLn (Text.bold "\n=== LCA causal hash ===")
  Text.putStrLn case causals.lca of
    Nothing -> "Nothing"
    Just causal -> "Just " <> Hash.toBase32HexText (unCausalHash causal.causalHash)

realDebugDiffs :: Merge.TwoWay (DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference) -> IO ()
realDebugDiffs diffs = do
  Text.putStrLn (Text.bold "\n=== LCA→Alice diff ===")
  renderDiff diffs.alice
  Text.putStrLn (Text.bold "\n=== LCA→Bob diff ===")
  renderDiff diffs.bob
  where
    renderDiff :: DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference -> IO ()
    renderDiff diff = do
      renderThings referentLabel diff.terms
      renderThings (const "type") diff.types

    renderThings :: (ref -> Text) -> Map Name (Merge.DiffOp (Merge.Synhashed ref)) -> IO ()
    renderThings label things =
      for_ (Map.toList things) \(name, op) ->
        let go color action x =
              color $
                action
                  <> " "
                  <> Text.italic (label (Synhashed.value x))
                  <> " "
                  <> Name.toText name
                  <> " #"
                  <> Hash.toBase32HexText (Synhashed.hash x)
         in Text.putStrLn case op of
              Merge.DiffOp'Add x -> go Text.green "+" x
              Merge.DiffOp'Delete x -> go Text.red "-" x
              Merge.DiffOp'Update x -> go Text.yellow "%" x.new

realDebugCombinedDiff :: DefnsF2 (Map Name) Merge.CombinedDiffOp Referent TypeReference -> IO ()
realDebugCombinedDiff diff = do
  Text.putStrLn (Text.bold "\n=== Combined diff ===")
  renderThings referentLabel Referent.toText diff.terms
  renderThings (const "type") Reference.toText diff.types
  where
    renderThings :: (ref -> Text) -> (ref -> Text) -> Map Name (Merge.CombinedDiffOp ref) -> IO ()
    renderThings label renderRef things =
      for_ (Map.toList things) \(name, op) ->
        Text.putStrLn case op of
          Merge.CombinedDiffOp'Add who ->
            Text.green $
              "+ "
                <> Text.italic (label (EitherWayI.value who))
                <> " "
                <> Name.toText name
                <> " "
                <> renderRef (EitherWayI.value who)
                <> " ("
                <> renderWho who
                <> ")"
          Merge.CombinedDiffOp'Delete who ->
            Text.red $
              "- "
                <> Text.italic (label (EitherWayI.value who))
                <> " "
                <> Name.toText name
                <> " "
                <> renderRef (EitherWayI.value who)
                <> " ("
                <> renderWho who
                <> ")"
          Merge.CombinedDiffOp'Update who ->
            Text.yellow $
              "% "
                <> Text.italic (label (EitherWayI.value who).new)
                <> " "
                <> Name.toText name
                <> " "
                <> renderRef (EitherWayI.value who).new
                <> " ("
                <> renderWho who
                <> ")"
          Merge.CombinedDiffOp'Conflict ref ->
            Text.magenta $
              "! "
                <> Text.italic (label ref.alice)
                <> "/"
                <> Text.italic (label ref.bob)
                <> " "
                <> Name.toText name
                <> " "
                <> renderRef ref.alice
                <> "/"
                <> renderRef ref.bob

    renderWho :: Merge.EitherWayI v -> Text
    renderWho = \case
      Merge.OnlyAlice _ -> "Alice"
      Merge.OnlyBob _ -> "Bob"
      Merge.AliceAndBob _ -> "Alice and Bob"

realDebugPartitionedDiff ::
  Merge.TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
  DefnsF Merge.Unconflicts Referent TypeReference ->
  IO ()
realDebugPartitionedDiff conflicts unconflicts = do
  Text.putStrLn (Text.bold "\n=== Alice conflicts ===")
  renderConflicts "termid" conflicts.alice.terms (Merge.Alice ())
  renderConflicts "typeid" conflicts.alice.types (Merge.Alice ())

  Text.putStrLn (Text.bold "\n=== Bob conflicts ===")
  renderConflicts "termid" conflicts.bob.terms (Merge.Bob ())
  renderConflicts "typeid" conflicts.bob.types (Merge.Bob ())

  Text.putStrLn (Text.bold "\n=== Alice unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.alice
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.alice
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.alice
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.alice
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.alice
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.alice

  Text.putStrLn (Text.bold "\n=== Bob unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.bob
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.bob
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.bob
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.bob
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.bob
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.bob

  Text.putStrLn (Text.bold "\n=== Alice-and-Bob unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.both
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.both
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.both
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.both
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.both
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.both
  where
    renderConflicts :: Text -> Map Name Reference.Id -> Merge.EitherWay () -> IO ()
    renderConflicts label conflicts who =
      for_ (Map.toList conflicts) \(name, ref) ->
        Text.putStrLn $
          Text.magenta $
            "! "
              <> Text.italic label
              <> " "
              <> Name.toText name
              <> " "
              <> Reference.idToText ref
              <> " ("
              <> (case who of Merge.Alice () -> "Alice"; Merge.Bob () -> "Bob")
              <> ")"

    renderUnconflicts ::
      (Text -> Text) ->
      Text ->
      (ref -> Text) ->
      (ref -> Text) ->
      Map Name ref ->
      IO ()
    renderUnconflicts color action label renderRef unconflicts =
      for_ (Map.toList unconflicts) \(name, ref) ->
        Text.putStrLn $
          color $
            action
              <> " "
              <> Text.italic (label ref)
              <> " "
              <> Name.toText name
              <> " "
              <> renderRef ref

referentLabel :: Referent -> Text
referentLabel ref
  | Referent'.isConstructor ref = "constructor"
  | otherwise = "term"
