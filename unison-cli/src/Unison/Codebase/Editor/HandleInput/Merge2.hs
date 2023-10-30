-- | @merge@ input handler
module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Lens (Lens', over, view, (%=), (.=), (.~), (^.))
import Control.Monad.Except qualified as Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Semialign (alignWith, unzip, zip)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import GHC.Clock (getMonotonicTime)
import Text.ANSI qualified as Text
import Text.Printf (printf)
import U.Codebase.Branch (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.BranchV3 (BranchV3 (..), CausalBranchV3)
import U.Codebase.BranchV3 qualified as BranchV3
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference
  ( Reference,
    Reference' (..),
    ReferenceType,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
  )
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite (ProjectBranch)
import U.Codebase.Sqlite.Queries qualified as Queries
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import U.Codebase.Term (Term)
import Unison.Builtin qualified as Builtins
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment, typecheckTerm)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as V1 (Branch (..), Branch0)
import Unison.Codebase.Branch qualified as V1.Branch
import Unison.Codebase.Causal qualified as V1 (Causal)
import Unison.Codebase.Causal qualified as V1.Causal
import Unison.Codebase.Causal.Type qualified as V1.Causal
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration qualified as V1 (Decl)
import Unison.DataDeclaration qualified as V1.Decl
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Merge2 (MergeOutput)
import Unison.Merge2 qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Reference (TermReference)
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.Result qualified as Result
import Unison.Server.Backend qualified as Backend
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Syntax.Parser qualified as Parser
import Unison.Term qualified as V1 (Term)
import Unison.Term qualified as V1.Term
import Unison.UnisonFile.Type (TypecheckedUnisonFile (TypecheckedUnisonFileId), UnisonFile)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Cache qualified as Cache
import Unison.Util.Map qualified as Map
import Unison.Util.Nametree
  ( Defns (..),
    Nametree (..),
    flattenNametree,
    traverseNametreeWithName,
    unflattenNametree,
  )
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.Star3 (Star3)
import Unison.Util.Star3 qualified as Star3
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip)

-- Temporary simple way to time a transaction
step :: Text -> Transaction a -> Transaction a
step name action = do
  t0 <- Sqlite.unsafeIO getMonotonicTime
  result <- action
  Sqlite.unsafeIO do
    t1 <- getMonotonicTime
    Text.putStrLn (Text.pack (printf "%4d ms | " (round ((t1 - t0) * 1000) :: Int)) <> name)
  pure result

data MergeResult v a
  = -- PPED is whatever `prettyUnisonFile` accepts
    MergePropagationNotTypecheck PPED.PrettyPrintEnvDecl (UnisonFile v a)
  | MergeConflicts
      (V1.Branch0 Transaction) -- The unconflicted stuff
      PPED.PrettyPrintEnvDecl
      (MergeOutput v a)
  | MergeDone

mkTypecheckFnCli :: Cli (UnisonFile Symbol Ann -> Transaction (Maybe (TypecheckedUnisonFile Symbol Ann)))
mkTypecheckFnCli = do
  Cli.Env {codebase, generateUniqueName} <- ask
  rootBranch <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  let parseNames = Backend.getCurrentParseNames (Backend.Within (Path.unabsolute currentPath)) rootBranch
  pure (mkTypecheckFn codebase generateUniqueName currentPath parseNames)

mkTypecheckFn ::
  Codebase.Codebase IO Symbol Ann ->
  IO Parser.UniqueName ->
  Path.Absolute ->
  NamesWithHistory.NamesWithHistory ->
  UnisonFile Symbol Ann ->
  Transaction (Maybe (TypecheckedUnisonFile Symbol Ann))
mkTypecheckFn codebase generateUniqueName currentPath parseNames unisonFile = do
  uniqueName <- Sqlite.unsafeIO generateUniqueName
  let parsingEnv =
        Parser.ParsingEnv
          { uniqueNames = uniqueName,
            uniqueTypeGuid = Cli.loadUniqueTypeGuid currentPath,
            names = parseNames
          }
  typecheckingEnv <-
    computeTypecheckingEnvironment (FileParsers.ShouldUseTndr'Yes parsingEnv) codebase [] unisonFile
  let Result.Result _notes maybeTypecheckedUnisonFile = FileParsers.synthesizeFile typecheckingEnv unisonFile
  pure maybeTypecheckedUnisonFile

handleMerge :: ProjectBranchName -> Cli ()
handleMerge bobBranchName = do
  typecheck <- mkTypecheckFnCli

  -- Load the current project branch ("alice"), and the branch from the same project to merge in ("bob")
  (ProjectAndBranch project aliceProjectBranch, _path) <- Cli.expectCurrentProjectBranch
  bobProjectBranch <- Cli.expectProjectBranchByName project bobBranchName
  let projectBranches = Merge.TwoWay {alice = aliceProjectBranch, bob = bobProjectBranch}
  let alicePath = Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) (aliceProjectBranch ^. #branchId))
  let bobPath = Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) (bobProjectBranch ^. #branchId))

  -- Create a bunch of cached database lookup functions
  Cli.Env {codebase} <- ask
  db@MergeDatabase {loadCausal, loadDecl, loadDeclType, loadTerm} <- makeMergeDatabase

  mergeResult <-
    Cli.runTransactionWithRollback \abort0 -> do
      -- Helper used throughout: abort this transaction with an output message.
      let abort :: Merge.PreconditionViolation -> Transaction void
          abort =
            mergePreconditionViolationToOutput db >=> abort0

      -- Load causals
      aliceCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
      bobCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)
      maybeLcaCausal <-
        step "compute lca" (Operations.lca (Causal.causalHash aliceCausal) (Causal.causalHash bobCausal)) >>= \case
          Nothing -> pure Nothing
          Just lcaCausalHash -> do
            -- If LCA == bob, then we are at or ahead of bob, so the merge is done.
            when (lcaCausalHash == bobCausal ^. #causalHash) do
              abort0 $
                Output.MergeAlreadyUpToDate
                  (Right (ProjectAndBranch project bobProjectBranch))
                  (Right (ProjectAndBranch project aliceProjectBranch))
            Just <$> loadCausal lcaCausalHash

      -- Load shallow branches
      aliceBranch <- Causal.value aliceCausal
      bobBranch <- Causal.value bobCausal

      -- Load deep definitions
      NamespaceInfo aliceCausalTree aliceConstructorNameToDeclName aliceDefns <-
        step "load alice definitions" $
          loadNamespaceInfo db abort (projectBranches ^. #alice . #name) (aliceCausal ^. #causalHash) aliceBranch
      NamespaceInfo bobCausalTree bobConstructorNameToDeclName bobDefns <-
        step "load bob definitions" $
          loadNamespaceInfo db abort (projectBranches ^. #bob . #name) (bobCausal ^. #causalHash) bobBranch
      let defns = Merge.TwoWay {alice = aliceDefns, bob = bobDefns}
      let causalHashes = Merge.TwoWay {alice = aliceCausalTree, bob = bobCausalTree}
      let constructorNameToDeclName = Merge.TwoWay {alice = aliceConstructorNameToDeclName, bob = bobConstructorNameToDeclName}

      (maybeLcaLibdeps, diffs) <- do
        case maybeLcaCausal of
          Nothing -> do
            diffs <-
              Merge.nameBasedNamespaceDiff
                loadDecl
                loadTerm
                Merge.TwoOrThreeWay {lca = Nothing, alice = aliceDefns, bob = bobDefns}
            pure (Nothing, diffs)
          Just lcaCausal -> do
            lcaBranch <- step "load lca shallow branch" $ Causal.value lcaCausal
            lcaDefns <- step "load lca definitions" $ loadLcaDefinitions abort (lcaCausal ^. #causalHash) lcaBranch
            diffs <-
              Merge.nameBasedNamespaceDiff
                loadDecl
                loadTerm
                Merge.TwoOrThreeWay {lca = Just lcaDefns, alice = aliceDefns, bob = bobDefns}
            abortIfAnyConflictedAliases abort projectBranches lcaDefns diffs
            lcaLibdeps <- step "load lca library dependencies" $ loadLibdeps lcaBranch
            pure (Just lcaLibdeps, diffs)

      let conflictedNames =
            Defns
              { terms = getConflicts (view #terms <$> diffs),
                types = getConflicts (view #types <$> diffs)
              }

      -- Load and merge libdeps
      (libdepsCausalParents, libdeps) <- do
        maybeAliceLibdeps <- step "load alice library dependencies" $ loadLibdeps aliceBranch
        maybeBobLibdeps <- step "load bob library dependencies" $ loadLibdeps bobBranch
        pure $
          ( Set.fromList (catMaybes [fst <$> maybeAliceLibdeps, fst <$> maybeBobLibdeps]),
            Merge.mergeLibdeps
              ((==) `on` Causal.causalHash)
              getTwoFreshNames
              (maybe Map.empty snd <$> maybeLcaLibdeps)
              (maybe Map.empty snd maybeAliceLibdeps)
              (maybe Map.empty snd maybeBobLibdeps)
          )

      let updates = filterUpdates defns diffs
      dependents <- collectDependentsOfInterest defns updates

      -- If there are no conflicts, then proceed to typechecking
      if null (conflictedNames ^. #terms) && null (conflictedNames ^. #types)
        then do
          whatToTypecheck :: Merge.DeepRefsId' <-
            step "compute whatToTypecheck" $
              Merge.whatToTypecheck
                ( aliceDefns & over #terms BiMultimap.range & over #types BiMultimap.range,
                  updates ^. #alice
                )
                ( bobDefns & over #terms BiMultimap.range & over #types BiMultimap.range,
                  updates ^. #bob
                )

          let namelookup :: Merge.RefToName =
                let multimapMerge :: forall a b. Ord a => Ord b => Merge.TwoWay (BiMultimap a b) -> Map a b
                    multimapMerge (Merge.TwoWay ma mb) =
                      Map.merge
                        (Map.mapMissing \_ -> NESet.findMin)
                        (Map.mapMissing \_ -> NESet.findMin)
                        ( Map.zipWithMatched \_ a b ->
                            let preferred = NESet.intersection a b
                             in case Set.lookupMin preferred of
                                  Just x -> x
                                  Nothing -> NESet.findMin a
                        )
                        (BiMultimap.domain ma)
                        (BiMultimap.domain mb)
                    termNames :: Map Referent Name
                    termNames = multimapMerge (view #terms <$> defns)
                    typeNames :: Map TypeReference Name
                    typeNames = multimapMerge (view #types <$> defns)
                 in Defns termNames typeNames

          uf <- do
            let combinedUpdates :: Merge.UpdatesRefnt
                combinedUpdates =
                  -- These left-biased unions are fine; at this point we know Alice's and Bob's updates
                  Defns
                    { terms = Map.union (updates ^. #alice . #terms) (updates ^. #bob . #terms),
                      types = Map.union (updates ^. #alice . #types) (updates ^. #bob . #types)
                    }
            Merge.computeUnisonFile namelookup loadTerm loadDecl loadDeclType whatToTypecheck combinedUpdates

          typecheck uf >>= \case
            Just tuf@(TypecheckedUnisonFileId {}) -> do
              let saveDefnsToCodebase = werror "saveDefnsToCodebase"
              let consAndSaveNamespace = werror "consAndSaveNamespace"
              saveDefnsToCodebase tuf
              consAndSaveNamespace tuf
              pure MergeDone
            Nothing -> do
              let ppe :: PrettyPrintEnvDecl = werror "ppe"
              pure $ MergePropagationNotTypecheck ppe (void uf)
        else do
          conflicted <- filterConflicts conflictedNames defns & onLeft abort
          let dirty = conflicted <> dependents
          let unconflicted = filterUnconflicted constructorNameToDeclName updates dirty defns

          let unconflictedNametree = makeNametreeFromUnconflicted unconflicted causalHashes
          let unconflictedV3Branch = namespaceToV3Branch db unconflictedNametree
          unconflictedV1Branch <-
            convertV3BranchAndLibdepsToV1Branch
              db
              unconflictedV3Branch
              libdepsCausalParents
              libdeps

          mergeOutput <-
            mkMergeOutput
              db
              (aliceProjectBranch ^. #name)
              (bobProjectBranch ^. #name)
              defns
              conflicted
              dependents

          names <- (<>) <$> convertDefnsToNames db aliceDefns <*> convertDefnsToNames db bobDefns
          ppe <- Codebase.hashLength <&> (`PPE.fromNamesDecl` (NamesWithHistory.fromCurrentNames names))
          pure (MergeConflicts unconflictedV1Branch ppe mergeOutput)

  scratchFile <-
    Cli.getLatestFile >>= \case
      Just (scratchFile, _) -> pure scratchFile
      Nothing -> pure "merge.u"

  case mergeResult of
    MergePropagationNotTypecheck ppe uf -> do
      Cli.respond $ Output.OutputMergeScratchFile ppe scratchFile (void uf)
    MergeConflicts unconflicted ppe mergeOutput -> do
      temporaryBranchName <- do
        -- Small race condition: since picking a branch name and creating the branch happen in different
        -- transactions, creating could fail.

        allBranchNames <-
          fmap (Set.fromList . map snd) do
            Cli.runTransaction do
              Queries.loadAllProjectBranchesBeginningWith
                (project ^. #projectId)
                Nothing

        let -- all branch name candidates in order of preference:
            --   merge-<alice>-into-<bob>
            --   merge-<alice>-into-<bob>-2
            --   merge-<alice>-into-<bob>-3
            --   ...
            allCandidates :: [ProjectBranchName]
            allCandidates =
              preferred : do
                n <- [(2 :: Int) ..]
                pure (unsafeFrom @Text (into @Text preferred <> "-" <> tShow n))
              where
                preferred :: ProjectBranchName
                preferred =
                  unsafeFrom @Text $
                    "merge-"
                      <> into @Text (bobProjectBranch ^. #name)
                      <> "-into-"
                      <> into @Text (aliceProjectBranch ^. #name)

        pure (fromJust (List.find (\name -> not (Set.member name allBranchNames)) allCandidates))

      temporaryBranchId <-
        HandleInput.Branch.doCreateBranch
          (HandleInput.Branch.CreateFrom'Branch (ProjectAndBranch project aliceProjectBranch))
          project
          temporaryBranchName
          ("merge " <> into @Text (bobProjectBranch ^. #name))

      let temporaryBranchPath :: Path
          temporaryBranchPath =
            Path.unabsolute (Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) temporaryBranchId))

      Cli.stepAt
        ("merge " <> into @Text (bobProjectBranch ^. #name))
        ( temporaryBranchPath,
          \_ -> V1.Branch.transform0 (Codebase.runTransaction codebase) unconflicted
        )

      (scratchFile, _) <- Cli.expectLatestFile
      Cli.respond $ Output.OutputMergeConflictScratchFile ppe scratchFile (void mergeOutput)
    MergeDone -> Cli.respond Output.Success

------------------------------------------------------------------------------------------------------------------------
-- Merge database

-- A mini record-of-functions that contains just the (possibly backed by a cache) database queries used in merge.
data MergeDatabase = MergeDatabase
  { loadCausal :: CausalHash -> Transaction (CausalBranch Transaction),
    loadDecl :: TypeReferenceId -> Transaction (V1.Decl Symbol Ann),
    loadDeclNumConstructors :: TypeReferenceId -> Transaction Int,
    loadDeclType :: TypeReference -> Transaction ConstructorType,
    loadTerm :: TermReferenceId -> Transaction (V1.Term Symbol Ann),
    loadV1Branch :: CausalHash -> Transaction (V1.Branch Transaction)
  }

makeMergeDatabase :: Cli MergeDatabase
makeMergeDatabase = do
  -- Create a bunch of cached database lookup functions
  Cli.Env {codebase} <- ask
  loadCausal <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache Operations.expectCausalBranchByCausalHash)
  loadDecl <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache (Codebase.unsafeGetTypeDeclaration codebase))
  loadDeclNumConstructors <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache Operations.expectDeclNumConstructors)
  -- Since loading a decl type loads the decl and projects out the decl type, just reuse the loadDecl cache
  let loadDeclType ref =
        case ref of
          ReferenceBuiltin name ->
            Map.lookup ref Builtins.builtinConstructorType
              & maybe (error ("Unknown builtin: " ++ Text.unpack name)) pure
          ReferenceDerived refId -> V1.Decl.constructorType <$> loadDecl refId
  loadTerm <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache (Codebase.unsafeGetTerm codebase))
  let loadV1Branch = Codebase.expectBranchForHash codebase
  pure MergeDatabase {loadCausal, loadDecl, loadDeclNumConstructors, loadDeclType, loadTerm, loadV1Branch}

------------------------------------------------------------------------------------------------------------------------
-- Loading namespace info from the database

-- Information we load and compute about a namespace.
data NamespaceInfo = NamespaceInfo
  { -- The causal hash at every node in a namespace.
    causalHashes :: !(Nametree CausalHash),
    -- A mapping from constructor name "foo.bar.Maybe.internal.Just" to decl name "foo.bar.Maybe"
    constructorNameToDeclName :: !(Map Name Name),
    -- The definitions in a namespace.
    definitions :: !(Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
  }

-- Load namespace info into memory.
--
-- Fails if:
--   * The "lib" namespace contains any top-level terms or decls. (Only child namespaces are expected here).
--   * One name is associated with more than one reference.
--   * Any type declarations are "incoherent" (see `checkDeclCoherency`)
loadNamespaceInfo ::
  MergeDatabase ->
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  ProjectBranchName ->
  CausalHash ->
  Branch Transaction ->
  Transaction NamespaceInfo
loadNamespaceInfo db abort branchName causalHash branch = do
  Map.lookup Name.libSegment (branch ^. #children) & onJust \libdepsCausal -> do
    libdepsBranch <- Causal.value libdepsCausal
    when (not (Map.null (libdepsBranch ^. #terms)) || not (Map.null (libdepsBranch ^. #types))) do
      abort Merge.DefnsInLib
  defns0 <- loadNamespaceInfo0 branch causalHash
  defns1 <- assertNamespaceHasNoConflictedNames defns0 & onLeft abort
  constructorNameToDeclName <- checkDeclCoherency db branchName defns1 & onLeftM abort
  let (definitions, causalHashes) = unzip defns1
  pure
    NamespaceInfo
      { causalHashes,
        constructorNameToDeclName,
        definitions =
          Defns
            { terms = flattenNametree (view #terms) definitions,
              types = flattenNametree (view #types) definitions
            }
      }

-- Like `loadNamespaceInfo`, but for loading the LCA, which has fewer preconditions.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadLcaDefinitions ::
  Monad m =>
  (forall void. Merge.PreconditionViolation -> m void) ->
  CausalHash ->
  Branch m ->
  m (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
loadLcaDefinitions abort causalHash branch = do
  defns0 <- loadNamespaceInfo0 branch causalHash
  defns1 <- assertNamespaceHasNoConflictedNames defns0 & onLeft abort
  let defns2 = fst <$> defns1
  pure
    Defns
      { terms = flattenNametree (view #terms) defns2,
        types = flattenNametree (view #types) defns2
      }

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceInfo0 ::
  Monad m =>
  Branch m ->
  CausalHash ->
  m
    ( Nametree
        ( Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)),
          CausalHash
        )
    )
loadNamespaceInfo0 branch causalHash = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  let value = (Defns {terms, types}, causalHash)
  children <-
    for (Map.delete Name.libSegment (branch ^. #children)) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadNamespaceInfo0_ childBranch (childCausal ^. #causalHash)
  pure Nametree {value, children}

loadNamespaceInfo0_ ::
  Monad m =>
  Branch m ->
  CausalHash ->
  m
    ( Nametree
        ( Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)),
          CausalHash
        )
    )
loadNamespaceInfo0_ branch causalHash = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  let value = (Defns {terms, types}, causalHash)
  children <-
    for (branch ^. #children) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadNamespaceInfo0_ childBranch (childCausal ^. #causalHash)
  pure Nametree {value, children}

-- | Assert that there are no unconflicted names in a namespace.
assertNamespaceHasNoConflictedNames ::
  Nametree
    ( Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)),
      CausalHash
    ) ->
  Either
    Merge.PreconditionViolation
    ( Nametree
        ( Defns (Map NameSegment Referent) (Map NameSegment TypeReference),
          CausalHash
        )
    )
assertNamespaceHasNoConflictedNames =
  traverseNametreeWithName \names (Defns {terms, types}, causalHash) -> do
    terms <-
      terms & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTermName (Name.fromReverseSegments (name :| names)))
    types <-
      types & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTypeName (Name.fromReverseSegments (name :| names)))
    pure (Defns terms types, causalHash)
  where
    assertUnconflicted :: (Set ref -> Merge.PreconditionViolation) -> Set ref -> Either Merge.PreconditionViolation ref
    assertUnconflicted conflicted refs =
      case Set.asSingleton refs of
        Nothing -> Left (conflicted refs)
        Just ref -> Right ref

-- The "decl coherency check": a type declaration in a namespace is "coherent" if it satisfies both of the following
-- criteria.
--
--   1. For each naming of the type decl (say "Foo"#foohash), there exists exactly one name for each of its constructors
--      arbitrarily deep in the corresponding namespace ("Foo" in this example).
--
--      This allows us to render the decl naturally, as in
--
--        structural type Foo
--          = Bar Nat Int
--          | internal.hello.Bonk Nat
--
--      which corresponds to the three names
--
--        "Foo"                     => #foohash
--        "Foo.Bar"                 => #foohash#0
--        "Foo.internal.hello.Bonk" => #foohash#1
--
--      We could not do if there was at least one constructor whose full name does not contain the full name of the type
--      decl itself as a prefix.
--
--      A notable consequence of this requirement is that a second naming of a decl (i.e. an alias) cannot be embedded
--      within the first naming, as in:
--
--        type Foo = ...
--        type Foo.some.inner.namespace = ... -- an alias of Foo
--
--   2. No constructor has a "stray" name that does not have a prefix that equals the type declaration's name. For
--      example, in the namespace
--
--        "Foo"                 => #foohash
--        "Foo.Bar"             => #foohash#0
--        "Deep.What.SomeAlias" => #foohash#0
--
--      the constructor "What.SomeAlias" is "stray", as the type decl #foohash has no name that matches any prefix
--      (i.e. "Deep.What" nor "Deep").
--
-- On to the implementation. We are going to traverse the namespace depth-first. As we go, we have a stateful mapping
-- between decl reference that we *have* seen a name for in one of our parent namespace, and its corresponding set of
-- constructors that we *haven't* yet seen names for, but expect to, before fully searching the corresponding
-- sub-namespace (e.g. the child namespace named "Foo" of the namepace that declares a decl "Foo").
--
-- When processing a namespace, we first process all terms. Each constructor will fall into one of three cases:
--
--   +----------------------------------------------------------------------------------------------------------------+
--   | Case         | Mapping before       | Encountered constructor | Mapping after                                  |
--   +----------------------------------------------------------------------------------------------------------------+
--   | Happy path   | { #foo : {0, 1, 2} } | #foo#1                  | { #foo : {0, 2} }                              |
--   | Already seen | { #foo : {0, 1, 2} } | #foo#5                  | Error: duplicate naming for constructor #foo#5 |
--   | Never seen   | { #foo : {0, 1, 2} } | #bar#2                  | Error: stray constructor #bar#2                |
--   +----------------------------------------------------------------------------------------------------------------+
--
-- In "happy path", we see a naming of a constructor that we're expecting, and check it off.
-- In "already seen", we see a second naming of a constructor that we're no longer expecting, and fail.
-- In "never seen", we see a naming of a constructor before any naming of its decl, so we fail.
--
-- Next, we process all type decls. Each will again fall into one of three cases:
--
--   +-----------------------------------------------------------------------------------------------------+
--   | Case             | Mapping before       | Declaration | Num constructors | New mapping              |
--   +-----------------------------------------------------------------------------------------------------+
--   | Uninhabited decl |                      | #foo        | 0                |                          |
--   | Inhabited decl   |                      | #foo        | 1 or more        | { #foo : {0, ..., n-1} } |
--   | Already seen     | { foo : {0, 1, 2}  } | #foo        | Irrelevant       | Error: nested decl alias |
--   +-----------------------------------------------------------------------------------------------------+
--
-- In "uninhabited decl", we find a decl with no constructors, so we don't expect anything new.
-- In "already seen", we find a second naming of a decl, whose constructors will necessarily violate coherency condition
--   (1) above.
--
-- In "inhabited decl", we find a decl with N constructors, and handle it by:
--   1. Adding to our state that we expect a name for each.
--   2. Recursing into the child namespace whose name matches the decl.
--   3. (If we return from the recursion without short-circuiting) remove the mapping added in step (1) and assert that
--      its value is the empty set (meaning we encountered a name for every constructor).
--
-- Note: This check could be moved into SQLite (with sufficient schema support) some day, but for now, because the merge
-- algorithm needs to pull lots of stuff into memory anyway, we just do this in memory, too.
--
-- Note: once upon a time, decls could be "incoherent". Then, we decided we want decls to be "coherent". Thus, this
-- machinery was invented.
checkDeclCoherency ::
  MergeDatabase ->
  ProjectBranchName ->
  ( Nametree
      ( Defns (Map NameSegment Referent) (Map NameSegment TypeReference),
        CausalHash
      )
  ) ->
  Transaction (Either Merge.PreconditionViolation (Map Name Name))
checkDeclCoherency MergeDatabase {loadDeclNumConstructors} branchName =
  runExceptT
    . fmap (view #constructorNameToDeclName)
    . (`State.execStateT` DeclCoherencyCheckState Map.empty Map.empty)
    . go []
  where
    go ::
      [NameSegment] ->
      ( Nametree
          ( Defns (Map NameSegment Referent) (Map NameSegment TypeReference),
            CausalHash
          )
      ) ->
      StateT DeclCoherencyCheckState (ExceptT Merge.PreconditionViolation Transaction) ()
    go prefix (Nametree (Defns {terms, types}, _) children) = do
      for_ (Map.toList terms) \case
        (_, Referent.Ref _) -> pure ()
        (_, Referent.Con (ReferenceBuiltin _) _) -> pure ()
        (name, Referent.Con (ReferenceDerived typeRef) conId) -> do
          DeclCoherencyCheckState {expectedConstructors} <- State.get
          expectedConstructors1 <- lift (Except.except (Map.upsertF f typeRef expectedConstructors))
          #expectedConstructors .= expectedConstructors1
          where
            f :: Maybe (IntMap MaybeConstructorName) -> Either Merge.PreconditionViolation (IntMap MaybeConstructorName)
            f = \case
              Nothing -> Left (Merge.StrayConstructor (fullName name))
              Just expected -> IntMap.alterF g (unsafeFrom @Word64 conId) expected
                where
                  g :: Maybe MaybeConstructorName -> Either Merge.PreconditionViolation (Maybe MaybeConstructorName)
                  g = \case
                    Nothing -> error "didnt put expected constructor id"
                    Just NoConstructorNameYet -> Right (Just (YesConstructorName (fullName name)))
                    Just (YesConstructorName firstName) -> Left (Merge.ConstructorAlias branchName firstName (fullName name))

      childrenWeWentInto <-
        forMaybe (Map.toList types) \case
          (_, ReferenceBuiltin _) -> pure Nothing
          (name, ReferenceDerived typeRef) -> do
            DeclCoherencyCheckState {expectedConstructors} <- State.get
            whatHappened <- do
              let recordNewDecl ::
                    Maybe (IntMap MaybeConstructorName) ->
                    Compose (ExceptT Merge.PreconditionViolation Transaction) WhatHappened (IntMap MaybeConstructorName)
                  recordNewDecl =
                    Compose . \case
                      Just _ -> Except.throwError (Merge.NestedDeclAlias typeName)
                      Nothing ->
                        lift (loadDeclNumConstructors typeRef) <&> \case
                          0 -> UninhabitedDecl
                          n -> InhabitedDecl (IntMap.fromAscList [(i, NoConstructorNameYet) | i <- [0 .. n - 1]])
              lift (getCompose (Map.upsertF recordNewDecl typeRef expectedConstructors))
            case whatHappened of
              UninhabitedDecl -> pure Nothing
              InhabitedDecl expectedConstructors1 -> do
                child <-
                  Map.lookup name children & onNothing do
                    Except.throwError (Merge.NoConstructorNames typeName)
                #expectedConstructors .= expectedConstructors1
                go (name : prefix) child
                DeclCoherencyCheckState {expectedConstructors} <- State.get
                -- fromJust is safe here because we upserted `typeRef` key above
                let (fromJust -> maybeConstructorNames, expectedConstructors1) =
                      Map.deleteLookup typeRef expectedConstructors
                constructorNames <-
                  unMaybeConstructorNames maybeConstructorNames & onNothing do
                    Except.throwError (Merge.MissingConstructorName typeName)
                #expectedConstructors .= expectedConstructors1
                #constructorNameToDeclName %= \constructorNameToDeclName ->
                  foldr
                    (\constructorName -> Map.insert constructorName typeName)
                    constructorNameToDeclName
                    constructorNames
                pure (Just name)
            where
              typeName = fullName name

      let childrenWeHaventGoneInto = children `Map.withoutKeys` Set.fromList childrenWeWentInto
      for_ (Map.toList childrenWeHaventGoneInto) \(name, child) -> go (name : prefix) child
      where
        fullName name =
          Name.fromReverseSegments (name :| prefix)

data DeclCoherencyCheckState = DeclCoherencyCheckState
  { expectedConstructors :: !(Map TypeReferenceId (IntMap MaybeConstructorName)),
    constructorNameToDeclName :: !(Map Name Name)
  }
  deriving stock (Generic)

data MaybeConstructorName
  = NoConstructorNameYet
  | YesConstructorName !Name

unMaybeConstructorNames :: IntMap MaybeConstructorName -> Maybe [Name]
unMaybeConstructorNames =
  traverse f . IntMap.elems
  where
    f :: MaybeConstructorName -> Maybe Name
    f = \case
      NoConstructorNameYet -> Nothing
      YesConstructorName name -> Just name

data WhatHappened a
  = UninhabitedDecl
  | InhabitedDecl !a
  deriving stock (Functor, Show)

-- | Load the library dependencies (lib.*) of a namespace.
loadLibdeps :: Branch Transaction -> Transaction (Maybe (CausalHash, Map NameSegment (CausalBranch Transaction)))
loadLibdeps branch =
  case Map.lookup Name.libSegment (Branch.children branch) of
    Nothing -> pure Nothing
    Just dependenciesCausal -> do
      dependenciesBranch <- Causal.value dependenciesCausal
      pure (Just (Causal.causalHash dependenciesCausal, Branch.children dependenciesBranch))

------------------------------------------------------------------------------------------------------------------------
-- Conflicts

-- `getConflicts diffs` returns the set of conflicted names in `diffs`, where `diffs` contains two branches' diffs from
-- their LCA.
getConflicts :: forall hash name. (Eq hash, Ord name) => Merge.TwoWay (Map name (Merge.DiffOp hash)) -> Set name
getConflicts (Merge.TwoWay aliceDiff bobDiff) =
  Map.keysSet (Map.mapMaybe id (alignWith f aliceDiff bobDiff))
  where
    f :: These (Merge.DiffOp hash) (Merge.DiffOp hash) -> Maybe ()
    f = \case
      These (Merge.Added x) (Merge.Added y) | x /= y -> Just ()
      These (Merge.Updated _ x) (Merge.Updated _ y) | x /= y -> Just ()
      -- Not a conflict:
      --   delete/delete
      -- Not a conflict, perhaps only temporarily, because it's easier to implement (we ignore these deletes):
      --   delete/update
      --   update/delete
      -- Impossible cases:
      --   add/delete
      --   add/update
      _ -> Nothing

------------------------------------------------------------------------------------------------------------------------
-- Rejecting merges that contain conflicted alias

abortIfAnyConflictedAliases ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  Merge.TwoWay Sqlite.ProjectBranch ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Merge.TwoWay (Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash))) ->
  Transaction ()
abortIfAnyConflictedAliases abort projectBranchNames lcaDefns diffs = do
  findConflictedAlias lcaDefns (diffs ^. #alice) & onJust \(name1, name2) ->
    abort (Merge.ConflictedAliases (projectBranchNames ^. #alice . #name) name1 name2)
  findConflictedAlias lcaDefns (diffs ^. #bob) & onJust \(name1, name2) ->
    abort (Merge.ConflictedAliases (projectBranchNames ^. #bob . #name) name1 name2)

-- @findConflictedAlias namespace diff@, given an old namespace and a diff to a new namespace, will return the first
-- "conflicted alias" encountered (if any), where a "conflicted alias" is a pair of names that referred to the same
-- thing in the old namespace, but different things in the new one.
--
-- For example, if the old namespace was
--
--   foo = #foo
--   bar = #foo
--
-- and the new namespace is
--
--   foo = #baz
--   bar = #qux
--
-- then (foo, bar) is a conflicted alias.
--
-- This function currently doesn't return whether the conflicted alias is a decl or a term, but it certainly could.
findConflictedAlias ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash)) ->
  Maybe (Name, Name)
findConflictedAlias defns diff =
  asum
    [ go (defns ^. #terms) (diff ^. #terms),
      go (defns ^. #types) (diff ^. #types)
    ]
  where
    go :: forall ref. Ord ref => BiMultimap ref Name -> Map Name (Merge.DiffOp Hash) -> Maybe (Name, Name)
    go namespace diff =
      asum (map f (Map.toList diff))
      where
        f :: (Name, Merge.DiffOp Hash) -> Maybe (Name, Name)
        f (name, op) =
          case op of
            Merge.Added _ -> Nothing
            Merge.Deleted _ -> Nothing
            Merge.Updated _ hash ->
              BiMultimap.lookupPreimage name namespace
                & Set.delete name
                & Set.toList
                & map (g hash)
                & asum
          where
            g :: Hash -> Name -> Maybe (Name, Name)
            g hash alias =
              case Map.lookup alias diff of
                Just (Merge.Updated _ hash2) | hash == hash2 -> Nothing
                _ -> Just (name, alias)

------------------------------------------------------------------------------------------------------------------------
-- Filtering definitions down to just updates, just conflicts, etc.

filterUpdates ::
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash))) ->
  Merge.TwoWay (Defns (Map Name Referent) (Map Name TypeReference))
filterUpdates defns diff =
  Merge.TwoWay
    { alice = filterUpdates1 (defns ^. #alice) (diff ^. #alice),
      bob = filterUpdates1 (defns ^. #bob) (diff ^. #bob)
    }

-- `filterUpdates1 defns diff` returns the subset of `defns` that corresponds to updates (according to `diff`).
filterUpdates1 ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash)) ->
  Defns (Map Name Referent) (Map Name TypeReference)
filterUpdates1 defns diff =
  defns
    & over #terms ((`Map.intersection` (Map.filter isUpdate (diff ^. #terms))) . BiMultimap.range)
    & over #types ((`Map.intersection` (Map.filter isUpdate (diff ^. #types))) . BiMultimap.range)
  where
    isUpdate :: Merge.DiffOp Hash -> Bool
    isUpdate = \case
      Merge.Added {} -> False
      Merge.Deleted {} -> False
      Merge.Updated {} -> True

-- `filterConflicts conflicts defns` filters `defns` down to just the conflicted type and term references.
--
-- Fails if it any conflict involving a builtin is discovered, since we can't handle those yet.
filterConflicts ::
  Defns (Set Name) (Set Name) ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Either Merge.PreconditionViolation (Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)))
filterConflicts conflicts defns = do
  alice <- filterConflicts1 conflicts (defns ^. #alice)
  bob <- filterConflicts1 conflicts (defns ^. #bob)
  pure Merge.TwoWay {alice, bob}

-- `filterConflicts1 defns conflicts` filters `defns` down to just the conflicted type and term references.
--
-- Fails if it any conflict involving a builtin is discovered, since we can't handle those yet.
filterConflicts1 ::
  Defns (Set Name) (Set Name) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Either Merge.PreconditionViolation (Defns (Set TermReferenceId) (Set TypeReferenceId))
filterConflicts1 conflicts defns = do
  terms <- foldlM doTerm Set.empty (Map.toList (onlyConflicted (conflicts ^. #terms) (defns ^. #terms)))
  types <- foldlM doType Set.empty (Map.toList (onlyConflicted (conflicts ^. #types) (defns ^. #types)))
  pure Defns {terms, types}
  where
    onlyConflicted :: Ord ref => Set Name -> BiMultimap ref Name -> Map Name ref
    onlyConflicted conflictedNames =
      (`Map.restrictKeys` conflictedNames) . BiMultimap.range

    doTerm :: Set TermReferenceId -> (Name, Referent) -> Either Merge.PreconditionViolation (Set TermReferenceId)
    doTerm acc (name, ref) =
      case ref of
        Referent.Con {} -> Right acc
        Referent.Ref (ReferenceBuiltin _) -> Left (Merge.ConflictInvolvingBuiltin name)
        Referent.Ref (ReferenceDerived ref) -> Right $! Set.insert ref acc

    doType :: Set TypeReferenceId -> (Name, TypeReference) -> Either Merge.PreconditionViolation (Set TypeReferenceId)
    doType acc (name, ref) =
      case ref of
        ReferenceBuiltin _ -> Left (Merge.ConflictInvolvingBuiltin name)
        ReferenceDerived ref -> Right $! Set.insert ref acc

-- `filterUnconflicted declName updates dirty defns` returns the subset of `defns` that are "unconflicted", i.e. ready
-- to put into a namespace and saved to the database.
--
--   * `declName`: Mappings from constructor name to decl name
--   * `dirty`: Conflicted things, plus dependents of interest (per other person's updates)
--   * `updates`: Updates
--   * `defns`: Definitions
filterUnconflicted ::
  Merge.TwoWay (Map Name Name) ->
  Merge.TwoWay (Defns (Map Name Referent) (Map Name TypeReference)) ->
  Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)) ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
filterUnconflicted constructorNameToDeclName updates dirty =
  f #bob #alice . f #alice #bob
  where
    f ::
      (forall a. Lens' (Merge.TwoWay a) a) ->
      (forall a. Lens' (Merge.TwoWay a) a) ->
      Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
      Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
    f alice bob =
      over alice (filterUnconflicted1 (constructorNameToDeclName ^. alice) (dirty ^. alice) (updates ^. bob))

-- `filterUnconflicted1 declName dirty updates defns` returns the subset of `defns` that are "unconflicted", i.e. ready
-- to put into a namespace and saved to the database.
--
--   * `declName`: Alice's mapping from constructor name to decl name
--   * `dirty`: Alice's conflicted things, plus her dependents of interest (per Bob's updates)
--   * `updates`: Bob's updates
--   * `defns`: Alice's definitions
filterUnconflicted1 ::
  Map Name Name ->
  Defns (Set TermReferenceId) (Set TypeReferenceId) ->
  Defns (Map Name Referent) (Map Name TypeReference) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)
filterUnconflicted1 aliceConstructorNameToDeclName aliceDirty bobUpdates =
  over #types filterUnconflictedTypes . over #terms filterUnconflictedTerms
  where
    filterUnconflictedTerms :: BiMultimap Referent Name -> BiMultimap Referent Name
    filterUnconflictedTerms =
      BiMultimap.filterDom isNotConflicted >>> BiMultimap.filterDomain wasNotUpdatedByBob
      where
        isNotConflicted :: Referent -> Bool
        isNotConflicted = \case
          -- Consider a constructor term "unconflicted" if its decl is unconflicted.
          Referent.Con (ReferenceDerived typeRef) _conId -> not (Set.member typeRef (aliceDirty ^. #types))
          Referent.Ref (ReferenceDerived termRef) -> not (Set.member termRef (aliceDirty ^. #terms))
          -- Keep builtin constructors (which don't even exist) and builtin terms (since they can't be
          -- conflicted, per a precondition)
          Referent.Con (ReferenceBuiltin _) _ -> True
          Referent.Ref (ReferenceBuiltin _) -> True

        wasNotUpdatedByBob :: Referent -> NESet Name -> Bool
        wasNotUpdatedByBob ref names1 =
          case ref of
            Referent.Con _ _ ->
              let declNames = Set.mapMaybe (`Map.lookup` aliceConstructorNameToDeclName) names
               in Set.disjoint typeNamesUpdatedByBob declNames
            Referent.Ref _ -> Set.disjoint termNamesUpdatedByBob names
          where
            names = Set.NonEmpty.toSet names1

    filterUnconflictedTypes :: BiMultimap TypeReference Name -> BiMultimap TypeReference Name
    filterUnconflictedTypes =
      BiMultimap.withoutDom dirty >>> BiMultimap.filterDomain wasNotUpdatedByBob
      where
        dirty :: Set TypeReference
        dirty =
          Set.map ReferenceDerived (aliceDirty ^. #types)

        wasNotUpdatedByBob :: TypeReference -> NESet Name -> Bool
        wasNotUpdatedByBob _ =
          Set.disjoint typeNamesUpdatedByBob . Set.NonEmpty.toSet

    termNamesUpdatedByBob :: Set Name
    termNamesUpdatedByBob =
      Map.keysSet (bobUpdates ^. #terms)

    typeNamesUpdatedByBob :: Set Name
    typeNamesUpdatedByBob =
      Map.keysSet (bobUpdates ^. #types)

------------------------------------------------------------------------------------------------------------------------
-- Dependents of interest

-- `collectDependentsOfInterest defns updates` computes the "dependents of interest", per all definitions `defns` and
-- direct updates `updates`, which are:
--
--   1. Alice's transitive dependents of her dependencies of interest (see below for a definition).
--   2. Bob's transitive dependents of his dependencies of interest (see below for a definition).
--
-- For example, if:
--
--   * Alice updated term "foo" from #oldfoo to #alicefoo, and
--   * Bob uses the name "foo" to refer to #bobfoo (but didn't directly update the name "foo", otherwise we wouldn't
--     have gotten to this code -- nonetheless #bobfoo could be different than #oldfoo due to auto-propagated updates),
--
-- then Bob's "dependencies of interest" are just #bobfoo, and all of his transitive dependents of #bobfoo are his
-- "dependents of interest".
collectDependentsOfInterest ::
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Defns (Map Name Referent) (Map Name TypeReference)) ->
  Transaction (Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)))
collectDependentsOfInterest defns updates = do
  alice <- getDependents (defns ^. #alice) (updates ^. #bob)
  bob <- getDependents (defns ^. #bob) (updates ^. #alice)
  pure Merge.TwoWay {alice, bob}
  where
    getDependents ::
      Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      Defns (Map Name Referent) (Map Name TypeReference) ->
      Transaction (Defns (Set TermReferenceId) (Set TypeReferenceId))
    getDependents defns updates =
      -- The `dependentsWithinScope` query hands back a `Map Reference.Id ReferenceType`, but we would rather
      -- have two different maps, so we twiddle.
      fmap (Map.foldlWithKey' f (Defns Set.empty Set.empty)) do
        Operations.dependentsWithinScope
          (defnsToScope defns)
          (Set.union termDependencies typeDependencies)
      where
        f ::
          Defns (Set TermReferenceId) (Set TypeReferenceId) ->
          Reference.Id ->
          ReferenceType ->
          Defns (Set TermReferenceId) (Set TypeReferenceId)
        f acc ref = \case
          Reference.RtTerm -> acc & over #terms (Set.insert ref)
          Reference.RtType -> acc & over #types (Set.insert ref)

        Defns termDependencies typeDependencies =
          collectDependenciesOfInterest defns updates

-- `defnsToScope defns` converts a flattened namespace `defns` to the set of untagged reference ids contained within,
-- for the purpose of searching for transitive dependents of conflicts that are contained in that set.
defnsToScope :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Set Reference.Id
defnsToScope (Defns terms types) =
  Set.union
    (Set.mapMaybe Referent.toReferenceId (BiMultimap.dom terms))
    (Set.mapMaybe Reference.toId (BiMultimap.dom types))

-- `collectDependenciesOfInterest defns updates` computes the "dependencies of interest", per all definitions `defns`
-- and direct updates `updates`, which are:
--
--   1. Whatever Alice refers to by names that Bob updated.
--   2. Whatever Bob refers to by names that Alice updated.
--
-- For example, if:
--
--   * Alice updated term "foo" from #oldfoo to #alicefoo, and
--   * Bob uses the name "foo" to refer to #bobfoo (but didn't directly update the name "foo", otherwise we wouldn't
--     have gotten to this code -- nonetheless #bobfoo could be different than #oldfoo due to auto-propagated updates),
--
-- then Bob's "dependencies of interest" are just #bobfoo.
collectDependenciesOfInterest ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Map Name Referent) (Map Name TypeReference) ->
  Defns (Set TermReference) (Set TypeReference)
collectDependenciesOfInterest aliceDefns bobUpdates =
  goTerms (BiMultimap.range (aliceDefns ^. #terms) `Map.intersection` (bobUpdates ^. #terms))
    <> goTypes (BiMultimap.range (aliceDefns ^. #types) `Map.intersection` (bobUpdates ^. #types))
  where
    -- Turn each referent into a reference:
    --
    --   1. For constructors, just ignore the constructor id and use the type reference.
    --   2. For terms, use that term reference.
    goTerms :: Foldable f => f Referent -> Defns (Set TermReference) (Set TypeReference)
    goTerms =
      foldl' f (Defns Set.empty Set.empty)
      where
        f ::
          Defns (Set TermReference) (Set TypeReference) ->
          Referent ->
          Defns (Set TermReference) (Set TypeReference)
        f acc = \case
          Referent.Con typeRef _conId -> acc & over #types (Set.insert typeRef)
          Referent.Ref termRef -> acc & over #terms (Set.insert termRef)

    goTypes :: Foldable f => f TypeReference -> Defns (Set terms) (Set TypeReference)
    goTypes types =
      Defns
        { terms = Set.empty,
          types = Set.fromList (toList types)
        }

------------------------------------------------------------------------------------------------------------------------
-- Merge output

mkMergeOutput ::
  MergeDatabase ->
  ProjectBranchName ->
  ProjectBranchName ->
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)) ->
  Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)) ->
  Transaction (Merge.MergeOutput Symbol ())
mkMergeOutput MergeDatabase {loadDecl, loadTerm} aliceProjectBranchName bobProjectBranchName defns nameConflicts potentialConflicts = do
  (termNameConflicts, typeNameConflicts) <- do
    mkConflictMaps
      nameConflicts
      ( Map.intersectionWith \a b ->
          Merge.Conflict $ Merge.ConflictUnknown aliceProjectBranchName bobProjectBranchName a b
      )
  (termPotentialConflicts, typePotentialConflicts) <- do
    mkConflictMaps potentialConflicts (\a b -> Merge.Good <$> Map.union a b)
  let termConflicts = termNameConflicts <> termPotentialConflicts
      typeConflicts = typeNameConflicts <> typePotentialConflicts
  pure (Merge.MergeProblem $ Defns termConflicts typeConflicts)
  where
    mkConflictMaps ::
      Merge.TwoWay (Defns (Set TermReferenceId) (Set TypeReferenceId)) ->
      (forall ref. Map Name ref -> Map Name ref -> Map Name (Merge.ConflictOrGood ref)) ->
      Transaction (Map Name (Merge.ConflictOrGood (V1.Term Symbol ())), Map Name (Merge.ConflictOrGood (TypeReference, V1.Decl Symbol ())))
    mkConflictMaps conflicts mergeMaps = do
      aliceTermMap <- mkTermMap (defns ^. #alice . #terms) (conflicts ^. #alice . #terms)
      bobTermMap <- mkTermMap (defns ^. #bob . #terms) (conflicts ^. #bob . #terms)

      aliceTypeMap <- mkTypeMap (defns ^. #alice . #types) (conflicts ^. #alice . #types)
      bobTypeMap <- mkTypeMap (defns ^. #bob . #types) (conflicts ^. #bob . #types)

      let termConflicts = aliceTermMap `mergeMaps` bobTermMap
      let typeConflicts = aliceTypeMap `mergeMaps` bobTypeMap

      pure (termConflicts, typeConflicts)

    mkTypeMap ::
      forall f.
      Foldable f =>
      BiMultimap TypeReference Name ->
      f TypeReferenceId ->
      Transaction (Map Name (TypeReference, V1.Decl Symbol ()))
    mkTypeMap types typeIds =
      mkNameMap ReferenceDerived types
        <$> traverse (\x -> (x,) . (ReferenceDerived x,) . forgetAnn <$> loadDecl x) (toList typeIds)
      where
        forgetAnn = \case
          Left x -> Left (x $> ())
          Right x -> Right (x $> ())

    mkTermMap ::
      forall f.
      Foldable f =>
      BiMultimap Referent Name ->
      f TermReferenceId ->
      Transaction (Map Name (V1.Term Symbol ()))
    mkTermMap terms termIds =
      mkNameMap Referent.fromTermReferenceId terms
        <$> traverse (\x -> (x,) . V1.Term.unannotate <$> loadTerm x) (toList termIds)

    mkNameMap ::
      forall ref toref x.
      Ord ref =>
      (toref -> ref) ->
      BiMultimap ref Name ->
      [(toref, x)] ->
      Map Name x
    mkNameMap toref bimulti =
      Map.fromList . concat . map f
      where
        f :: (toref, x) -> [(Name, x)]
        f (trefid, term) =
          let names = BiMultimap.lookupDom (toref trefid) bimulti
           in [(n, term) | n <- toList names]

------------------------------------------------------------------------------------------------------------------------
-- Pretty-print environment, names, and output message utils

-- `convertDefnsToNames db defns` makes a Names from definitions.
convertDefnsToNames ::
  MergeDatabase ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Transaction Names.Names
convertDefnsToNames db = \case
  Defns terms types -> do
    termNames <- do
      termList <- traverse (\(k, v) -> (k,) <$> referent2to1 db v) (Map.toList (BiMultimap.range terms))
      pure (foldr (\(k, v) -> Names.addTerm k v) mempty termList)
    pure (Map.foldrWithKey Names.addType termNames (BiMultimap.range types))

-- Convert a merge precondition violation to an output message.
mergePreconditionViolationToOutput :: MergeDatabase -> Merge.PreconditionViolation -> Transaction Output.Output
mergePreconditionViolationToOutput db = \case
  Merge.ConflictedAliases branch name1 name2 -> pure (Output.MergeConflictedAliases branch name1 name2)
  Merge.ConflictedTermName name refs -> Output.MergeConflictedTermName name <$> Set.traverse (referent2to1 db) refs
  Merge.ConflictedTypeName name refs -> pure (Output.MergeConflictedTypeName name refs)
  Merge.ConflictInvolvingBuiltin name -> pure (Output.MergeConflictInvolvingBuiltin name)
  Merge.ConstructorAlias branch name1 name2 -> pure (Output.MergeConstructorAlias branch name1 name2)
  Merge.DefnsInLib -> pure Output.MergeDefnsInLib
  Merge.MissingConstructorName name -> pure (Output.MergeMissingConstructorName name)
  Merge.NestedDeclAlias name -> pure (Output.MergeNestedDeclAlias name)
  Merge.NoConstructorNames name -> pure (Output.MergeNoConstructorNames name)
  Merge.StrayConstructor name -> pure (Output.MergeStrayConstructor name)

------------------------------------------------------------------------------------------------------------------------
-- Constructing database entities
--
-- These utilities help convert in-memory structures into database types for saving

makeNametreeFromUnconflicted ::
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Merge.TwoWay (Nametree CausalHash) ->
  Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference), [CausalHash])
makeNametreeFromUnconflicted unconflicted causalHashes =
  alignWith
    ( \case
        This (aliceDefns, aliceCausal) -> (aliceDefns, [aliceCausal])
        That (bobDefns, bobCausal) -> (bobDefns, [bobCausal])
        These (aliceDefns, aliceCausal) (bobDefns, bobCausal) ->
          -- A left-biased union is fine here because we are merging unconflicted things; where the maps aren't
          -- disjoint, the values are equal
          (aliceDefns <> bobDefns, [aliceCausal, bobCausal])
    )
    (zip (unflattenDefns (unconflicted ^. #alice)) (causalHashes ^. #alice))
    (zip (unflattenDefns (unconflicted ^. #bob)) (causalHashes ^. #bob))
  where
    unflattenDefns ::
      Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))
    unflattenDefns (Defns terms types) =
      alignWith theseToDefns (unflattenNametree terms) (unflattenNametree types)

    theseToDefns ::
      These (Map NameSegment Referent) (Map NameSegment TypeReference) ->
      Defns (Map NameSegment Referent) (Map NameSegment TypeReference)
    theseToDefns = \case
      This terms -> Defns {terms, types = Map.empty}
      That types -> Defns {terms = Map.empty, types}
      These terms types -> Defns {terms, types}

namespaceToV3Branch ::
  MergeDatabase ->
  Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference), [CausalHash]) ->
  BranchV3 Transaction
namespaceToV3Branch db (Nametree (Defns {terms, types}, _causalParents) children) =
  BranchV3.BranchV3
    { terms,
      types,
      children = namespaceToV3Causal db <$> children
    }

namespaceToV3Causal ::
  MergeDatabase ->
  Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference), [CausalHash]) ->
  BranchV3.CausalBranchV3 Transaction
namespaceToV3Causal db@MergeDatabase {loadCausal} namespace =
  HashHandle.mkCausal
    v2HashHandle
    (HashHandle.hashBranchV3 v2HashHandle v3Branch)
    (Map.fromList (map (\ch -> (ch, loadCausal ch)) (snd (namespace ^. #value))))
    (pure v3Branch)
  where
    v3Branch :: BranchV3 Transaction
    v3Branch =
      namespaceToV3Branch db namespace

------------------------------------------------------------------------------------------------------------------------
-- Compat with V1 types
--
-- These utilities create V1 types for compatibility with the deprecated architecture of having the entire codebase
-- loaded into memory as a V1 type

-- Convert a V3 branch of definitions, plus libdeps, into a V1 branch.
convertV3BranchAndLibdepsToV1Branch ::
  MergeDatabase ->
  BranchV3 Transaction ->
  Set CausalHash ->
  Map NameSegment (CausalBranch Transaction) ->
  Transaction (V1.Branch0 Transaction)
convertV3BranchAndLibdepsToV1Branch db BranchV3 {terms, types, children} libdepsCausalParents libdeps = do
  terms1 <- traverse (referent2to1 db) terms
  children1 <- traverse (convertV3CausalToV1Causal db) children
  libdepsV1Causal <- convertLibdepsToV1Causal db libdepsCausalParents libdeps
  let children2 = Map.insert Name.libSegment libdepsV1Causal children1
  pure (V1.Branch.branch0 (makeStar3 terms1) (makeStar3 types) children2 Map.empty)

-- Convert a V3 branch of defninitions into a V1 branch.
convertV3BranchToV1Branch :: MergeDatabase -> BranchV3 Transaction -> Transaction (V1.Branch0 Transaction)
convertV3BranchToV1Branch db BranchV3 {terms, types, children} = do
  terms1 <- traverse (referent2to1 db) terms
  children1 <- traverse (convertV3CausalToV1Causal db) children
  pure (V1.Branch.branch0 (makeStar3 terms1) (makeStar3 types) children1 Map.empty)

-- `convertLibdepsToV1Causal db parents libdeps` loads `libdeps` as a V1 branch (without history), and then turns it
-- into a V1 causal using `parents` as history.
convertLibdepsToV1Causal ::
  MergeDatabase ->
  Set CausalHash ->
  Map NameSegment (CausalBranch Transaction) ->
  Transaction (V1.Branch Transaction)
convertLibdepsToV1Causal db@MergeDatabase {loadCausal, loadDeclType} parents libdeps = do
  let branch :: Branch Transaction
      branch =
        Branch
          { terms = Map.empty,
            types = Map.empty,
            patches = Map.empty,
            children = libdeps
          }

  branchHash <- HashHandle.hashBranch v2HashHandle branch

  v1Branch <- do
    -- We make a fresh branch cache to load the branch of libdeps.
    -- It would probably be better to reuse the codebase's branch cache.
    -- FIXME how slow/bad is this without that branch cache?
    branchCache <- Sqlite.unsafeIO newBranchCache
    Conversions.branch2to1 branchCache loadDeclType branch

  pure $
    addCausalHistoryV1
      db
      (HashHandle.hashCausal v2HashHandle branchHash parents)
      branchHash
      v1Branch
      (Map.fromSet loadCausal parents)

-- Convert a V3 causal to a V1 causal.
convertV3CausalToV1Causal :: MergeDatabase -> CausalBranchV3 Transaction -> Transaction (V1.Branch Transaction)
convertV3CausalToV1Causal db causal = do
  branch <- causal ^. #value
  head <- convertV3BranchToV1Branch db branch
  pure (addCausalHistoryV1 db (causal ^. #causalHash) (causal ^. #valueHash) head (causal ^. #parents))

-- Add causal history to a V1 branch (V1.Branch0), making it a V1 causal (V1.Branch).
addCausalHistoryV1 ::
  MergeDatabase ->
  CausalHash ->
  BranchHash ->
  V1.Branch0 Transaction ->
  Map CausalHash (Transaction (CausalBranch Transaction)) ->
  V1.Branch Transaction
addCausalHistoryV1 MergeDatabase {loadV1Branch} currentHash valueHash0 head parents =
  V1.Branch case Map.toList parents of
    [] -> V1.Causal.UnsafeOne {currentHash, valueHash, head}
    [(parentHash, parent)] ->
      V1.Causal.UnsafeCons
        { currentHash,
          valueHash,
          head,
          tail = (parentHash, convertParent parent)
        }
    _ ->
      V1.Causal.UnsafeMerge
        { currentHash,
          valueHash,
          head,
          tails = convertParent <$> parents
        }
  where
    convertParent :: Transaction (CausalBranch Transaction) -> Transaction (V1.Causal Transaction (V1.Branch0 Transaction))
    convertParent loadParent = do
      parent <- loadParent
      v1Branch <- loadV1Branch (parent ^. #causalHash)
      pure (V1.Branch._history v1Branch)

    valueHash =
      coerce @BranchHash @(Hash.HashFor (V1.Branch0 Transaction)) valueHash0

makeStar3 :: Ord ref => Map NameSegment ref -> Star3 ref NameSegment x y
makeStar3 =
  foldr (\(name, ref) -> Star3.insertD1 (ref, name)) emptyStar3 . Map.toList
  where
    emptyStar3 =
      Star3.Star3 Set.empty Relation.empty Relation.empty Relation.empty

-- Convert a v2 referent (missing decl type) to a v1 referent using the provided lookup-decl-type function.
referent2to1 :: MergeDatabase -> Referent -> Transaction V1.Referent
referent2to1 MergeDatabase {loadDeclType} = \case
  Referent.Con typeRef conId -> do
    declTy <- loadDeclType typeRef
    pure (V1.Referent.Con (ConstructorReference typeRef conId) declTy)
  Referent.Ref termRef -> pure (V1.Referent.Ref termRef)

------------------------------------------------------------------------------------------------------------------------
-- Misc. utils

-- Given a name like "base", try "base__1", then "base__2", etc, until we find a name that doesn't
-- clash with any existing dependencies.
getTwoFreshNames :: Set NameSegment -> NameSegment -> (NameSegment, NameSegment)
getTwoFreshNames names name0 =
  go2 0
  where
    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go2 4 = ("base__4", "base__7")
    go2 :: Integer -> (NameSegment, NameSegment)
    go2 !i
      | Set.member name names = go2 (i + 1)
      | otherwise = (name, go1 (i + 1))
      where
        name = mangled i

    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go1 5 = "base__7"
    go1 :: Integer -> NameSegment
    go1 !i
      | Set.member name names = go1 (i + 1)
      | otherwise = name
      where
        name = mangled i

    mangled :: Integer -> NameSegment
    mangled i =
      NameSegment (NameSegment.toText name0 <> "__" <> tShow i)

-----------------------------------------------------------------------------------------------------------------------
-- Debug show/print utils

showCausal :: CausalBranch m -> Text
showCausal =
  showCausalHash . Causal.causalHash

showCausalHash :: CausalHash -> Text
showCausalHash =
  ("#" <>) . Text.take 4 . Hash.toBase32HexText . unCausalHash

showNamedReference :: Name -> Reference -> Text
showNamedReference name ref =
  Name.toText name <> showReference ref

showNamedReferent :: Name -> Referent -> Text
showNamedReferent name ref =
  Name.toText name <> showReferent ref

showNamespaceHash :: BranchHash -> Text
showNamespaceHash =
  ("#" <>) . Text.take 4 . Hash.toBase32HexText . unBranchHash

showReference :: Reference -> Text
showReference =
  showShortHash . Reference.toShortHash

showReferent :: Referent -> Text
showReferent =
  showShortHash . Referent.toShortHash

showShortHash :: ShortHash -> Text
showShortHash =
  ShortHash.toText . ShortHash.shortenTo 4

printConflicted ::
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Set TermReferenceId) (Set TypeReferenceId) ->
  IO ()
printConflicted (Defns terms types) (Defns conflictedTermRefs conflictedTypeRefs) =
  printNamespace (Defns conflictedTerms conflictedTypes)
  where
    conflictedTerms = BiMultimap.restrictDom (Set.map (Referent.Ref . ReferenceDerived) conflictedTermRefs) terms
    conflictedTypes = BiMultimap.restrictDom (Set.map ReferenceDerived conflictedTypeRefs) types

printNamespace :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> IO ()
printNamespace (Defns terms types) =
  Text.putStr . Text.unlines $
    map (\(name, ref) -> "term " <> showNamedReferent name ref) (Map.toList (BiMultimap.range terms))
      ++ map (\(name, ref) -> "type " <> showNamedReference name ref) (Map.toList (BiMultimap.range types))

printTypesDiff :: BiMultimap TypeReference Name -> Map Name (Merge.DiffOp Hash) -> IO ()
printTypesDiff declNames = do
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f :: (Name, Merge.DiffOp Hash) -> Text
    f (name, op) =
      case op of
        Merge.Added _ -> Text.green ("decl " <> Name.toText name) <> ref
        Merge.Deleted _ -> Text.red ("decl " <> Name.toText name) <> ref
        Merge.Updated _ _ -> Text.magenta ("decl " <> Name.toText name) <> ref
      where
        ref =
          Text.brightBlack (showReference (fromJust (BiMultimap.lookupRan name declNames)))

printTermsDiff :: BiMultimap Referent Name -> Map Name (Merge.DiffOp Hash) -> IO ()
printTermsDiff termNames = do
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f :: (Name, Merge.DiffOp Hash) -> Text
    f (name, op) =
      case op of
        Merge.Added _ -> Text.green ("term " <> Name.toText name) <> ref
        Merge.Deleted _ -> Text.red ("term " <> Name.toText name) <> ref
        Merge.Updated _ _ -> Text.magenta ("decl " <> Name.toText name) <> ref
      where
        ref =
          Text.brightBlack (showReferent (fromJust (BiMultimap.lookupRan name termNames)))

printLibdeps :: Map NameSegment (CausalBranch Transaction) -> IO ()
printLibdeps =
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f (name, causal) =
      "dependency " <> NameSegment.toText name <> Text.brightBlack (showCausal causal)

printTypeConflicts :: Set Name -> IO ()
printTypeConflicts =
  Text.putStrLn . Text.unwords . map (("decl " <>) . Name.toText) . Set.toList

printTermConflicts :: Set Name -> IO ()
printTermConflicts =
  Text.putStrLn . Text.unwords . map (("term " <>) . Name.toText) . Set.toList

-----------------------------------------------------------------------------------------------------------------------
-- Utilities for caching transaction calls
--
-- These ought to be in a more general-puprose location, but defining here for now

cacheTransaction :: forall k v. Cache.Cache k v -> (k -> Transaction v) -> (k -> Transaction v)
cacheTransaction cache f k =
  unTransactionWithMonadIO (Cache.apply cache (TransactionWithMonadIO . f) k)

newtype TransactionWithMonadIO a
  = TransactionWithMonadIO (Transaction a)
  deriving newtype (Applicative, Functor, Monad)

unTransactionWithMonadIO :: TransactionWithMonadIO a -> Transaction a
unTransactionWithMonadIO (TransactionWithMonadIO m) = m

instance MonadIO TransactionWithMonadIO where
  liftIO :: forall a. IO a -> TransactionWithMonadIO a
  liftIO = coerce @(IO a -> Transaction a) Sqlite.unsafeIO
