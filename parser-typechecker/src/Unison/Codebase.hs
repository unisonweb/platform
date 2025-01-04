module Unison.Codebase
  ( Codebase,

    -- * UCM session state
    expectCurrentProjectPath,
    setCurrentProjectPath,
    resolveProjectPathIds,

    -- * Terms
    getTerm,
    unsafeGetTerm,
    unsafeGetTermWithType,
    getTermComponentWithTypes,
    unsafeGetTermComponent,
    getTypeOfTerm,
    getDeclType,
    unsafeGetTypeOfTermById,
    isTerm,
    putTerm,
    putTermComponent,

    -- ** Referents (sorta-termlike)
    getTypeOfReferent,

    -- ** Search
    termsOfType,
    filterTermsByReferenceIdHavingType,
    filterTermsByReferentHavingType,
    termsMentioningType,
    SqliteCodebase.Operations.termReferencesByPrefix,
    termReferentsByPrefix,

    -- * Type declarations
    getTypeDeclaration,
    unsafeGetTypeDeclaration,
    SqliteCodebase.Operations.getDeclComponent,
    putTypeDeclaration,
    putTypeDeclarationComponent,
    SqliteCodebase.Operations.typeReferencesByPrefix,
    isType,

    -- * Branches
    SqliteCodebase.Operations.branchExists,
    getBranchForHash,
    expectBranchForHash,
    putBranch,
    SqliteCodebase.Operations.causalHashesByPrefix,
    lca,
    SqliteCodebase.Operations.before,
    getShallowBranchAtPath,
    getMaybeShallowBranchAtPath,
    getShallowCausalAtPath,
    Operations.expectCausalBranchByCausalHash,
    getShallowCausalAtPathFromRootHash,
    getShallowProjectBranchRoot,
    expectShallowProjectBranchRoot,
    getShallowBranchAtProjectPath,
    getMaybeShallowBranchAtProjectPath,
    getShallowProjectRootByNames,
    expectProjectBranchRoot,
    getBranchAtProjectPath,
    preloadProjectBranch,

    -- * Root branch
    SqliteCodebase.Operations.namesAtPath,

    -- * Patches
    SqliteCodebase.Operations.patchExists,
    SqliteCodebase.Operations.getPatch,
    SqliteCodebase.Operations.putPatch,

    -- * Watches
    getWatch,
    lookupWatchCache,
    SqliteCodebase.Operations.watches,
    SqliteCodebase.Operations.putWatch,
    Queries.clearWatches,

    -- * Reflog
    Operations.getDeprecatedRootReflog,
    Operations.getProjectBranchReflog,
    Operations.getProjectReflog,
    Operations.getGlobalReflog,

    -- * Unambiguous hash length
    SqliteCodebase.Operations.hashLength,
    SqliteCodebase.Operations.branchHashLength,

    -- * Dependents
    dependents,
    dependentsOfComponent,

    -- * Sync

    -- * Codebase path
    getCodebaseDir,
    CodebasePath,

    -- * Direct codebase access
    runTransaction,
    runTransactionWithRollback,
    withConnection,
    withConnectionIO,

    -- * Misc (organize these better)
    addDefsToCodebase,
    componentReferencesForReference,
    installUcmDependencies,
    typeLookupForDependencies,
    unsafeGetComponentLength,
    SqliteCodebase.Operations.emptyCausalHash,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.DbId qualified as Db
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Builtin qualified as Builtin
import Unison.Builtin.Terms qualified as Builtin
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation (builtinAnnotation))
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.Codebase.SqliteCodebase.Operations qualified as SqliteCodebase.Operations
import Unison.Codebase.Type (Codebase (..))
import Unison.CodebasePath (CodebasePath, getCodebaseDir)
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.Core.Project (ProjectAndBranch)
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DD
import Unison.Hash (Hash)
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Parser
import Unison.Prelude
import Unison.Project (ProjectAndBranch (ProjectAndBranch), ProjectBranchName, ProjectName)
import Unison.Reference (Reference, TermReference, TermReferenceId, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker.TypeLookup (TypeLookup (TypeLookup))
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.UnisonFile qualified as UF
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Recursion (XNor (Both, Neither), cata)
import Unison.Util.Relation qualified as Rel
import Unison.Var (Var)
import Unison.WatchKind qualified as WK

-- | Run a transaction on a codebase.
runTransaction :: (MonadIO m) => Codebase m v a -> Sqlite.Transaction b -> m b
runTransaction Codebase {withConnection} action =
  withConnection \conn -> Sqlite.runTransaction conn action

runTransactionWithRollback ::
  (MonadIO m) =>
  Codebase m v a ->
  ((forall void. b -> Sqlite.Transaction void) -> Sqlite.Transaction b) ->
  m b
runTransactionWithRollback Codebase {withConnection} action =
  withConnection \conn -> Sqlite.runTransactionWithRollback conn action

getShallowCausalAtPathFromRootHash ::
  -- Causal to start at, if Nothing use the codebase's root branch.
  CausalHash ->
  Path.Path ->
  Sqlite.Transaction (V2Branch.CausalBranch Sqlite.Transaction)
getShallowCausalAtPathFromRootHash rootCausalHash p = do
  rootCausal <- Operations.expectCausalBranchByCausalHash rootCausalHash
  getShallowCausalAtPath p rootCausal

-- | Recursively descend into causals following the given path,
-- Use the root causal if none is provided.
getShallowCausalAtPath ::
  Path ->
  (V2Branch.CausalBranch Sqlite.Transaction) ->
  Sqlite.Transaction (V2Branch.CausalBranch Sqlite.Transaction)
getShallowCausalAtPath = cata \case
  Neither -> pure
  Both ns fn -> maybe (pure $ Cv.causalbranch1to2 Branch.empty) fn . V2Branch.childAt ns <=< V2Causal.value

-- | Recursively descend into causals following the given path,
-- Use the root causal if none is provided.
getShallowBranchAtPath ::
  Path ->
  V2Branch.Branch Sqlite.Transaction ->
  Sqlite.Transaction (V2Branch.Branch Sqlite.Transaction)
getShallowBranchAtPath path branch = fromMaybe V2Branch.empty <$> getMaybeShallowBranchAtPath path branch

-- | Recursively descend into causals following the given path,
-- Use the root causal if none is provided.
getMaybeShallowBranchAtPath ::
  Path ->
  V2Branch.Branch Sqlite.Transaction ->
  Sqlite.Transaction (Maybe (V2Branch.Branch Sqlite.Transaction))
getMaybeShallowBranchAtPath = cata \case
  Neither -> pure . Just
  Both ns fn -> maybe (pure Nothing) (fn <=< V2Causal.value) . V2Branch.childAt ns

-- | Recursively descend into causals following the given path,
-- Use the root causal if none is provided.
getShallowBranchAtProjectPath ::
  PP.ProjectPath ->
  Sqlite.Transaction (V2Branch.Branch Sqlite.Transaction)
getShallowBranchAtProjectPath pp = fromMaybe V2Branch.empty <$> getMaybeShallowBranchAtProjectPath pp

-- | Recursively descend into causals following the given path,
-- Use the root causal if none is provided.
getMaybeShallowBranchAtProjectPath ::
  PP.ProjectPath ->
  Sqlite.Transaction (Maybe (V2Branch.Branch Sqlite.Transaction))
getMaybeShallowBranchAtProjectPath (PP.ProjectPath _project projectBranch path) = do
  getShallowProjectBranchRoot projectBranch >>= \case
    Nothing -> pure Nothing
    Just projectRootBranch -> getMaybeShallowBranchAtPath (Path.unabsolute path) projectRootBranch

getShallowProjectRootByNames :: ProjectAndBranch ProjectName ProjectBranchName -> Sqlite.Transaction (Maybe (V2Branch.CausalBranch Sqlite.Transaction))
getShallowProjectRootByNames (ProjectAndBranch projectName branchName) = runMaybeT do
  ProjectBranch {projectId, branchId} <- MaybeT $ Q.loadProjectBranchByNames projectName branchName
  causalHashId <- lift $ Q.expectProjectBranchHead projectId branchId
  causalHash <- lift $ Q.expectCausalHash causalHashId
  lift $ Operations.expectCausalBranchByCausalHash causalHash

expectProjectBranchRoot :: (MonadIO m) => Codebase m v a -> Db.ProjectId -> Db.ProjectBranchId -> m (Branch m)
expectProjectBranchRoot codebase projectId branchId = do
  causalHash <- runTransaction codebase $ do
    causalHashId <- Q.expectProjectBranchHead projectId branchId
    Q.expectCausalHash causalHashId
  expectBranchForHash codebase causalHash

expectShallowProjectBranchRoot :: ProjectBranch -> Sqlite.Transaction (V2Branch.Branch Sqlite.Transaction)
expectShallowProjectBranchRoot ProjectBranch {projectId, branchId} = do
  causalHashId <- Q.expectProjectBranchHead projectId branchId
  causalHash <- Q.expectCausalHash causalHashId
  Operations.expectCausalBranchByCausalHash causalHash >>= V2Causal.value

getShallowProjectBranchRoot :: ProjectBranch -> Sqlite.Transaction (Maybe (V2Branch.Branch Sqlite.Transaction))
getShallowProjectBranchRoot ProjectBranch {projectId, branchId} = do
  causalHashId <- Q.expectProjectBranchHead projectId branchId
  causalHash <- Q.expectCausalHash causalHashId
  Operations.loadCausalBranchByCausalHash causalHash >>= traverse V2Causal.value

getBranchAtProjectPath ::
  (MonadIO m) =>
  Codebase m v a ->
  PP.ProjectPath ->
  m (Maybe (Branch m))
getBranchAtProjectPath codebase pp = runMaybeT do
  rootBranch <- lift $ expectProjectBranchRoot codebase pp.branch.projectId pp.branch.branchId
  hoistMaybe $ Branch.getAt (pp ^. PP.path_) rootBranch

-- | Like 'getBranchForHash', but for when the hash is known to be in the codebase.
expectBranchForHash :: (Monad m) => Codebase m v a -> CausalHash -> m (Branch m)
expectBranchForHash codebase hash =
  getBranchForHash codebase hash >>= \case
    Just branch -> pure branch
    Nothing -> error $ reportBug "E412939" ("expectBranchForHash: " ++ show hash ++ " not found in codebase")

-- | Get the lowest common ancestor of two branches, i.e. the most recent branch that is an ancestor of both branches.
lca :: (MonadIO m) => Codebase m v a -> Branch m -> Branch m -> m (Maybe (Branch m))
lca code b1@(Branch.headHash -> h1) b2@(Branch.headHash -> h2) = do
  action <-
    runTransaction code do
      eb1 <- SqliteCodebase.Operations.branchExists h1
      eb2 <- SqliteCodebase.Operations.branchExists h2
      if eb1 && eb2
        then do
          Operations.lca h1 h2 >>= \case
            Just h -> pure (getBranchForHash code h)
            Nothing -> pure (pure Nothing) -- no common ancestor
        else pure (Branch.lca b1 b2)
  action

debug :: Bool
debug = False

-- | Write all of UCM's dependencies (builtins types and an empty namespace) into the codebase
installUcmDependencies :: Codebase m Symbol Parser.Ann -> Sqlite.Transaction ()
installUcmDependencies c = do
  let uf =
        ( UF.typecheckedUnisonFile
            (Map.fromList Builtin.builtinDataDecls)
            (Map.fromList Builtin.builtinEffectDecls)
            [Builtin.builtinTermsSrc Parser.Intrinsic]
            mempty
        )
  addDefsToCodebase c uf

-- Feel free to refactor this to use some other type than TypecheckedUnisonFile
-- if it makes sense to later.
addDefsToCodebase ::
  forall m v a.
  (Var v, Show a) =>
  Codebase m v a ->
  UF.TypecheckedUnisonFile v a ->
  Sqlite.Transaction ()
addDefsToCodebase c uf = do
  traverse_ (goType Right) (UF.dataDeclarationsId' uf)
  traverse_ (goType Left) (UF.effectDeclarationsId' uf)
  -- put terms
  traverse_ goTerm (UF.hashTermsId uf)
  where
    goTerm t | debug && trace ("Codebase.addDefsToCodebase.goTerm " ++ show t) False = undefined
    goTerm (_, r, wk, tm, tp) = when (WK.watchKindShouldBeStoredInDatabase wk) (putTerm c r tm tp)
    goType :: (Show t) => (t -> Decl v a) -> (Reference.Id, t) -> Sqlite.Transaction ()
    goType _f pair | debug && trace ("Codebase.addDefsToCodebase.goType " ++ show pair) False = undefined
    goType f (ref, decl) = putTypeDeclaration c ref (f decl)

getTypeOfConstructor :: (Ord v) => Codebase m v a -> ConstructorReference -> Sqlite.Transaction (Maybe (Type v a))
getTypeOfConstructor codebase (ConstructorReference r0 cid) =
  case r0 of
    Reference.DerivedId r -> do
      maybeDecl <- getTypeDeclaration codebase r
      pure $ case maybeDecl of
        Nothing -> Nothing
        Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
    Reference.Builtin _ -> error (reportBug "924628772" "Attempt to load a type declaration which is a builtin!")

-- | Like 'getWatch', but first looks up the given reference as a regular watch, then as a test watch.
--
-- @
-- lookupWatchCache codebase ref =
--   runMaybeT do
--     MaybeT (getWatch codebase RegularWatch ref)
--       <|> MaybeT (getWatch codebase TestWatch ref))
-- @
lookupWatchCache :: Codebase m v a -> Reference.Id -> Sqlite.Transaction (Maybe (Term v a))
lookupWatchCache codebase h = do
  m1 <- getWatch codebase WK.RegularWatch h
  maybe (getWatch codebase WK.TestWatch h) (pure . Just) m1

-- | Make a @TypeLookup@ that is suitable for looking up information about all of the given type-or-term references,
-- and all of their type dependencies, including builtins.
typeLookupForDependencies ::
  Codebase IO Symbol Ann ->
  DefnsF Set TermReference TypeReference ->
  Sqlite.Transaction (TL.TypeLookup Symbol Ann)
typeLookupForDependencies codebase s = do
  when debug $ traceM $ "typeLookupForDependencies " ++ show s
  (<> Builtin.typeLookup) <$> depthFirstAccum s
  where
    depthFirstAccum ::
      DefnsF Set TermReference TypeReference ->
      Sqlite.Transaction (TL.TypeLookup Symbol Ann)
    depthFirstAccum refs = do
      tl <- depthFirstAccumTypes mempty refs.types
      foldM goTerm tl (Set.filter (unseen tl) refs.terms)

    depthFirstAccumTypes ::
      TL.TypeLookup Symbol Ann ->
      Set TypeReference ->
      Sqlite.Transaction (TL.TypeLookup Symbol Ann)
    depthFirstAccumTypes tl refs =
      foldM goType tl (Set.filter (unseen tl) refs)

    -- We need the transitive dependencies of data decls
    -- that are scrutinized in a match expression for
    -- pattern match coverage checking (specifically for
    -- the inhabitation check). We ensure these are found
    -- by collecting all transitive type dependencies.
    goTerm :: TypeLookup Symbol Ann -> TermReference -> Sqlite.Transaction (TypeLookup Symbol Ann)
    goTerm tl ref =
      getTypeOfTerm codebase ref >>= \case
        Just typ ->
          let z = tl <> TypeLookup (Map.singleton ref typ) mempty mempty
           in depthFirstAccumTypes z (Type.dependencies typ)
        Nothing -> pure tl

    goType :: TypeLookup Symbol Ann -> TypeReference -> Sqlite.Transaction (TypeLookup Symbol Ann)
    goType tl ref@(Reference.DerivedId id) =
      getTypeDeclaration codebase id >>= \case
        Just (Left ed) ->
          let z = tl <> TypeLookup mempty mempty (Map.singleton ref ed)
           in depthFirstAccumTypes z (DD.typeDependencies $ DD.toDataDecl ed)
        Just (Right dd) ->
          let z = tl <> TypeLookup mempty (Map.singleton ref dd) mempty
           in depthFirstAccumTypes z (DD.typeDependencies dd)
        Nothing -> pure tl
    goType tl Reference.Builtin {} = pure tl -- codebase isn't consulted for builtins
    unseen :: TL.TypeLookup Symbol a -> Reference -> Bool
    unseen tl r =
      isNothing
        ( Map.lookup r (TL.dataDecls tl) $> ()
            <|> Map.lookup r (TL.typeOfTerms tl) $> ()
            <|> Map.lookup r (TL.effectDecls tl) $> ()
        )

-- | Get the type of a term.
--
-- Note that it is possible to call 'putTerm', then 'getTypeOfTerm', and receive @Nothing@, per the semantics of
-- 'putTerm'.
getTypeOfTerm ::
  (BuiltinAnnotation a) =>
  Codebase m Symbol a ->
  Reference ->
  Sqlite.Transaction (Maybe (Type Symbol a))
getTypeOfTerm _c r | debug && trace ("Codebase.getTypeOfTerm " ++ show r) False = undefined
getTypeOfTerm c r = case r of
  Reference.DerivedId h -> getTypeOfTermImpl c h
  r@Reference.Builtin {} ->
    pure $
      fmap (const builtinAnnotation)
        <$> Map.lookup r Builtin.termRefTypes

-- | Get the type of a referent.
getTypeOfReferent ::
  (BuiltinAnnotation a) =>
  Codebase m Symbol a ->
  Referent.Referent ->
  Sqlite.Transaction (Maybe (Type Symbol a))
getTypeOfReferent c = \case
  Referent.Ref r -> getTypeOfTerm c r
  Referent.Con r _ -> getTypeOfConstructor c r

componentReferencesForReference :: Reference -> Sqlite.Transaction (Set Reference)
componentReferencesForReference = \case
  r@Reference.Builtin {} -> pure (Set.singleton r)
  Reference.Derived h _i ->
    Set.mapMonotonic Reference.DerivedId . Reference.componentFromLength h <$> unsafeGetComponentLength h

-- | Get the set of terms, type declarations, and builtin types that depend on the given term, type declaration, or
-- builtin type.
dependents :: Queries.DependentsSelector -> Reference -> Sqlite.Transaction (Set Reference)
dependents selector r =
  Set.union (Builtin.builtinTypeDependents r)
    . Set.map Reference.DerivedId
    <$> SqliteCodebase.Operations.dependentsImpl selector r

dependentsOfComponent :: Hash -> Sqlite.Transaction (Set Reference)
dependentsOfComponent h =
  Set.union (Builtin.builtinTypeDependentsOfComponent h)
    . Set.map Reference.DerivedId
    <$> SqliteCodebase.Operations.dependentsOfComponentImpl h

-- | Get the set of terms-or-constructors that have the given type.
termsOfType :: (Var v) => Codebase m v a -> Type v a -> Sqlite.Transaction (Set Referent.Referent)
termsOfType c ty = termsOfTypeByReference c $ Hashing.typeToReference ty

-- | Get all terms which match the exact type the provided reference points to.
termsOfTypeByReference :: (Var v) => Codebase m v a -> Reference -> Sqlite.Transaction (Set Referent.Referent)
termsOfTypeByReference c r =
  Set.union (Rel.lookupDom r Builtin.builtinTermsByType)
    . Set.map (fmap Reference.DerivedId)
    <$> termsOfTypeImpl c r

filterTermsByReferentHavingType ::
  (Var v) =>
  Codebase m v a ->
  Type v a ->
  Set Referent.Referent ->
  Sqlite.Transaction (Set Referent.Referent)
filterTermsByReferentHavingType c ty = filterTermsByReferentHavingTypeByReference c $ Hashing.typeToReference ty

filterTermsByReferenceIdHavingType ::
  (Var v) =>
  Codebase m v a ->
  Type v a ->
  Set TermReferenceId ->
  Sqlite.Transaction (Set TermReferenceId)
filterTermsByReferenceIdHavingType c ty = filterTermsByReferenceIdHavingTypeImpl c (Hashing.typeToReference ty)

-- | Find the subset of `tms` which match the exact type `r` points to.
filterTermsByReferentHavingTypeByReference ::
  Codebase m v a ->
  TypeReference ->
  Set Referent.Referent ->
  Sqlite.Transaction (Set Referent.Referent)
filterTermsByReferentHavingTypeByReference c r tms = do
  let (builtins, derived) = partitionEithers . map p $ Set.toList tms
  let builtins' =
        Set.intersection
          (Set.fromList builtins)
          (Rel.lookupDom r Builtin.builtinTermsByType)
  derived' <- filterTermsByReferentIdHavingTypeImpl c r (Set.fromList derived)
  pure $ builtins' <> Set.mapMonotonic Referent.fromId derived'
  where
    p :: Referent.Referent -> Either Referent.Referent Referent.Id
    p r = case Referent.toId r of
      Just rId -> Right rId
      Nothing -> Left r

-- | Get the set of terms-or-constructors mention the given type anywhere in their signature.
termsMentioningType :: (Var v) => Codebase m v a -> Type v a -> Sqlite.Transaction (Set Referent.Referent)
termsMentioningType c ty =
  Set.union (Rel.lookupDom r Builtin.builtinTermsByTypeMention)
    . Set.map (fmap Reference.DerivedId)
    <$> termsMentioningTypeImpl c r
  where
    r = Hashing.typeToReference ty

-- | Check whether a reference is a term.
isTerm ::
  (BuiltinAnnotation a) =>
  Codebase m Symbol a ->
  Reference ->
  Sqlite.Transaction Bool
isTerm code = fmap isJust . getTypeOfTerm code

isType :: Codebase m v a -> Reference -> Sqlite.Transaction Bool
isType c r = case r of
  Reference.Builtin {} -> pure $ Builtin.isBuiltinType r
  Reference.DerivedId r -> isJust <$> getTypeDeclaration c r

unsafeGetComponentLength :: (HasCallStack) => Hash -> Sqlite.Transaction Reference.CycleSize
unsafeGetComponentLength h =
  Operations.getCycleLen h >>= \case
    Nothing -> error (reportBug "E713350" ("component with hash " ++ show h ++ " not found"))
    Just size -> pure size

-- | Like 'getTerm', for when the term is known to exist in the codebase.
unsafeGetTerm :: (HasCallStack) => Codebase m v a -> Reference.Id -> Sqlite.Transaction (Term v a)
unsafeGetTerm codebase rid =
  getTerm codebase rid >>= \case
    Nothing -> error (reportBug "E520818" ("term " ++ show rid ++ " not found"))
    Just term -> pure term

-- | Like 'getTypeDeclaration', for when the type declaration is known to exist in the codebase.
unsafeGetTypeDeclaration :: (HasCallStack) => Codebase m v a -> Reference.Id -> Sqlite.Transaction (Decl v a)
unsafeGetTypeDeclaration codebase rid =
  getTypeDeclaration codebase rid >>= \case
    Nothing -> error (reportBug "E129043" ("type decl " ++ show rid ++ " not found"))
    Just decl -> pure decl

-- | Like 'getTypeOfTerm', but for when the term is known to exist in the codebase.
unsafeGetTypeOfTermById :: (HasCallStack) => Codebase m v a -> Reference.Id -> Sqlite.Transaction (Type v a)
unsafeGetTypeOfTermById codebase rid =
  getTypeOfTermImpl codebase rid >>= \case
    Nothing -> error (reportBug "E377910" ("type of term " ++ show rid ++ " not found"))
    Just ty -> pure ty

-- | Like 'unsafeGetTerm', but returns the type of the term, too.
unsafeGetTermWithType :: (HasCallStack) => Codebase m v a -> Reference.Id -> Sqlite.Transaction (Term v a, Type v a)
unsafeGetTermWithType codebase rid = do
  term <- unsafeGetTerm codebase rid
  ty <-
    -- A term is sometimes stored with a type annotation (specifically, when the annotation is different from the
    -- inferred type). In this case, we can avoid looking up the type separately.
    case term of
      Term.Ann' _ ty -> pure ty
      _ -> unsafeGetTypeOfTermById codebase rid
  pure (term, ty)

-- | Like 'getTermComponentWithTypes', for when the term component is known to exist in the codebase.
unsafeGetTermComponent ::
  (HasCallStack) =>
  Codebase m v a ->
  Hash ->
  Sqlite.Transaction [(Term v a, Type v a)]
unsafeGetTermComponent codebase hash =
  getTermComponentWithTypes codebase hash <&> \case
    Nothing -> error (reportBug "E769004" ("term component " ++ show hash ++ " not found"))
    Just terms -> terms

expectCurrentProjectPath :: (HasCallStack) => Sqlite.Transaction PP.ProjectPath
expectCurrentProjectPath = do
  (projectId, projectBranchId, path) <- Q.expectCurrentProjectPath
  proj <- Q.expectProject projectId
  projBranch <- Q.expectProjectBranch projectId projectBranchId
  let absPath = Path.Absolute (Path.fromList path)
  pure $ PP.ProjectPath proj projBranch absPath

setCurrentProjectPath :: PP.ProjectPathIds -> Sqlite.Transaction ()
setCurrentProjectPath (PP.ProjectPath projectId projectBranchId path) =
  Q.setCurrentProjectPath projectId projectBranchId (Path.toList (Path.unabsolute path))

-- | Hydrate the project and branch from IDs.
resolveProjectPathIds :: PP.ProjectPathIds -> Sqlite.Transaction PP.ProjectPath
resolveProjectPathIds (PP.ProjectPath projectId projectBranchId path) = do
  proj <- Q.expectProject projectId
  projBranch <- Q.expectProjectBranch projectId projectBranchId
  pure $ PP.ProjectPath proj projBranch path

-- | Starts loading the given project branch into cache in a background thread without blocking.
preloadProjectBranch :: (MonadUnliftIO m) => Codebase m v a -> ProjectAndBranch Db.ProjectId Db.ProjectBranchId -> m ()
preloadProjectBranch codebase (ProjectAndBranch projectId branchId) = do
  ch <- runTransaction codebase $ do
    causalHashId <- Q.expectProjectBranchHead projectId branchId
    Q.expectCausalHash causalHashId
  preloadBranch codebase ch
