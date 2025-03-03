module U.Codebase.Sqlite.Operations
  ( -- * branches
    loadCausalHashAtPath,
    expectCausalHashAtPath,
    loadCausalBranchAtPath,
    loadBranchAtPath,
    saveBranch,
    saveBranchV3,
    loadCausalBranchByCausalHash,
    expectCausalBranchByCausalHash,
    expectBranchByCausalHashId,
    expectBranchByBranchHash,
    expectBranchByBranchHashId,
    expectNamespaceStatsByHash,
    expectNamespaceStatsByHashId,
    tryGetSquashResult,
    saveSquashResult,

    -- * terms
    Q.saveTermComponent,
    loadTermComponent,
    loadTermByReference,
    loadTypeOfTermByTermReference,

    -- * decls
    Q.saveDeclComponent,
    loadDeclComponent,
    loadDeclByReference,
    expectDeclByReference,
    expectDeclNumConstructors,
    expectDeclTypeById,

    -- * terms/decls
    getCycleLen,

    -- * patches
    savePatch,
    expectPatch,

    -- * test for stuff in codebase
    objectExistsForHash,

    -- * watch expression cache
    saveWatch,
    loadWatch,
    listWatches,
    Q.clearWatches,

    -- * indexes

    -- ** nearest common ancestor
    before,
    lca,

    -- ** prefix index
    componentReferencesByPrefix,
    termReferentsByPrefix,
    declReferentsByPrefix,
    causalHashesByPrefix,

    -- ** dependents index
    directDependenciesOfScope,
    dependents,
    dependentsOfComponent,
    directDependentsWithinScope,
    transitiveDependentsWithinScope,

    -- ** type index
    Q.addTypeToIndexForTerm,
    termsHavingType,
    filterTermsByReferenceHavingType,
    filterTermsByReferentHavingType,

    -- ** type mentions index
    Q.addTypeMentionsToIndexForTerm,
    termsMentioningType,

    -- ** name lookup index
    allNamesInPerspective,
    NamesInPerspective (..),
    NamesPerspective (..),
    termNamesForRefWithinNamespace,
    typeNamesForRefWithinNamespace,
    termNamesBySuffix,
    typeNamesBySuffix,
    termRefsForExactName,
    typeRefsForExactName,
    recursiveTermNameSearch,
    recursiveTypeNameSearch,
    checkBranchHashNameLookupExists,
    buildNameLookupForBranchHash,
    associateNameLookupMounts,
    longestMatchingTermNameForSuffixification,
    longestMatchingTypeNameForSuffixification,
    deleteNameLookupsExceptFor,
    fuzzySearchDefinitions,
    namesPerspectiveForRootAndPath,

    -- * Projects
    expectProjectAndBranchNames,
    expectProjectBranchHead,

    -- * reflog
    getDeprecatedRootReflog,
    getProjectReflog,
    getProjectBranchReflog,
    getGlobalReflog,
    appendProjectReflog,

    -- * low-level stuff
    expectDbBranch,
    saveDbBranch,
    saveDbBranchUnderHashId,
    expectDbPatch,
    saveDbPatch,
    expectDbBranchByCausalHashId,
    namespaceStatsForDbBranch,

    -- * somewhat unexpectedly unused definitions
    c2sReferenceId,
    c2sReferentId,
    diffPatch,
    decodeTermElementWithType,
    loadTermWithTypeByReference,
    Q.s2cTermWithType,
    Q.s2cDecl,
    declReferencesByPrefix,
    namespaceHashesByPrefix,
    derivedDependencies,

    -- * internal stuff that probably need not be exported, but the 1->2 migration needs it
    BranchV (..),
    DbBranchV (..),
  )
where

import Control.Lens hiding (children)
import Control.Monad.Extra qualified as Monad
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable qualified as Foldable
import Data.List.Extra qualified as List
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Map qualified as Map
import Data.Map.Merge.Lazy qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Tuple.Extra (uncurry3, (***))
import U.Codebase.Branch.Type (NamespaceStats (..))
import U.Codebase.Branch.Type qualified as C.Branch
import U.Codebase.BranchV3 qualified as C.BranchV3
import U.Codebase.Causal qualified as C
import U.Codebase.Causal qualified as C.Causal
import U.Codebase.Decl (ConstructorId)
import U.Codebase.Decl qualified as C
import U.Codebase.Decl qualified as C.Decl
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), PatchHash (..))
import U.Codebase.Reference qualified as C
import U.Codebase.Reference qualified as C.Reference
import U.Codebase.Referent qualified as C
import U.Codebase.Referent qualified as C.Referent
import U.Codebase.Reflog qualified as Reflog
import U.Codebase.Sqlite.Branch.Diff qualified as S.Branch
import U.Codebase.Sqlite.Branch.Diff qualified as S.Branch.Diff
import U.Codebase.Sqlite.Branch.Diff qualified as S.BranchDiff
import U.Codebase.Sqlite.Branch.Format qualified as S
import U.Codebase.Sqlite.Branch.Format qualified as S.BranchFormat
import U.Codebase.Sqlite.Branch.Full qualified as S
import U.Codebase.Sqlite.Branch.Full qualified as S.Branch.Full
import U.Codebase.Sqlite.Branch.Full qualified as S.MetadataSet (DbMetadataSet, MetadataSetFormat' (..))
import U.Codebase.Sqlite.DbId qualified as Db
import U.Codebase.Sqlite.Decl.Format qualified as S.Decl
import U.Codebase.Sqlite.Decode
import U.Codebase.Sqlite.HashHandle (HashHandle (..))
import U.Codebase.Sqlite.LocalIds (LocalIds, WatchLocalIds)
import U.Codebase.Sqlite.LocalizeObject qualified as LocalizeObject
import U.Codebase.Sqlite.NameLookups (PathSegments (..))
import U.Codebase.Sqlite.NameLookups qualified as NameLookups
import U.Codebase.Sqlite.NameLookups qualified as S
import U.Codebase.Sqlite.NamedRef qualified as S
import U.Codebase.Sqlite.ObjectType qualified as ObjectType
import U.Codebase.Sqlite.Patch.Diff qualified as S
import U.Codebase.Sqlite.Patch.Format qualified as S
import U.Codebase.Sqlite.Patch.Format qualified as S.Patch.Format
import U.Codebase.Sqlite.Patch.Full qualified as S (LocalPatch, Patch, Patch' (..))
import U.Codebase.Sqlite.Patch.TermEdit qualified as S
import U.Codebase.Sqlite.Patch.TermEdit qualified as S.TermEdit
import U.Codebase.Sqlite.Patch.TypeEdit qualified as S
import U.Codebase.Sqlite.Patch.TypeEdit qualified as S.TypeEdit
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.ProjectReflog qualified as ProjectReflog
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Reference qualified as S
import U.Codebase.Sqlite.Reference qualified as S.Reference
import U.Codebase.Sqlite.Referent qualified as S
import U.Codebase.Sqlite.Referent qualified as S.Referent
import U.Codebase.Sqlite.Serialization qualified as S
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Sqlite.Term.Format qualified as S.Term
import U.Codebase.Term qualified as C
import U.Codebase.Term qualified as C.Term
import U.Codebase.TermEdit qualified as C
import U.Codebase.TermEdit qualified as C.TermEdit
import U.Codebase.TypeEdit qualified as C
import U.Codebase.TypeEdit qualified as C.TypeEdit
import U.Codebase.WatchKind (WatchKind)
import U.Util.Base32Hex qualified as Base32Hex
import U.Util.Serialization qualified as S
import Unison.Core.Project (ProjectBranchName, ProjectName)
import Unison.Hash qualified as H
import Unison.Hash32 qualified as Hash32
import Unison.NameSegment (NameSegment)
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Prelude
import Unison.ShortHash (ShortCausalHash (..), ShortNamespaceHash (..))
import Unison.Sqlite
import Unison.Util.Defns (DefnsF)
import Unison.Util.List qualified as List
import Unison.Util.Map qualified as Map
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Set qualified as Set

-- * Error handling

debug :: Bool
debug = False

-- * Database lookups

objectExistsForHash :: H.Hash -> Transaction Bool
objectExistsForHash h =
  isJust <$> runMaybeT do
    id <- MaybeT . Q.loadHashId . Hash32.fromHash $ h
    MaybeT $ Q.loadObjectIdForAnyHashId id

expectValueHashByCausalHashId :: Db.CausalHashId -> Transaction BranchHash
expectValueHashByCausalHashId = loadValueHashById <=< Q.expectCausalValueHashId
  where
    loadValueHashById :: Db.BranchHashId -> Transaction BranchHash
    loadValueHashById = fmap BranchHash . Q.expectHash . Db.unBranchHashId

-- | Load the causal hash at the given path from the provided root, if Nothing, use the
-- codebase root.
loadCausalHashAtPath :: CausalHash -> [NameSegment] -> Transaction (Maybe CausalHash)
loadCausalHashAtPath rootCausalHash =
  let go :: Db.CausalHashId -> [NameSegment] -> MaybeT Transaction CausalHash
      go hashId = \case
        [] -> lift (Q.expectCausalHash hashId)
        t : ts -> do
          tid <- MaybeT (Q.loadTextId $ NameSegment.toUnescapedText t)
          S.Branch {children} <- MaybeT (loadDbBranchByCausalHashId hashId)
          (_, hashId') <- MaybeT (pure (Map.lookup tid children))
          go hashId' ts
   in \path -> do
        hashId <- Q.expectCausalHashIdByCausalHash rootCausalHash
        runMaybeT (go hashId path)

-- | Expect the causal hash at the given path from the provided root, if Nothing, use the
-- codebase root.
expectCausalHashAtPath :: CausalHash -> [NameSegment] -> Transaction CausalHash
expectCausalHashAtPath rootCausalHash =
  let go :: Db.CausalHashId -> [NameSegment] -> Transaction CausalHash
      go hashId = \case
        [] -> Q.expectCausalHash hashId
        t : ts -> do
          tid <- Q.expectTextId $ NameSegment.toUnescapedText t
          S.Branch {children} <- expectDbBranchByCausalHashId hashId
          let (_, hashId') = children Map.! tid
          go hashId' ts
   in \path -> do
        hashId <- Q.expectCausalHashIdByCausalHash rootCausalHash
        go hashId path

loadCausalBranchAtPath ::
  CausalHash ->
  [NameSegment] ->
  Transaction (Maybe (C.Branch.CausalBranch Transaction))
loadCausalBranchAtPath rootCausalHash path =
  loadCausalHashAtPath rootCausalHash path >>= \case
    Nothing -> pure Nothing
    Just causalHash -> Just <$> expectCausalBranchByCausalHash causalHash

loadBranchAtPath :: CausalHash -> [NameSegment] -> Transaction (Maybe (C.Branch.Branch Transaction))
loadBranchAtPath rootCausalHash path =
  loadCausalBranchAtPath rootCausalHash path >>= \case
    Nothing -> pure Nothing
    Just causal -> Just <$> C.Causal.value causal

-- * Reference transformations

-- ** read existing references

-- | Assumes that a derived reference would already exist in the database
--  (by virtue of dependencies being stored before dependents), but does
--  not assume a builtin reference would.
c2sReference :: C.Reference -> Transaction S.Reference
c2sReference = bitraverse Q.saveText Q.expectObjectIdForPrimaryHash

c2sTextReference :: C.Reference -> S.TextReference
c2sTextReference = bimap id H.toBase32Hex

s2cReference :: S.Reference -> Transaction C.Reference
s2cReference = bitraverse Q.expectText Q.expectPrimaryHashByObjectId

s2cTextReference :: S.TextReference -> C.Reference
s2cTextReference = bimap id H.fromBase32Hex

c2sReferenceId :: C.Reference.Id -> Transaction S.Reference.Id
c2sReferenceId = C.Reference.idH Q.expectObjectIdForPrimaryHash

s2cReferenceId :: S.Reference.Id -> Transaction C.Reference.Id
s2cReferenceId = C.Reference.idH Q.expectPrimaryHashByObjectId

h2cReferenceId :: S.Reference.IdH -> Transaction C.Reference.Id
h2cReferenceId = C.Reference.idH Q.expectHash

h2cReference :: S.ReferenceH -> Transaction C.Reference
h2cReference = bitraverse Q.expectText Q.expectHash

c2hReference :: C.Reference -> MaybeT Transaction S.ReferenceH
c2hReference = bitraverse (MaybeT . Q.loadTextId) (MaybeT . Q.loadHashIdByHash)

s2cReferent :: S.Referent -> Transaction C.Referent
s2cReferent = bitraverse s2cReference s2cReference

s2cTextReferent :: S.TextReferent -> C.Referent
s2cTextReferent = bimap s2cTextReference s2cTextReference

s2cConstructorType :: S.ConstructorType -> C.ConstructorType
s2cConstructorType = \case
  S.DataConstructor -> C.DataConstructor
  S.EffectConstructor -> C.EffectConstructor

c2sConstructorType :: C.ConstructorType -> S.ConstructorType
c2sConstructorType = \case
  C.DataConstructor -> S.DataConstructor
  C.EffectConstructor -> S.EffectConstructor

s2cReferentId :: S.Referent.Id -> Transaction C.Referent.Id
s2cReferentId = bitraverse Q.expectPrimaryHashByObjectId Q.expectPrimaryHashByObjectId

c2sReferent :: C.Referent -> Transaction S.Referent
c2sReferent = bitraverse c2sReference c2sReference

c2sTextReferent :: C.Referent -> S.TextReferent
c2sTextReferent = bimap c2sTextReference c2sTextReference

c2sReferentId :: C.Referent.Id -> Transaction S.Referent.Id
c2sReferentId = bitraverse Q.expectObjectIdForPrimaryHash Q.expectObjectIdForPrimaryHash

h2cReferent :: S.ReferentH -> Transaction C.Referent
h2cReferent = bitraverse h2cReference h2cReference

-- ** convert and save references

saveReferentH :: C.Referent -> Transaction S.ReferentH
saveReferentH = bitraverse Q.saveReferenceH Q.saveReferenceH

-- ** Edits transformations

s2cTermEdit :: S.TermEdit -> Transaction C.TermEdit
s2cTermEdit = \case
  S.TermEdit.Replace r t -> C.TermEdit.Replace <$> s2cReferent r <*> pure (s2cTyping t)
  S.TermEdit.Deprecate -> pure C.TermEdit.Deprecate

s2cTyping :: S.TermEdit.Typing -> C.TermEdit.Typing
s2cTyping = \case
  S.TermEdit.Same -> C.TermEdit.Same
  S.TermEdit.Subtype -> C.TermEdit.Subtype
  S.TermEdit.Different -> C.TermEdit.Different

c2sTyping :: C.TermEdit.Typing -> S.TermEdit.Typing
c2sTyping = \case
  C.TermEdit.Same -> S.TermEdit.Same
  C.TermEdit.Subtype -> S.TermEdit.Subtype
  C.TermEdit.Different -> S.TermEdit.Different

s2cTypeEdit :: S.TypeEdit -> Transaction C.TypeEdit
s2cTypeEdit = \case
  S.TypeEdit.Replace r -> C.TypeEdit.Replace <$> s2cReference r
  S.TypeEdit.Deprecate -> pure C.TypeEdit.Deprecate

-- | assumes that all relevant defns are already in the DB
c2sPatch :: C.Branch.Patch -> Transaction S.Patch
c2sPatch (C.Branch.Patch termEdits typeEdits) =
  S.Patch
    <$> Map.bitraverse saveReferentH (Set.traverse c2sTermEdit) termEdits
    <*> Map.bitraverse Q.saveReferenceH (Set.traverse c2sTypeEdit) typeEdits
  where
    c2sTermEdit = \case
      C.TermEdit.Replace r t -> S.TermEdit.Replace <$> c2sReferent r <*> pure (c2sTyping t)
      C.TermEdit.Deprecate -> pure S.TermEdit.Deprecate

    c2sTypeEdit = \case
      C.TypeEdit.Replace r -> S.TypeEdit.Replace <$> c2sReference r
      C.TypeEdit.Deprecate -> pure S.TypeEdit.Deprecate

-- | produces a diff
-- diff = full - ref; full = diff + ref
diffPatch :: S.LocalPatch -> S.LocalPatch -> S.LocalPatchDiff
diffPatch (S.Patch fullTerms fullTypes) (S.Patch refTerms refTypes) =
  (S.PatchDiff addTermEdits addTypeEdits removeTermEdits removeTypeEdits)
  where
    -- add: present in full. but absent in ref.
    addTermEdits = Map.merge Map.preserveMissing Map.dropMissing addDiffSet fullTerms refTerms
    addTypeEdits = Map.merge Map.preserveMissing Map.dropMissing addDiffSet fullTypes refTypes
    -- remove: present in ref. but absent in full.
    removeTermEdits = Map.merge Map.dropMissing Map.preserveMissing removeDiffSet fullTerms refTerms
    removeTypeEdits = Map.merge Map.dropMissing Map.preserveMissing removeDiffSet fullTypes refTypes
    -- things that are present in full but absent in ref
    addDiffSet,
      removeDiffSet ::
        (Ord k, Ord a) => Map.WhenMatched Identity k (Set a) (Set a) (Set a)
    addDiffSet = Map.zipWithMatched (const Set.difference)
    removeDiffSet = Map.zipWithMatched (const (flip Set.difference))

getCycleLen :: H.Hash -> Transaction (Maybe Word64)
getCycleLen h = do
  when debug $ traceM $ "\ngetCycleLen " ++ (Text.unpack . Base32Hex.toText $ H.toBase32Hex h)
  runMaybeT do
    -- actually want Nothing in case of non term/decl component hash
    oid <- MaybeT (Q.loadObjectIdForAnyHash h)
    -- todo: decodeComponentLengthOnly is unintentionally a hack that relies on
    -- the fact the two things that references can refer to (term and decl
    -- components) have the same basic serialized structure: first a format
    -- byte that is always 0 for now, followed by a framed array representing
    -- the strongly-connected component. :grimace:
    lift (Q.expectObject oid decodeComponentLengthOnly)

-- | Get the 'C.DeclType.DeclType' of a 'C.Reference.Id'.
expectDeclTypeById :: C.Reference.Id -> Transaction C.Decl.DeclType
expectDeclTypeById =
  fmap C.Decl.declType . expectDeclByReference

componentByObjectId :: Db.ObjectId -> Transaction [S.Reference.Id]
componentByObjectId id = do
  when debug . traceM $ "Operations.componentByObjectId " ++ show id
  len <- Q.expectObject id decodeComponentLengthOnly
  pure [C.Reference.Id id i | i <- [0 .. len - 1]]

-- * Codebase operations

-- ** Saving & loading terms

loadTermComponent :: H.Hash -> MaybeT Transaction [(C.Term Symbol, C.Term.Type Symbol)]
loadTermComponent h = do
  oid <- MaybeT (Q.loadObjectIdForAnyHash h)
  S.Term.Term (S.Term.LocallyIndexedComponent elements) <- MaybeT (Q.loadTermObject oid decodeTermFormat)
  lift . traverse (uncurry3 Q.s2cTermWithType) $ Foldable.toList elements

loadTermWithTypeByReference :: C.Reference.Id -> MaybeT Transaction (C.Term Symbol, C.Term.Type Symbol)
loadTermWithTypeByReference (C.Reference.Id h i) = do
  oid <- MaybeT (Q.loadObjectIdForPrimaryHash h)
  -- retrieve and deserialize the blob
  (localIds, term, typ) <- MaybeT (Q.loadTermObject oid (decodeTermElementWithType i))
  lift (Q.s2cTermWithType localIds term typ)

loadTermByReference :: C.Reference.Id -> MaybeT Transaction (C.Term Symbol)
loadTermByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "loadTermByReference " ++ show r
  oid <- MaybeT (Q.loadObjectIdForPrimaryHash h)
  -- retrieve and deserialize the blob
  (localIds, term) <- MaybeT (Q.loadTermObject oid (decodeTermElementDiscardingType i))
  lift (s2cTerm localIds term)

loadTypeOfTermByTermReference :: C.Reference.Id -> MaybeT Transaction (C.Term.Type Symbol)
loadTypeOfTermByTermReference id@(C.Reference.Id h i) = do
  when debug . traceM $ "loadTypeOfTermByTermReference " ++ show id
  oid <- MaybeT (Q.loadObjectIdForPrimaryHash h)
  -- retrieve and deserialize the blob
  (localIds, typ) <- MaybeT (Q.loadTermObject oid (decodeTermElementDiscardingTerm i))
  lift (s2cTypeOfTerm localIds typ)

s2cTerm :: LocalIds -> S.Term.Term -> Transaction (C.Term Symbol)
s2cTerm ids tm = do
  (substText, substHash) <- Q.localIdsToLookups Q.expectText Q.expectPrimaryHashByObjectId ids
  pure $ Q.x2cTerm substText substHash tm

s2cTypeOfTerm :: LocalIds -> S.Term.Type -> Transaction (C.Term.Type Symbol)
s2cTypeOfTerm ids tp = do
  (substText, substHash) <- Q.localIdsToLookups Q.expectText Q.expectPrimaryHashByObjectId ids
  pure $ Q.x2cTType substText substHash tp

-- *** Watch expressions

listWatches :: WatchKind -> Transaction [C.Reference.Id]
listWatches k = Q.loadWatchesByWatchKind k >>= traverse h2cReferenceId

-- | returns Nothing if the expression isn't cached.
loadWatch :: WatchKind -> C.Reference.Id -> MaybeT Transaction (C.Term Symbol)
loadWatch k r = do
  r' <- C.Reference.idH (MaybeT . Q.loadHashIdByHash) r
  S.Term.WatchResult wlids t <- MaybeT (Q.loadWatch k r' decodeWatchResultFormat)
  lift (w2cTerm wlids t)

saveWatch :: WatchKind -> C.Reference.Id -> C.Term Symbol -> Transaction ()
saveWatch w r t = do
  rs <- C.Reference.idH Q.saveHashHash r
  wterm <- c2wTerm t
  let bytes = S.putBytes S.putWatchResultFormat (uncurry S.Term.WatchResult wterm)
  Q.saveWatch w rs bytes

c2wTerm :: C.Term Symbol -> Transaction (WatchLocalIds, S.Term.Term)
c2wTerm tm = Q.c2xTerm Q.saveText Q.saveHashHash tm Nothing <&> \(w, tm, _) -> (w, tm)

w2cTerm :: WatchLocalIds -> S.Term.Term -> Transaction (C.Term Symbol)
w2cTerm ids tm = do
  (substText, substHash) <- Q.localIdsToLookups Q.expectText Q.expectHash ids
  pure $ Q.x2cTerm substText substHash tm

-- ** Saving & loading type decls

loadDeclComponent :: H.Hash -> MaybeT Transaction [C.Decl Symbol]
loadDeclComponent h = do
  oid <- MaybeT (Q.loadObjectIdForAnyHash h)
  S.Decl.Decl (S.Decl.LocallyIndexedComponent elements) <- MaybeT (Q.loadDeclObject oid decodeDeclFormat)
  lift . traverse (uncurry Q.s2cDecl) $ Foldable.toList elements

loadDeclByReference :: C.Reference.Id -> MaybeT Transaction (C.Decl Symbol)
loadDeclByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "loadDeclByReference " ++ show r
  oid <- MaybeT (Q.loadObjectIdForPrimaryHash h)
  (localIds, decl) <- MaybeT (Q.loadDeclObject oid (decodeDeclElement i))
  lift (Q.s2cDecl localIds decl)

expectDeclByReference :: C.Reference.Id -> Transaction (C.Decl Symbol)
expectDeclByReference r@(C.Reference.Id h i) = do
  when debug . traceM $ "expectDeclByReference " ++ show r
  -- retrieve the blob
  Q.expectObjectIdForPrimaryHash h
    >>= (\oid -> Q.expectDeclObject oid (decodeDeclElement i))
    >>= uncurry Q.s2cDecl

expectDeclNumConstructors :: C.Reference.Id -> Transaction Int
expectDeclNumConstructors (C.Reference.Id h i) = do
  oid <- Q.expectObjectIdForPrimaryHash h
  Q.expectDeclObject oid (decodeDeclElementNumConstructors i)

-- * Branch transformation

s2cBranch :: S.DbBranch -> Transaction (C.Branch.Branch Transaction)
s2cBranch (S.Branch.Full.Branch tms tps patches children) =
  C.Branch.Branch
    <$> doTerms tms
    <*> doTypes tps
    <*> doPatches patches
    <*> doChildren children
  where
    doTerms ::
      Map Db.TextId (Map S.Referent S.DbMetadataSet) ->
      Transaction (Map NameSegment (Map C.Referent (Transaction C.Branch.MdValues)))
    doTerms =
      Map.bitraverse
        Q.expectNameSegment
        ( Map.bitraverse s2cReferent \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> Set.traverse s2cReference rs
        )
    doTypes ::
      Map Db.TextId (Map S.Reference S.DbMetadataSet) ->
      Transaction (Map NameSegment (Map C.Reference (Transaction C.Branch.MdValues)))
    doTypes =
      Map.bitraverse
        Q.expectNameSegment
        ( Map.bitraverse s2cReference \case
            S.MetadataSet.Inline rs ->
              pure $ C.Branch.MdValues <$> Set.traverse s2cReference rs
        )
    doPatches ::
      Map Db.TextId Db.PatchObjectId ->
      Transaction (Map NameSegment (PatchHash, Transaction C.Branch.Patch))
    doPatches = Map.bitraverse Q.expectNameSegment \patchId -> do
      h <- PatchHash <$> (Q.expectPrimaryHashByObjectId . Db.unPatchObjectId) patchId
      pure (h, expectPatch patchId)

    doChildren ::
      Map Db.TextId (Db.BranchObjectId, Db.CausalHashId) ->
      Transaction (Map NameSegment (C.Branch.CausalBranch Transaction))
    doChildren = Map.bitraverse Q.expectNameSegment \(boId, chId) ->
      C.Causal
        <$> Q.expectCausalHash chId
        <*> expectValueHashByCausalHashId chId
        <*> headParents chId
        <*> pure (expectBranch boId)
      where
        headParents :: Db.CausalHashId -> Transaction (Map CausalHash (Transaction (C.Branch.CausalBranch Transaction)))
        headParents chId = do
          parentsChIds <- Q.loadCausalParents chId
          fmap Map.fromList $ traverse pairParent parentsChIds
        pairParent :: Db.CausalHashId -> Transaction (CausalHash, Transaction (C.Branch.CausalBranch Transaction))
        pairParent chId = do
          h <- Q.expectCausalHash chId
          pure (h, loadCausal chId)
        loadCausal :: Db.CausalHashId -> Transaction (C.Branch.CausalBranch Transaction)
        loadCausal chId = do
          C.Causal
            <$> Q.expectCausalHash chId
            <*> expectValueHashByCausalHashId chId
            <*> headParents chId
            <*> pure (loadValue chId)
        loadValue :: Db.CausalHashId -> Transaction (C.Branch.Branch Transaction)
        loadValue chId = do
          boId <- Q.expectBranchObjectIdByCausalHashId chId
          expectBranch boId

-- saveBranch is kind of a "deep save causal"

-- we want a "shallow save causal" that could take a
--   forall m e. Causal m CausalHash BranchHash e
--
-- data Identity a = Identity
-- e == ()
--
-- data C.Branch m = Branch
--   { terms    :: Map NameSegment (Map Referent (m MdValues)),
--     types    :: Map NameSegment (Map Reference (m MdValues)),
--     patches  :: Map NameSegment (PatchHash, m Patch),
--     children :: Map NameSegment (Causal m)
--   }
--
-- U.Codebase.Sqlite.Branch.Full.Branch'
-- type ShallowBranch = Branch' NameSegment Hash PatchHash CausalHash
-- data ShallowBranch causalHash patchHash = ShallowBranch
--   { terms    :: Map NameSegment (Map Referent MdValues),
--     types    :: Map NameSegment (Map Reference MdValues),
--     patches  :: Map NameSegment patchHash,
--     children :: Map NameSegment causalHash
--   }
--
-- data Causal m hc he e = Causal
--  { causalHash :: hc,
--    valueHash :: he,
--    parents :: Map hc (m (Causal m hc he e)),
--    value :: m e
--  }
-- data ShallowCausal causalHash branchHash = ShallowCausal
--  { causalHash :: causalHash,
--    valueHash :: branchHash,
--    parents :: Set causalHash,
--  }
--
-- References, but also values
-- Shallow - Hash? representation of the database relationships

-- A couple small internal helper type that unifies V2 and V3 branches. This should be used as input to operations that
-- can work on either kind of branch.

data BranchV m
  = BranchV2 !(C.Branch.Branch m)
  | BranchV3 !(C.BranchV3.BranchV3 m)

data DbBranchV
  = DbBranchV2 !S.DbBranch
  | DbBranchV3 !S.DbBranchV3

saveBranch ::
  HashHandle ->
  C.Branch.CausalBranch Transaction ->
  Transaction (Db.BranchObjectId, Db.CausalHashId)
saveBranch hh causal@(C.Causal hc he parents me) = do
  when debug $ traceM $ "\nOperations.saveBranch \n  hc = " ++ show hc ++ ",\n  he = " ++ show he ++ ",\n  parents = " ++ show (Map.keys parents)
  (chId, bhId) <- saveCausalObject hh causal
  boId <- saveNamespace hh bhId (BranchV2 <$> me)
  pure (boId, chId)

saveBranchV3 :: HashHandle -> C.BranchV3.CausalBranchV3 Transaction -> Transaction (Db.BranchObjectId, Db.CausalHashId)
saveBranchV3 hh causal = do
  (chId, bhId) <- saveCausalObject hh causal
  boId <- saveNamespace hh bhId (BranchV3 <$> (causal ^. #value))
  pure (boId, chId)

-- Save a namespace. Internal helper shared by `saveBranch` ("save V2 namespace") and `saveBranchV3`
-- ("save V3 namespace").
saveNamespace :: HashHandle -> Db.BranchHashId -> Transaction (BranchV Transaction) -> Transaction Db.BranchObjectId
saveNamespace hh bhId me = do
  Q.loadBranchObjectIdByBranchHashId bhId & onNothingM do
    branch <- me
    dbBranch <- c2sBranch branch
    stats <- namespaceStatsForDbBranch dbBranch
    saveDbBranchUnderHashId hh bhId stats dbBranch
  where
    c2sBranch :: BranchV Transaction -> Transaction DbBranchV
    c2sBranch = \case
      BranchV2 branch -> do
        terms <- Map.bitraverse Q.saveNameSegment (Map.bitraverse c2sReferent c2sMetadata) (branch ^. #terms)
        types <- Map.bitraverse Q.saveNameSegment (Map.bitraverse c2sReference c2sMetadata) (branch ^. #types)
        patches <- Map.bitraverse Q.saveNameSegment savePatchObjectId (branch ^. #patches)
        children <- Map.bitraverse Q.saveNameSegment (saveBranch hh) (branch ^. #children)
        pure (DbBranchV2 S.Branch {terms, types, patches, children})
      BranchV3 branch -> do
        children <- Map.bitraverse Q.saveNameSegment (saveBranchV3 hh) (branch ^. #children)
        terms <- Map.bitraverse Q.saveNameSegment c2sReferent (branch ^. #terms)
        types <- Map.bitraverse Q.saveNameSegment c2sReference (branch ^. #types)
        pure (DbBranchV3 S.BranchV3 {children, terms, types})

    c2sMetadata :: Transaction C.Branch.MdValues -> Transaction S.Branch.Full.DbMetadataSet
    c2sMetadata mm = do
      C.Branch.MdValues m <- mm
      S.Branch.Full.Inline <$> Set.traverse c2sReference m

    savePatchObjectId :: (PatchHash, Transaction C.Branch.Patch) -> Transaction Db.PatchObjectId
    savePatchObjectId (h, mp) = do
      Q.loadPatchObjectIdForPrimaryHash h & onNothingM do
        patch <- mp
        savePatch hh h patch

-- Save just the causal object (i.e. the `causal` row and its associated `causal_parents`). Internal helper shared by
-- `saveBranch` and `saveBranchV3`.
saveCausalObject ::
  HashHandle ->
  C.Causal Transaction CausalHash BranchHash (C.Branch.Branch Transaction) branch ->
  Transaction (Db.CausalHashId, Db.BranchHashId)
saveCausalObject hh (C.Causal.Causal hc he parents _) = do
  Q.loadCausalByCausalHash hc & onNothingM do
    -- if not exist, create these
    chId <- Q.saveCausalHash hc
    bhId <- Q.saveBranchHash he

    parentCausalHashIds <-
      -- so try to save each parent (recursively) before continuing to save hc
      for (Map.toList parents) $ \(parentHash, mcausal) ->
        -- check if we can short circuit the parent before loading it,
        -- by checking if there are causal parents associated with hc
        (flip Monad.fromMaybeM)
          (Q.loadCausalHashIdByCausalHash parentHash)
          (mcausal >>= fmap snd . saveBranch hh)

    -- Save these CausalHashIds to the causal_parents table,
    Q.saveCausal hh chId bhId parentCausalHashIds
    pure (chId, bhId)

loadCausalBranchByCausalHash :: CausalHash -> Transaction (Maybe (C.Branch.CausalBranch Transaction))
loadCausalBranchByCausalHash hc = do
  Q.loadCausalHashIdByCausalHash hc >>= \case
    Just chId -> Just <$> expectCausalBranchByCausalHashId chId
    Nothing -> pure Nothing

expectCausalBranchByCausalHashId :: Db.CausalHashId -> Transaction (C.Branch.CausalBranch Transaction)
expectCausalBranchByCausalHashId id = do
  hc <- Q.expectCausalHash id
  hb <- expectValueHashByCausalHashId id
  parentHashIds <- Q.loadCausalParents id
  loadParents <- for parentHashIds \hId -> do
    h <- Q.expectCausalHash hId
    pure (h, expectCausalBranchByCausalHashId hId)
  pure $ C.Causal hc hb (Map.fromList loadParents) (expectBranchByCausalHashId id)

expectCausalBranchByCausalHash :: CausalHash -> Transaction (C.Branch.CausalBranch Transaction)
expectCausalBranchByCausalHash hash = do
  chId <- Q.expectCausalHashIdByCausalHash hash
  expectCausalBranchByCausalHashId chId

expectBranchByCausalHashId :: Db.CausalHashId -> Transaction (C.Branch.Branch Transaction)
expectBranchByCausalHashId id = do
  boId <- Q.expectBranchObjectIdByCausalHashId id
  expectBranch boId

-- | Load a branch value given its causal hash id.
loadDbBranchByCausalHashId :: Db.CausalHashId -> Transaction (Maybe S.DbBranch)
loadDbBranchByCausalHashId causalHashId =
  Q.loadBranchObjectIdByCausalHashId causalHashId >>= \case
    Nothing -> pure Nothing
    Just branchObjectId -> Just <$> expectDbBranch branchObjectId

expectBranchByBranchHashId :: Db.BranchHashId -> Transaction (C.Branch.Branch Transaction)
expectBranchByBranchHashId bhId = do
  boId <- Q.expectBranchObjectIdByBranchHashId bhId
  expectBranch boId

expectBranchByBranchHash :: BranchHash -> Transaction (C.Branch.Branch Transaction)
expectBranchByBranchHash bh = do
  bhId <- Q.expectBranchHashId bh
  expectBranchByBranchHashId bhId

-- | Expect a branch value given its causal hash id.
expectDbBranchByCausalHashId :: Db.CausalHashId -> Transaction S.DbBranch
expectDbBranchByCausalHashId causalHashId = do
  branchObjectId <- Q.expectBranchObjectIdByCausalHashId causalHashId
  expectDbBranch branchObjectId

expectDbBranch :: Db.BranchObjectId -> Transaction S.DbBranch
expectDbBranch id =
  deserializeBranchObject id >>= \case
    S.BranchFormat.Full li f -> pure (S.BranchFormat.localToDbBranch li f)
    S.BranchFormat.Diff r li d -> doDiff r [S.BranchFormat.localToDbDiff li d]
  where
    deserializeBranchObject :: Db.BranchObjectId -> Transaction S.BranchFormat
    deserializeBranchObject id = do
      when debug $ traceM $ "deserializeBranchObject " ++ show id
      Q.expectNamespaceObject (Db.unBranchObjectId id) decodeBranchFormat

    doDiff :: Db.BranchObjectId -> [S.Branch.Diff] -> Transaction S.DbBranch
    doDiff ref ds =
      deserializeBranchObject ref >>= \case
        S.BranchFormat.Full li f -> pure (joinFull (S.BranchFormat.localToDbBranch li f) ds)
        S.BranchFormat.Diff ref' li' d' -> doDiff ref' (S.BranchFormat.localToDbDiff li' d' : ds)
      where
        joinFull :: S.DbBranch -> [S.Branch.Diff] -> S.DbBranch
        joinFull f [] = f
        joinFull
          (S.Branch.Full.Branch tms tps patches children)
          (S.Branch.Diff tms' tps' patches' children' : ds) = joinFull f' ds
            where
              f' =
                S.Branch.Full.Branch
                  (mergeDefns tms tms')
                  (mergeDefns tps tps')
                  (mergePatches patches patches')
                  (mergeChildren children children')
        mergeChildren ::
          (Ord ns) =>
          Map ns (Db.BranchObjectId, Db.CausalHashId) ->
          Map ns S.BranchDiff.ChildOp ->
          Map ns (Db.BranchObjectId, Db.CausalHashId)
        mergeChildren =
          Map.merge
            Map.preserveMissing
            (Map.mapMissing fromChildOp)
            (Map.zipWithMaybeMatched mergeChildOp)
        mergeChildOp ::
          ns ->
          (Db.BranchObjectId, Db.CausalHashId) ->
          S.BranchDiff.ChildOp ->
          Maybe (Db.BranchObjectId, Db.CausalHashId)
        mergeChildOp =
          const . const \case
            S.BranchDiff.ChildAddReplace id -> Just id
            S.BranchDiff.ChildRemove -> Nothing
        fromChildOp :: ns -> S.BranchDiff.ChildOp -> (Db.BranchObjectId, Db.CausalHashId)
        fromChildOp = const \case
          S.BranchDiff.ChildAddReplace id -> id
          S.BranchDiff.ChildRemove -> error "diff tries to remove a nonexistent child"
        mergePatches ::
          (Ord ns) =>
          Map ns Db.PatchObjectId ->
          Map ns S.BranchDiff.PatchOp ->
          Map ns Db.PatchObjectId
        mergePatches =
          Map.merge Map.preserveMissing (Map.mapMissing fromPatchOp) (Map.zipWithMaybeMatched mergePatchOp)
        fromPatchOp :: ns -> S.BranchDiff.PatchOp -> Db.PatchObjectId
        fromPatchOp = const \case
          S.BranchDiff.PatchAddReplace id -> id
          S.BranchDiff.PatchRemove -> error "diff tries to remove a nonexistent child"
        mergePatchOp :: ns -> Db.PatchObjectId -> S.BranchDiff.PatchOp -> Maybe Db.PatchObjectId
        mergePatchOp =
          const . const \case
            S.BranchDiff.PatchAddReplace id -> Just id
            S.BranchDiff.PatchRemove -> Nothing

        mergeDefns ::
          (Ord ns, Ord r) =>
          Map ns (Map r S.MetadataSet.DbMetadataSet) ->
          Map ns (Map r S.BranchDiff.DefinitionOp) ->
          Map ns (Map r S.MetadataSet.DbMetadataSet)
        mergeDefns =
          Map.merge
            Map.preserveMissing
            (Map.mapMissing (const (fmap fromDefnOp)))
            (Map.zipWithMatched (const mergeDefnOp))
        fromDefnOp :: S.BranchDiff.DefinitionOp -> S.MetadataSet.DbMetadataSet
        fromDefnOp = \case
          S.Branch.Diff.AddDefWithMetadata md -> S.MetadataSet.Inline md
          S.Branch.Diff.RemoveDef -> error "diff tries to remove a nonexistent definition"
          S.Branch.Diff.AlterDefMetadata _md -> error "diff tries to change metadata for a nonexistent definition"
        mergeDefnOp ::
          (Ord r) =>
          Map r S.MetadataSet.DbMetadataSet ->
          Map r S.BranchDiff.DefinitionOp ->
          Map r S.MetadataSet.DbMetadataSet
        mergeDefnOp =
          Map.merge
            Map.preserveMissing
            (Map.mapMissing (const fromDefnOp))
            (Map.zipWithMaybeMatched (const mergeDefnOp'))
        mergeDefnOp' ::
          S.MetadataSet.DbMetadataSet ->
          S.BranchDiff.DefinitionOp ->
          Maybe S.MetadataSet.DbMetadataSet
        mergeDefnOp' (S.MetadataSet.Inline md) = \case
          S.Branch.Diff.AddDefWithMetadata _md ->
            error "diff tries to create a child that already exists"
          S.Branch.Diff.RemoveDef -> Nothing
          S.Branch.Diff.AlterDefMetadata md' ->
            let (Set.fromList -> adds, Set.fromList -> removes) = S.BranchDiff.addsRemoves md'
             in Just . S.MetadataSet.Inline $ (Set.union adds $ Set.difference md removes)

-- | Save a 'S.DbBranch', given its hash (which the caller is expected to produce from the branch).
--
-- Note: long-standing question: should this package depend on the hashing package? (If so, we would only need to take
-- the DbBranch, and hash internally).
saveDbBranch ::
  HashHandle ->
  BranchHash ->
  C.Branch.NamespaceStats ->
  DbBranchV ->
  Transaction Db.BranchObjectId
saveDbBranch hh hash stats branch = do
  hashId <- Q.saveBranchHash hash
  saveDbBranchUnderHashId hh hashId stats branch

-- | Variant of 'saveDbBranch' that might be preferred by callers that already have a hash id, not a hash.
saveDbBranchUnderHashId ::
  HashHandle ->
  Db.BranchHashId ->
  C.Branch.NamespaceStats ->
  DbBranchV ->
  Transaction Db.BranchObjectId
saveDbBranchUnderHashId hh bhId@(Db.unBranchHashId -> hashId) stats = \case
  DbBranchV2 branch -> saveV2 branch
  -- Here, we elect to serialize V3 branches as V2 branches just before saving them to the database. We could save a
  -- little space instead by actually having a proper serialization format for V3 branches.
  DbBranchV3 S.BranchV3 {children, terms, types} ->
    saveV2
      S.Branch
        { children,
          patches = Map.empty,
          terms = Map.map unconflictedAndWithoutMetadata terms,
          types = Map.map unconflictedAndWithoutMetadata types
        }
    where
      -- Carry a v3 term or type (one ref, no metadata) to a v2 term or type (set of refs, each with metadata)
      unconflictedAndWithoutMetadata :: ref -> Map ref S.DbMetadataSet
      unconflictedAndWithoutMetadata ref =
        Map.singleton ref (S.MetadataSet.Inline Set.empty)
  where
    saveV2 :: S.DbBranch -> Transaction Db.BranchObjectId
    saveV2 branch = do
      let (localBranchIds, localBranch) = LocalizeObject.localizeBranch branch
      let bytes = S.putBytes S.putBranchFormat $ S.BranchFormat.Full localBranchIds localBranch
      oId <- Q.saveObject hh hashId ObjectType.Namespace bytes
      Q.saveNamespaceStats bhId stats
      pure $ Db.BranchObjectId oId

expectBranch :: Db.BranchObjectId -> Transaction (C.Branch.Branch Transaction)
expectBranch id =
  expectDbBranch id >>= s2cBranch

-- * Patch transformation

expectPatch :: Db.PatchObjectId -> Transaction C.Branch.Patch
expectPatch patchId =
  expectDbPatch patchId >>= s2cPatch

expectDbPatch :: Db.PatchObjectId -> Transaction S.Patch
expectDbPatch patchId =
  deserializePatchObject patchId >>= \case
    S.Patch.Format.Full li p -> pure (S.Patch.Format.localPatchToPatch li p)
    S.Patch.Format.Diff ref li d -> doDiff ref [S.Patch.Format.localPatchDiffToPatchDiff li d]
  where
    doDiff :: Db.PatchObjectId -> [S.PatchDiff] -> Transaction S.Patch
    doDiff ref ds =
      deserializePatchObject ref >>= \case
        S.Patch.Format.Full li f -> pure (S.Patch.Format.applyPatchDiffs (S.Patch.Format.localPatchToPatch li f) ds)
        S.Patch.Format.Diff ref' li' d' -> doDiff ref' (S.Patch.Format.localPatchDiffToPatchDiff li' d' : ds)

savePatch ::
  HashHandle ->
  PatchHash ->
  C.Branch.Patch ->
  Transaction Db.PatchObjectId
savePatch hh h c = do
  (li, lPatch) <- LocalizeObject.localizePatch <$> c2sPatch c
  saveDbPatch hh h (S.Patch.Format.Full li lPatch)

saveDbPatch ::
  HashHandle ->
  PatchHash ->
  S.PatchFormat ->
  Transaction Db.PatchObjectId
saveDbPatch hh hash patch = do
  hashId <- Q.saveHashHash (unPatchHash hash)
  let bytes = S.putBytes S.putPatchFormat patch
  Db.PatchObjectId <$> Q.saveObject hh hashId ObjectType.Patch bytes

s2cPatch :: S.Patch -> Transaction C.Branch.Patch
s2cPatch (S.Patch termEdits typeEdits) =
  C.Branch.Patch
    <$> Map.bitraverse h2cReferent (Set.traverse s2cTermEdit) termEdits
    <*> Map.bitraverse h2cReference (Set.traverse s2cTypeEdit) typeEdits

deserializePatchObject :: Db.PatchObjectId -> Transaction S.PatchFormat
deserializePatchObject id = do
  when debug $ traceM $ "Operations.deserializePatchObject " ++ show id
  Q.expectPatchObject (Db.unPatchObjectId id) decodePatchFormat

lca :: CausalHash -> CausalHash -> Transaction (Maybe CausalHash)
lca h1 h2 = runMaybeT do
  chId1 <- MaybeT $ Q.loadCausalHashIdByCausalHash h1
  chId2 <- MaybeT $ Q.loadCausalHashIdByCausalHash h2
  chId3 <- MaybeT $ Q.lca chId1 chId2
  lift (Q.expectCausalHash chId3)

before :: CausalHash -> CausalHash -> Transaction (Maybe Bool)
before h1 h2 = runMaybeT do
  chId2 <- MaybeT $ Q.loadCausalHashIdByCausalHash h2
  lift (Q.loadCausalHashIdByCausalHash h1) >>= \case
    Just chId1 -> lift (Q.before chId1 chId2)
    Nothing -> pure False

-- * Searches

termsHavingType :: C.Reference -> Transaction (Set C.Referent.Id)
termsHavingType cTypeRef =
  runMaybeT (c2hReference cTypeRef) >>= \case
    Nothing -> pure Set.empty
    Just sTypeRef -> do
      sIds <- Q.getReferentsByType sTypeRef
      set <- traverse s2cReferentId sIds
      pure (Set.fromList set)

filterTermsByReferenceHavingType :: C.TypeReference -> [C.Reference.Id] -> Transaction [C.Reference.Id]
filterTermsByReferenceHavingType cTypeRef cTermRefIds =
  runMaybeT (c2hReference cTypeRef) >>= \case
    Nothing -> pure []
    Just sTypeRef -> do
      sTermRefIds <- traverse c2sReferenceId cTermRefIds
      matches <- Q.filterTermsByReferenceHavingType sTypeRef sTermRefIds
      traverse s2cReferenceId matches

filterTermsByReferentHavingType :: C.TypeReference -> [C.Referent.Id] -> Transaction [C.Referent.Id]
filterTermsByReferentHavingType cTypeRef cTermRefIds =
  runMaybeT (c2hReference cTypeRef) >>= \case
    Nothing -> pure []
    Just sTypeRef -> do
      sTermRefIds <- traverse c2sReferentId cTermRefIds
      matches <- Q.filterTermsByReferentHavingType sTypeRef sTermRefIds
      traverse s2cReferentId matches

termsMentioningType :: C.Reference -> Transaction (Set C.Referent.Id)
termsMentioningType cTypeRef =
  runMaybeT (c2hReference cTypeRef) >>= \case
    Nothing -> pure Set.empty
    Just sTypeRef -> do
      sIds <- Q.getReferentsByTypeMention sTypeRef
      set <- traverse s2cReferentId sIds
      pure (Set.fromList set)

-- something kind of funny here.  first, we don't need to enumerate all the reference pos if we're just picking one
-- second, it would be nice if we could leave these as S.References a little longer
-- so that we remember how to blow up if they're missing
componentReferencesByPrefix :: ObjectType.ObjectType -> Text -> Maybe C.Reference.Pos -> Transaction [S.Reference.Id]
componentReferencesByPrefix ot b32prefix pos = do
  oIds :: [Db.ObjectId] <- Q.objectIdByBase32Prefix ot b32prefix
  let test = maybe (const True) (==) pos
  let filterComponent l = [x | x@(C.Reference.Id _ pos) <- l, test pos]
  join <$> traverse (fmap filterComponent . componentByObjectId) oIds

-- | Get the set of user-defined terms whose hash matches the given prefix.
termReferencesByPrefix :: Text -> Maybe Word64 -> Transaction [C.Reference.Id]
termReferencesByPrefix t w =
  componentReferencesByPrefix ObjectType.TermComponent t w
    >>= traverse (C.Reference.idH Q.expectPrimaryHashByObjectId)

declReferencesByPrefix :: Text -> Maybe Word64 -> Transaction [C.Reference.Id]
declReferencesByPrefix t w =
  componentReferencesByPrefix ObjectType.DeclComponent t w
    >>= traverse (C.Reference.idH Q.expectPrimaryHashByObjectId)

termReferentsByPrefix :: Text -> Maybe Word64 -> Transaction [C.Referent.Id]
termReferentsByPrefix b32prefix pos =
  fmap C.Referent.RefId <$> termReferencesByPrefix b32prefix pos

-- todo: simplify this if we stop caring about constructor type
-- todo: remove the cycle length once we drop it from Unison.Reference
declReferentsByPrefix ::
  Text ->
  Maybe C.Reference.Pos ->
  Maybe ConstructorId ->
  Transaction [(H.Hash, C.Reference.Pos, C.DeclType, [C.Decl.ConstructorId])]
declReferentsByPrefix b32prefix pos cid = do
  componentReferencesByPrefix ObjectType.DeclComponent b32prefix pos
    >>= traverse (loadConstructors cid)
  where
    loadConstructors ::
      Maybe Word64 ->
      S.Reference.Id ->
      Transaction (H.Hash, C.Reference.Pos, C.DeclType, [ConstructorId])
    loadConstructors cid rid@(C.Reference.Id oId pos) = do
      (dt, ctorCount) <- getDeclCtorCount rid
      h <- Q.expectPrimaryHashByObjectId oId
      let cids =
            case cid of
              Nothing -> take ctorCount [0 :: ConstructorId ..]
              Just cid -> if fromIntegral cid < ctorCount then [cid] else []
      pure (h, pos, dt, cids)
    getDeclCtorCount :: S.Reference.Id -> Transaction (C.Decl.DeclType, Int)
    getDeclCtorCount id@(C.Reference.Id r i) = do
      when debug $ traceM $ "getDeclCtorCount " ++ show id
      (_localIds, decl) <- Q.expectDeclObject r (decodeDeclElement i)
      pure (C.Decl.declType decl, length (C.Decl.constructorTypes decl))

namespaceHashesByPrefix :: ShortNamespaceHash -> Transaction (Set BranchHash)
namespaceHashesByPrefix (ShortNamespaceHash b32prefix) = do
  hashIds <- Q.namespaceHashIdByBase32Prefix b32prefix
  hashes <- traverse (Q.expectHash . Db.unBranchHashId) hashIds
  pure $ Set.fromList . map BranchHash $ hashes

causalHashesByPrefix :: ShortCausalHash -> Transaction (Set CausalHash)
causalHashesByPrefix (ShortCausalHash b32prefix) = do
  hashIds <- Q.causalHashIdByBase32Prefix b32prefix
  hashes <- traverse (Q.expectHash . Db.unCausalHashId) hashIds
  pure $ Set.fromList . map CausalHash $ hashes

directDependenciesOfScope ::
  (C.Reference -> Bool) ->
  DefnsF Set C.TermReferenceId C.TypeReferenceId ->
  Transaction (DefnsF Set C.TermReference C.TypeReference)
directDependenciesOfScope isBuiltinType scope0 = do
  -- Convert C -> S
  scope1 <- bitraverse (Set.traverse c2sReferenceId) (Set.traverse c2sReferenceId) scope0

  -- Do the query
  dependencies0 <- Q.getDirectDependenciesOfScope (fmap isBuiltinType . s2cReference) scope1

  -- Convert S -> C
  dependencies1 <- bitraverse (Set.traverse s2cReference) (Set.traverse s2cReference) dependencies0

  pure dependencies1

-- | returns a list of known definitions referencing `r`
dependents :: Q.DependentsSelector -> C.Reference -> Transaction (Set C.Reference.Id)
dependents selector r = do
  mr <- case r of
    C.ReferenceBuiltin {} -> pure (Just r)
    C.ReferenceDerived id_ ->
      objectExistsForHash (view C.idH id_) <&> \case
        True -> Just r
        False -> Nothing
  case mr of
    Nothing -> pure mempty
    Just r -> do
      r' <- c2sReference r
      sIds <- Q.getDependentsForDependency selector r'
      Set.traverse s2cReferenceId sIds

-- | `directDependentsWithinScope scope query` returns all direct dependents of `query` that are in `scope` (not
-- including `query` itself).
directDependentsWithinScope ::
  Set C.Reference.Id ->
  Set C.Reference ->
  Transaction (DefnsF Set C.TermReferenceId C.TypeReferenceId)
directDependentsWithinScope scope0 query0 = do
  -- Convert C -> S
  scope1 <- Set.traverse c2sReferenceId scope0
  query1 <- Set.traverse c2sReference query0

  -- Do the query
  dependents0 <- Q.getDirectDependentsWithinScope scope1 query1

  -- Convert S -> C
  dependents1 <- bitraverse (Set.traverse s2cReferenceId) (Set.traverse s2cReferenceId) dependents0

  pure dependents1

-- | `transitiveDependentsWithinScope scope query` returns all transitive dependents of `query` that are in `scope` (not
-- including `query` itself).
transitiveDependentsWithinScope ::
  Set C.Reference.Id ->
  Set C.Reference ->
  Transaction (DefnsF Set C.TermReferenceId C.TypeReferenceId)
transitiveDependentsWithinScope scope0 query0 = do
  -- Convert C -> S
  scope1 <- Set.traverse c2sReferenceId scope0
  query1 <- Set.traverse c2sReference query0

  -- Do the query
  dependents0 <- Q.getTransitiveDependentsWithinScope scope1 query1

  -- Convert S -> C
  dependents1 <- bitraverse (Set.traverse s2cReferenceId) (Set.traverse s2cReferenceId) dependents0

  pure dependents1

-- | returns a list of known definitions referencing `h`
dependentsOfComponent :: H.Hash -> Transaction (Set C.Reference.Id)
dependentsOfComponent h = do
  oId <- Q.expectObjectIdForPrimaryHash h
  sIds :: [S.Reference.Id] <- Q.getDependentsForDependencyComponent oId
  cIds <- traverse s2cReferenceId sIds
  pure $ Set.fromList cIds

-- | returns empty set for unknown inputs; doesn't distinguish between term and decl
derivedDependencies :: C.Reference.Id -> Transaction (Set C.Reference.Id)
derivedDependencies cid = do
  sid <- c2sReferenceId cid
  sids <- Q.getDependencyIdsForDependent sid
  cids <- traverse s2cReferenceId sids
  pure $ Set.fromList cids

-- | Apply a set of name updates to an existing index.
buildNameLookupForBranchHash ::
  -- The existing name lookup index to copy before applying the diff.
  -- If Nothing, run the diff against an empty index.
  -- If Just, the name lookup must exist or an error will be thrown.
  Maybe BranchHash ->
  BranchHash ->
  ( ( -- (add terms, remove terms)
      ([S.NamedRef (C.Referent, Maybe C.ConstructorType)], [S.NamedRef C.Referent]) ->
      --  (add types, remove types)
      ([S.NamedRef C.Reference], [S.NamedRef C.Reference]) ->
      Transaction ()
    ) ->
    Transaction ()
  ) ->
  Transaction ()
buildNameLookupForBranchHash mayExistingBranchIndex newBranchHash callback = do
  newBranchHashId <- Q.expectBranchHashId newBranchHash
  Q.trackNewBranchHashNameLookup newBranchHashId
  case mayExistingBranchIndex of
    Nothing -> pure ()
    Just existingBranchIndex -> do
      unlessM (checkBranchHashNameLookupExists existingBranchIndex) $ error "buildNameLookupForBranchHash: existingBranchIndex was provided, but no index was found for that branch hash."
      existingBranchHashId <- Q.expectBranchHashId existingBranchIndex
      Q.copyScopedNameLookup existingBranchHashId newBranchHashId
  callback \(newTermNames, removedTermNames) (newTypeNames, removedTypeNames) -> do
    Q.removeScopedTermNames newBranchHashId ((fmap c2sTextReferent <$> removedTermNames))
    Q.removeScopedTypeNames newBranchHashId ((fmap c2sTextReference <$> removedTypeNames))
    Q.insertScopedTermNames newBranchHashId (fmap (c2sTextReferent *** fmap c2sConstructorType) <$> newTermNames)
    Q.insertScopedTypeNames newBranchHashId (fmap c2sTextReference <$> newTypeNames)

-- | Save a list of (mount-path, branch hash) mounts for the provided name lookup index branch
-- hash.
--
-- E.g. associateNameLookupMounts #roothash [(["lib", "base"], #basehash)]
associateNameLookupMounts :: BranchHash -> [(PathSegments, BranchHash)] -> Transaction ()
associateNameLookupMounts rootBh dependencyMounts = do
  rootBhId <- Q.expectBranchHashId rootBh
  depMounts <- for dependencyMounts \(path, branchHash) -> do
    branchHashId <- Q.expectBranchHashId branchHash
    pure (path, branchHashId)
  Q.associateNameLookupMounts rootBhId depMounts

-- | Any time we need to lookup or search names we need to know what the scope of that search
-- should be. This can be complicated to keep track of, so this is a helper type to make it
-- easy to pass around.
--
-- You should use 'namesPerspectiveForRootAndPath' to construct this type.
--
-- E.g. if we're in loose code, we need to search the correct name lookup for the
-- user's perspective. If their perspective is "myprojects.json.latest.lib.base.data.List",
-- we need to search names using the name index mounted at "myprojects.json.latest.lib.base".
--
-- The NamesPerspective representing this viewpoint would be:
--
-- @@
-- NamesPerspective
--  { nameLookupBranchHashId = #libbasehash
--  , pathToMountedNameLookup = ["myprojects.json", "latest", "lib", "base"]
--  , relativePerspective = ["data", "List"]
--  }
-- @@
data NamesPerspective = NamesPerspective
  { -- | The branch hash of the name lookup we'll use for queries
    nameLookupBranchHashId :: Db.BranchHashId,
    -- | Where the name lookup is mounted relative to the root branch
    pathToMountedNameLookup :: PathSegments,
    -- | The path to the perspective relative to the current name lookup
    relativePerspective :: PathSegments
  }
  deriving (Eq, Show)

-- | Determine which nameLookup is the closest parent of the provided perspective.
--
-- Returns (rootBranchId of the closest parent index, namespace that index is mounted at, location of the perspective within the mounted namespace)
--
-- E.g.
-- If your namespace is "lib.distributed.lib.base.data.List", you'd get back
-- (rootBranchId of the lib.distributed.lib.base name lookup, "lib.distributed.lib.base", "data.List")
--
-- Or if your namespace is "subnamespace.user", you'd get back
-- (the rootBranchId you provided, "", "subnamespace.user")
namesPerspectiveForRootAndPath :: BranchHash -> PathSegments -> Transaction NamesPerspective
namesPerspectiveForRootAndPath rootBh namespace = do
  rootBhId <- Q.expectBranchHashId rootBh
  namesPerspectiveForRootAndPathHelper rootBhId namespace
  where
    namesPerspectiveForRootAndPathHelper :: Db.BranchHashId -> PathSegments -> Transaction NamesPerspective
    namesPerspectiveForRootAndPathHelper rootBhId pathSegments = do
      let defaultPerspective =
            NamesPerspective
              { nameLookupBranchHashId = rootBhId,
                pathToMountedNameLookup = (PathSegments []),
                relativePerspective = pathSegments
              }
      fmap (fromMaybe defaultPerspective) . runMaybeT $
        do
          mounts <- lift $ Q.listNameLookupMounts rootBhId
          mounts
            & altMap \(mountPathSegments, mountBranchHash) -> do
              case List.splitOnLongestCommonPrefix (into @[Text] pathSegments) (into @[Text] mountPathSegments) of
                -- The path is within this mount:
                (_, remainingPath, []) ->
                  lift $
                    namesPerspectiveForRootAndPathHelper mountBranchHash (into @PathSegments remainingPath)
                      <&> \(NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup = mountLocation, relativePerspective}) ->
                        NamesPerspective
                          { nameLookupBranchHashId,
                            -- Ensure we return the correct mount location even if the mount is
                            -- several levels deep
                            pathToMountedNameLookup = mountPathSegments <> mountLocation,
                            relativePerspective
                          }
                -- The path is not within this mount:
                _ -> empty

-- | Check whether we've already got an index for a given branch hash.
checkBranchHashNameLookupExists :: BranchHash -> Transaction Bool
checkBranchHashNameLookupExists bh = do
  bhId <- Q.expectBranchHashId bh
  Q.checkBranchHashNameLookupExists bhId

data NamesInPerspective = NamesInPerspective
  { termNamesInPerspective :: [S.NamedRef (C.Referent, Maybe C.ConstructorType)],
    typeNamesInPerspective :: [S.NamedRef C.Reference]
  }

-- | Get all the term and type names for the given namespace from the lookup table.
-- Requires that an index for this branch hash already exists, which is currently
-- only true on Share.
allNamesInPerspective ::
  NamesPerspective ->
  Transaction NamesInPerspective
allNamesInPerspective NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} = do
  termNamesInPerspective <- Q.termNamesWithinNamespace nameLookupBranchHashId mempty
  typeNamesInPerspective <- Q.typeNamesWithinNamespace nameLookupBranchHashId mempty
  let convertTerms = prefixNamedRef pathToMountedNameLookup . fmap (bimap s2cTextReferent (fmap s2cConstructorType))
  let convertTypes = prefixNamedRef pathToMountedNameLookup . fmap s2cTextReference
  pure $
    NamesInPerspective
      { termNamesInPerspective = convertTerms <$> termNamesInPerspective,
        typeNamesInPerspective = convertTypes <$> typeNamesInPerspective
      }

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of a names for a given Referent.
termNamesForRefWithinNamespace :: NamesPerspective -> C.Referent -> Maybe S.ReversedName -> Transaction [S.ReversedName]
termNamesForRefWithinNamespace NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} ref maySuffix = do
  Q.termNamesForRefWithinNamespace nameLookupBranchHashId mempty (c2sTextReferent ref) maySuffix
    <&> fmap (prefixReversedName pathToMountedNameLookup)

-- | NOTE: requires that the codebase has an up-to-date name lookup index. As of writing, this
-- is only true on Share.
--
-- Get the list of a names for a given Reference, with an optional required suffix.
typeNamesForRefWithinNamespace :: NamesPerspective -> C.Reference -> Maybe S.ReversedName -> Transaction [S.ReversedName]
typeNamesForRefWithinNamespace NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} ref maySuffix = do
  Q.typeNamesForRefWithinNamespace nameLookupBranchHashId mempty (c2sTextReference ref) maySuffix
    <&> fmap (prefixReversedName pathToMountedNameLookup)

termNamesBySuffix :: NamesPerspective -> S.ReversedName -> Transaction [S.NamedRef (C.Referent, Maybe C.ConstructorType)]
termNamesBySuffix NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} suffix = do
  Q.termNamesBySuffix nameLookupBranchHashId mempty suffix
    <&> fmap (prefixNamedRef pathToMountedNameLookup >>> fmap (bimap s2cTextReferent (fmap s2cConstructorType)))

typeNamesBySuffix :: NamesPerspective -> S.ReversedName -> Transaction [S.NamedRef C.Reference]
typeNamesBySuffix NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} suffix = do
  Q.typeNamesBySuffix nameLookupBranchHashId mempty suffix
    <&> fmap (prefixNamedRef pathToMountedNameLookup >>> fmap s2cTextReference)

-- | Helper for findings refs by name within the correct mounted indexes.
refsForExactName ::
  (Db.BranchHashId -> S.ReversedName -> Transaction [S.NamedRef ref]) ->
  NamesPerspective ->
  S.ReversedName ->
  Transaction [S.NamedRef ref]
refsForExactName query NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} name = do
  namedRefs <- query nameLookupBranchHashId name
  pure $
    namedRefs
      <&> prefixNamedRef pathToMountedNameLookup

-- | Requalifies a NamedRef to some namespace prefix.
prefixNamedRef :: NameLookups.PathSegments -> S.NamedRef ref -> S.NamedRef ref
prefixNamedRef prefix S.NamedRef {reversedSegments, ref} =
  S.NamedRef {reversedSegments = prefixReversedName prefix reversedSegments, ref}

-- | Requalifies a ReversedName to some namespace prefix.
prefixReversedName :: PathSegments -> S.ReversedName -> S.ReversedName
prefixReversedName (S.PathSegments prefix) (S.ReversedName reversedSegments) =
  S.ReversedName $ NonEmpty.appendl reversedSegments (reverse prefix)

termRefsForExactName :: NamesPerspective -> S.ReversedName -> Transaction [S.NamedRef (C.Referent, Maybe C.ConstructorType)]
termRefsForExactName namesPerspective reversedName = do
  refsForExactName Q.termRefsForExactName namesPerspective reversedName
    <&> fmap (fmap (bimap s2cTextReferent (fmap s2cConstructorType)))

typeRefsForExactName :: NamesPerspective -> S.ReversedName -> Transaction [S.NamedRef C.Reference]
typeRefsForExactName namesPerspective reversedName = do
  refsForExactName Q.typeRefsForExactName namesPerspective reversedName <&> fmap (fmap s2cTextReference)

-- | Get the name within the provided namespace that has the longest matching suffix
-- with the provided name, but a different ref.
-- This is a bit of a hack but allows us to shortcut suffixification.
-- We can clean this up if we make a custom PPE type just for sqlite pretty printing, but
-- for now this works fine.
longestMatchingTermNameForSuffixification :: NamesPerspective -> S.NamedRef C.Referent -> Transaction (Maybe (S.NamedRef (C.Referent, Maybe C.ConstructorType)))
longestMatchingTermNameForSuffixification NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} namedRef = do
  Q.longestMatchingTermNameForSuffixification nameLookupBranchHashId mempty (c2sTextReferent <$> namedRef)
    <&> fmap (prefixNamedRef pathToMountedNameLookup >>> fmap (bimap s2cTextReferent (fmap s2cConstructorType)))

-- | Get the name within the provided namespace that has the longest matching suffix
-- with the provided name, but a different ref.
-- This is a bit of a hack but allows us to shortcut suffixification.
-- We can clean this up if we make a custom PPE type just for sqlite pretty printing, but
-- for now this works fine.
longestMatchingTypeNameForSuffixification :: NamesPerspective -> S.NamedRef C.Reference -> Transaction (Maybe (S.NamedRef C.Reference))
longestMatchingTypeNameForSuffixification NamesPerspective {nameLookupBranchHashId, pathToMountedNameLookup} namedRef = do
  Q.longestMatchingTypeNameForSuffixification nameLookupBranchHashId mempty (c2sTextReference <$> namedRef)
    <&> fmap (prefixNamedRef pathToMountedNameLookup >>> fmap s2cTextReference)

-- | Searches all dependencies transitively looking for the provided ref within the
-- provided namespace.
-- Prefer 'termNamesForRefWithinNamespace' in most cases.
-- This is slower and only necessary when resolving the name of refs when you don't know which
-- dependency it may exist in.
--
-- Searching transitive dependencies is exponential so we want to replace this with a more
-- efficient approach as soon as possible.
--
-- Note: this returns the first name it finds by searching in order of:
-- Names in the current namespace, then names in the current namespace's dependencies, then
-- through the current namespace's dependencies' dependencies, etc.
recursiveTermNameSearch :: NamesPerspective -> C.Referent -> Transaction (Maybe S.ReversedName)
recursiveTermNameSearch NamesPerspective {nameLookupBranchHashId} r = do
  Q.recursiveTermNameSearch nameLookupBranchHashId (c2sTextReferent r)

-- | Searches all dependencies transitively looking for the provided ref within the provided
-- namespace.
-- Prefer 'typeNamesForRefWithinNamespace' in most cases.
-- This is slower and only necessary when resolving the name of references when you don't know which
-- dependency it may exist in.
--
-- Searching transitive dependencies is exponential so we want to replace this with a more
-- efficient approach as soon as possible.
--
-- Note: this returns the first name it finds by searching in order of:
-- Names in the current namespace, then names in the current namespace's dependencies, then
-- through the current namespace's dependencies' dependencies, etc.
recursiveTypeNameSearch :: NamesPerspective -> C.Reference -> Transaction (Maybe S.ReversedName)
recursiveTypeNameSearch NamesPerspective {nameLookupBranchHashId} r = do
  Q.recursiveTypeNameSearch nameLookupBranchHashId (c2sTextReference r)

-- | Looks up statistics for a given branch, if none exist, we compute them and save them
-- then return them.
expectNamespaceStatsByHash :: BranchHash -> Transaction C.Branch.NamespaceStats
expectNamespaceStatsByHash bh = do
  bhId <- Q.expectBranchHashId bh
  expectNamespaceStatsByHashId bhId

-- | Looks up statistics for a given branch, if none exist, we compute them and save them
-- then return them.
expectNamespaceStatsByHashId :: Db.BranchHashId -> Transaction C.Branch.NamespaceStats
expectNamespaceStatsByHashId bhId = do
  Q.loadNamespaceStatsByHashId bhId `whenNothingM` do
    boId <- Db.BranchObjectId <$> Q.expectObjectIdForPrimaryHashId (Db.unBranchHashId bhId)
    dbBranch <- expectDbBranch boId
    stats <- namespaceStatsForDbBranch (DbBranchV2 dbBranch)
    Q.saveNamespaceStats bhId stats
    pure stats

namespaceStatsForDbBranch :: DbBranchV -> Transaction NamespaceStats
namespaceStatsForDbBranch = \case
  DbBranchV2 S.Branch {terms, types, patches, children} -> do
    let myStats =
          NamespaceStats
            { numContainedTerms = lengthOf (folded . folded) terms,
              numContainedTypes = lengthOf (folded . folded) types,
              numContainedPatches = Map.size patches
            }
    childrenStats <- getChildrenStats children
    pure (myStats <> childrenStats)
  DbBranchV3 S.BranchV3 {children, terms, types} -> do
    let myStats =
          NamespaceStats
            { numContainedTerms = Map.size terms,
              numContainedTypes = Map.size types,
              numContainedPatches = 0
            }
    childrenStats <- getChildrenStats children
    pure (myStats <> childrenStats)
  where
    getChildrenStats :: Map Db.TextId (Db.BranchObjectId, Db.CausalHashId) -> Transaction NamespaceStats
    getChildrenStats =
      foldMapM \(_boId, chId) -> do
        bhId <- Q.expectCausalValueHashId chId
        expectNamespaceStatsByHashId bhId

-- | Gets the specified number of reflog entries in chronological order, most recent first.
getDeprecatedRootReflog :: Int -> Transaction [Reflog.Entry CausalHash Text]
getDeprecatedRootReflog numEntries = do
  entries <- Q.getDeprecatedRootReflog numEntries
  traverse (bitraverse Q.expectCausalHash pure) entries

-- | Gets the specified number of reflog entries for the given project in chronological order, most recent first.
getProjectReflog :: Int -> Db.ProjectId -> Transaction [ProjectReflog.Entry Project ProjectBranch CausalHash]
getProjectReflog numEntries projectId = do
  entries <- Q.getProjectReflog numEntries projectId
  traverse hydrateProjectReflogEntry entries

-- | Gets the specified number of reflog entries for the specified ProjectBranch in chronological order, most recent first.
getProjectBranchReflog :: Int -> Db.ProjectBranchId -> Transaction [ProjectReflog.Entry Project ProjectBranch CausalHash]
getProjectBranchReflog numEntries projectBranchId = do
  entries <- Q.getProjectBranchReflog numEntries projectBranchId
  traverse hydrateProjectReflogEntry entries

-- | Gets the specified number of reflog entries in chronological order, most recent first.
getGlobalReflog :: Int -> Transaction [ProjectReflog.Entry Project ProjectBranch CausalHash]
getGlobalReflog numEntries = do
  entries <- Q.getGlobalReflog numEntries
  traverse hydrateProjectReflogEntry entries

hydrateProjectReflogEntry :: ProjectReflog.Entry Db.ProjectId Db.ProjectBranchId Db.CausalHashId -> Transaction (ProjectReflog.Entry Project ProjectBranch CausalHash)
hydrateProjectReflogEntry entry = do
  traverse Q.expectCausalHash entry
    >>= ProjectReflog.projectAndBranch_
      %%~ ( \(projId, branchId) -> do
              proj <- Q.expectProject projId
              branch <- Q.expectProjectBranch projId branchId
              pure (proj, branch)
          )

appendProjectReflog :: ProjectReflog.Entry Db.ProjectId Db.ProjectBranchId CausalHash -> Transaction ()
appendProjectReflog entry = do
  dbEntry <- traverse Q.saveCausalHash entry
  Q.appendProjectBranchReflog dbEntry

-- | Delete any name lookup that's not in the provided list.
--
-- This can be used to garbage collect unreachable name lookups.
deleteNameLookupsExceptFor :: Set BranchHash -> Transaction ()
deleteNameLookupsExceptFor reachable = do
  bhIds <- for (Set.toList reachable) Q.expectBranchHashId
  Q.deleteNameLookupsExceptFor bhIds

-- | Get the causal hash which would be the result of squashing the provided branch hash.
-- Returns Nothing if we haven't computed it before.
tryGetSquashResult :: BranchHash -> Transaction (Maybe CausalHash)
tryGetSquashResult bh = do
  bhId <- Q.expectBranchHashId bh
  chId <- Q.tryGetSquashResult bhId
  traverse Q.expectCausalHash chId

-- | Saves the result of a squash
saveSquashResult :: BranchHash -> CausalHash -> Transaction ()
saveSquashResult bh ch = do
  bhId <- Q.expectBranchHashId bh
  chId <- Q.saveCausalHash ch
  Q.saveSquashResult bhId chId

-- | Search for term or type names which contain the provided list of segments in order.
-- Search is case insensitive.
fuzzySearchDefinitions ::
  Bool ->
  NamesPerspective ->
  -- | Will return at most n terms and n types; i.e. max number of results is 2n
  Int ->
  [Text] ->
  Transaction ([S.NamedRef (C.Referent, Maybe C.ConstructorType)], [S.NamedRef C.Reference])
fuzzySearchDefinitions includeDependencies NamesPerspective {nameLookupBranchHashId, relativePerspective} limit querySegments = do
  termNames <-
    Q.fuzzySearchTerms includeDependencies nameLookupBranchHashId limit relativePerspective querySegments
      <&> fmap \termName ->
        termName
          & (fmap (bimap s2cTextReferent (fmap s2cConstructorType)))
          & stripPrefixFromNamedRef relativePerspective
  typeNames <-
    Q.fuzzySearchTypes includeDependencies nameLookupBranchHashId limit relativePerspective querySegments
      <&> fmap (fmap s2cTextReference)
      <&> fmap \typeName ->
        typeName
          & stripPrefixFromNamedRef relativePerspective
  pure (termNames, typeNames)

-- | Strips a prefix path from a named ref. No-op if the prefix doesn't match.
--
-- >>> stripPrefixFromNamedRef (PathSegments ["foo", "bar"]) (S.NamedRef (S.ReversedName ("baz" NonEmpty.:| ["bar", "foo"])) ())
-- NamedRef {reversedSegments = ReversedName ("baz" :| []), ref = ()}
--
-- >>> stripPrefixFromNamedRef (PathSegments ["no", "match"]) (S.NamedRef (S.ReversedName ("baz" NonEmpty.:| ["bar", "foo"])) ())
-- NamedRef {reversedSegments = ReversedName ("baz" :| ["bar","foo"]), ref = ()}
stripPrefixFromNamedRef :: PathSegments -> S.NamedRef r -> S.NamedRef r
stripPrefixFromNamedRef (PathSegments prefix) namedRef =
  let newReversedName =
        S.reversedSegments namedRef
          & \case
            reversedName@(S.ReversedName (name NonEmpty.:| reversedPath)) ->
              case List.stripSuffix (reverse prefix) reversedPath of
                Nothing -> reversedName
                Just strippedReversedPath -> S.ReversedName (name NonEmpty.:| strippedReversedPath)
   in namedRef {S.reversedSegments = newReversedName}

expectProjectAndBranchNames :: Db.ProjectId -> Db.ProjectBranchId -> Transaction (ProjectName, ProjectBranchName)
expectProjectAndBranchNames projectId projectBranchId = do
  Project {name = pName} <- Q.expectProject projectId
  ProjectBranch {name = bName} <- Q.expectProjectBranch projectId projectBranchId
  pure (pName, bName)

expectProjectBranchHead :: Db.ProjectId -> Db.ProjectBranchId -> Transaction CausalHash
expectProjectBranchHead projId projectBranchId = do
  chId <- Q.expectProjectBranchHead projId projectBranchId
  Q.expectCausalHash chId
