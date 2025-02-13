{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains sqlite-specific operations on high-level "parser-typechecker" types all in the Transaction
-- monad.
--
-- The Codebase record-of-functions wraps this functionality, and runs each transaction to IO, so that the operations'
-- are unified with non-sqlite operations in the Codebase interface, like 'appendReflog'.
module Unison.Codebase.SqliteCodebase.Operations where

import Control.Comonad.Cofree qualified as Cofree
import Data.Bitraversable (bitraverse)
import Data.Either.Extra ()
import Data.Functor.Compose (Compose (..))
import Data.List qualified as List
import Data.List.NonEmpty.Extra (NonEmpty ((:|)), maximum1)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.UUID.V4 qualified as UUID
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Branch.Diff (TreeDiff (TreeDiff))
import U.Codebase.Branch.Diff qualified as BranchDiff
import U.Codebase.HashTags (BranchHash, CausalHash (unCausalHash), PatchHash)
import U.Codebase.Projects qualified as Projects
import U.Codebase.Reference qualified as C.Reference
import U.Codebase.Referent qualified as C.Referent
import U.Codebase.Sqlite.DbId (ObjectId)
import U.Codebase.Sqlite.DbId qualified as Db
import U.Codebase.Sqlite.NameLookups (PathSegments (..), ReversedName (..))
import U.Codebase.Sqlite.NamedRef qualified as S
import U.Codebase.Sqlite.ObjectType qualified as OT
import U.Codebase.Sqlite.Operations (NamesInPerspective (..))
import U.Codebase.Sqlite.Operations qualified as Ops
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.Project qualified as Project
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Builtin qualified as Builtins
import Unison.Codebase.Branch (Branch (..))
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.SqliteCodebase.Branch.Cache (BranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.Hash (Hash)
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Names (Names (Names))
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Sqlite.Transaction qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Util.Cache qualified as Cache
import Unison.Util.Relation qualified as Rel
import Unison.Util.Set qualified as Set
import Unison.WatchKind qualified as UF
import UnliftIO.STM

createSchema :: Transaction ()
createSchema = do
  Q.runCreateSql
  Q.addTempEntityTables
  Q.addNamespaceStatsTables
  Q.addReflogTable
  Q.fixScopedNameLookupTables
  Q.addProjectTables
  Q.addMostRecentBranchTable
  Q.addNameLookupMountTables
  Q.addMostRecentNamespaceTable
  Sqlite.execute insertSchemaVersionSql
  Q.addSquashResultTable
  Q.addCurrentProjectPathTable
  Q.addProjectBranchReflogTable
  Q.addProjectBranchCausalHashIdColumn
  Q.addProjectBranchLastAccessedColumn
  (_, emptyCausalHashId) <- emptyCausalHash
  (_, ProjectBranch {projectId, branchId}) <- insertProjectAndBranch scratchProjectName scratchBranchName emptyCausalHashId
  Q.setCurrentProjectPath projectId branchId []
  Q.trackLatestRemoteHead
  where
    scratchProjectName = UnsafeProjectName "scratch"
    scratchBranchName = UnsafeProjectBranchName "main"
    currentSchemaVersion = Q.currentSchemaVersion
    insertSchemaVersionSql =
      [Sqlite.sql|
        INSERT INTO schema_version (version)
        VALUES (:currentSchemaVersion)
      |]

------------------------------------------------------------------------------------------------------------------------
-- Buffer entry

-- 1) buffer up the component
-- 2) in the event that the component is complete, then what?
--  * can write component provided all of its dependency components are complete.
--    if dependency not complete,
--    register yourself to be written when that dependency is complete

-- an entry for a single hash
data BufferEntry a = BufferEntry
  { -- First, you are waiting for the cycle to fill up with all elements
    -- Then, you check: are all dependencies of the cycle in the db?
    --   If yes: write yourself to database and trigger check of dependents
    --   If no: just wait, do nothing
    beComponentTargetSize :: Maybe Word64,
    beComponent :: Map Reference.Pos a,
    beMissingDependencies :: Set Hash,
    beWaitingDependents :: Set Hash
  }
  deriving (Eq, Show)

prettyBufferEntry :: (Show a) => Hash -> BufferEntry a -> String
prettyBufferEntry (h :: Hash) BufferEntry {..} =
  "BufferEntry "
    ++ show h
    ++ "\n"
    ++ "  { beComponentTargetSize = "
    ++ show beComponentTargetSize
    ++ "\n"
    ++ "  , beComponent = "
    ++ if Map.size beComponent < 2
      then show $ Map.toList beComponent
      else
        mkString (Map.toList beComponent) (Just "\n      [ ") "      , " (Just "]\n")
          ++ "  , beMissingDependencies ="
          ++ if Set.size beMissingDependencies < 2
            then show $ Set.toList beMissingDependencies
            else
              mkString (Set.toList beMissingDependencies) (Just "\n      [ ") "      , " (Just "]\n")
                ++ "  , beWaitingDependents ="
                ++ if Set.size beWaitingDependents < 2
                  then show $ Set.toList beWaitingDependents
                  else
                    mkString (Set.toList beWaitingDependents) (Just "\n      [ ") "      , " (Just "]\n")
                      ++ "  }"
  where
    mkString :: (Foldable f, Show a) => f a -> Maybe String -> String -> Maybe String -> String
    mkString as start middle end =
      fromMaybe "" start ++ List.intercalate middle (show <$> toList as) ++ fromMaybe "" end

type TermBufferEntry = BufferEntry (Term Symbol Ann, Type Symbol Ann)

type DeclBufferEntry = BufferEntry (Decl Symbol Ann)

getBuffer :: TVar (Map Hash (BufferEntry a)) -> Hash -> IO (BufferEntry a)
getBuffer tv h = do
  (Map.lookup h <$> readTVarIO tv) <&> \case
    Just e -> e
    Nothing -> BufferEntry Nothing Map.empty Set.empty Set.empty

putBuffer :: TVar (Map Hash (BufferEntry a)) -> Hash -> BufferEntry a -> IO ()
putBuffer tv h e =
  atomically $ modifyTVar' tv (Map.insert h e)

removeBuffer :: TVar (Map Hash (BufferEntry a)) -> Hash -> IO ()
removeBuffer tv h =
  atomically $ modifyTVar' tv (Map.delete h)

addBufferDependent :: Hash -> TVar (Map Hash (BufferEntry a)) -> Hash -> IO ()
addBufferDependent dependent tv dependency = do
  be <- getBuffer tv dependency
  putBuffer tv dependency be {beWaitingDependents = Set.insert dependent $ beWaitingDependents be}

tryFlushBuffer ::
  forall a.
  (Show a) =>
  TVar (Map Hash (BufferEntry a)) ->
  (Hash -> [a] -> Transaction ()) ->
  (Hash -> Transaction ()) ->
  Hash ->
  Transaction ()
tryFlushBuffer buf saveComponent tryWaiting h =
  -- skip if it has already been flushed
  unlessM (Ops.objectExistsForHash h) do
    BufferEntry size comp (Set.delete h -> missing) waiting <- Sqlite.unsafeIO (getBuffer buf h)
    case size of
      Just size -> do
        missing' <- filterM (fmap not . Ops.objectExistsForHash) (toList missing)
        if null missing' && size == fromIntegral (length comp)
          then do
            saveComponent h (toList comp)
            Sqlite.unsafeIO (removeBuffer buf h)
            traverse_ tryWaiting waiting
          else Sqlite.unsafeIO do
            putBuffer buf h $
              BufferEntry (Just size) comp (Set.fromList missing') waiting
      Nothing ->
        -- it's never even been added, so there's nothing to do.
        pure ()

------------------------------------------------------------------------------------------------------------------------
-- Operations

getTerm ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Reference.Id ->
  Transaction (Maybe (Term Symbol Ann))
getTerm doGetDeclType (Reference.Id h i) =
  runMaybeT do
    term2 <- Ops.loadTermByReference (C.Reference.Id h i)
    lift (Cv.term2to1 h doGetDeclType term2)

getDeclType :: C.Reference.Reference -> Transaction CT.ConstructorType
getDeclType = \case
  C.Reference.ReferenceBuiltin t ->
    let err =
          error $
            "I don't know about the builtin type ##"
              ++ show t
              ++ ", but I've been asked for it's ConstructorType."
     in pure . fromMaybe err $
          Map.lookup (Reference.Builtin t) Builtins.builtinConstructorType
  C.Reference.ReferenceDerived i -> expectDeclTypeById i

expectDeclTypeById :: C.Reference.Id -> Transaction CT.ConstructorType
expectDeclTypeById = fmap Cv.decltype2to1 . Ops.expectDeclTypeById

getTypeOfTermImpl :: Reference.Id -> Transaction (Maybe (Type Symbol Ann))
getTypeOfTermImpl (Reference.Id h i) =
  runMaybeT do
    type2 <- Ops.loadTypeOfTermByTermReference (C.Reference.Id h i)
    pure (Cv.ttype2to1 type2)

getTermComponentWithTypes ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Hash ->
  Transaction (Maybe [(Term Symbol Ann, Type Symbol Ann)])
getTermComponentWithTypes doGetDeclType h =
  runMaybeT do
    tms <- Ops.loadTermComponent h
    for tms (bitraverse (lift . Cv.term2to1 h doGetDeclType) (pure . Cv.ttype2to1))

getTypeDeclaration :: Reference.Id -> Transaction (Maybe (Decl Symbol Ann))
getTypeDeclaration (Reference.Id h i) =
  runMaybeT do
    decl2 <- Ops.loadDeclByReference (C.Reference.Id h i)
    pure (Cv.decl2to1 h decl2)

getDeclComponent :: Hash -> Transaction (Maybe [Decl Symbol Ann])
getDeclComponent h =
  runMaybeT do
    decl2 <- Ops.loadDeclComponent h
    pure (map (Cv.decl2to1 h) decl2)

-- | Like 'getDeclComponent', for when the decl component is known to exist in the codebase.
expectDeclComponent :: (HasCallStack) => Hash -> Transaction [Decl Symbol Ann]
expectDeclComponent hash =
  getDeclComponent hash <&> \case
    Nothing -> error (reportBug "E101611" ("decl component " ++ show hash ++ " not found"))
    Just decls -> decls

putTermComponent ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  -- | The hash of the term component.
  Hash ->
  [(Term Symbol Ann, Type Symbol Ann)] ->
  Transaction ()
putTermComponent termBuffer declBuffer h component =
  unlessM (Ops.objectExistsForHash h) do
    for_ (Reference.componentFor h component) \(ref, (tm, tp)) -> do
      putTerm_ termBuffer declBuffer ref tm tp
      tryFlushTermBuffer termBuffer h

putTerm ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Reference.Id ->
  Term Symbol Ann ->
  Type Symbol Ann ->
  Transaction ()
putTerm termBuffer declBuffer ref@(Reference.Id h _) tm tp =
  unlessM (Ops.objectExistsForHash h) do
    putTerm_ termBuffer declBuffer ref tm tp
    tryFlushTermBuffer termBuffer h

putTerm_ ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Reference.Id ->
  Term Symbol Ann ->
  Type Symbol Ann ->
  Transaction ()
putTerm_ termBuffer declBuffer (Reference.Id h i) tm tp = do
  BufferEntry size comp missing waiting <- Sqlite.unsafeIO (getBuffer termBuffer h)
  let termDependencies = Set.toList $ Term.termDependencies tm
  -- update the component target size if we encounter any higher self-references
  let size' = max size (Just $ biggestSelfReference + 1)
        where
          biggestSelfReference =
            maximum1 $
              i :| [i' | Reference.Derived h' i' <- termDependencies, h == h']
  let comp' = Map.insert i (tm, tp) comp
  -- for the component element that's been passed in, add its dependencies to missing'
  missingTerms' <-
    filterM
      (fmap not . Ops.objectExistsForHash)
      [h | Reference.Derived h _i <- termDependencies]
  missingTypes' <-
    filterM (fmap not . Ops.objectExistsForHash) $
      [h | Reference.Derived h _i <- Set.toList $ Term.typeDependencies tm]
        ++ [h | Reference.Derived h _i <- Set.toList $ Type.dependencies tp]
  let missing' = missing <> Set.fromList (missingTerms' <> missingTypes')
  Sqlite.unsafeIO do
    -- notify each of the dependencies that h depends on them.
    traverse_ (addBufferDependent h termBuffer) missingTerms'
    traverse_ (addBufferDependent h declBuffer) missingTypes'
    putBuffer termBuffer h (BufferEntry size' comp' missing' waiting)

tryFlushTermBuffer :: TVar (Map Hash TermBufferEntry) -> Hash -> Transaction ()
tryFlushTermBuffer termBuffer =
  let loop h =
        tryFlushBuffer
          termBuffer
          (\h2 component -> void $ Q.saveTermComponent v2HashHandle Nothing h2 (Cv.termComponent1to2 h component))
          loop
          h
   in loop

addDeclComponentTypeIndex :: ObjectId -> [[Type Symbol Ann]] -> Transaction ()
addDeclComponentTypeIndex oId ctorss =
  for_ (ctorss `zip` [0 ..]) \(ctors, i) ->
    for_ (ctors `zip` [0 ..]) \(tp, j) -> do
      let self = C.Referent.ConId (C.Reference.Id oId i) j
          typeForIndexing = Hashing.typeToReference tp
          typeMentionsForIndexing = Hashing.typeToReferenceMentions tp
      Ops.addTypeToIndexForTerm self (Cv.reference1to2 typeForIndexing)
      Ops.addTypeMentionsToIndexForTerm self (Set.map Cv.reference1to2 typeMentionsForIndexing)

putTypeDeclarationComponent ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Hash ->
  [Decl Symbol Ann] ->
  Transaction ()
putTypeDeclarationComponent termBuffer declBuffer h decls =
  unlessM (Ops.objectExistsForHash h) do
    for_ (Reference.componentFor h decls) \(ref, decl) ->
      putTypeDeclaration_ declBuffer ref decl
    tryFlushDeclBuffer termBuffer declBuffer h

putTypeDeclaration ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Reference.Id ->
  Decl Symbol Ann ->
  Transaction ()
putTypeDeclaration termBuffer declBuffer ref@(Reference.Id h _) decl = do
  unlessM (Ops.objectExistsForHash h) do
    putTypeDeclaration_ declBuffer ref decl
    tryFlushDeclBuffer termBuffer declBuffer h

putTypeDeclaration_ ::
  TVar (Map Hash DeclBufferEntry) ->
  Reference.Id ->
  Decl Symbol Ann ->
  Transaction ()
putTypeDeclaration_ declBuffer (Reference.Id h i) decl = do
  BufferEntry size comp missing waiting <- Sqlite.unsafeIO (getBuffer declBuffer h)
  let declDependencies = Set.toList $ Decl.declTypeDependencies decl
  let size' = max size (Just $ biggestSelfReference + 1)
        where
          biggestSelfReference =
            maximum1 $
              i :| [i' | Reference.Derived h' i' <- declDependencies, h == h']
  let comp' = Map.insert i decl comp
  moreMissing <-
    filterM (fmap not . Ops.objectExistsForHash) $
      [h | Reference.Derived h _i <- declDependencies]
  let missing' = missing <> Set.fromList moreMissing
  Sqlite.unsafeIO do
    traverse_ (addBufferDependent h declBuffer) moreMissing
    putBuffer declBuffer h (BufferEntry size' comp' missing' waiting)

tryFlushDeclBuffer ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Hash ->
  Transaction ()
tryFlushDeclBuffer termBuffer declBuffer =
  let loop h =
        tryFlushBuffer
          declBuffer
          ( \h2 component ->
              void $
                Q.saveDeclComponent
                  v2HashHandle
                  Nothing
                  h2
                  (fmap (Cv.decl1to2 h) component)
          )
          (\h -> tryFlushTermBuffer termBuffer h >> loop h)
          h
   in loop

-- if this blows up on cromulent hashes, then switch from `hashToHashId`
-- to one that returns Maybe.
getBranchForHash ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  BranchCache Sqlite.Transaction ->
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  CausalHash ->
  Transaction (Maybe (Branch Transaction))
getBranchForHash branchCache doGetDeclType h = do
  Ops.loadCausalBranchByCausalHash h >>= \case
    Nothing -> pure Nothing
    Just causal2 -> do
      branch1 <- Cv.causalbranch2to1 branchCache doGetDeclType causal2
      pure (Just branch1)

putBranch :: Branch Transaction -> Transaction ()
putBranch =
  void . Ops.saveBranch v2HashHandle . Cv.causalbranch1to2

-- | Check whether the given branch exists in the codebase.
branchExists :: CausalHash -> Transaction Bool
branchExists h =
  Q.loadHashIdByHash (unCausalHash h) >>= \case
    Nothing -> pure False
    Just hId -> Q.isCausalHash hId

getPatch :: PatchHash -> Transaction (Maybe Patch)
getPatch h =
  runMaybeT do
    patchId <- MaybeT (Q.loadPatchObjectIdForPrimaryHash h)
    patch <- lift (Ops.expectPatch patchId)
    pure (Cv.patch2to1 patch)

-- | Put a patch into the codebase.
--
-- Note that 'putBranch' may also put patches.
putPatch :: PatchHash -> Patch -> Transaction ()
putPatch h p =
  void $ Ops.savePatch v2HashHandle h (Cv.patch1to2 p)

-- | Check whether the given patch exists in the codebase.
patchExists :: PatchHash -> Transaction Bool
patchExists h = fmap isJust $ Q.loadPatchObjectIdForPrimaryHash h

dependentsImpl :: Q.DependentsSelector -> Reference -> Transaction (Set Reference.Id)
dependentsImpl selector r =
  Set.map Cv.referenceid2to1
    <$> Ops.dependents selector (Cv.reference1to2 r)

dependentsOfComponentImpl :: Hash -> Transaction (Set Reference.Id)
dependentsOfComponentImpl h =
  Set.map Cv.referenceid2to1 <$> Ops.dependentsOfComponent h

-- | @watches k@ returns all of the references @r@ that were previously put by a @putWatch k r t@. @t@ can be
-- retrieved by @getWatch k r@.
watches :: UF.WatchKind -> Transaction [Reference.Id]
watches w =
  Ops.listWatches (Cv.watchKind1to2 w) <&> fmap Cv.referenceid2to1

getWatch ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  UF.WatchKind ->
  Reference.Id ->
  Transaction (Maybe (Term Symbol Ann))
getWatch doGetDeclType k r@(Reference.Id h _i) =
  if elem k standardWatchKinds
    then runMaybeT do
      watch <- Ops.loadWatch (Cv.watchKind1to2 k) (Cv.referenceid1to2 r)
      lift (Cv.term2to1 h doGetDeclType watch)
    else pure Nothing

-- | @putWatch k r t@ puts a watch of kind @k@, with hash-of-expression @r@ and decompiled result @t@ into the
-- codebase.
--
-- For example, in the watch expression below, @k@ is 'WK.Regular', @r@ is the hash of @x@, and @t@ is @7@.
--
-- @
-- > x = 3 + 4
--   ⧩
--   7
-- @
putWatch :: UF.WatchKind -> Reference.Id -> Term Symbol Ann -> Transaction ()
putWatch k r@(Reference.Id h _i) tm =
  when (elem k standardWatchKinds) do
    Ops.saveWatch
      (Cv.watchKind1to2 k)
      (Cv.referenceid1to2 r)
      (Cv.term1to2 h tm)

standardWatchKinds :: [UF.WatchKind]
standardWatchKinds = [UF.RegularWatch, UF.TestWatch]

termsOfTypeImpl ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Reference ->
  Transaction (Set Referent.Id)
termsOfTypeImpl doGetDeclType r =
  Ops.termsHavingType (Cv.reference1to2 r)
    >>= Set.traverse (Cv.referentid2to1 doGetDeclType)

termsMentioningTypeImpl ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Reference ->
  Transaction (Set Referent.Id)
termsMentioningTypeImpl doGetDeclType r =
  Ops.termsMentioningType (Cv.reference1to2 r)
    >>= Set.traverse (Cv.referentid2to1 doGetDeclType)

filterReferencesHavingTypeImpl :: Reference -> Set Reference.Id -> Transaction (Set Reference.Id)
filterReferencesHavingTypeImpl typRef termRefs =
  Ops.filterTermsByReferenceHavingType (Cv.reference1to2 typRef) (Cv.referenceid1to2 <$> toList termRefs)
    <&> fmap Cv.referenceid2to1
    <&> Set.fromList

filterReferentsHavingTypeImpl ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Reference ->
  Set Referent.Id ->
  Transaction (Set Referent.Id)
filterReferentsHavingTypeImpl doGetDeclType typRef termRefs =
  Ops.filterTermsByReferentHavingType (Cv.reference1to2 typRef) (Cv.referentid1to2 <$> toList termRefs)
    >>= traverse (Cv.referentid2to1 doGetDeclType)
    <&> Set.fromList

-- | The number of base32 characters needed to distinguish any two references in the codebase.
hashLength :: Transaction Int
hashLength = pure 10

-- | The number of base32 characters needed to distinguish any two branch in the codebase.
branchHashLength :: Transaction Int
branchHashLength = pure 10

defnReferencesByPrefix :: OT.ObjectType -> ShortHash -> Transaction (Set Reference.Id)
defnReferencesByPrefix _ (ShortHash.Builtin _) = pure mempty
defnReferencesByPrefix ot (ShortHash.ShortHash prefix cycle _cid) = do
  refs <- do
    Ops.componentReferencesByPrefix ot prefix cycle
      >>= traverse (C.Reference.idH Q.expectPrimaryHashByObjectId)
      >>= pure . Set.fromList
  pure $ Set.map Cv.referenceid2to1 refs

termReferencesByPrefix :: ShortHash -> Transaction (Set Reference.Id)
termReferencesByPrefix = defnReferencesByPrefix OT.TermComponent

-- | Get the set of type declarations whose hash matches the given prefix.
typeReferencesByPrefix :: ShortHash -> Transaction (Set Reference.Id)
typeReferencesByPrefix = defnReferencesByPrefix OT.DeclComponent

referentsByPrefix ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  ShortHash ->
  Transaction (Set Referent.Id)
referentsByPrefix _doGetDeclType ShortHash.Builtin {} = pure mempty
referentsByPrefix doGetDeclType (ShortHash.ShortHash prefix cycle cid) = do
  termReferents <-
    Ops.termReferentsByPrefix prefix cycle
      >>= traverse (Cv.referentid2to1 doGetDeclType)
  declReferents' <- Ops.declReferentsByPrefix prefix cycle cid
  let declReferents =
        [ Referent.ConId (ConstructorReference (Reference.Id h pos) (fromIntegral cid)) (Cv.decltype2to1 ct)
          | (h, pos, ct, cids) <- declReferents',
            cid <- cids
        ]
  pure . Set.fromList $ termReferents <> declReferents

-- | Get the set of branches whose hash matches the given prefix.
causalHashesByPrefix :: ShortCausalHash -> Transaction (Set CausalHash)
causalHashesByPrefix sh = do
  -- given that a Branch is shallow, it's really `CausalHash` that you'd
  -- refer to to specify a full namespace w/ history.
  -- but do we want to be able to refer to a namespace without its history?
  Ops.causalHashesByPrefix (Cv.sch1to2 sh)

-- well one or the other. :zany_face: the thinking being that they wouldn't hash-collide
termExists, declExists :: Hash -> Transaction Bool
termExists = fmap isJust . Q.loadObjectIdForPrimaryHash
declExists = termExists

-- `before b1 b2` is undefined if `b2` not in the codebase
before :: CausalHash -> CausalHash -> Transaction Bool
before h1 h2 =
  fromJust <$> Ops.before h1 h2

-- | Construct a 'ScopedNames' which can produce names which are relative to the provided
-- Path.
--
-- NOTE: this method requires an up-to-date name lookup index
namesAtPath ::
  BranchHash ->
  -- Include names from the project which contains this path.
  Path ->
  Transaction Names
namesAtPath bh path = do
  let namesRoot = PathSegments . coerce . Path.toList $ path
  namesPerspective@Ops.NamesPerspective {relativePerspective} <- Ops.namesPerspectiveForRootAndPath bh namesRoot
  let relativePath = Path.fromList $ coerce relativePerspective
  NamesInPerspective {termNamesInPerspective, typeNamesInPerspective} <- Ops.allNamesInPerspective namesPerspective
  let termsInPath = convertTerms termNamesInPerspective
  let typesInPath = convertTypes typeNamesInPerspective
  let relativeScopedNames =
        case relativePath of
          Path.Empty -> (Names {terms = Rel.fromList termsInPath, types = Rel.fromList typesInPath})
          p ->
            let reversedPathSegments = reverse . Path.toList $ p
                relativeTerms = mapMaybe (stripPathPrefix reversedPathSegments) termsInPath
                relativeTypes = mapMaybe (stripPathPrefix reversedPathSegments) typesInPath
             in (Names {terms = Rel.fromList relativeTerms, types = Rel.fromList relativeTypes})
  pure $ relativeScopedNames
  where
    convertTypes names =
      names <&> \(S.NamedRef {reversedSegments, ref}) ->
        (Name.fromReverseSegments (coerce reversedSegments), Cv.reference2to1 ref)
    convertTerms names =
      names <&> \(S.NamedRef {reversedSegments, ref = (ref, ct)}) ->
        let v1ref = Cv.referent2to1UsingCT (fromMaybe (error "Required constructor type for constructor but it was null") ct) ref
         in (Name.fromReverseSegments (coerce reversedSegments), v1ref)

    -- If the given prefix matches the given name, the prefix is stripped and it's collected
    -- on the left, otherwise it's left as-is and collected on the right.
    -- >>> stripPathPrefix ["b", "a"] ("a.b.c", ())
    -- ([(c,())])
    stripPathPrefix :: [NameSegment] -> (Name, r) -> Maybe (Name, r)
    stripPathPrefix reversedPathSegments (n, ref) =
      case Name.stripReversedPrefix n reversedPathSegments of
        Nothing -> Nothing
        Just stripped -> Just (Name.makeRelative stripped, ref)

-- | Add an index for the provided branch hash if one doesn't already exist.
ensureNameLookupForBranchHash ::
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  -- | An optional branch which we may already have an index for.
  -- This should be a branch which is relatively similar to the branch we're creating a name
  -- lookup for, e.g. a recent ancestor of the new branch. The more similar it is, the faster
  -- the less work we'll need to do.
  Maybe BranchHash ->
  BranchHash ->
  Sqlite.Transaction ()
ensureNameLookupForBranchHash getDeclType mayFromBranchHash toBranchHash = do
  Ops.checkBranchHashNameLookupExists toBranchHash >>= \case
    True -> pure ()
    False -> do
      (fromBranch, mayExistingLookupBH) <- case mayFromBranchHash of
        Nothing -> pure (V2Branch.empty, Nothing)
        Just fromBH -> do
          Ops.checkBranchHashNameLookupExists fromBH >>= \case
            True -> (,Just fromBH) <$> Ops.expectBranchByBranchHash fromBH
            False -> do
              -- TODO: We can probably infer a good starting branch by crawling through
              -- history looking for a Branch Hash we already have an index for.
              pure (V2Branch.empty, Nothing)
      toBranch <- Ops.expectBranchByBranchHash toBranchHash
      depMounts <- Projects.inferDependencyMounts toBranch <&> fmap (first (coerce @_ @PathSegments . Path.toList))
      let depMountPaths = (Path.fromList . coerce) . fst <$> depMounts
      treeDiff <- ignoreDepMounts depMountPaths <$> BranchDiff.diffBranches fromBranch toBranch
      let namePrefix = Nothing
      Ops.buildNameLookupForBranchHash
        mayExistingLookupBH
        toBranchHash
        ( \save -> do
            BranchDiff.streamNameChanges namePrefix treeDiff \_prefix (BranchDiff.NameChanges {termNameAdds, termNameRemovals, typeNameAdds, typeNameRemovals}) -> do
              termNameAddsWithCT <- do
                for termNameAdds \(name, ref) -> do
                  refWithCT <- addReferentCT ref
                  pure $ toNamedRef (name, refWithCT)
              save (termNameAddsWithCT, toNamedRef <$> termNameRemovals) (toNamedRef <$> typeNameAdds, toNamedRef <$> typeNameRemovals)
        )
      -- Ensure all of our dependencies have name lookups too.
      for_ depMounts \(_path, depBranchHash) -> do
        -- TODO: see if we can find a way to infer a good fromHash for dependencies
        ensureNameLookupForBranchHash getDeclType Nothing depBranchHash
      Ops.associateNameLookupMounts toBranchHash depMounts
  where
    alterTreeDiffAtPath :: (Functor m) => Path -> (TreeDiff m -> TreeDiff m) -> TreeDiff m -> TreeDiff m
    alterTreeDiffAtPath path f (TreeDiff cfr) =
      case path of
        Path.Empty -> f (TreeDiff cfr)
        (segment Path.:< rest) ->
          let (a Cofree.:< (Compose rest')) = cfr
           in TreeDiff (a Cofree.:< Compose (Map.adjust (fmap (coerce $ alterTreeDiffAtPath rest f)) segment rest'))
    -- Delete portions of the diff which are covered by dependency mounts.
    ignoreDepMounts :: (Applicative m) => [Path] -> TreeDiff m -> TreeDiff m
    ignoreDepMounts depMounts treeDiff =
      foldl' (\acc path -> alterTreeDiffAtPath path (const mempty) acc) treeDiff depMounts
    toNamedRef :: (Name, ref) -> S.NamedRef ref
    toNamedRef (name, ref) = S.NamedRef {reversedSegments = coerce $ Name.reverseSegments name, ref = ref}
    addReferentCT :: C.Referent.Referent -> Transaction (C.Referent.Referent, Maybe C.Referent.ConstructorType)
    addReferentCT referent = case referent of
      C.Referent.Ref {} -> pure (referent, Nothing)
      C.Referent.Con ref _conId -> do
        ct <- getDeclType ref
        pure (referent, Just $ Cv.constructorType1to2 ct)

-- | Regenerate the name lookup index for the given branch hash from scratch.
-- This shouldn't be necessary in normal operation, but it's useful to fix name lookups if
-- they somehow get corrupt, or during local testing and debugging.
regenerateNameLookup ::
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  BranchHash ->
  Sqlite.Transaction ()
regenerateNameLookup getDeclType bh = do
  Ops.checkBranchHashNameLookupExists bh >>= \case
    True -> do
      bhId <- Q.expectBranchHashId bh
      Q.deleteNameLookup bhId
      ensureNameLookupForBranchHash getDeclType Nothing bh
    False -> ensureNameLookupForBranchHash getDeclType Nothing bh

-- | Given a transaction, return a transaction that first checks a semispace cache of the given size.
--
-- The transaction should probably be read-only, as we (of course) don't hit SQLite on a cache hit.
makeCachedTransaction :: (Ord a, MonadIO m) => Word -> (a -> Sqlite.Transaction b) -> m (a -> Sqlite.Transaction b)
makeCachedTransaction size action = do
  cache <- Cache.semispaceCache size
  pure \x -> do
    conn <- Sqlite.unsafeGetConnection
    Sqlite.unsafeIO (Cache.apply cache (\x -> Sqlite.unsafeUnTransaction (action x) conn) x)

-- | Like 'makeCachedTransaction', but for when the transaction returns a Maybe; only cache the Justs.
makeMaybeCachedTransaction ::
  (Ord a, MonadIO m) =>
  Word ->
  (a -> Sqlite.Transaction (Maybe b)) ->
  m (a -> Sqlite.Transaction (Maybe b))
makeMaybeCachedTransaction size action = do
  cache <- Cache.semispaceCache size
  pure \x -> do
    conn <- Sqlite.unsafeGetConnection
    Sqlite.unsafeIO (Cache.applyDefined cache (\x -> Sqlite.unsafeUnTransaction (action x) conn) x)

-- | Creates a project by name if one doesn't already exist, creates a branch in that project, then returns the project and branch ids. Fails if a branch by that name already exists in the project.
insertProjectAndBranch :: ProjectName -> ProjectBranchName -> Db.CausalHashId -> Sqlite.Transaction (Project, ProjectBranch)
insertProjectAndBranch projectName branchName chId = do
  projectId <- whenNothingM (fmap Project.projectId <$> Q.loadProjectByName projectName) do
    projectId <- Sqlite.unsafeIO (Db.ProjectId <$> UUID.nextRandom)
    Q.insertProject projectId projectName
    pure projectId
  branchId <- Sqlite.unsafeIO (Db.ProjectBranchId <$> UUID.nextRandom)
  let projectBranch =
        ProjectBranch
          { projectId,
            branchId,
            name = branchName,
            parentBranchId = Nothing
          }
  Q.insertProjectBranch
    "Project Created"
    chId
    projectBranch
  Q.setMostRecentBranch projectId branchId
  pure (Project {name = projectName, projectId}, ProjectBranch {projectId, name = branchName, branchId, parentBranchId = Nothing})

-- | Often we need to assign something to an empty causal, this ensures the empty causal
-- exists in the codebase and returns its hash.
emptyCausalHash :: Sqlite.Transaction (CausalHash, Db.CausalHashId)
emptyCausalHash = do
  let emptyBranch = Branch.empty
  putBranch emptyBranch
  let causalHash = Branch.headHash emptyBranch
  causalHashId <- Q.expectCausalHashIdByCausalHash causalHash
  pure (causalHash, causalHashId)
