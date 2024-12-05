-- | This module contains miscellaneous helper utils for rote actions in the Cli monad, like resolving a relative path
-- to an absolute path, per the current path.
module Unison.Cli.MonadUtils
  ( -- * Paths
    getCurrentPath,
    getCurrentProjectName,
    getCurrentProjectBranchName,
    getCurrentProjectPath,
    resolvePath,
    resolvePath',
    resolvePath'ToAbsolute,
    resolveHQName,
    resolveName,

    -- * Project and branch resolution
    getCurrentProjectAndBranch,
    getCurrentProjectBranch,

    -- * Branches

    -- ** Resolving branch identifiers
    resolveAbsBranchId,
    resolveAbsBranchIdV2,
    resolveBranchId,
    resolveBranchIdToAbsBranchId,
    resolveShortCausalHash,

    -- ** Getting/setting branches
    getCurrentProjectRoot,
    getCurrentProjectRoot0,
    getCurrentBranch,
    getCurrentBranch0,
    getProjectBranchRoot,
    getBranchFromProjectPath,
    getBranch0FromProjectPath,
    getMaybeBranchFromProjectPath,
    getMaybeBranch0FromProjectPath,
    expectBranchAtPath,
    expectBranchAtPath',
    expectBranch0AtPath,
    expectBranch0AtPath',
    assertNoBranchAtPath',
    branchExistsAtPath',

    -- ** Updating branches
    stepAt',
    stepAt,
    stepAtM,
    stepManyAt,
    stepManyAtM,
    updateProjectBranchRoot,
    updateProjectBranchRoot_,
    updateAtM,
    updateAt,
    updateAndStepAt,

    -- * Terms
    getTermsAt,

    -- * Types
    getTypesAt,

    -- * Patches

    -- ** Default patch
    defaultPatchPath,

    -- ** Getting patches
    getPatchAt,

    -- * Latest touched Unison file
    getLatestFile,
    getLatestParsedFile,
    getNamesFromLatestFile,
    getTermFromLatestParsedFile,
    expectLatestFile,
    expectLatestParsedFile,
    getLatestTypecheckedFile,
    expectLatestTypecheckedFile,

    -- * Parsing env
    makeParsingEnv,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State
import Data.Bitraversable (bitraverse)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set qualified as Set
import U.Codebase.Branch qualified as V2 (Branch)
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.Project (Project)
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.UniqueTypeGuidLookup (loadUniqueTypeGuid)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch (..), Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Patch (Patch (..))
import Unison.Codebase.Patch qualified as Patch
import Unison.Codebase.Path (Path, Path' (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Syntax.Parser (ParsingEnv (..))
import Unison.Term qualified as Term
import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UFN
import Unison.Util.Set qualified as Set
import Unison.Var qualified as Var

------------------------------------------------------------------------------------------------------------------------
-- Getting paths, path resolution, etc.

getCurrentProjectPath :: Cli PP.ProjectPath
getCurrentProjectPath = do
  ppIds <- Cli.getProjectPathIds
  Cli.runTransaction $ Codebase.resolveProjectPathIds ppIds

getCurrentProjectAndBranch :: Cli (ProjectAndBranch Project ProjectBranch)
getCurrentProjectAndBranch = do
  PP.toProjectAndBranch <$> getCurrentProjectPath

getCurrentProjectBranch :: Cli ProjectBranch
getCurrentProjectBranch = do
  view #branch <$> getCurrentProjectPath

-- | Get the current path relative to the current project.
getCurrentPath :: Cli Path.Absolute
getCurrentPath = do
  view PP.absPath_ <$> getCurrentProjectPath

getCurrentProjectName :: Cli ProjectName
getCurrentProjectName = do
  view (#project . #name) <$> getCurrentProjectPath

getCurrentProjectBranchName :: Cli ProjectBranchName
getCurrentProjectBranchName = do
  view (#branch . #name) <$> getCurrentProjectPath

-- | Resolve a @Path@ (interpreted as relative) to a @Path.Absolute@, per the current path.
resolvePath :: Path -> Cli PP.ProjectPath
resolvePath path = do
  pp <- getCurrentProjectPath
  pure $ pp & PP.absPath_ %~ \p -> Path.resolve p path

-- | Resolve a @Path'@ to a @Path.Absolute@, per the current path.
resolvePath' :: Path' -> Cli PP.ProjectPath
resolvePath' path' = do
  pp <- getCurrentProjectPath
  pure $ pp & PP.absPath_ %~ \p -> Path.resolve p path'

resolvePath'ToAbsolute :: Path' -> Cli Path.Absolute
resolvePath'ToAbsolute path' = do
  view PP.absPath_ <$> resolvePath' path'

-- | Resolve a path split, per the current path.
resolveHQName :: HQ'.HashQualified Name -> Cli (PP.ProjectPath, HQ'.HashQualified NameSegment.NameSegment)
resolveHQName hq =
  let (path, seg) = Path.parentOfName $ HQ'.toName hq
   in (,seg <$ hq) <$> resolvePath' path

-- | Resolve a path split, per the current path.
resolveName :: Name -> Cli (PP.ProjectPath, NameSegment.NameSegment)
resolveName = bitraverse resolvePath' pure . Path.parentOfName

------------------------------------------------------------------------------------------------------------------------
-- Branch resolution

-- | Resolve an @AbsBranchId@ to the corresponding @Branch IO@, or fail if no such branch hash is found. (Non-existent
-- branches by path are OK - the empty branch will be returned).
resolveAbsBranchId :: Input.AbsBranchId -> Cli (Branch IO)
resolveAbsBranchId = \case
  Input.BranchAtSCH hash -> resolveShortCausalHash hash
  Input.BranchAtPath absPath -> do
    pp <- resolvePath' (Path' (Left absPath))
    getBranchFromProjectPath pp
  Input.BranchAtProjectPath pp -> getBranchFromProjectPath pp

-- | V2 version of 'resolveAbsBranchId2'.
resolveAbsBranchIdV2 ::
  (forall void. Output.Output -> Sqlite.Transaction void) ->
  ProjectAndBranch Project ProjectBranch ->
  Input.AbsBranchId ->
  Sqlite.Transaction (V2.Branch Sqlite.Transaction)
resolveAbsBranchIdV2 rollback (ProjectAndBranch proj branch) = \case
  Input.BranchAtSCH shortHash -> do
    hash <- resolveShortCausalHashToCausalHash rollback shortHash
    causal <- (Codebase.expectCausalBranchByCausalHash hash)
    V2Causal.value causal
  Input.BranchAtPath absPath -> do
    let pp = PP.ProjectPath proj branch absPath
    Codebase.getShallowBranchAtProjectPath pp
  Input.BranchAtProjectPath pp -> Codebase.getShallowBranchAtProjectPath pp

-- | Resolve a @BranchId@ to the corresponding @Branch IO@, or fail if no such branch hash is found. (Non-existent
-- branches by path are OK - the empty branch will be returned).
resolveBranchId :: Input.BranchId -> Cli (Branch IO)
resolveBranchId branchId = do
  absBranchId <- resolveBranchIdToAbsBranchId branchId
  resolveAbsBranchId absBranchId

-- | Resolve a @BranchId@ to an @AbsBranchId@.
resolveBranchIdToAbsBranchId :: Input.BranchId -> Cli Input.AbsBranchId
resolveBranchIdToAbsBranchId =
  traverse (fmap (view PP.absPath_) . resolvePath')

-- | Resolve a @ShortCausalHash@ to the corresponding @Branch IO@, or fail if no such branch hash is found.
resolveShortCausalHash :: ShortCausalHash -> Cli (Branch IO)
resolveShortCausalHash shortHash = do
  Cli.time "resolveShortCausalHash" do
    Cli.Env {codebase} <- ask
    hash <- Cli.runTransactionWithRollback \rollback -> resolveShortCausalHashToCausalHash rollback shortHash
    branch <- liftIO (Codebase.getBranchForHash codebase hash)
    pure (fromMaybe Branch.empty branch)

resolveShortCausalHashToCausalHash ::
  (forall void. Output.Output -> Sqlite.Transaction void) ->
  ShortCausalHash ->
  Sqlite.Transaction CausalHash
resolveShortCausalHashToCausalHash rollback shortHash = do
  hashes <- Codebase.causalHashesByPrefix shortHash
  Set.asSingleton hashes & onNothing do
    if Set.null hashes
      then rollback (Output.NoBranchWithHash shortHash)
      else do
        len <- Codebase.branchHashLength
        rollback (Output.BranchHashAmbiguous shortHash (Set.map (SCH.fromHash len) hashes))

------------------------------------------------------------------------------------------------------------------------
-- Getting/Setting branches

-- | Get the root branch.
getCurrentProjectRoot :: Cli (Branch IO)
getCurrentProjectRoot = do
  Cli.Env {codebase} <- ask
  ProjectAndBranch proj branch <- getCurrentProjectAndBranch
  liftIO $ Codebase.expectProjectBranchRoot codebase proj.projectId branch.branchId

-- | Get the root branch0.
getCurrentProjectRoot0 :: Cli (Branch0 IO)
getCurrentProjectRoot0 =
  Branch.head <$> getCurrentProjectRoot

-- | Get the current branch.
getCurrentBranch :: Cli (Branch IO)
getCurrentBranch = do
  Cli.Env {codebase} <- ask
  pp <- getCurrentProjectPath
  fromMaybe Branch.empty <$> liftIO (Codebase.getBranchAtProjectPath codebase pp)

-- | Get the current branch0.
getCurrentBranch0 :: Cli (Branch0 IO)
getCurrentBranch0 = do
  Branch.head <$> getCurrentBranch

-- | Get the branch at an absolute path from the project root.
getBranchFromProjectPath :: PP.ProjectPath -> Cli (Branch IO)
getBranchFromProjectPath pp =
  getMaybeBranchFromProjectPath pp <&> fromMaybe Branch.empty

-- | Get the branch0 at an absolute path.
getBranch0FromProjectPath :: PP.ProjectPath -> Cli (Branch0 IO)
getBranch0FromProjectPath pp =
  Branch.head <$> getBranchFromProjectPath pp

getProjectBranchRoot :: ProjectBranch -> Cli (Branch IO)
getProjectBranchRoot projectBranch = do
  Cli.Env {codebase} <- ask
  liftIO $ Codebase.expectProjectBranchRoot codebase projectBranch.projectId projectBranch.branchId

-- | Get the maybe-branch at an absolute path.
getMaybeBranchFromProjectPath :: PP.ProjectPath -> Cli (Maybe (Branch IO))
getMaybeBranchFromProjectPath pp = do
  Cli.Env {codebase} <- ask
  liftIO $ Codebase.getBranchAtProjectPath codebase pp

-- | Get the maybe-branch0 at an absolute path.
getMaybeBranch0FromProjectPath :: PP.ProjectPath -> Cli (Maybe (Branch0 IO))
getMaybeBranch0FromProjectPath pp =
  fmap Branch.head <$> getMaybeBranchFromProjectPath pp

-- | Get the branch at a relative path, or return early if there's no such branch.
expectBranchAtPath :: Path -> Cli (Branch IO)
expectBranchAtPath =
  expectBranchAtPath' . Path' . Right . Path.Relative

-- | Get the branch at an absolute or relative path, or return early if there's no such branch.
expectBranchAtPath' :: Path' -> Cli (Branch IO)
expectBranchAtPath' path0 = do
  path <- resolvePath' path0
  getMaybeBranchFromProjectPath path & onNothingM (Cli.returnEarly (Output.BranchNotFound path0))

-- | Get the branch0 at an absolute or relative path, or return early if there's no such branch.
expectBranch0AtPath' :: Path' -> Cli (Branch0 IO)
expectBranch0AtPath' =
  fmap Branch.head . expectBranchAtPath'

-- | Get the branch0 at a relative path, or return early if there's no such branch.
expectBranch0AtPath :: Path -> Cli (Branch0 IO)
expectBranch0AtPath =
  expectBranch0AtPath' . Path' . Right . Path.Relative

-- | Assert that there's "no branch" at an absolute or relative path, or return early if there is one, where "no branch"
-- means either there's actually no branch, or there is a branch whose head is empty (i.e. it may have a history, but no
-- current terms/types etc).
assertNoBranchAtPath' :: Path' -> Cli ()
assertNoBranchAtPath' path' = do
  whenM (branchExistsAtPath' path') do
    Cli.returnEarly (Output.BranchAlreadyExists path')

-- | Check if there's a branch at an absolute or relative path
--
-- "no branch" means either there's actually no branch, or there is a branch whose head is empty (i.e. it may have a history, but no
-- current terms/types etc).
branchExistsAtPath' :: Path' -> Cli Bool
branchExistsAtPath' path' = do
  pp <- resolvePath' path'
  Cli.runTransaction do
    branch <- Codebase.getShallowBranchAtProjectPath pp
    isEmpty <- V2Branch.isEmpty branch
    pure (not isEmpty)

------------------------------------------------------------------------------------------------------------------------
-- Updating branches

makeActionsUnabsolute :: (Functor f) => f (Path.Absolute, x) -> f (Path, x)
makeActionsUnabsolute = fmap (first Path.unabsolute)

stepAt ::
  Text ->
  (ProjectPath, Branch0 IO -> Branch0 IO) ->
  Cli ()
stepAt cause (pp, action) = stepManyAt pp.branch cause [(pp.absPath, action)]

stepAt' ::
  Text ->
  (ProjectPath, Branch0 IO -> Cli (Branch0 IO)) ->
  Cli Bool
stepAt' cause (pp, action) = stepManyAt' pp.branch cause [(pp.absPath, action)]

stepAtM ::
  Text ->
  (ProjectPath, Branch0 IO -> IO (Branch0 IO)) ->
  Cli ()
stepAtM cause (pp, action) = stepManyAtM pp.branch cause [(pp.absPath, action)]

stepManyAt ::
  ProjectBranch ->
  Text ->
  [(Path.Absolute, Branch0 IO -> Branch0 IO)] ->
  Cli ()
stepManyAt pb reason actions = do
  updateProjectBranchRoot_ pb reason $ Branch.stepManyAt (makeActionsUnabsolute actions)

stepManyAt' ::
  ProjectBranch ->
  Text ->
  [(Path.Absolute, Branch0 IO -> Cli (Branch0 IO))] ->
  Cli Bool
stepManyAt' pb reason actions = do
  origRoot <- getProjectBranchRoot pb
  newRoot <- Branch.stepManyAtM (makeActionsUnabsolute actions) origRoot
  didChange <- updateProjectBranchRoot pb reason (\oldRoot -> pure (newRoot, oldRoot /= newRoot))
  pure didChange

-- Like stepManyAt, but doesn't update the last saved root
stepManyAtM ::
  ProjectBranch ->
  Text ->
  [(Path.Absolute, Branch0 IO -> IO (Branch0 IO))] ->
  Cli ()
stepManyAtM pb reason actions = do
  updateProjectBranchRoot pb reason \oldRoot -> do
    newRoot <- liftIO (Branch.stepManyAtM (makeActionsUnabsolute actions) oldRoot)
    pure (newRoot, ())

-- | Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAtM ::
  Text ->
  ProjectPath ->
  (Branch IO -> Cli (Branch IO)) ->
  Cli Bool
updateAtM reason pp f = do
  oldRootBranch <- getProjectBranchRoot (pp ^. #branch)
  newRootBranch <- Branch.modifyAtM (pp ^. PP.path_) f oldRootBranch
  updateProjectBranchRoot_ (pp ^. #branch) reason (const newRootBranch)
  pure $ oldRootBranch /= newRootBranch

-- | Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAt ::
  Text ->
  ProjectPath ->
  (Branch IO -> Branch IO) ->
  Cli Bool
updateAt reason pp f = do
  updateAtM reason pp (pure . f)

updateAndStepAt ::
  (Foldable f, Foldable g, Functor g) =>
  Text ->
  ProjectBranch ->
  f (Path.Absolute, Branch IO -> Branch IO) ->
  g (Path.Absolute, Branch0 IO -> Branch0 IO) ->
  Cli ()
updateAndStepAt reason projectBranch updates steps = do
  let f b =
        b
          & (\root -> foldl' (\b (Path.Absolute p, f) -> Branch.modifyAt p f b) root updates)
          & (Branch.stepManyAt (first Path.unabsolute <$> steps))
  updateProjectBranchRoot_ projectBranch reason f

updateProjectBranchRoot :: ProjectBranch -> Text -> (Branch IO -> Cli (Branch IO, r)) -> Cli r
updateProjectBranchRoot projectBranch reason f = do
  Cli.Env {codebase} <- ask
  Cli.time "updateProjectBranchRoot" do
    old <- getProjectBranchRoot projectBranch
    (new, result) <- f old
    when (old /= new) do
      liftIO $ Codebase.putBranch codebase new
      Cli.runTransaction $ do
        -- TODO: If we transactionally check that the project branch hasn't changed while we were computing the new
        -- branch, and if it has, abort the transaction and return an error, then we can
        -- remove the single UCM per codebase restriction.
        causalHashId <- Q.expectCausalHashIdByCausalHash (Branch.headHash new)
        Q.setProjectBranchHead reason (projectBranch ^. #projectId) (projectBranch ^. #branchId) causalHashId
    pure result

updateProjectBranchRoot_ :: ProjectBranch -> Text -> (Branch IO -> Branch IO) -> Cli ()
updateProjectBranchRoot_ projectBranch reason f = do
  updateProjectBranchRoot projectBranch reason (\b -> pure (f b, ()))

------------------------------------------------------------------------------------------------------------------------
-- Getting terms

getTermsAt :: (PP.ProjectPath, HQ'.HQSegment) -> Cli (Set Referent)
getTermsAt (pp, hqSeg) = do
  rootBranch0 <- getBranch0FromProjectPath pp
  pure (BranchUtil.getTerm (mempty, hqSeg) rootBranch0)

------------------------------------------------------------------------------------------------------------------------
-- Getting types

getTypesAt :: (PP.ProjectPath, HQ'.HQSegment) -> Cli (Set TypeReference)
getTypesAt (pp, hqSeg) = do
  rootBranch0 <- getBranch0FromProjectPath pp
  pure (BranchUtil.getType (mempty, hqSeg) rootBranch0)

------------------------------------------------------------------------------------------------------------------------
-- Getting patches

-- | The default patch path.
defaultPatchPath :: Name
defaultPatchPath = Name.fromReverseSegments $ NameSegment.defaultPatchSegment :| []

-- | Get the patch at a path, or the empty patch if there's no such patch.
getPatchAt :: Name -> Cli Patch
getPatchAt path =
  getMaybePatchAt path <&> fromMaybe Patch.empty

-- | Get the patch at a path.
getMaybePatchAt :: Name -> Cli (Maybe Patch)
getMaybePatchAt path0 = do
  (pp, name) <- resolveName path0
  branch <- getBranch0FromProjectPath pp
  liftIO (Branch.getMaybePatch name branch)

------------------------------------------------------------------------------------------------------------------------
-- Latest (typechecked) unison file utils

getLatestFile :: Cli (Maybe (FilePath, Bool))
getLatestFile = do
  use #latestFile

expectLatestFile :: Cli (FilePath, Bool)
expectLatestFile = do
  getLatestFile & onNothingM (Cli.returnEarly Output.NoUnisonFile)

-- | Get the latest typechecked unison file.
getLatestTypecheckedFile :: Cli (Maybe (TypecheckedUnisonFile Symbol Ann))
getLatestTypecheckedFile = do
  oe <- use #latestTypecheckedFile
  pure $ case oe of
    Just (Right tf) -> Just tf
    _ -> Nothing

-- | Get the latest parsed unison file.
getLatestParsedFile :: Cli (Maybe (UnisonFile Symbol Ann))
getLatestParsedFile = do
  oe <- use #latestTypecheckedFile
  pure $ case oe of
    Just (Left uf) -> Just uf
    Just (Right tf) -> Just $ UF.discardTypes tf
    _ -> Nothing

expectLatestParsedFile :: Cli (UnisonFile Symbol Ann)
expectLatestParsedFile =
  getLatestParsedFile & onNothingM (Cli.returnEarly Output.NoUnisonFile)

-- | Returns a parsed term (potentially with free variables) from the latest file.
-- This term will refer to other terms in the file by vars, not by hash.
-- Used to implement rewriting and other refactorings on the current file.
getTermFromLatestParsedFile :: HQ.HashQualified Name.Name -> Cli (Maybe (Term.Term Symbol Ann))
getTermFromLatestParsedFile (HQ.NameOnly n) = do
  uf <- getLatestParsedFile
  pure $ case uf of
    Nothing -> Nothing
    Just uf ->
      case UF.typecheckingTerm uf of
        Term.LetRecNamed' bs _ -> lookup (Var.named (Name.toText n)) bs
        _ -> Nothing
getTermFromLatestParsedFile _ = pure Nothing

-- | Gets the names from the latest typechecked unison file, or latest parsed file if it
-- didn't typecheck.
getNamesFromLatestFile :: Cli Names
getNamesFromLatestFile = do
  use #latestTypecheckedFile <&> \case
    Just (Right tf) -> UFN.typecheckedToNames tf
    Just (Left uf) -> UFN.toNames uf
    Nothing -> mempty

-- | Get the latest typechecked unison file, or return early if there isn't one.
expectLatestTypecheckedFile :: Cli (TypecheckedUnisonFile Symbol Ann)
expectLatestTypecheckedFile =
  getLatestTypecheckedFile & onNothingM (Cli.returnEarly Output.NoUnisonFile)

-- @makeParsingEnv path names@ makes a parsing environment with @names@ in scope, which are all relative to @path@.
makeParsingEnv :: ProjectPath -> Names -> Cli (ParsingEnv Transaction)
makeParsingEnv path names = do
  Cli.Env {generateUniqueName} <- ask
  uniqueName <- liftIO generateUniqueName
  pure do
    ParsingEnv
      { uniqueNames = uniqueName,
        uniqueTypeGuid = loadUniqueTypeGuid path,
        names,
        maybeNamespace = Nothing,
        localNamespacePrefixedTypesAndConstructors = mempty
      }
