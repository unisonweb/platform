module Unison.Codebase.Editor.HandleInput.DeleteNamespace
  ( handleDeleteNamespace,
    getEndangeredDependents,
  )
where

import Control.Lens hiding (from)
import Control.Lens qualified as Lens
import Control.Monad.State qualified as State
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as ProjectPath
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Referent qualified as Referent
import Unison.Sqlite qualified as Sqlite

handleDeleteNamespace :: Input -> Insistence -> Maybe (Path, NameSegment.NameSegment) -> Cli ()
handleDeleteNamespace input insistence = \case
  Nothing -> do
    loopState <- State.get
    if loopState.lastInput == Just input || insistence == Force
      then do
        pp <- Cli.getCurrentProjectPath
        _ <- Cli.updateAt (commandName <> " .") pp (const Branch.empty)
        Cli.respond DeletedEverything
      else Cli.respond DeleteEverythingConfirmation
  Just p@(parentPath, childName) -> do
    branch <- Cli.expectBranchAtPath (Path.unsplit p)
    let toDelete =
          Names.prefix0
            (Name.fromReverseSegments $ childName :| reverse (toList $ Path.toSeq parentPath))
            (Branch.toNames (Branch.head branch))
    afterDelete <- do
      currentBranch <- Cli.getCurrentProjectRoot0
      let names = Branch.toNames currentBranch
          namesSansLib = Branch.toNames (Branch.deleteLibdeps currentBranch)
      endangerments <- Cli.runTransaction (getEndangeredDependents toDelete Set.empty names namesSansLib)
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
    let description = commandName <> " " <> into @Text (parentPathAbs & ProjectPath.absPath_ %~ (`Lens.snoc` childName))
    -- We have to modify the parent in order to also wipe out the history at the
    -- child.
    Cli.updateAt description parentPathAbs (Branch.modifyAt (Path.singleton childName) \_ -> Branch.empty)
    afterDelete
  where
    commandName :: Text
    commandName =
      case insistence of
        Try -> "delete.namespace"
        Force -> "delete.namespace.force"

-- How I might do it (is this any better than the current algorithm?)
--
-- 1. Get all direct dependents of the deleted things in the current namespace.
-- 2. For each direct dependent, check a Names built from the deleted namespace – is it there? If not it's the last
--    name.

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
  -- | Names from the current branch, sans `lib`
  Names ->
  -- | map from references going extinct to the set of endangered dependents
  Sqlite.Transaction (Map LabeledDependency (NESet LabeledDependency))
getEndangeredDependents targetToDelete otherDesiredDeletions rootNames rootNamesSansLib = do
  -- deleting and not left over
  let extinct :: Set LabeledDependency
      extinct = Names.labeledReferences targetToDelete `Set.difference` refsAfterDeletingTarget rootNames

  let accumulateDependents :: LabeledDependency -> Sqlite.Transaction (Map LabeledDependency (Set LabeledDependency))
      accumulateDependents ld =
        let ref = LD.fold id Referent.toReference ld
         in Map.singleton ld . Set.map LD.termRef <$> Codebase.dependents Queries.ExcludeOwnComponent ref

  -- All dependents of extinct, including terms which might themselves be in the process of being deleted.
  allDependentsOfExtinct :: Map LabeledDependency (Set LabeledDependency) <-
    Map.unionsWith (<>) <$> for (Set.toList extinct) accumulateDependents

  -- Of all the dependents of things going extinct, we filter down to only those that are not themselves being deleted
  -- too (per `otherDesiredDeletion`), and are also somewhere outside `lib`. This allows us to proceed with deleting
  -- an entire dependency out of `lib` even if for some reason it contains the only source of names for some other
  -- dependency.
  let extinctToEndangered :: Map LabeledDependency (NESet LabeledDependency)
      extinctToEndangered =
        Map.mapMaybe
          ( NESet.nonEmptySet
              . Set.intersection (Set.difference (refsAfterDeletingTarget rootNamesSansLib) otherDesiredDeletions)
          )
          allDependentsOfExtinct
  pure extinctToEndangered
  where
    refsAfterDeletingTarget :: Names -> Set LabeledDependency
    refsAfterDeletingTarget names =
      Names.labeledReferences (names `Names.difference` targetToDelete)
