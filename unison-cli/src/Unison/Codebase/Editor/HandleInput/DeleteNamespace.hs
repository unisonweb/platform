module Unison.Codebase.Editor.HandleInput.DeleteNamespace
  ( handleDeleteNamespace,
    getEndangeredDependents,
  )
where

import Control.Lens hiding (from)
import Control.Monad.State qualified as State
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Referent qualified as Referent
import Unison.Sqlite qualified as Sqlite

handleDeleteNamespace ::
  Input ->
  (Input -> Cli Text) ->
  Insistence ->
  Maybe (Path, NameSegment.NameSegment) ->
  Cli ()
handleDeleteNamespace input inputDescription insistence = \case
  Nothing -> do
    hasConfirmed <- confirmedCommand input
    if hasConfirmed || insistence == Force
      then do
        description <- inputDescription input
        pp <- Cli.getCurrentProjectPath
        _ <- Cli.updateAt description pp (const Branch.empty)
        Cli.respond DeletedEverything
      else Cli.respond DeleteEverythingConfirmation
  Just p@(parentPath, childName) -> do
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

confirmedCommand :: Input -> Cli Bool
confirmedCommand i = do
  loopState <- State.get
  pure $ Just i == (loopState ^. #lastInput)

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
