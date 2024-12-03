module Unison.Codebase.Editor.HandleInput.NamespaceDependencies
  ( handleNamespaceDependencies,
  )
where

import Control.Monad.Reader (ask)
import Control.Monad.Trans.Maybe
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration qualified as DD
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Util.Relation qualified as Relation

handleNamespaceDependencies :: Maybe Path.Path' -> Cli.Cli ()
handleNamespaceDependencies namespacePath' = do
  Cli.Env {codebase} <- ask
  pp <- maybe Cli.getCurrentProjectPath Cli.resolvePath' namespacePath'
  let pb = pp ^. #branch
  branch <-
    Cli.getMaybeBranch0FromProjectPath pp & onNothingM do
      Cli.returnEarly (Output.BranchEmpty (Output.WhichBranchEmptyPath pp))
  externalDependencies <-
    Cli.runTransaction (namespaceDependencies codebase branch)
  names <- Cli.projectBranchNames pb

  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let ppe = PPED.unsuffixifiedPPE pped
  Cli.respondNumbered $ Output.ListNamespaceDependencies ppe pp externalDependencies

-- | Check the dependencies of all types and terms in the current namespace,
-- returns a map of dependencies which do not have a name within the current namespace,
-- alongside the names of all of that thing's dependents.
--
-- This is non-transitive, i.e. only the first layer of external dependencies is returned.
--
-- So if my namespace depends on .base.Bag.map; which depends on base.Map.mapKeys, only
-- .base.Bag.map is returned unless some other definition inside my namespace depends
-- on base.Map.mapKeys directly.
--
-- Returns a Set of names rather than using the PPE since we already have the correct names in
-- scope on this branch, and also want to list ALL names of dependents, including aliases.
namespaceDependencies :: Codebase m Symbol a -> Branch0 m -> Sqlite.Transaction (Map LabeledDependency (Set Name))
namespaceDependencies codebase branch = do
  typeDeps <-
    for (Map.toList (Relation.domain (Branch.deepTypes branchWithoutLibdeps))) \(typeRef, names) ->
      fmap (fromMaybe Map.empty) . runMaybeT $ do
        refId <- MaybeT . pure $ Reference.toId typeRef
        decl <- MaybeT $ Codebase.getTypeDeclaration codebase refId
        let typeDeps = Set.map LD.typeRef $ DD.typeDependencies (DD.asDataDecl decl)
        pure $ foldMap (`Map.singleton` names) typeDeps

  termDeps <-
    for (Map.toList (Relation.domain (Branch.deepTerms branchWithoutLibdeps))) \(termRef, names) ->
      fmap (fromMaybe Map.empty) . runMaybeT $ do
        refId <- MaybeT . pure $ Referent.toReferenceId termRef
        term <- MaybeT $ Codebase.getTerm codebase refId
        let termDeps = Term.labeledDependencies term
        pure $ foldMap (`Map.singleton` names) termDeps

  let dependenciesToDependents :: Map LabeledDependency (Set Name)
      dependenciesToDependents =
        Map.unionsWith (<>) (typeDeps ++ termDeps)

  let onlyExternalDeps :: Map LabeledDependency (Set Name)
      onlyExternalDeps =
        Map.filterWithKey
          ( \x _ ->
              LD.fold
                (\k -> not (Relation.memberDom k (Branch.deepTypes branch)))
                (\k -> not (Relation.memberDom k (Branch.deepTerms branch)))
                x
          )
          dependenciesToDependents

  pure onlyExternalDeps
  where
    branchWithoutLibdeps = branch & over Branch.children (Map.delete NameSegment.libSegment)
