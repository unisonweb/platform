module Unison.Codebase.Editor.HandleInput.NamespaceDependencies
  ( namespaceDependencies,
  )
where

import Control.Monad.Trans.Maybe
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.DataDeclaration qualified as DD
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Util.Relation qualified as Relation

-- | Check the dependencies of all types, terms, and metadata in the current namespace,
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
    for (Map.toList currentBranchTypeRefs) $ \(typeRef, names) -> fmap (fromMaybe Map.empty) . runMaybeT $ do
      refId <- MaybeT . pure $ Reference.toId typeRef
      decl <- MaybeT $ Codebase.getTypeDeclaration codebase refId
      let typeDeps = Set.map LD.typeRef $ DD.typeDependencies (DD.asDataDecl decl)
      pure $ foldMap (`Map.singleton` names) typeDeps

  termDeps <- for (Map.toList currentBranchTermRefs) $ \(termRef, names) -> fmap (fromMaybe Map.empty) . runMaybeT $ do
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
                (`Map.notMember` currentBranchTypeRefs)
                (`Map.notMember` currentBranchTermRefs)
                x
          )
          dependenciesToDependents
  pure onlyExternalDeps
  where
    currentBranchTermRefs :: Map Referent (Set Name)
    currentBranchTermRefs = Relation.domain (Branch.deepTerms branch)
    currentBranchTypeRefs :: Map Reference (Set Name)
    currentBranchTypeRefs = Relation.domain (Branch.deepTypes branch)
