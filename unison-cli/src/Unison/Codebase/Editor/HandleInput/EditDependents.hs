module Unison.Codebase.Editor.HandleInput.EditDependents
  ( handleEditDependents,
  )
where

import Control.Monad.Reader (ask)
import Data.Bifoldable (bifold)
import Data.Set qualified as Set
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NameResolutionUtils (resolveHQName)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.EditNamespace (getNamesForEdit)
import Unison.Codebase.Editor.HandleInput.ShowDefinition (showDefinitions)
import Unison.Codebase.Editor.Input (OutputLocation (..), RelativeToFold (..))
import Unison.Codebase.Editor.Output qualified as Output
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Names (Names (..))
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Defns qualified as Defns
import Unison.Util.Relation qualified as Relation

handleEditDependents :: HQ.HashQualified Name -> Cli ()
handleEditDependents name = do
  -- Get all of the referents and type references this name refers to
  refs0 <- resolveHQName name

  -- Since we don't track constructor dependents precisely, convert to just the term and type references
  let refs :: DefnsF Set TermReference TypeReference
      refs =
        let f = \case
              Referent.Con ref _ -> Defns.fromTypes (Set.singleton (ref ^. ConstructorReference.reference_))
              Referent.Ref ref -> Defns.fromTerms (Set.singleton ref)
         in Defns Set.empty refs0.types <> foldMap f refs0.terms

  (ppe, types, terms) <-
    Cli.withRespondRegion \respondRegion -> do
      respondRegion (Output.Literal "Loading branch...")

      -- Load the current project namespace and throw away the libdeps
      branch <- Cli.getCurrentBranch0
      let ppe =
            let names = Branch.toNames branch
             in PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHashName names)

      -- Throw away the libdeps
      let branchWithoutLibdeps = Branch.deleteLibdeps branch

      -- Identify the local dependents of the input name
      respondRegion (Output.Literal "Identifying dependents...")
      dependents <-
        Cli.runTransaction do
          Operations.transitiveDependentsWithinScope
            (Branch.deepTermReferenceIds branchWithoutLibdeps <> Branch.deepTypeReferenceIds branchWithoutLibdeps)
            (bifold refs)

      let refsAndDependents =
            Defns
              { terms =
                  Set.unions
                    [ Set.mapMonotonic Referent.fromTermReference refs.terms,
                      Set.mapMonotonic Referent.fromTermReferenceId dependents.terms
                    ],
                types =
                  Set.unions
                    [ refs.types,
                      Set.mapMonotonic Reference.fromId dependents.types
                    ]
              }

      respondRegion (Output.Literal "Loading dependents...")
      env <- ask
      (types, terms) <-
        Cli.runTransaction
          ( getNamesForEdit
              env.codebase
              ppe
              Names
                { terms =
                    branchWithoutLibdeps
                      & Branch.deepTerms
                      & Relation.restrictDom refsAndDependents.terms
                      & Relation.swap,
                  types =
                    branchWithoutLibdeps
                      & Branch.deepTypes
                      & Relation.restrictDom refsAndDependents.types
                      & Relation.swap
                }
          )
      pure (ppe, types, terms)

  let misses = []
  showDefinitions (LatestFileLocation WithinFold) ppe terms types misses
