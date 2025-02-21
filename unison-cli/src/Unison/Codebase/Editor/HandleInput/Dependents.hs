module Unison.Codebase.Editor.HandleInput.Dependents
  ( handleDependents,
  )
where

import Data.Bifoldable (bifoldMap, binull)
import Data.Set qualified as Set
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NameResolutionUtils (resolveHQName)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.HashQualifiedPrime qualified as HQ' (toText)
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Set qualified as Set

handleDependents :: HQ.HashQualified Name -> Cli ()
handleDependents hq = do
  refs <- resolveHQName hq

  when (binull refs) do
    Cli.returnEarly (LabeledReferenceNotFound hq)

  namespace <- Cli.getCurrentProjectRoot0
  let ppe =
        let names = Branch.toNames namespace
         in PPE.makePPE (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  let namespaceWithoutLibdeps = Branch.deleteLibdeps namespace
  let ppeWithoutLibdeps =
        let names = Branch.toNames namespaceWithoutLibdeps
         in PPE.makePPE (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  dependents <- do
    Cli.runTransaction do
      Operations.directDependentsWithinScope
        ( Set.union
            (Set.mapMaybe Reference.toId (Branch.deepTypeReferences namespaceWithoutLibdeps))
            (Set.mapMaybe Referent.toTermReferenceId (Branch.deepReferents namespaceWithoutLibdeps))
        )
        (bifoldMap (Set.map Referent.toReference) id refs)

  let dependentNames ::
        DefnsF
          []
          (HQ'.HashQualified Name, HQ'.HashQualified Name)
          (HQ'.HashQualified Name, HQ'.HashQualified Name)
      dependentNames =
        bimap
          (f (Referent.fromTermReferenceId >>> PPE.termNames ppeWithoutLibdeps))
          (f (Reference.fromId >>> PPE.typeNames ppeWithoutLibdeps))
          dependents
        where
          f g =
            Set.toList
              >>> mapMaybe (g >>> listToMaybe)
              >>> Name.sortByText (fst >>> HQ'.toText)

  -- Set numbered args
  (dependentNames.types ++ dependentNames.terms)
    & map (SA.HashQualified . HQ'.toHQ . fst)
    & Cli.setNumberedArgs

  let lds = bifoldMap (Set.map LD.referent) (Set.map LD.typeRef) refs
  Cli.respond (ListDependents ppe lds dependentNames)
