module Unison.Codebase.Editor.HandleInput.Dependencies
  ( handleDependencies,
  )
where

import Data.Bifoldable (bifoldMap, binull)
import Data.Set qualified as Set
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NameResolutionUtils (resolveHQName)
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.HashQualifiedPrime qualified as HQ'
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Defns qualified as Defns
import qualified Unison.Builtin as Builtin

handleDependencies :: HQ.HashQualified Name -> Cli ()
handleDependencies hq = do
  refs <- resolveHQName hq

  when (binull refs) do
    Cli.returnEarly (LabeledReferenceNotFound hq)

  namespace <- Cli.getCurrentProjectRoot0
  let ppe =
        let names = Branch.toNames namespace
         in PPE.makePPE (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)

  dependencies <- do
    Cli.runTransaction do
      Operations.directDependenciesOfScope
        Builtin.isBuiltinType
        ( let refToIds :: Reference -> Set Reference.Id
              refToIds =
                maybe Set.empty Set.singleton . Reference.toId
           in bifoldMap
                ( foldMap \case
                    Referent.Con ref _ -> Defns.fromTypes (refToIds (ref ^. ConstructorReference.reference_))
                    Referent.Ref ref -> Defns.fromTerms (refToIds ref)
                )
                (foldMap (refToIds >>> Defns.fromTypes))
                refs
        )

  let dependencyNames ::
        DefnsF
          []
          (HQ'.HashQualified Name, HQ'.HashQualified Name)
          (HQ'.HashQualified Name, HQ'.HashQualified Name)
      dependencyNames =
        bimap
          (f (Referent.fromTermReference >>> PPE.termNames ppe))
          (f (PPE.typeNames ppe))
          dependencies
        where
          f g =
            Set.toList
              >>> mapMaybe (g >>> listToMaybe)
              >>> Name.sortByText (fst >>> HQ'.toText)

  -- Set numbered args
  (dependencyNames.types ++ dependencyNames.terms)
    & map (SA.HashQualified . HQ'.toHQ . fst)
    & Cli.setNumberedArgs

  let lds = bifoldMap (Set.map LD.referent) (Set.map LD.typeRef) refs
  Cli.respond (ListDependencies ppe lds dependencyNames)
