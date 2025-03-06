module Unison.Codebase.Editor.HandleInput.Dependencies
  ( handleDependencies,
  )
where

import Control.Arrow ((***))
import Data.Bifoldable (bifoldMap, binull)
import Data.Set qualified as Set
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Builtin qualified as Builtin
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
          (HQ.HashQualified Name, HQ.HashQualified Name)
          (HQ.HashQualified Name, HQ.HashQualified Name)
      dependencyNames =
        bimap
          (f (Referent.fromTermReference >>> PPE.termNames ppe))
          (f (PPE.typeNames ppe))
          dependencies
        where
          f ::
            (Reference -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]) ->
            Set Reference ->
            [(HQ.HashQualified Name, HQ.HashQualified Name)]
          f g =
            Set.toList
              -- Pick the best name for a reference (with `listToMaybe`), else use the ref (if nameless)
              >>> map (\x -> maybe (Left x) Right (listToMaybe (g x)))
              >>> partitionEithers
              -- Sort the named references alphabetically, then stick the hash-only ones at the end
              >>> h

          h ::
            ([Reference], [(HQ'.HashQualified Name, HQ'.HashQualified Name)]) ->
            [(HQ.HashQualified Name, HQ.HashQualified Name)]
          h (nameless, named) =
            concat
              [ named
                  & Name.sortByText (fst >>> HQ'.toText)
                  & map (HQ'.toHQ *** HQ'.toHQ),
                nameless
                  & map (\x -> let y = HQ.fromReference x in (y, y))
              ]

  -- Set numbered args
  (dependencyNames.types ++ dependencyNames.terms)
    & map (SA.HashQualified . fst)
    & Cli.setNumberedArgs

  let lds = bifoldMap (Set.map LD.referent) (Set.map LD.typeRef) refs
  Cli.respond (ListDependencies ppe lds dependencyNames)
