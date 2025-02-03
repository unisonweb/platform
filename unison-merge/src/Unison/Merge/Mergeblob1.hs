module Unison.Merge.Mergeblob1
  ( Mergeblob1 (..),
    hydratedDefnsLabeledDependencies,
    makeMergeblob1,
  )
where

import Control.Lens
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DataDeclaration.Dependencies qualified as Decl
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.LabeledDependency qualified as LD
import Unison.Merge.CombineDiffs (CombinedDiffOp, combineDiffs)
import Unison.Merge.DeclCoherencyCheck (IncoherentDeclReason, checkDeclCoherency, lenientCheckDeclCoherency)
import Unison.Merge.Diff (humanizeDiffs, nameBasedNamespaceDiff)
import Unison.Merge.DiffOp (DiffOp)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.HumanDiffOp (HumanDiffOp)
import Unison.Merge.Libdeps (applyLibdepsDiff, diffLibdeps, getTwoFreshLibdepNames, mergeLibdepsDiffs)
import Unison.Merge.Mergeblob0 (Mergeblob0 (..))
import Unison.Merge.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.Merge.PartitionCombinedDiffs (partitionCombinedDiffs)
import Unison.Merge.Synhashed (Synhashed)
import Unison.Merge.ThreeWay (ThreeWay)
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Unconflicts (Unconflicts)
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Names (Names)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3)

data Mergeblob1 libdep = Mergeblob1
  { conflicts :: TwoWay (DefnsF (Map Name) TermReference TypeReference),
    declNameLookups :: TwoWay DeclNameLookup,
    defns :: ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
    diff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference,
    diffsFromLCA :: TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference),
    humanDiffsFromLCA :: TwoWay (DefnsF2 (Map Name) HumanDiffOp Referent TypeReference),
    hydratedDefns ::
      ThreeWay
        ( DefnsF
            (Map Name)
            (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
            (TypeReferenceId, Decl Symbol Ann)
        ),
    lcaDeclNameLookup :: PartialDeclNameLookup,
    lcaLibdeps :: Map NameSegment libdep,
    libdeps :: Map NameSegment libdep,
    libdepsDiffs :: TwoWay (Map NameSegment (DiffOp libdep)),
    unconflicts :: DefnsF Unconflicts Referent TypeReference
  }

-- | Get a names object for all the hydrated definitions AND their direct dependencies
hydratedDefnsLabeledDependencies ::
  DefnsF
    (Map Name)
    (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
    (TypeReferenceId, Decl Symbol Ann) ->
  Set LD.LabeledDependency
hydratedDefnsLabeledDependencies defns =
  let termDeps :: Set LD.LabeledDependency
      termDeps =
        foldOf
          ( folded
              . beside
                (to Reference.DerivedId . to LD.TermReference . to Set.singleton)
                (beside (to Term.labeledDependencies) (to Type.labeledDependencies))
          )
          defns.terms

      typeDeps :: Set LD.LabeledDependency
      typeDeps =
        defns.types
          & foldMap \(typeRefId, typeDecl) ->
            Decl.labeledDeclDependenciesIncludingSelfAndFieldAccessors (Reference.DerivedId typeRefId) typeDecl
   in Set.union termDeps typeDeps

makeMergeblob1 ::
  forall libdep.
  (Eq libdep) =>
  Mergeblob0 libdep ->
  ThreeWay Names {- Names for _at least_ every reference in 'hydratedDefnDependencies' -} ->
  ThreeWay
    ( DefnsF
        (Map Name)
        (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
        (TypeReferenceId, Decl Symbol Ann)
    ) ->
  Either (EitherWay IncoherentDeclReason) (Mergeblob1 libdep)
makeMergeblob1 blob names3 hydratedDefns = do
  let ppeds3 :: ThreeWay PPED.PrettyPrintEnvDecl
      ppeds3 = names3 <&> \names -> PPED.makePPED (PPE.namer names) (PPE.suffixifyByHash names)
  -- Make one big constructor count lookup for all type decls
  let numConstructors =
        Map.empty
          & f (Map.elems hydratedDefns.alice.types)
          & f (Map.elems hydratedDefns.bob.types)
          & f (Map.elems hydratedDefns.lca.types)
        where
          f :: [(TypeReferenceId, Decl Symbol Ann)] -> Map TypeReferenceId Int -> Map TypeReferenceId Int
          f types acc =
            List.foldl'
              ( \acc (ref, decl) ->
                  Map.insert ref (DataDeclaration.constructorCount (DataDeclaration.asDataDecl decl)) acc
              )
              acc
              types

  -- Make Alice/Bob decl name lookups, which can fail if either have an incoherent decl
  declNameLookups <- do
    alice <- checkDeclCoherency blob.nametrees.alice numConstructors & mapLeft Alice
    bob <- checkDeclCoherency blob.nametrees.bob numConstructors & mapLeft Bob
    pure TwoWay {alice, bob}

  -- Make LCA decl name lookup
  let lcaDeclNameLookup =
        lenientCheckDeclCoherency blob.nametrees.lca numConstructors

  -- Diff LCA->Alice and LCA->Bob
  let (diffsFromLCA, propagatedUpdates) =
        nameBasedNamespaceDiff
          declNameLookups
          lcaDeclNameLookup
          ppeds3
          blob.defns
          Defns
            { terms =
                foldMap
                  (List.foldl' (\acc (ref, (term, _)) -> Map.insert ref term acc) Map.empty . Map.elems . (.terms))
                  hydratedDefns,
              types =
                foldMap
                  (List.foldl' (\acc (ref, typ) -> Map.insert ref typ acc) Map.empty . Map.elems . (.types))
                  hydratedDefns
            }

  -- Combine the LCA->Alice and LCA->Bob diffs together
  let diff = combineDiffs diffsFromLCA

  let humanDiffsFromLCA = humanizeDiffs names3 diffsFromLCA propagatedUpdates

  -- Partition the combined diff into the conflicted things and the unconflicted things
  let (conflicts, unconflicts) =
        partitionCombinedDiffs (ThreeWay.forgetLca blob.defns) declNameLookups diff

  -- Diff and merge libdeps
  let libdepsDiffs :: TwoWay (Map NameSegment (DiffOp libdep))
      libdepsDiffs =
        diffLibdeps blob.libdeps

  let libdeps :: Map NameSegment libdep
      libdeps =
        applyLibdepsDiff getTwoFreshLibdepNames blob.libdeps (mergeLibdepsDiffs libdepsDiffs)

  pure
    Mergeblob1
      { conflicts,
        declNameLookups,
        defns = blob.defns,
        diff,
        diffsFromLCA,
        humanDiffsFromLCA,
        hydratedDefns,
        lcaDeclNameLookup,
        lcaLibdeps = blob.libdeps.lca,
        libdeps,
        libdepsDiffs,
        unconflicts
      }
