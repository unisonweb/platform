module Unison.Merge.Mergeblob1
  ( Mergeblob1 (..),
    makeMergeblob1,
  )
where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.Merge.CombineDiffs (CombinedDiffOp, combineDiffs)
import Unison.Merge.DeclCoherencyCheck (IncoherentDeclReason, checkDeclCoherency, lenientCheckDeclCoherency)
import Unison.Merge.Diff (nameBasedNamespaceDiff)
import Unison.Merge.DiffOp (DiffOp)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.Libdeps (LibdepDiffOp, applyLibdepsDiff, diffLibdeps, getTwoFreshLibdepNames)
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
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3)

data Mergeblob1 libdep = Mergeblob1
  { conflicts :: TwoWay (DefnsF (Map Name) TermReference TypeReference),
    declNameLookups :: TwoWay DeclNameLookup,
    defns :: ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
    diff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference,
    diffs :: TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference),
    hydratedDefns ::
      ThreeWay
        ( DefnsF
            (Map Name)
            (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
            (TypeReferenceId, Decl Symbol Ann)
        ),
    lcaDeclNameLookup :: PartialDeclNameLookup,
    libdeps :: Map NameSegment libdep,
    libdepsDiff :: Map NameSegment (LibdepDiffOp libdep),
    lcaLibdeps :: Map NameSegment libdep,
    unconflicts :: DefnsF Unconflicts Referent TypeReference
  }

makeMergeblob1 ::
  forall libdep.
  (Eq libdep) =>
  Mergeblob0 libdep ->
  ThreeWay
    ( DefnsF
        (Map Name)
        (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
        (TypeReferenceId, Decl Symbol Ann)
    ) ->
  Either (EitherWay IncoherentDeclReason) (Mergeblob1 libdep)
makeMergeblob1 blob hydratedDefns = do
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
  let diffs =
        nameBasedNamespaceDiff
          declNameLookups
          lcaDeclNameLookup
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
  let diff =
        combineDiffs diffs

  -- Partition the combined diff into the conflicted things and the unconflicted things
  let (conflicts, unconflicts) =
        partitionCombinedDiffs (ThreeWay.forgetLca blob.defns) declNameLookups diff

  -- Diff and merge libdeps
  let libdepsDiff :: Map NameSegment (LibdepDiffOp libdep)
      libdepsDiff =
        diffLibdeps blob.libdeps

  let libdeps :: Map NameSegment libdep
      libdeps =
        applyLibdepsDiff getTwoFreshLibdepNames blob.libdeps libdepsDiff

  pure
    Mergeblob1
      { conflicts,
        declNameLookups,
        defns = blob.defns,
        diff,
        diffs,
        hydratedDefns,
        lcaDeclNameLookup,
        libdeps,
        libdepsDiff,
        lcaLibdeps = blob.libdeps.lca,
        unconflicts
      }
