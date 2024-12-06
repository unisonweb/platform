module Unison.Merge.Mergeblob2
  ( Mergeblob2 (..),
    Mergeblob2Error (..),
    makeMergeblob2,
  )
where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.FindConflictedAlias (findConflictedAlias)
import Unison.Merge.Mergeblob1 (Mergeblob1 (..))
import Unison.Merge.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.Merge.PartitionCombinedDiffs (narrowConflictsToNonBuiltins)
import Unison.Merge.ThreeWay (ThreeWay)
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.Unconflicts (Unconflicts)
import Unison.Merge.Unconflicts qualified as Unconflicts
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defn (Defn)
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty, zipDefnsWith)

data Mergeblob2 libdep = Mergeblob2
  { conflicts :: TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId),
    coreDependencies :: TwoWay (DefnsF Set TermReference TypeReference),
    declNameLookups :: TwoWay DeclNameLookup,
    defns :: ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
    hasConflicts :: Bool,
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
    soloUpdatesAndDeletes :: TwoWay (DefnsF Set Name Name),
    unconflicts :: DefnsF Unconflicts Referent TypeReference
  }

data Mergeblob2Error
  = Mergeblob2Error'ConflictedAlias (EitherWay (Defn (Name, Name) (Name, Name)))
  | Mergeblob2Error'ConflictedBuiltin (Defn Name Name)

makeMergeblob2 :: Mergeblob1 libdep -> Either Mergeblob2Error (Mergeblob2 libdep)
makeMergeblob2 blob = do
  -- Bail early if it looks like we can't proceed with the merge, because Alice or Bob has one or more conflicted alias
  for_ ((,) <$> TwoWay Alice Bob <*> blob.diffs) \(who, diff) ->
    whenJust (findConflictedAlias blob.defns.lca diff) $
      Left . Mergeblob2Error'ConflictedAlias . who

  conflicts <- narrowConflictsToNonBuiltins blob.conflicts & mapLeft Mergeblob2Error'ConflictedBuiltin

  let soloUpdatesAndDeletes :: TwoWay (DefnsF Set Name Name)
      soloUpdatesAndDeletes =
        Unconflicts.soloUpdatesAndDeletes blob.unconflicts

  let coreDependencies :: TwoWay (DefnsF Set TermReference TypeReference)
      coreDependencies =
        identifyCoreDependencies
          (ThreeWay.forgetLca blob.defns)
          (bimap (Set.fromList . Map.elems) (Set.fromList . Map.elems) <$> conflicts)
          soloUpdatesAndDeletes

  pure
    Mergeblob2
      { conflicts,
        coreDependencies,
        declNameLookups = blob.declNameLookups,
        defns = blob.defns,
        -- Eh, they'd either both be null, or neither, but just check both maps anyway
        hasConflicts = not (defnsAreEmpty conflicts.alice) || not (defnsAreEmpty conflicts.bob),
        hydratedDefns = blob.hydratedDefns,
        lcaDeclNameLookup = blob.lcaDeclNameLookup,
        lcaLibdeps = blob.lcaLibdeps,
        libdeps = blob.libdeps,
        soloUpdatesAndDeletes,
        unconflicts = blob.unconflicts
      }

identifyCoreDependencies ::
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  TwoWay (DefnsF Set TermReferenceId TypeReferenceId) ->
  TwoWay (DefnsF Set Name Name) ->
  TwoWay (DefnsF Set TermReference TypeReference)
identifyCoreDependencies defns conflicts soloUpdatesAndDeletes = do
  fold
    [ -- One source of dependencies: Alice's versions of Bob's unconflicted deletes and updates, and vice-versa.
      --
      -- This is name-based: if Bob updates the *name* "foo", then we go find the thing that Alice calls "foo" (if
      -- anything), no matter what its hash is.
      defnsReferences
        <$> ( zipDefnsWith BiMultimap.restrictRan BiMultimap.restrictRan
                <$> TwoWay.swap soloUpdatesAndDeletes
                <*> defns
            ),
      -- The other source of dependencies: Alice's own conflicted things, and ditto for Bob.
      --
      -- An example: suppose Alice has foo#alice and Bob has foo#bob, so foo is conflicted. Furthermore, suppose
      -- Alice has bar#bar that depends on foo#alice.
      --
      -- We want Alice's #alice to be considered a dependency, so that when we go off and find dependents of these
      -- dependencies to put in the scratch file for type checking and propagation, we find bar#bar.
      --
      -- Note that this is necessary even if bar#bar is unconflicted! We don't want bar#bar to be put directly
      -- into the namespace / parsing context for the conflicted merge, because it has an unnamed reference on
      -- foo#alice. It rather ought to be in the scratchfile alongside the conflicted foo#alice and foo#bob, so
      -- that when that conflict is resolved, it will propagate to bar.
      bimap (Set.map Reference.DerivedId) (Set.map Reference.DerivedId) <$> conflicts
    ]

defnsReferences ::
  Defns (BiMultimap Referent name) (BiMultimap TypeReference name) ->
  DefnsF Set TermReference TypeReference
defnsReferences defns =
  List.foldl' f Defns {terms = Set.empty, types = BiMultimap.dom defns.types} (Set.toList (BiMultimap.dom defns.terms))
  where
    f :: DefnsF Set TermReference TypeReference -> Referent -> DefnsF Set TermReference TypeReference
    f acc = \case
      Referent.Con (ConstructorReference ref _) _ ->
        let !types = Set.insert ref acc.types
         in Defns {terms = acc.terms, types}
      Referent.Ref ref ->
        let !terms = Set.insert ref acc.terms
         in Defns {terms, types = acc.types}
