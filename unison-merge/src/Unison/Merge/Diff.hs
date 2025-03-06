module Unison.Merge.Diff
  ( nameBasedNamespaceDiff,
    humanizeDiffs,
  )
where

import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty qualified as NEList
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.These (These (..))
import Data.Zip qualified as Zip
import U.Codebase.Reference (TypeReference)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.DeclNameLookup qualified as DeclNameLookup
import Unison.Hash (Hash (Hash))
import Unison.Merge.DiffOp (DiffOp (..), DiffOp2 (..))
import Unison.Merge.HumanDiffOp (HumanDiffOp (..))
import Unison.Merge.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.Merge.Synhash qualified as Synhash
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.Synhashed qualified as Synhashed
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Updated (Updated (..))
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (Reference' (..), TermReference, TermReferenceId, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF2, DefnsF3, unzipDefns, zipDefnsWith)
import Unison.Util.Defns qualified as Defns
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Rel

-- | @nameBasedNamespaceDiff db declNameLookups defns@ returns Alice's and Bob's name-based namespace diffs, each in the
-- form:
--
-- > terms :: Map Name (DiffOp (Synhashed Referent))
-- > types :: Map Name (DiffOp (Synhashed TypeReference))
--
-- where each name is paired with its diff-op (added, deleted, or updated), relative to the LCA between Alice and Bob's
-- branches. If the hash of a name did not change, it will not appear in the map.
nameBasedNamespaceDiff ::
  (HasCallStack) =>
  TwoWay DeclNameLookup ->
  PartialDeclNameLookup ->
  ThreeWay PPED.PrettyPrintEnvDecl ->
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Defns (Map TermReferenceId (Term Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)) ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference),
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    TwoWay (DefnsF2 (Map Name) Updated Referent TypeReference)
  )
nameBasedNamespaceDiff declNameLookups lcaDeclNameLookup ppeds defns hydratedDefns =
  Zip.unzip $
    diffHashedNamespaceDefns (synhashLcaDefns synhashPPE lcaDeclNameLookup defns.lca hydratedDefns)
      <$> (synhashDefns synhashPPE hydratedDefns <$> declNameLookups <*> ThreeWay.forgetLca defns)
  where
    synhashPPE :: PPE.PrettyPrintEnv
    synhashPPE =
      let ThreeWay {lca = lcaPPE, alice = alicePPE, bob = bobPPE} = PPED.unsuffixifiedPPE <$> ppeds
       in alicePPE `PPE.addFallback` bobPPE `PPE.addFallback` lcaPPE

diffHashedNamespaceDefns ::
  (Eq term, Eq typ) =>
  DefnsF2 (Map Name) Synhashed term typ ->
  DefnsF2 (Map Name) Synhashed term typ ->
  ( -- Core diffs, i.e. adds, deletes, and updates which have different synhashes.
    DefnsF3 (Map Name) DiffOp Synhashed term typ,
    -- Propagated updates, i.e. updates which have the same synhash but different Unison hashes.
    DefnsF2 (Map Name) Updated term typ
  )
diffHashedNamespaceDefns old new =
  unzipDefns (zipDefnsWith f f old new)
  where
    f ::
      (Eq ref) =>
      Map Name (Synhashed ref) ->
      Map Name (Synhashed ref) ->
      (Map Name (DiffOp (Synhashed ref)), Map Name (Updated ref))
    f old new =
      partitionPropagated (computeDiff old new)

    -- Compute the diff by comparing old-and-new values, resulting in either an add, delete, update (propagated or not),
    -- or dropping the thing entirely (because old and new have the same hash).
    computeDiff ::
      (Eq ref) =>
      Map Name (Synhashed ref) ->
      Map Name (Synhashed ref) ->
      Map Name (DiffOp2 (Synhashed ref))
    computeDiff =
      Map.merge
        (Map.mapMissing (\_ -> DiffOp2'Delete))
        (Map.mapMissing (\_ -> DiffOp2'Add))
        ( let f old new
                | Synhashed.value old == Synhashed.value new = Nothing
                | otherwise = Just (DiffOp2'Update Updated {old, new} (old == new))
           in Map.zipWithMaybeMatched \_ -> f
        )

    -- Partition add/delete/update/propagated-update into add/delete/update + propagated-update
    partitionPropagated ::
      Map Name (DiffOp2 (Synhashed ref)) ->
      (Map Name (DiffOp (Synhashed ref)), Map Name (Updated ref))
    partitionPropagated =
      Map.mapEither \case
        DiffOp2'Add ref -> Left (DiffOp'Add ref)
        DiffOp2'Delete ref -> Left (DiffOp'Delete ref)
        DiffOp2'Update refs propagated
          | propagated -> Right (Synhashed.value <$> refs)
          | otherwise -> Left (DiffOp'Update refs)

-- | Post-process a diff to identify relationships humans might care about, such as whether a given addition could be
-- interpreted as an alias of an existing definition, or whether an add and deletion could be a rename.
humanizeDiffs ::
  ThreeWay Names ->
  TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) ->
  TwoWay (DefnsF2 (Map Name) Updated Referent TypeReference) ->
  TwoWay (DefnsF2 (Map Name) HumanDiffOp Referent TypeReference)
humanizeDiffs names3 =
  let names3' = names3 <&> \names -> Defns names.terms names.types
   in zipWith3
        (Defns.zipDefnsWith4 computeHumanDiffOp computeHumanDiffOp names3'.lca)
        (ThreeWay.forgetLca names3')
  where
    zipWith3 :: (Zip.Zip f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    zipWith3 f a b =
      Zip.zipWith (uncurry f) (Zip.zip a b)

    computeHumanDiffOp ::
      forall ref.
      (Show ref, Ord ref) =>
      Relation Name ref ->
      Relation Name ref ->
      Map Name (DiffOp (Synhashed ref)) ->
      Map Name (Updated ref) ->
      Map Name (HumanDiffOp ref)
    computeHumanDiffOp oldNamespace newNamespace =
      alignWith \case
        This diff -> humanizeDiffOp (Synhashed.value <$> diff)
        That updated -> HumanDiffOp'PropagatedUpdate updated
        These diff updated ->
          error $
            reportBug
              "E488729"
              ( "The impossible happened, an update in merge was detected as both a propagated AND core update "
                  ++ show diff
                  ++ " and "
                  ++ show updated
              )
      where
        humanizeDiffOp :: DiffOp ref -> HumanDiffOp ref
        humanizeDiffOp = \case
          DiffOp'Add ref ->
            -- This name is newly added. We need to check if it's a new definition, an alias, or a rename.
            case Set.toList (Rel.lookupRan ref oldNamespace) of
              -- No old names for this ref, so it's a new addition not an alias
              [] -> HumanDiffOp'Add ref
              -- There are old names for this ref, but not old refs for this name, so it's
              -- either a new alias or a rename.
              --
              -- If at least one old name for this ref no longer exists, we treat it like a
              -- rename.
              n : ns -> do
                let existingNames = NESet.fromList (n NEList.:| ns)
                case NESet.nonEmptySet (Rel.lookupRan ref newNamespace) of
                  Nothing -> error (reportBug "E458329" ("Expected to find at least one name for ref in new namespace, since we found the ref by the name."))
                  Just allNewNames ->
                    case NESet.nonEmptySet (NESet.difference existingNames allNewNames) of
                      -- If all the old names still exist in the new namespace, it's a new alias.
                      Nothing -> HumanDiffOp'AliasOf ref existingNames
                      -- Otherwise, treat it as a rename.
                      Just namesWhichDisappeared ->
                        HumanDiffOp'RenamedFrom ref namesWhichDisappeared
          DiffOp'Delete ref ->
            case NEL.nonEmpty $ Set.toList (Rel.lookupRan ref newNamespace) of
              -- No names for this ref, it was removed.
              Nothing -> HumanDiffOp'Delete ref
              Just newNames -> HumanDiffOp'RenamedTo ref (NESet.fromList newNames)
          DiffOp'Update Updated {old, new} -> HumanDiffOp'Update Updated {old, new}

------------------------------------------------------------------------------------------------------------------------
-- Syntactic hashing

synhashLcaDefns ::
  (HasCallStack) =>
  PrettyPrintEnv ->
  PartialDeclNameLookup ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Defns (Map TermReferenceId (Term Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)) ->
  DefnsF2 (Map Name) Synhashed Referent TypeReference
synhashLcaDefns ppe declNameLookup defns hydratedDefns =
  synhashDefnsWith hashReferent hashType defns
  where
    -- For the LCA only, if we don't have a name for every constructor, or we don't have a name for a decl, that's okay,
    -- just use a dummy syntactic hash (e.g. where we return `Hash mempty` below in two places).
    --
    -- This is safe and correct; Alice/Bob can't have such a decl (it violates a merge precondition), so there's no risk
    -- that we accidentally get an equal hash and classify a real update as unchanged.

    hashReferent :: Name -> Referent -> Hash
    hashReferent name = \case
      Referent.Con (ConstructorReference ref _) _ ->
        case Map.lookup name declNameLookup.constructorToDecl of
          Nothing -> Hash mempty -- see note above
          Just declName -> hashType declName ref
      Referent.Ref ref -> synhashTermReference ppe hydratedDefns.terms ref

    hashType :: Name -> TypeReference -> Hash
    hashType name = \case
      ReferenceBuiltin builtin -> Synhash.synhashBuiltinDecl builtin
      ReferenceDerived ref ->
        case sequence (declNameLookup.declToConstructors Map.! name) of
          Nothing -> Hash mempty -- see note above
          Just names -> synhashDerivedDecl ppe hydratedDefns.types names name ref

synhashDefns ::
  (HasCallStack) =>
  PrettyPrintEnv ->
  Defns (Map TermReferenceId (Term Symbol Ann)) (Map TypeReferenceId (Decl Symbol Ann)) ->
  DeclNameLookup ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  DefnsF2 (Map Name) Synhashed Referent TypeReference
synhashDefns ppe hydratedDefns declNameLookup =
  synhashDefnsWith hashReferent hashType
  where
    hashReferent :: Name -> Referent -> Hash
    hashReferent name = \case
      -- We say that a referent constructor *in the namespace* (distinct from a referent that is in a term body) has a
      -- synhash that is simply equal to the synhash of its type declaration. This is because the type declaration and
      -- constructors are changed in lock-step: it is not possible to change one, but not the other.
      --
      -- For example, if Alice updates `type Foo = Bar Nat` to `type Foo = Bar Nat Nat`, we want different synhashes on
      -- both the type (Foo) and the constructor (Foo.Bar).
      Referent.Con (ConstructorReference ref _) _ -> hashType (DeclNameLookup.expectDeclName declNameLookup name) ref
      Referent.Ref ref -> synhashTermReference ppe hydratedDefns.terms ref

    hashType :: Name -> TypeReference -> Hash
    hashType name = \case
      ReferenceBuiltin builtin -> Synhash.synhashBuiltinDecl builtin
      ReferenceDerived ref ->
        synhashDerivedDecl ppe hydratedDefns.types (DeclNameLookup.expectConstructorNames declNameLookup name) name ref

synhashDerivedDecl ::
  (HasCallStack) =>
  PrettyPrintEnv ->
  Map TypeReferenceId (Decl Symbol Ann) ->
  [Name] ->
  Name ->
  TypeReferenceId ->
  Hash
synhashDerivedDecl ppe declsById names name ref =
  declsById
    & expectDecl ref
    & DataDeclaration.setConstructorNames (map Name.toVar names)
    & Synhash.synhashDerivedDecl ppe name

synhashTermReference :: (HasCallStack) => PrettyPrintEnv -> Map TermReferenceId (Term Symbol Ann) -> TermReference -> Hash
synhashTermReference ppe termsById = \case
  ReferenceBuiltin builtin -> Synhash.synhashBuiltinTerm builtin
  ReferenceDerived ref -> Synhash.synhashDerivedTerm ppe (expectTerm ref termsById)

synhashDefnsWith ::
  (HasCallStack) =>
  (Name -> term -> Hash) ->
  (Name -> typ -> Hash) ->
  Defns (BiMultimap term Name) (BiMultimap typ Name) ->
  DefnsF2 (Map Name) Synhashed term typ
synhashDefnsWith hashTerm hashType = do
  bimap
    (Map.mapWithKey hashTerm1 . BiMultimap.range)
    (Map.mapWithKey hashType1 . BiMultimap.range)
  where
    hashTerm1 name term =
      Synhashed (hashTerm name term) term

    hashType1 name typ =
      Synhashed (hashType name typ) typ

------------------------------------------------------------------------------------------------------------------------
-- Looking up terms and decls that we expect to be there

expectTerm :: (HasCallStack) => TermReferenceId -> Map TermReferenceId (Term Symbol Ann) -> Term Symbol Ann
expectTerm ref termsById =
  case Map.lookup ref termsById of
    Nothing -> error (reportBug "E488229" ("term ref " ++ show ref ++ " not found in map " ++ show termsById))
    Just term -> term

expectDecl :: (HasCallStack) => TypeReferenceId -> Map TypeReferenceId (Decl Symbol Ann) -> Decl Symbol Ann
expectDecl ref declsById =
  case Map.lookup ref declsById of
    Nothing -> error (reportBug "E663160" ("type ref " ++ show ref ++ " not found in map " ++ show declsById))
    Just decl -> decl
