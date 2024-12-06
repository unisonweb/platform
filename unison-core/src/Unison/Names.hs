{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Names
  ( Names (..),
    addTerm,
    addType,
    labeledReferences,
    conflicts,
    contains,
    difference,
    filter,
    filterByHQs,
    filterBySHs,
    filterTypes,
    fromReferenceIds,
    fromUnconflictedReferenceIds,
    map,
    makeAbsolute,
    makeRelative,
    fuzzyFind,
    hqName,
    hqTermName,
    hqTypeName,
    hqTermName',
    hqTypeName',
    _hqTermName,
    _hqTypeName,
    _hqTermAliases,
    _hqTypeAliases,
    mapNames,
    prefix0,
    restrictReferences,
    refTermsNamed,
    refTermsHQNamed,
    referenceIds,
    termReferences,
    termReferents,
    typeReferences,
    termsNamed,
    typesNamed,
    shadowing,
    shadowing1,
    preferring,
    namesForReference,
    namesForReferent,
    shadowTerms,
    importing,
    constructorsForType,
    expandWildcardImport,
    isEmpty,
    hashQualifyTypesRelation,
    hashQualifyTermsRelation,
    fromTermsAndTypes,
    lenientToNametree,
    resolveName,
    resolveNameIncludingNames,
  )
where

import Control.Lens (_2)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import Text.FuzzyFind qualified as FZF
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Names.ResolvesTo (ResolvesTo (..))
import Unison.Prelude
import Unison.Reference (Reference, TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Nametree (Nametree, unflattenNametree)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as R
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set (mapMaybe)
import Prelude hiding (filter, map)
import Prelude qualified

-- This will support the APIs of both PrettyPrintEnv and the old Names.
-- For pretty-printing, we need to look up names for References.
-- For parsing (both .u files and command-line args)
data Names = Names
  { terms :: Relation Name Referent,
    types :: Relation Name TypeReference
  }
  deriving (Eq, Ord, Show, Generic)

instance Semigroup (Names) where
  Names e1 t1 <> Names e2 t2 =
    Names (e1 <> e2) (t1 <> t2)

instance Monoid (Names) where
  mempty = Names mempty mempty

isEmpty :: Names -> Bool
isEmpty n = R.null n.terms && R.null n.types

-- | Construct a 'Names' from unconflicted reference ids.
fromReferenceIds :: DefnsF (Relation Name) TermReferenceId TypeReferenceId -> Names
fromReferenceIds defns =
  Names
    { terms = Relation.mapRan Referent.fromTermReferenceId defns.terms,
      types = Relation.mapRan Reference.fromId defns.types
    }

-- | Construct a 'Names' from unconflicted reference ids.
fromUnconflictedReferenceIds :: DefnsF (Map Name) TermReferenceId TypeReferenceId -> Names
fromUnconflictedReferenceIds defns =
  Names
    { terms = Relation.fromMap (Map.map Referent.fromTermReferenceId defns.terms),
      types = Relation.fromMap (Map.map Reference.fromId defns.types)
    }

map :: (Name -> Name) -> Names -> Names
map f (Names {terms, types}) = Names terms' types'
  where
    terms' = R.mapDom f terms
    types' = R.mapDom f types

makeAbsolute :: Names -> Names
makeAbsolute = map Name.makeAbsolute

makeRelative :: Names -> Names
makeRelative = map Name.makeRelative

-- Finds names that are supersequences of all the given strings, ordered by
-- score and grouped by name.
fuzzyFind ::
  (Name -> Text) ->
  [String] ->
  Names ->
  [(FZF.Alignment, Name, Set (Either Referent TypeReference))]
fuzzyFind nameToText query names =
  fmap flatten
    . fuzzyFinds (Text.unpack . nameToText . fst) query
    . Prelude.filter prefilter
    . Map.toList
    -- `mapMonotonic` is safe here and saves a log n factor
    $ (Set.mapMonotonic Left <$> R.toMultimap names.terms)
      <> (Set.mapMonotonic Right <$> R.toMultimap names.types)
  where
    lowerqueryt = Text.toLower . Text.pack <$> query
    -- For performance, case-insensitive substring matching as a pre-filter
    -- This finds fewer matches than subsequence matching, but is
    -- (currently) way faster even on large name sets.
    prefilter (nameToText -> name, _) = case lowerqueryt of
      -- Special cases here just to help optimizer, since
      -- not sure if `all` will get sufficiently unrolled for
      -- Text fusion to work out.
      [q] -> q `Text.isInfixOf` lowername
      [q1, q2] -> q1 `Text.isInfixOf` lowername && q2 `Text.isInfixOf` lowername
      query -> all (`Text.isInfixOf` lowername) query
      where
        lowername = Text.toLower name
    flatten (a, (b, c)) = (a, b, c)
    fuzzyFinds :: (a -> String) -> [String] -> [a] -> [(FZF.Alignment, a)]
    fuzzyFinds f query d =
      d
        >>= ( \s ->
                toList $
                  (,s)
                    <$> foldl'
                      (\a q -> (<>) <$> a <*> FZF.bestMatch q (f s))
                      (Just mempty)
                      query
            )

-- | Get all (untagged) term/type references ids in a @Names@.
referenceIds :: Names -> Set Reference.Id
referenceIds Names {terms, types} =
  fromTerms <> fromTypes
  where
    fromTerms = Set.mapMaybe Referent.toReferenceId (Relation.ran terms)
    fromTypes = Set.mapMaybe Reference.toId (Relation.ran types)

-- | Returns all constructor term references. Constructors are omitted.
termReferences :: Names -> Set TermReference
termReferences Names {..} = Set.mapMaybe Referent.toTermReference $ R.ran terms

typeReferences :: Names -> Set TypeReference
typeReferences Names {..} = R.ran types

-- | Collect all references in the given Names, tagged with their type.
labeledReferences :: Names -> Set LabeledDependency
labeledReferences Names {..} =
  Set.map LD.typeRef (Relation.ran types)
    <> Set.map LD.referent (Relation.ran terms)

termReferents :: Names -> Set Referent
termReferents Names {..} = R.ran terms

restrictReferences :: Set Reference -> Names -> Names
restrictReferences refs Names {..} = Names terms' types'
  where
    terms' = R.filterRan ((`Set.member` refs) . Referent.toReference) terms
    types' = R.filterRan (`Set.member` refs) types

-- | Construct names from a left-biased map union of the domains of the input names. That is, for each distinct name,
-- if it refers to *any* references in the left argument, use those (ignoring the right).
--
-- This is appropriate for shadowing names in the codebase with names in a Unison file, for instance:
--
-- @shadowing scratchFileNames codebaseNames@
shadowing :: Names -> Names -> Names
shadowing a b =
  Names (shadowing1 a.terms b.terms) (shadowing1 a.types b.types)

shadowing1 :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
shadowing1 =
  Relation.unionDomainWith (\_ x _ -> x)

-- | Construct names from a left-biased map union of the ranges of the input names. That is, for each distinct
-- reference, if it is referred to by *any* names in the left argument, use those (ignoring the right).
--
-- This is appropriate for biasing a PPE towards picking names in the left argument.
preferring :: Names -> Names -> Names
preferring xs ys =
  Names (preferring1 xs.terms ys.terms) (preferring1 xs.types ys.types)
  where
    preferring1 :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
    preferring1 =
      Relation.unionRangeWith (\_ x _ -> x)

-- | TODO: get this from database. For now it's a constant.
numHashChars :: Int
numHashChars = 3

termsNamed :: Names -> Name -> Set Referent
termsNamed = flip R.lookupDom . (.terms)

-- | Get all terms with a specific name.
refTermsNamed :: Names -> Name -> Set TermReference
refTermsNamed names n =
  Set.mapMaybe Referent.toTermReference (termsNamed names n)

-- | Get all terms with a specific hash-qualified name.
refTermsHQNamed :: Names -> HQ.HashQualified Name -> Set TermReference
refTermsHQNamed names = \case
  HQ.NameOnly name -> refTermsNamed names name
  HQ.HashOnly _hash -> Set.empty
  HQ.HashQualified name hash ->
    let f :: Referent -> Maybe TermReference
        f ref0 = do
          ref <- Referent.toTermReference ref0
          guard (Reference.isPrefixOf hash ref)
          Just ref
     in Set.mapMaybe f (termsNamed names name)

typesNamed :: Names -> Name -> Set TypeReference
typesNamed = flip R.lookupDom . (.types)

namesForReferent :: Names -> Referent -> Set Name
namesForReferent names r = R.lookupRan r names.terms

namesForReference :: Names -> TypeReference -> Set Name
namesForReference names r = R.lookupRan r names.types

termAliases :: Names -> Name -> Referent -> Set Name
termAliases names n r = Set.delete n $ namesForReferent names r

typeAliases :: Names -> Name -> TypeReference -> Set Name
typeAliases names n r = Set.delete n $ namesForReference names r

addType :: Name -> TypeReference -> Names -> Names
addType n r = (<> fromTypes [(n, r)])

addTerm :: Name -> Referent -> Names -> Names
addTerm n r = (<> fromTerms [(n, r)])

-- | Like hqTermName and hqTypeName, but considers term and type names to
-- conflict with each other (so will hash-qualify if there is e.g. both a term
-- and a type named "foo").
--
-- This is useful in contexts such as printing branch diffs. Example:
--
--     - Deletes:
--
--       foo
--       foo
--
-- We want to append the hash regardless of whether or not one is a term and the
-- other is a type.
hqName :: Names -> Name -> Either TypeReference Referent -> HQ'.HashQualified Name
hqName b n = \case
  Left r -> if ambiguous then _hqTypeName' b n r else HQ'.fromName n
  Right r -> if ambiguous then _hqTermName' b n r else HQ'.fromName n
  where
    ambiguous = Set.size (termsNamed b n) + Set.size (typesNamed b n) > 1

-- Conditionally apply hash qualifier to term name.
-- Should be the same as the input name if the Names is unconflicted.
hqTermName :: Int -> Names -> Name -> Referent -> HQ'.HashQualified Name
hqTermName hqLen b n r =
  if Set.size (termsNamed b n) > 1
    then hqTermName' hqLen n r
    else HQ'.fromName n

hqTypeName :: Int -> Names -> Name -> TypeReference -> HQ'.HashQualified Name
hqTypeName hqLen b n r =
  if Set.size (typesNamed b n) > 1
    then hqTypeName' hqLen n r
    else HQ'.fromName n

_hqTermName :: Names -> Name -> Referent -> HQ'.HashQualified Name
_hqTermName b n r =
  if Set.size (termsNamed b n) > 1
    then _hqTermName' b n r
    else HQ'.fromName n

_hqTypeName :: Names -> Name -> TypeReference -> HQ'.HashQualified Name
_hqTypeName b n r =
  if Set.size (typesNamed b n) > 1
    then _hqTypeName' b n r
    else HQ'.fromName n

_hqTypeAliases :: Names -> Name -> TypeReference -> Set (HQ'.HashQualified Name)
_hqTypeAliases b n r = Set.map (flip (_hqTypeName b) r) (typeAliases b n r)

_hqTermAliases :: Names -> Name -> Referent -> Set (HQ'.HashQualified Name)
_hqTermAliases b n r = Set.map (flip (_hqTermName b) r) (termAliases b n r)

-- Unconditionally apply hash qualifier long enough to distinguish all the
-- References in this Names.
hqTermName' :: Int -> Name -> Referent -> HQ'.HashQualified Name
hqTermName' hqLen n r =
  HQ'.take hqLen $ HQ'.fromNamedReferent n r

hqTypeName' :: Int -> Name -> TypeReference -> HQ'.HashQualified Name
hqTypeName' hqLen n r =
  HQ'.take hqLen $ HQ'.fromNamedReference n r

_hqTermName' :: Names -> Name -> Referent -> HQ'.HashQualified Name
_hqTermName' _b n r =
  HQ'.take numHashChars $ HQ'.fromNamedReferent n r

_hqTypeName' :: Names -> Name -> TypeReference -> HQ'.HashQualified Name
_hqTypeName' _b n r =
  HQ'.take numHashChars $ HQ'.fromNamedReference n r

fromTerms :: [(Name, Referent)] -> Names
fromTerms ts = Names (R.fromList ts) mempty

fromTypes :: [(Name, TypeReference)] -> Names
fromTypes ts = Names mempty (R.fromList ts)

fromTermsAndTypes :: [(Name, Referent)] -> [(Name, TypeReference)] -> Names
fromTermsAndTypes terms types =
  fromTerms terms <> fromTypes types

-- | Map over each name in a 'Names'.
mapNames :: (Name -> Name) -> Names -> Names
mapNames f Names {terms, types} =
  Names
    { terms = R.mapDom f terms,
      types = R.mapDom f types
    }

-- | @prefix0 n ns@ prepends @n@ to each name in @ns@.
--
-- Precondition: every name in @ns@ is relative.
prefix0 :: Name -> Names -> Names
prefix0 n =
  mapNames (Name.joinDot n)

filter :: (Name -> Bool) -> Names -> Names
filter f (Names terms types) = Names (R.filterDom f terms) (R.filterDom f types)

-- currently used for filtering before a conditional `add`
filterByHQs :: Set (HQ'.HashQualified Name) -> Names -> Names
filterByHQs hqs Names {..} = Names terms' types'
  where
    terms' = R.filter f terms
    types' = R.filter g types
    f (n, r) = any (HQ'.matchesNamedReferent n r) hqs
    g (n, r) = any (HQ'.matchesNamedReference n r) hqs

filterBySHs :: Set ShortHash -> Names -> Names
filterBySHs shs Names {..} = Names terms' types'
  where
    terms' = R.filter f terms
    types' = R.filter g types
    f (_n, r) = any (`SH.isPrefixOf` Referent.toShortHash r) shs
    g (_n, r) = any (`SH.isPrefixOf` Reference.toShortHash r) shs

filterTypes :: (Name -> Bool) -> Names -> Names
filterTypes f (Names terms types) = Names terms (R.filterDom f types)

difference :: Names -> Names -> Names
difference a b =
  Names
    (R.difference a.terms b.terms)
    (R.difference a.types b.types)

contains :: Names -> Reference -> Bool
contains names =
  -- We want to compute `termsReferences` only once, if `contains` is partially applied to a `Names`, and called over
  -- and over for different references. GHC would probably float `termsReferences` out without the explicit lambda, but
  -- it's written like this just to be sure.
  \r -> Set.member r termsReferences || R.memberRan r names.types
  where
    -- this check makes `contains` O(n) instead of O(log n)
    termsReferences :: Set TermReference
    termsReferences =
      Set.map Referent.toReference (R.ran names.terms)

-- | filters out everything from the domain except what's conflicted
conflicts :: Names -> Names
conflicts Names {..} = Names (R.filterManyDom terms) (R.filterManyDom types)

-- Deletes from the `n0 : Names` any definitions whose names
-- are in `ns`. Does so using logarithmic time lookups,
-- traversing only `ns`.
--
-- See usage in `FileParser` for handling precendence of symbol
-- resolution where local names are preferred to codebase names.
shadowTerms :: [Name] -> Names -> Names
shadowTerms ns n0 = Names terms' n0.types
  where
    terms' = foldl' go n0.terms ns
    go ts name = R.deleteDom name ts

-- | Given a mapping from name to qualified name, update a `Names`,
-- so for instance if the input has [(Some, Optional.Some)],
-- and `Optional.Some` is a constructor in the input `Names`,
-- the alias `Some` will map to that same constructor and shadow
-- anything else that is currently called `Some`.
importing :: [(Name, Name)] -> Names -> Names
importing shortToLongName ns =
  Names
    (foldl' go ns.terms shortToLongName)
    (foldl' go ns.types shortToLongName)
  where
    go :: (Ord r) => Relation Name r -> (Name, Name) -> Relation Name r
    go m (shortname, qname) = case Name.searchByRankedSuffix qname m of
      s
        | Set.null s -> m
        | otherwise -> R.insertManyRan shortname s (R.deleteDom shortname m)

-- | Converts a wildcard import into a list of explicit imports, of the form
-- [(suffix, full)]. Example: if `io` contains two functions, `foo` and
-- `bar`, then `expandWildcardImport io` will produce
-- `[(foo, io.foo), (bar, io.bar)]`.
expandWildcardImport :: Name -> Names -> [(Name, Name)]
expandWildcardImport prefix ns =
  [(suffix, full) | Just (suffix, full) <- go <$> R.toList ns.terms]
    <> [(suffix, full) | Just (suffix, full) <- go <$> R.toList ns.types]
  where
    go :: (Name, a) -> Maybe (Name, Name)
    go (full, _) = do
      -- running example:
      --   prefix = Int
      --   full = builtin.Int.negate
      rem <- Name.suffixFrom prefix full
      -- rem = Int.negate
      suffix <- Name.stripNamePrefix prefix rem
      -- suffix = negate
      pure (suffix, full)

-- Finds all the constructors for the given type in the `Names`
constructorsForType :: TypeReference -> Names -> [(Name, Referent)]
constructorsForType r ns =
  let -- rather than searching all of names, we use the known possible forms
      -- that the constructors can take
      possibleDatas = [Referent.Con (ConstructorReference r cid) CT.Data | cid <- [0 ..]]
      possibleEffects = [Referent.Con (ConstructorReference r cid) CT.Effect | cid <- [0 ..]]
      trim [] = []
      trim (h : t) = case R.lookupRan h ns.terms of
        s
          | Set.null s -> []
          | otherwise -> [(n, h) | n <- toList s] ++ trim t
   in trim possibleEffects ++ trim possibleDatas

hashQualifyTermsRelation :: R.Relation Name Referent -> R.Relation (HQ.HashQualified Name) Referent
hashQualifyTermsRelation = hashQualifyRelation HQ.fromNamedReferent

hashQualifyTypesRelation :: R.Relation Name TypeReference -> R.Relation (HQ.HashQualified Name) TypeReference
hashQualifyTypesRelation = hashQualifyRelation HQ.fromNamedReference

hashQualifyRelation :: (Ord r) => (Name -> r -> HQ.HashQualified Name) -> R.Relation Name r -> R.Relation (HQ.HashQualified Name) r
hashQualifyRelation fromNamedRef rel = R.map go rel
  where
    go (n, r) =
      if Set.size (R.lookupDom n rel) > 1
        then (HQ.take numHashChars $ fromNamedRef n r, r)
        else (HQ.NameOnly n, r)

-- | "Leniently" view a Names as a NameTree
--
-- This function is "lenient" in the sense that it does not handle conflicted names with any smarts whatsoever. The
-- resulting nametree will simply contain one of the associated references of a conflicted name - we don't specify
-- which.
lenientToNametree :: Names -> Nametree (DefnsF (Map NameSegment) Referent TypeReference)
lenientToNametree names =
  alignWith
    ( \case
        This terms -> Defns {terms, types = Map.empty}
        That types -> Defns {terms = Map.empty, types}
        These terms types -> Defns {terms, types}
    )
    (lenientRelationToNametree names.terms)
    (lenientRelationToNametree names.types)
  where
    lenientRelationToNametree :: (Ord a) => Relation Name a -> Nametree (Map NameSegment a)
    lenientRelationToNametree =
      -- The partial `Set.findMin` is fine here because Relation.domain only has non-empty Set values. A NESet would be
      -- better.
      unflattenNametree . Map.map Set.findMin . Relation.domain

-- Given a namespace and locally-bound names that shadow it (i.e. from a Unison file that hasn't been typechecked yet),
-- determine what the name resolves to, per the usual suffix-matching rules (where local defnintions and direct
-- dependencies are preferred to indirect dependencies).
resolveName :: forall ref. (Ord ref, Show ref) => Relation Name ref -> Set Name -> Name -> Set (ResolvesTo ref)
resolveName namespace locals =
  \name ->
    let exactNamespaceMatches :: Set ref
        exactNamespaceMatches =
          Relation.lookupDom name namespace
        localsPlusNamespaceSuffixMatches :: Set (ResolvesTo ref)
        localsPlusNamespaceSuffixMatches =
          Name.searchByRankedSuffix name localsPlusNamespace
     in if
          | Set.member name locals -> Set.singleton (ResolvesToLocal name)
          | Set.size exactNamespaceMatches == 1 -> Set.mapMonotonic ResolvesToNamespace exactNamespaceMatches
          | otherwise -> localsPlusNamespaceSuffixMatches
  where
    localsPlusNamespace :: Relation Name (ResolvesTo ref)
    localsPlusNamespace =
      shadowing1
        ( List.foldl'
            (\acc name -> Relation.insert name (ResolvesToLocal name) acc)
            Relation.empty
            (Set.toList locals)
        )
        ( Relation.map
            (over _2 ResolvesToNamespace)
            namespace
        )

-- | Like 'resolveName', but include the names in the output.
resolveNameIncludingNames ::
  forall ref.
  (Ord ref, Show ref) =>
  Relation Name ref ->
  Set Name ->
  Name ->
  Relation Name (ResolvesTo ref)
resolveNameIncludingNames namespace locals =
  \name ->
    let exactNamespaceMatches :: Set ref
        exactNamespaceMatches =
          Relation.lookupDom name namespace
        localsPlusNamespaceSuffixMatches :: Relation Name (ResolvesTo ref)
        localsPlusNamespaceSuffixMatches =
          Name.filterByRankedSuffix name localsPlusNamespace
     in if
          | Set.member name locals -> Relation.singleton name (ResolvesToLocal name)
          | Set.size exactNamespaceMatches == 1 -> Relation.singleton name (ResolvesToNamespace (Set.findMin exactNamespaceMatches))
          | otherwise -> localsPlusNamespaceSuffixMatches
  where
    localsPlusNamespace :: Relation Name (ResolvesTo ref)
    localsPlusNamespace =
      shadowing1
        ( List.foldl'
            (\acc name -> Relation.insert name (ResolvesToLocal name) acc)
            Relation.empty
            (Set.toList locals)
        )
        ( Relation.map
            (over _2 ResolvesToNamespace)
            namespace
        )
