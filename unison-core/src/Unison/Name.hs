module Unison.Name
  ( Name,

    -- * Basic construction
    cons,
    snoc,
    joinDot,
    fromSegment,
    fromSegments,
    fromReverseSegments,

    -- * Basic queries
    countSegments,
    isAbsolute,
    isRelative,
    isPrefixOf,
    beginsWithSegment,
    endsWith,
    endsWithReverseSegments,
    endsWithSegments,
    stripReversedPrefix,
    tryStripReversedPrefix,
    reverseSegments,
    segments,
    suffixes,
    lastSegment,

    -- * Basic manipulation
    makeAbsolute,
    makeRelative,
    setPosition,
    parent,
    stripNamePrefix,
    unqualified,
    isUnqualified,

    -- * To organize later
    commonPrefix,
    preferShallowLibDepth,
    searchByRankedSuffix,
    searchBySuffix,
    filterBySuffix,
    filterByRankedSuffix,
    suffixifyByName,
    suffixifyByHash,
    suffixifyByHashName,
    sortByText,
    sortNamed,
    sortNames,
    splits,
    suffixFrom,

    -- * Re-exports
    module Unison.Util.Alphabetical,

    -- * Exported for testing
    compareSuffix,
  )
where

import Control.Lens (mapped, _1, _2)
import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.RFC5051 qualified as RFC5051
import Data.Set qualified as Set
import Unison.Name.Internal
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Position (Position (..))
import Unison.Prelude
import Unison.Util.Alphabetical (Alphabetical, compareAlphabetical)
import Unison.Util.List qualified as List
import Unison.Util.Relation qualified as R

-- | @compareSuffix x y@ compares the suffix of @y@ (in reverse segment order) that is as long as @x@ to @x@ (in reverse
-- segment order).
--
-- >>> compareSuffix "b.c" "a.b.c"
-- EQ -- because [c,b] == [c,b]
--
-- >>> compareSuffix "b.c" "a.b.b"
-- LT -- because [b,b] < [c,b]
--
-- >>> compareSuffix "a.b.c" "b.c"
-- LT -- because [c,b] < [c,b,a]
--
-- >>> compareSuffix "b.b" "a.b.c"
-- GT -- because [c,b] > [b,b]
--
-- Used for suffix-based lookup of a name. For instance, given a @r : Relation Name x@,
-- @Relation.searchDom (compareSuffix "foo.bar") r@ will find all @r@ whose name has @foo.bar@ as a suffix.
--
-- This is only exported for testing; use 'searchBySuffix' or 'shortestUniqueSuffix' instead.
--
-- /O(n)/, where /n/ is the number of name segments.
compareSuffix :: Name -> Name -> Ordering
compareSuffix (Name _ ss0) =
  foldr f (const EQ) ss0 . List.NonEmpty.toList . reverseSegments
  where
    f :: NameSegment -> ([NameSegment] -> Ordering) -> ([NameSegment] -> Ordering)
    f x acc = \case
      [] -> LT
      y : ys -> compare y x <> acc ys

-- | Cons a name segment onto the head of a relative name. Not monotonic with respect to ordering! It is not safe to use
-- @cons s@ as the first argument to @Map.mapKeysMonotonic@!
--
-- /Precondition/: the name is relative
--
-- /O(n)/, where /n/ is the number of segments.
cons :: (HasCallStack) => NameSegment -> Name -> Name
cons x name =
  case name of
    Name Absolute _ ->
      error $
        reportBug
          "E495986"
          ("cannot cons " ++ show x ++ " onto absolute name" ++ show name)
    Name Relative (y :| ys) -> Name Relative (y :| ys ++ [x])

-- | Snoc a name segment onto the end of a name.
--
-- /O(1)/.
snoc :: Name -> NameSegment -> Name
snoc (Name pos (s1 :| ss)) s0 =
  Name pos (s0 :| s1 : ss)

-- | Return the number of name segments in a name.
--
-- /O(n)/, where /n/ is the number of name segments.
countSegments :: Name -> Int
countSegments (Name _ ss) =
  length ss

-- | Is this name relative?
--
-- /O(1)/.
isRelative :: Name -> Bool
isRelative = \case
  Name Absolute _ -> False
  Name Relative _ -> True

-- | @beginsWithSegment name segment@ returns whether @name@'s first name segment is @segment@.
--
-- >>> beginsWithSegment "abc.def" "abc"
-- True
--
-- >>> beginsWithSegment "abc.def" "ab"
-- False
--
-- /O(n)/, where /n/ is the number of name segments.
beginsWithSegment :: Name -> NameSegment -> Bool
beginsWithSegment name segment =
  segment == List.NonEmpty.head (segments name)

-- | @endsWithSegments x y@ returns whether @x@ ends with @y@.
--
-- >>> endsWithSegments "a.b.c" ["b", "c"]
-- True
--
-- >>> endsWithSegments "a.b.c" ["d"]
-- False
--
-- >>> endsWithSegments "a.b.c" []
-- True
--
-- /O(n)/, where /n/ is the number of name segments.
endsWithSegments :: Name -> [NameSegment] -> Bool
endsWithSegments name ss =
  endsWithReverseSegments name (reverse ss)

-- | Like 'endsWithSegments', but accepts a list of name segments in reverse order.
--
-- Slightly more efficient than 'endsWithSegments'.
--
-- >>> endsWithReverseSegments "a.b.c" ["c", "b"]
-- True
endsWithReverseSegments :: Name -> [NameSegment] -> Bool
endsWithReverseSegments (Name _ ss0) ss1 =
  List.NonEmpty.isPrefixOf ss1 ss0

-- >>> endsWith "a.b.c" "b.c"
-- True
endsWith :: Name -> Name -> Bool
endsWith overall suffix = endsWithReverseSegments overall (toList $ reverseSegments suffix)

-- >>> stripReversedPrefix (fromReverseSegments ("c" :| ["b", "a"])) ["b", "a"]
-- Just (Name Relative (NameSegment {toText = "c"} :| []))
-- >>> stripReversedPrefix (fromReverseSegments ("y" :| ["x"])) ["b", "a"]
-- Nothing
--
-- >>> stripReversedPrefix (fromReverseSegments ("c" :| ["b", "a"])) ["b", "a"]
-- Just (Name Relative (NameSegment {toText = "c"} :| []))
stripReversedPrefix :: Name -> [NameSegment] -> Maybe Name
stripReversedPrefix (Name p segs) suffix = do
  stripped <- List.stripSuffix suffix (toList segs)
  nonEmptyStripped <- List.NonEmpty.nonEmpty stripped
  pure $ Name p nonEmptyStripped

-- | Like 'stripReversedPrefix' but if the prefix doesn't match, or if it would strip the
-- entire name away just return the original name.
--
-- >>> tryStripReversedPrefix (fromReverseSegments ("c" :| ["b", "a"])) ["b", "a"]
-- Name Relative (NameSegment {toText = "c"} :| [])
-- >>> tryStripReversedPrefix (fromReverseSegments ("y" :| ["x"])) ["b", "a"]
-- Name Relative (NameSegment {toText = "y"} :| [NameSegment {toText = "x"}])
--
-- >>> tryStripReversedPrefix (fromReverseSegments ("c" :| ["b", "a"])) ["b", "a"]
-- Name Relative (NameSegment {toText = "c"} :| [])
tryStripReversedPrefix :: Name -> [NameSegment] -> Name
tryStripReversedPrefix n s = fromMaybe n (stripReversedPrefix n s)

-- | @isPrefixOf x y@ returns whether @x@ is a prefix of (or equivalent to) @y@, which is false if one name is relative
-- and the other is absolute.
--
-- >>> isPrefixOf "a.b" "a.b.c"
-- True
--
-- >>> isPrefixOf "a.b.c" "a.b.c"
-- True
--
-- >>> isPrefixOf ".a.b" "a.b.c"
-- False
--
-- /O(n)/, where /n/ is the number of name segments.
isPrefixOf :: Name -> Name -> Bool
isPrefixOf (Name p0 ss0) (Name p1 ss1) =
  p0 == p1 && List.isPrefixOf (reverse (toList ss0)) (reverse (toList ss1))

joinDot :: (HasCallStack) => Name -> Name -> Name
joinDot n1@(Name p0 ss0) n2@(Name p1 ss1) =
  case p1 of
    Relative -> Name p0 (ss1 <> ss0)
    Absolute ->
      error $
        reportBug
          "E261635"
          ( "joinDot: second name cannot be absolute. (name 1 = "
              ++ show n1
              ++ ", name 2 = "
              ++ show n2
              ++ ")"
          )

-- | Make a name absolute. No-op if the name is already absolute.
--
-- /O(1)/.
makeAbsolute :: Name -> Name
makeAbsolute = setPosition Absolute

-- | Make a name relative. No-op if the name is already relative.
--
-- /O(1)/.
makeRelative :: Name -> Name
makeRelative = setPosition Relative

-- | Overwrite a name's position.
-- This only changes the name's tag, it performs no manipulations to
-- the segments of the name.
--
-- /O(1)/.
setPosition :: Position -> Name -> Name
setPosition pos (Name _ ss) =
  Name pos ss

-- | Compute the "parent" of a name, unless the name is only a single segment, in which case it has no parent.
--
-- >>> parent "a.b.c"
-- Just "a.b"
--
-- >>> parent ".a.b.c"
-- Just ".a.b"
--
-- >>> parent "a"
-- Nothing
parent :: Name -> Maybe Name
parent (Name p ss0) =
  Name p <$> List.NonEmpty.nonEmpty (List.NonEmpty.tail ss0)

-- | Construct a relative name from a name segment.
--
-- /O(1)/.
fromSegment :: NameSegment -> Name
fromSegment s =
  Name Relative (s :| [])

-- | Construct a relative name from a list of name segments.
--
-- >>> fromSegments ("a" :| ["b", "c"])
-- "a.b.c"
--
-- /O(n)/, where /n/ is the number of name segments.
fromSegments :: NonEmpty NameSegment -> Name
fromSegments ss =
  Name Relative (List.NonEmpty.reverse ss)

-- | Construct a relative name from a list of name segments which are in reverse order
--
-- >>> fromReverseSegments ("c" :| ["b", "a"])
-- a.b.c
--
-- /O(1)/
fromReverseSegments :: NonEmpty NameSegment -> Name
fromReverseSegments rs =
  Name Relative rs

-- | Return the name segments of a name, in reverse order.
--
-- >>> reverseSegments "a.b.c"
-- "c" :| ["b", "a"]
--
-- /O(1)/.
reverseSegments :: Name -> NonEmpty NameSegment
reverseSegments (Name _ ss) =
  ss

-- | Return the final segment of a name.
--
-- >>> lastSegment (fromSegments ("base" :| ["List", "map"]))
-- NameSegment {toText = "map"}
lastSegment :: Name -> NameSegment
lastSegment = List.NonEmpty.head . reverseSegments

-- If there's no exact matches for `suffix` in `rel`, find all
-- `r` in `rel` whose corresponding name `suffix` as a suffix.
-- For example, `searchBySuffix List.map {(base.List.map, r1)}`
-- will return `{r1}`.
--
-- NB: Implementation uses logarithmic time lookups, not a linear scan.
searchBySuffix :: (Ord r) => Name -> R.Relation Name r -> Set r
searchBySuffix suffix rel =
  R.lookupDom suffix rel `orElse` R.searchDom (compareSuffix suffix) rel
  where
    orElse s1 s2 = if Set.null s1 then s2 else s1

-- | Like 'searchBySuffix', but also keeps the names around.
filterBySuffix :: (Ord r) => Name -> R.Relation Name r -> R.Relation Name r
filterBySuffix suffix rel =
  case Map.lookup suffix (R.domain rel) of
    Just refs -> R.fromManyRan suffix refs
    Nothing -> R.searchDomG R.fromManyRan (compareSuffix suffix) rel

-- Like `searchBySuffix`, but prefers local (outside `lib`) and direct (one `lib` deep) names to indirect (two or more
-- `lib` deep) names.
searchByRankedSuffix :: (Ord r) => Name -> R.Relation Name r -> Set r
searchByRankedSuffix suffix rel =
  let rs = searchBySuffix suffix rel
   in case Set.size rs <= 1 of
        True -> rs
        False ->
          let ok name = compareSuffix suffix name == EQ
              withNames = map (\r -> (filter ok (toList (R.lookupRan r rel)), r)) (toList rs)
           in preferShallowLibDepth withNames

-- | Like 'searchByRankedSuffix', but also keeps the names around.
filterByRankedSuffix :: (Ord r) => Name -> R.Relation Name r -> R.Relation Name r
filterByRankedSuffix suffix rel =
  let matches = filterBySuffix suffix rel
      highestNamePriority = foldMap prio (R.dom matches)
      keep (name, _) = prio name <= highestNamePriority
   in -- Keep only names that are at or less than the highest name priority. This effectively throws out all indirect
      -- dependencies (NamePriorityTwo) if there are any direct dependencies (NamePriorityOne) or local definitions
      -- (also NamePriorityOne).
      R.filter keep matches
  where
    prio = nameLocationPriority . classifyNameLocation

-- | precondition: input list is deduped, and so is the Name list in
-- the tuple
preferShallowLibDepth :: (Ord r) => [([Name], r)] -> Set r
preferShallowLibDepth = \case
  [] -> Set.empty
  [x] -> Set.singleton (snd x)
  rs ->
    let byPriority = List.multimap (map (first minLibs) rs)
        minLibs [] = NamePriorityOne ()
        minLibs ns = minimum (map (nameLocationPriority . classifyNameLocation) ns)
     in case Map.lookup (NamePriorityOne ()) byPriority <|> Map.lookup (NamePriorityTwo ()) byPriority of
          Nothing -> Set.fromList (map snd rs)
          Just rs -> Set.fromList rs

data NameLocation
  = NameLocation'Local -- outside lib
  | NameLocation'DirectDep -- inside lib, but outside lib.*.lib
  | NameLocation'IndirectDep -- inside lib.*.lib

classifyNameLocation :: Name -> NameLocation
classifyNameLocation name =
  case segments name of
    ((== NameSegment.libSegment) -> True) :| _ : ((== NameSegment.libSegment) -> True) : _ -> NameLocation'IndirectDep
    ((== NameSegment.libSegment) -> True) :| _ -> NameLocation'DirectDep
    _ -> NameLocation'Local

data NamePriority a
  = NamePriorityOne !a -- highest priority: local names and direct dep names
  | NamePriorityTwo !a -- lowest priority: indirect dep names
  deriving stock (Eq, Functor, Ord)

instance (Monoid a) => Monoid (NamePriority a) where
  mempty = NamePriorityTwo mempty

instance (Semigroup a) => Semigroup (NamePriority a) where
  NamePriorityOne x <> NamePriorityOne y = NamePriorityOne (x <> y)
  NamePriorityOne x <> NamePriorityTwo _ = NamePriorityOne x
  NamePriorityTwo _ <> NamePriorityOne y = NamePriorityOne y
  NamePriorityTwo x <> NamePriorityTwo y = NamePriorityTwo (x <> y)

unNamePriority :: NamePriority a -> a
unNamePriority = \case
  NamePriorityOne x -> x
  NamePriorityTwo x -> x

nameLocationPriority :: NameLocation -> NamePriority ()
nameLocationPriority = \case
  NameLocation'Local -> NamePriorityOne ()
  NameLocation'DirectDep -> NamePriorityOne ()
  NameLocation'IndirectDep -> NamePriorityTwo ()

sortByText :: (a -> Text) -> [a] -> [a]
sortByText by as =
  let as' = [(a, by a) | a <- as]
      comp (_, s) (_, s2) = RFC5051.compareUnicode s s2
   in fst <$> List.sortBy comp as'

sortNamed :: (Name -> Text) -> (a -> Name) -> [a] -> [a]
sortNamed toText f =
  sortByText (toText . f)

sortNames :: (Name -> Text) -> [Name] -> [Name]
sortNames toText =
  sortNamed toText id

-- | Return all "splits" of a relative name, which pair a possibly-empty prefix of name segments with a suffix, such
-- that the original name is equivalent to @prefix + suffix@.
--
-- Note: always returns a non-empty list, but (currently) does not use @NonEmpty@ for convenience, as none of the
-- call-sites care if the list is empty or not.
--
-- @
-- > splits foo.bar.baz
--
--   prefix    suffix
--   ------    ------
--   ∅         foo.bar.baz
--   foo       bar.baz
--   foo.bar   baz
-- @
--
-- /Precondition/: the name is relative.
splits :: (HasCallStack) => Name -> [([NameSegment], Name)]
splits (Name p ss0) =
  ss0
    & List.NonEmpty.toList
    & reverse
    & splits0
    & over (mapped . _2) (Name p . List.NonEmpty.reverse)
  where
    -- splits a.b.c
    -- ([], a.b.c) : over (mapped . _1) (a.) (splits b.c)
    -- ([], a.b.c) : over (mapped . _1) (a.) (([], b.c) : over (mapped . _1) (b.) (splits c))
    -- [([], a.b.c), ([a], b.c), ([a.b], c)]
    splits0 :: (HasCallStack) => [a] -> [([a], NonEmpty a)]
    splits0 = \case
      [] -> []
      [x] -> [([], x :| [])]
      x : xs -> ([], x :| xs) : over (mapped . _1) (x :) (splits0 xs)

-- | @stripNamePrefix x y@ strips prefix @x@ from name @y@, and returns the resulting name. Returns @Nothing@ @x@ is not
-- a proper (meaning shorter-than) prefix of @y@.
--
-- >>> stripNamePrefix "a.b" "a.b.c"
-- Just "c"
--
-- >>> stripNamePrefix ".a.b" "a.b.c"
-- Nothing
--
-- >>> stripNamePrefix "a.b.c" "a.b.c"
-- Nothing
stripNamePrefix :: Name -> Name -> Maybe Name
stripNamePrefix (Name p0 ss0) (Name p1 ss1) = do
  guard (p0 == p1)
  s : ss <- List.stripPrefix (reverse (toList ss0)) (reverse (toList ss1))
  pure (Name Relative (List.NonEmpty.reverse (s :| ss)))

-- | Return all relative suffixes of a name, in ascending-length order. The returned list will always be non-empty.
--
-- >>> suffixes "a.b.c"
-- ["a.b.c", "a.b", "c"]
--
-- >>> suffixes ".a.b.c"
-- ["a.b.c", "a.b", "c"]
suffixes :: Name -> [Name]
suffixes (Name _ ss0) = do
  ss <- List.NonEmpty.tail (List.NonEmpty.inits ss0)
  -- fromList is safe here because all elements of `tail . inits` are non-empty
  pure (Name Relative (List.NonEmpty.fromList ss))

-- suffixFrom Int builtin.Int.+ ==> Int.+
-- suffixFrom Int Int.negate    ==> Int.negate
--
-- Currently used as an implementation detail of expanding wildcard
-- imports, (like `use Int` should catch `builtin.Int.+`)
-- but it may be generally useful elsewhere. See `expandWildcardImports`
-- for details.
suffixFrom :: Name -> Name -> Maybe Name
suffixFrom (Name p0 ss0) (Name _ ss1) = do
  -- it doesn't make sense to pass an absolute name as the first arg
  Relative <- Just p0
  s : ss <- align (toList ss0) (toList ss1)
  -- the returned name is always relative... right?
  pure (Name Relative (s :| ss))
  where
    -- Slide the first non-empty list along the second; if there's a match, return the prefix of the second that ends on
    -- that match.
    --
    -- align [a,b] [x,a,b,y] = Just [x,a,b]
    align :: forall a. (Eq a) => [a] -> [a] -> Maybe [a]
    align xs =
      go id
      where
        go :: ([a] -> [a]) -> [a] -> Maybe [a]
        go prepend = \case
          [] -> Nothing
          ys0@(y : ys) ->
            if List.isPrefixOf xs ys0
              then Just (prepend xs)
              else go (prepend . (y :)) ys

-- | Drop all leading segments from a name, retaining only the last segment as a relative name.
--
-- >>> unqualified "a.b.c"
-- "c"
--
-- >>> unqualified ".a.b.c"
-- "c"
unqualified :: Name -> Name
unqualified (Name _ (s :| _)) =
  Name Relative (s :| [])

isUnqualified :: Name -> Bool
isUnqualified = \case
  Name Relative (_ :| []) -> True
  Name _ (_ :| _) -> False

-- Tries to shorten `fqn` to the smallest suffix that still unambiguously refers to the same name.
--
-- Indirect dependency names don't cause ambiguity in the presence of one or more non-indirect-dependency names. For
-- example, if there are two names "lib.base.List.map" and "lib.something.lib.base.Set.map", then "map" would
-- unambiguously refer to "lib.base.List.map".
--
-- Uses an efficient logarithmic lookup in the provided relation.
--
-- NB: Only works if the `Ord` instance for `Name` orders based on `Name.reverseSegments`.
suffixifyByName :: forall r. (Ord r) => Name -> R.Relation Name r -> Name
suffixifyByName fqn rel =
  fromMaybe fqn (List.find isOk (suffixes fqn))
  where
    isOk :: Name -> Bool
    isOk suffix = matchingNameCount == 1
      where
        matchingNameCount :: Int
        matchingNameCount =
          getSum (unNamePriority (R.searchDomG f (compareSuffix suffix) rel))
          where
            f :: Name -> Set r -> NamePriority (Sum Int)
            f name _refs =
              case nameLocationPriority (classifyNameLocation name) of
                NamePriorityOne () -> NamePriorityOne (Sum 1)
                NamePriorityTwo () -> NamePriorityTwo (Sum 1)

-- Tries to shorten `fqn` to the smallest suffix that still refers the same references.
--
-- Like `suffixifyByName`, indirect dependency names don't cause ambiguity in the presence of one or more
-- non-indirect-dependency names.
--
-- Uses an efficient logarithmic lookup in the provided relation. The returned `Name` may refer to multiple hashes if
-- the original FQN did as well.
--
-- NB: Only works if the `Ord` instance for `Name` orders based on `Name.reverseSegments`.
suffixifyByHash :: forall r. (Ord r) => Name -> R.Relation Name r -> Name
suffixifyByHash fqn rel =
  fromMaybe fqn (List.find isOk (suffixes fqn))
  where
    allRefs :: Set r
    allRefs =
      R.lookupDom fqn rel

    isOk :: Name -> Bool
    isOk suffix =
      matchingRefs == allRefs
      where
        matchingRefs :: Set r
        matchingRefs =
          unNamePriority (R.searchDomG f (compareSuffix suffix) rel)
          where
            f :: Name -> Set r -> NamePriority (Set r)
            f name refs =
              refs <$ nameLocationPriority (classifyNameLocation name)

-- Like `suffixifyByHash`, but "keeps going" (i.e. keeps adding more segments, looking for the best name) if the current
-- suffix could refer to a local definition (i.e. outside lib). This is because such definitions could end up being
-- edited in a scratch file, where "suffixify by hash" doesn't work.
suffixifyByHashName :: forall r. (Ord r) => Name -> R.Relation Name r -> Name
suffixifyByHashName fqn rel =
  fromMaybe fqn (List.find isOk (suffixes fqn))
  where
    allRefs :: Set r
    allRefs =
      R.lookupDom fqn rel

    isOk :: Name -> Bool
    isOk suffix =
      matchingRefs == allRefs
        -- Don't use a suffix of 2+ aliases if any of then are non-local names
        && case numLocalNames of
          0 -> True
          1 -> numNonLocalNames == 0
          _ -> False
      where
        numLocalNames :: Int
        numNonLocalNames :: Int
        matchingRefs :: Set r
        (getSum -> numLocalNames, getSum -> numNonLocalNames, unNamePriority -> matchingRefs) =
          R.searchDomG f (compareSuffix suffix) rel
          where
            f :: Name -> Set r -> (Sum Int, Sum Int, NamePriority (Set r))
            f name refs =
              (numLocal, numNonLocal, refs <$ nameLocationPriority location)
              where
                location = classifyNameLocation name
                numLocal =
                  case location of
                    NameLocation'Local -> Sum 1
                    NameLocation'DirectDep -> Sum 0
                    NameLocation'IndirectDep -> Sum 0
                numNonLocal =
                  case location of
                    NameLocation'Local -> Sum 0
                    NameLocation'DirectDep -> Sum 1
                    NameLocation'IndirectDep -> Sum 1

-- | Returns the common prefix of two names as segments
--
-- Note: the returned segments are NOT reversed.
--
-- >>> commonPrefix "a.b.x" "a.b.y"
-- [a,b]
--
-- >>> commonPrefix "x.y.z" "a.b.c"
-- []
--
-- >>> commonPrefix "a.b.c" "a.b.c.d.e"
-- [a,b,c]
--
-- Must have equivalent positions or no there's no common prefix
-- >>> commonPrefix ".a.b.c" "a.b.c.d.e"
-- []
--
-- Prefix matches are performed at the *segment* level:
-- >>> commonPrefix "a.bears" "a.beats"
-- [a]
commonPrefix :: Name -> Name -> [NameSegment]
commonPrefix x@(Name p1 _) y@(Name p2 _)
  | p1 /= p2 = []
  | otherwise =
      commonPrefix' (toList $ segments x) (toList $ segments y)
  where
    commonPrefix' (a : as) (b : bs)
      | a == b = a : commonPrefix' as bs
    commonPrefix' _ _ = []
