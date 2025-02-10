module Unison.Util.Nametree
  ( -- * Nametree
    Nametree (..),
    traverseNametreeWithName,
    unfoldNametree,
    unionWith,

    -- ** Flattening and unflattening
    flattenNametree,
    flattenNametrees,
    unflattenNametree,
    unflattenNametrees,
  )
where

import Data.List.NonEmpty (NonEmpty, pattern (:|))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Strict qualified as Map
import Data.Semialign (Semialign (alignWith), Unzip (unzipWith), Zip (zipWith))
import Data.These (These (..), these)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF)
import Prelude hiding (zipWith)

-- | A nametree has a value, and a collection of children nametrees keyed by name segment.
data Nametree a = Nametree
  { value :: !a,
    children :: !(Map NameSegment (Nametree a))
  }
  deriving stock (Functor, Foldable, Traversable, Generic, Show)

unionWith :: (a -> a -> a) -> Nametree a -> Nametree a -> Nametree a
unionWith f (Nametree x xs) (Nametree y ys) =
  Nametree (f x y) (Map.unionWith (unionWith f) xs ys)

instance (Semigroup a) => Semigroup (Nametree a) where
  (<>) = unionWith (<>)

instance (Monoid a) => Monoid (Nametree a) where
  mempty = Nametree mempty mempty

instance Semialign Nametree where
  alignWith :: (These a b -> c) -> Nametree a -> Nametree b -> Nametree c
  alignWith f (Nametree x xs) (Nametree y ys) =
    Nametree (f (These x y)) (alignWith (these (fmap (f . This)) (fmap (f . That)) (alignWith f)) xs ys)

instance Zip Nametree where
  zipWith :: (a -> b -> c) -> Nametree a -> Nametree b -> Nametree c
  zipWith f (Nametree x xs) (Nametree y ys) =
    Nametree (f x y) (zipWith (zipWith f) xs ys)

instance Unzip Nametree where
  unzipWith :: (c -> (a, b)) -> Nametree c -> (Nametree a, Nametree b)
  unzipWith f (Nametree x xs) =
    (Nametree y ys, Nametree z zs)
    where
      (y, z) = f x
      (ys, zs) = unzipWith (unzipWith f) xs

-- | Traverse over a nametree, with access to the list of name segments (in reverse order) leading to each value.
traverseNametreeWithName :: (Applicative f) => ([NameSegment] -> a -> f b) -> Nametree a -> f (Nametree b)
traverseNametreeWithName f =
  go []
  where
    go names (Nametree x xs) =
      Nametree <$> f names x <*> Map.traverseWithKey (\name -> go (name : names)) xs

-- | Build a nametree from a seed value.
unfoldNametree :: (a -> (b, Map NameSegment a)) -> a -> Nametree b
unfoldNametree f x =
  let (y, ys) = f x
   in Nametree y (unfoldNametree f <$> ys)

-- | 'flattenNametree' organizes a nametree like
--
-- > "foo" = #foo
-- > "foo": {
-- >   "bar" = #bar
-- >   "bar": {
-- >     "baz" = #baz
-- >   }
-- > }
--
-- into an equivalent-but-flat association between names and definitions, like
--
-- > {
-- >   "foo" = #bar,
-- >   "foo.bar" = #bar,
-- >   "foo.bar.baz" = #baz
-- > }
flattenNametree ::
  forall a b.
  (Ord b) =>
  (a -> Map NameSegment b) ->
  Nametree a ->
  BiMultimap b Name
flattenNametree f =
  go []
  where
    go :: [NameSegment] -> Nametree a -> BiMultimap b Name
    go prefix (Nametree node children) =
      foldr
        ( \(name, child) ->
            -- This union is safe because the keys are disjoint
            BiMultimap.unsafeUnion (go (name : prefix) child)
        )
        ( BiMultimap.fromRange
            ( Map.mapKeysMonotonic
                (\name -> Name.fromReverseSegments (name :| prefix))
                (f node)
            )
        )
        (Map.toList children)

-- | Like 'flattenNametree', but works on both the types and terms namespace at once.
flattenNametrees ::
  (Ord term, Ord typ) =>
  Nametree (DefnsF (Map NameSegment) term typ) ->
  Defns (BiMultimap term Name) (BiMultimap typ Name)
flattenNametrees defns =
  Defns
    { terms = flattenNametree (view #terms) defns,
      types = flattenNametree (view #types) defns
    }

-- | 'unflattenNametree' organizes an association between names and definitions like
--
-- > {
-- >   "foo" = #bar,
-- >   "foo.bar" = #bar,
-- >   "foo.bar.baz" = #baz
-- > }
--
-- into an equivalent-but-less-flat nametree, like
--
-- > "foo" = #foo
-- > "foo": {
-- >   "bar" = #bar
-- >   "bar": {
-- >     "baz" = #baz
-- >   }
-- > }
unflattenNametree :: (Ord a) => Map Name a -> Nametree (Map NameSegment a)
unflattenNametree =
  unfoldNametree unflattenLevel . map (first Name.segments) . Map.toList
  where
    unflattenLevel :: [(NonEmpty NameSegment, a)] -> (Map NameSegment a, Map NameSegment [(NonEmpty NameSegment, a)])
    unflattenLevel =
      foldl' phi (Map.empty, Map.empty)
      where
        phi (!accValue, !accChildren) = \case
          (NameHere n, v) -> (Map.insert n v accValue, accChildren)
          (NameThere n ns, v) -> (accValue, Map.insertWith (++) n [(ns, v)] accChildren)

-- | Like 'unflattenNametree', but works on both the types and terms namespace at once.
unflattenNametrees :: (Ord term, Ord typ) => DefnsF (Map Name) term typ -> Nametree (DefnsF (Map NameSegment) term typ)
unflattenNametrees defns =
  alignWith
    ( \case
        This terms -> Defns {terms, types = Map.empty}
        That types -> Defns {terms = Map.empty, types}
        These terms types -> Defns {terms, types}
    )
    (unflattenNametree defns.terms)
    (unflattenNametree defns.types)

-- Helper patterns for switching on "name here" (1 name segment) or "name there" (2+ name segments)

pattern NameHere :: a -> NonEmpty a
pattern NameHere x <- x :| (List.NonEmpty.nonEmpty -> Nothing)

pattern NameThere :: a -> NonEmpty a -> NonEmpty a
pattern NameThere x xs <- x :| (List.NonEmpty.nonEmpty -> Just xs)

{-# COMPLETE NameHere, NameThere #-}
