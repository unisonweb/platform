module Unison.LSP.Util.IntersectionMap
  ( -- * Intersection map
    intersectionsFromList,
    intersectionsSingleton,
    IntersectionRange (..),
    IntersectionMap,
    smallestIntersection,

    -- * Keyed intersection map
    KeyedIntersectionMap,
    keyedFromList,
    keyedSingleton,
    keyedSmallestIntersection,
  )
where

import Data.List qualified as List
import Data.Map qualified as Map
import Language.LSP.Protocol.Types qualified as LSP
import Unison.Prelude
import Unison.Util.List (safeHead)

-- | An intersection map where intersections are partitioned by a key.
newtype KeyedIntersectionMap k pos a = KeyedIntersectionMap (Map k (IntersectionMap pos a))
  deriving stock (Show, Eq)

instance (Ord k, Ord pos) => Semigroup (KeyedIntersectionMap k pos a) where
  KeyedIntersectionMap a <> KeyedIntersectionMap b = KeyedIntersectionMap (Map.unionWith (<>) a b)

instance (Ord k, Ord pos) => Monoid (KeyedIntersectionMap k pos a) where
  mempty = KeyedIntersectionMap Map.empty

keyedFromList :: (Ord k, IntersectionRange pos) => [(k, ((pos, pos), a))] -> KeyedIntersectionMap k pos a
keyedFromList elems =
  KeyedIntersectionMap $
    elems
      & fmap (\(k, (range, v)) -> (k, intersectionsSingleton range v))
      & Map.fromListWith (<>)

keyedSingleton :: (Ord k, IntersectionRange pos) => k -> (pos, pos) -> a -> KeyedIntersectionMap k pos a
keyedSingleton k range a = keyedFromList [(k, (range, a))]

-- | NOTE: Assumes that ranges only NEST and never overlap, which is an invariant that should
-- be maintained by the ABT annotations.
--
-- Returns the value associated with the tightest span which intersects with the given position.
keyedSmallestIntersection :: (Ord k, IntersectionRange pos) => k -> pos -> KeyedIntersectionMap k pos a -> Maybe ((pos, pos), a)
keyedSmallestIntersection k p (KeyedIntersectionMap m) = do
  intersections <- Map.lookup k m
  smallestIntersection p intersections

newtype IntersectionMap pos a = IntersectionMap (Map (pos, pos) a)
  deriving stock (Show, Eq)

instance (Ord pos) => Semigroup (IntersectionMap pos a) where
  IntersectionMap a <> IntersectionMap b = IntersectionMap (a <> b)

instance (Ord pos) => Monoid (IntersectionMap pos a) where
  mempty = IntersectionMap mempty

-- | Class for types that can be used as ranges for intersection maps.
class Ord pos => IntersectionRange pos where
  intersects :: pos -> (pos, pos) -> Bool

  -- Returns true if the first bound is tighter than the second.
  isTighterThan :: (pos, pos) -> (pos, pos) -> Bool

instance IntersectionRange LSP.Position where
  intersects (LSP.Position l c) ((LSP.Position lStart cStart), (LSP.Position lEnd cEnd)) =
    (l >= lStart && l <= lEnd)
      && if
          | l == lStart && l == lEnd -> c >= cStart && c <= cEnd
          | l == lStart -> c >= cStart
          | l == lEnd -> c <= cEnd
          | otherwise -> True

  ((LSP.Position lStartA cStartA), (LSP.Position lEndA cEndA)) `isTighterThan` ((LSP.Position lStartB cStartB), (LSP.Position lEndB cEndB)) =
    if lStartA == lStartB && lEndA == lEndB
      then cStartA >= cStartB && cEndA <= cEndB
      else lStartA >= lStartB && lEndA <= lEndB

-- | Construct an intersection map from a list of ranges and values.
-- Duplicates are dropped.
intersectionsFromList :: (Ord pos) => [((pos, pos), a)] -> IntersectionMap pos a
intersectionsFromList elems =
  IntersectionMap $ Map.fromList elems

intersectionsSingleton :: (pos, pos) -> a -> IntersectionMap pos a
intersectionsSingleton range a = IntersectionMap $ Map.singleton range a

-- | NOTE: Assumes that ranges only NEST and never overlap, which is an invariant that should
-- be maintained by the ABT annotations.
--
-- Returns the value associated with the tightest span which intersects with the given position.
--
-- >>> smallestIntersection (LSP.Position 5 1) (intersectionsFromList [((LSP.Position 1 1, LSP.Position 3 1), "a"), ((LSP.Position 2 1, LSP.Position 8 1), "b"), ((LSP.Position 4 1, LSP.Position 6 1), "c")])
-- Just ((Position {_line = 4, _character = 1},Position {_line = 6, _character = 1}),"c")
-- >>> smallestIntersection (LSP.Position 5 3) (intersectionsFromList [((LSP.Position 1 1, LSP.Position 3 1), "a"), ((LSP.Position 4 2, LSP.Position 6 5), "b"), ((LSP.Position 4 1, LSP.Position 6 6), "c"), ((LSP.Position 7 1, LSP.Position 9 1), "d")])
-- Just ((Position {_line = 4, _character = 2},Position {_line = 6, _character = 5}),"b")
smallestIntersection :: IntersectionRange pos => pos -> IntersectionMap pos a -> Maybe ((pos, pos), a)
smallestIntersection p (IntersectionMap bounds) =
  bounds
    & Map.filterWithKey (\b _ -> p `intersects` b)
    & Map.toList
    & List.sortBy cmp
    & safeHead
  where
    cmp (a, _) (b, _) =
      if a `isTighterThan` b
        then LT
        else GT
