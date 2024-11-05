module Unison.Util.EnumContainers
  ( EnumMap,
    EnumSet,
    EnumKey (..),
    mapFromList,
    setFromList,
    setToList,
    mapSingleton,
    setSingleton,
    mapInsert,
    unionWith,
    intersectionWith,
    hasKey,
    keys,
    keysSet,
    restrictKeys,
    withoutKeys,
    mapDifference,
    member,
    lookup,
    lookupWithDefault,
    mapWithKey,
    foldMapWithKey,
    mapToList,
    (!),
    findMin,
    interverse,
    traverseSet_,
    traverseWithKey,
    setSize,
  )
where

import Data.Bifunctor
import Data.Functor.Classes (Eq1, Ord1)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Word (Word16, Word64)
import Prelude hiding (lookup)

class EnumKey k where
  keyToInt :: k -> Int
  intToKey :: Int -> k

instance EnumKey Word64 where
  keyToInt e = fromIntegral e
  intToKey i = fromIntegral i

instance EnumKey Word16 where
  keyToInt e = fromIntegral e
  intToKey i = fromIntegral i

newtype EnumMap k a = EM (IM.IntMap a)
  deriving stock
    ( Functor,
      Foldable,
      Traversable,
      Show,
      Eq,
      Ord
    )
  deriving newtype
    ( Monoid,
      Semigroup,
      Eq1,
      Ord1
    )

newtype EnumSet k = ES IS.IntSet
  deriving stock
    ( Show,
      Eq,
      Ord
    )
  deriving newtype
    ( Monoid,
      Semigroup
    )

mapFromList :: (EnumKey k) => [(k, a)] -> EnumMap k a
mapFromList = EM . IM.fromList . fmap (first keyToInt)

setFromList :: (EnumKey k) => [k] -> EnumSet k
setFromList = ES . IS.fromList . fmap keyToInt

setToList :: (EnumKey k) => EnumSet k -> [k]
setToList (ES s) = intToKey <$> IS.toList s

mapSingleton :: (EnumKey k) => k -> a -> EnumMap k a
mapSingleton e a = EM $ IM.singleton (keyToInt e) a

setSingleton :: (EnumKey k) => k -> EnumSet k
setSingleton e = ES . IS.singleton $ keyToInt e

mapInsert :: (EnumKey k) => k -> a -> EnumMap k a -> EnumMap k a
mapInsert e x (EM m) = EM $ IM.insert (keyToInt e) x m

unionWith ::
  (EnumKey k) =>
  (a -> a -> a) ->
  EnumMap k a ->
  EnumMap k a ->
  EnumMap k a
unionWith f (EM l) (EM r) = EM $ IM.unionWith f l r

intersectionWith ::
  (a -> b -> c) ->
  EnumMap k a ->
  EnumMap k b ->
  EnumMap k c
intersectionWith f (EM l) (EM r) = EM $ IM.intersectionWith f l r

keys :: (EnumKey k) => EnumMap k a -> [k]
keys (EM m) = fmap intToKey . IM.keys $ m

keysSet :: (EnumKey k) => EnumMap k a -> EnumSet k
keysSet (EM m) = ES (IM.keysSet m)

restrictKeys :: (EnumKey k) => EnumMap k a -> EnumSet k -> EnumMap k a
restrictKeys (EM m) (ES s) = EM $ IM.restrictKeys m s

withoutKeys :: (EnumKey k) => EnumMap k a -> EnumSet k -> EnumMap k a
withoutKeys (EM m) (ES s) = EM $ IM.withoutKeys m s

mapDifference :: (EnumKey k) => EnumMap k a -> EnumMap k b -> EnumMap k a
mapDifference (EM l) (EM r) = EM $ IM.difference l r

member :: (EnumKey k) => k -> EnumSet k -> Bool
member e (ES s) = IS.member (keyToInt e) s

hasKey :: (EnumKey k) => k -> EnumMap k a -> Bool
hasKey k (EM m) = IM.member (keyToInt k) m

lookup :: (EnumKey k) => k -> EnumMap k a -> Maybe a
lookup e (EM m) = IM.lookup (keyToInt e) m

lookupWithDefault :: (EnumKey k) => a -> k -> EnumMap k a -> a
lookupWithDefault d e (EM m) = IM.findWithDefault d (keyToInt e) m

mapWithKey :: (EnumKey k) => (k -> a -> b) -> EnumMap k a -> EnumMap k b
mapWithKey f (EM m) = EM $ IM.mapWithKey (f . intToKey) m

foldMapWithKey :: (EnumKey k) => (Monoid m) => (k -> a -> m) -> EnumMap k a -> m
foldMapWithKey f (EM m) = IM.foldMapWithKey (f . intToKey) m

mapToList :: (EnumKey k) => EnumMap k a -> [(k, a)]
mapToList (EM m) = first intToKey <$> IM.toList m

(!) :: (EnumKey k) => EnumMap k a -> k -> a
(!) (EM m) e = m IM.! keyToInt e

findMin :: (EnumKey k) => EnumSet k -> k
findMin (ES s) = intToKey $ IS.findMin s

traverseSet_ ::
  (Applicative f) => (EnumKey k) => (k -> f ()) -> EnumSet k -> f ()
traverseSet_ f (ES s) =
  IS.foldr (\i r -> f (intToKey i) *> r) (pure ()) s

interverse ::
  (Applicative f) =>
  (a -> b -> f c) ->
  EnumMap k a ->
  EnumMap k b ->
  f (EnumMap k c)
interverse f (EM l) (EM r) =
  fmap EM . traverse id $ IM.intersectionWith f l r

traverseWithKey ::
  (Applicative f) =>
  (EnumKey k) =>
  (k -> a -> f b) ->
  EnumMap k a ->
  f (EnumMap k b)
traverseWithKey f (EM m) = EM <$> IM.traverseWithKey (f . intToKey) m

setSize :: EnumSet k -> Int
setSize (ES s) = IS.size s
