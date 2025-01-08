module Unison.Util.EnumContainers
  ( EnumMap,
    EnumSet,
    EnumKey (..),
    mapNull,
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
import Prelude hiding (lookup, null)

class EnumKey k where
  keyToInt :: k -> Int
  intToKey :: Int -> k

instance EnumKey Word64 where
  {-# INLINE keyToInt #-}
  keyToInt e = fromIntegral e
  {-# INLINE intToKey #-}
  intToKey i = fromIntegral i

instance EnumKey Word16 where
  {-# INLINE keyToInt #-}
  keyToInt e = fromIntegral e
  {-# INLINE intToKey #-}
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

{-# INLINE mapNull #-}
mapNull :: EnumMap k a -> Bool
mapNull (EM m) = IM.null m

{-# INLINE mapFromList #-}
mapFromList :: (EnumKey k) => [(k, a)] -> EnumMap k a
mapFromList = EM . IM.fromList . fmap (first keyToInt)

{-# INLINE setFromList #-}
setFromList :: (EnumKey k) => [k] -> EnumSet k
setFromList = ES . IS.fromList . fmap keyToInt

{-# INLINE setToList #-}
setToList :: (EnumKey k) => EnumSet k -> [k]
setToList (ES s) = intToKey <$> IS.toList s

{-# INLINE mapSingleton #-}
mapSingleton :: (EnumKey k) => k -> a -> EnumMap k a
mapSingleton e a = EM $ IM.singleton (keyToInt e) a

{-# INLINE setSingleton #-}
setSingleton :: (EnumKey k) => k -> EnumSet k
setSingleton e = ES . IS.singleton $ keyToInt e

{-# INLINE mapInsert #-}
mapInsert :: (EnumKey k) => k -> a -> EnumMap k a -> EnumMap k a
mapInsert e x (EM m) = EM $ IM.insert (keyToInt e) x m

{-# INLINE unionWith #-}
unionWith ::
  (EnumKey k) =>
  (a -> a -> a) ->
  EnumMap k a ->
  EnumMap k a ->
  EnumMap k a
unionWith f (EM l) (EM r) = EM $ IM.unionWith f l r

{-# INLINE intersectionWith #-}
intersectionWith ::
  (a -> b -> c) ->
  EnumMap k a ->
  EnumMap k b ->
  EnumMap k c
intersectionWith f (EM l) (EM r) = EM $ IM.intersectionWith f l r

{-# INLINE keys #-}
keys :: (EnumKey k) => EnumMap k a -> [k]
keys (EM m) = fmap intToKey . IM.keys $ m

{-# INLINE keysSet #-}
keysSet :: (EnumKey k) => EnumMap k a -> EnumSet k
keysSet (EM m) = ES (IM.keysSet m)

{-# INLINE restrictKeys #-}
restrictKeys :: (EnumKey k) => EnumMap k a -> EnumSet k -> EnumMap k a
restrictKeys (EM m) (ES s) = EM $ IM.restrictKeys m s

{-# INLINE withoutKeys #-}
withoutKeys :: (EnumKey k) => EnumMap k a -> EnumSet k -> EnumMap k a
withoutKeys (EM m) (ES s) = EM $ IM.withoutKeys m s

{-# INLINE mapDifference #-}
mapDifference :: (EnumKey k) => EnumMap k a -> EnumMap k b -> EnumMap k a
mapDifference (EM l) (EM r) = EM $ IM.difference l r

{-# INLINE member #-}
member :: (EnumKey k) => k -> EnumSet k -> Bool
member e (ES s) = IS.member (keyToInt e) s

{-# INLINE hasKey #-}
hasKey :: (EnumKey k) => k -> EnumMap k a -> Bool
hasKey k (EM m) = IM.member (keyToInt k) m

{-# INLINE lookup #-}
lookup :: (EnumKey k) => k -> EnumMap k a -> Maybe a
lookup e (EM m) = IM.lookup (keyToInt e) m

{-# INLINE lookupWithDefault #-}
lookupWithDefault :: (EnumKey k) => a -> k -> EnumMap k a -> a
lookupWithDefault d e (EM m) = IM.findWithDefault d (keyToInt e) m

{-# INLINE mapWithKey #-}
mapWithKey :: (EnumKey k) => (k -> a -> b) -> EnumMap k a -> EnumMap k b
mapWithKey f (EM m) = EM $ IM.mapWithKey (f . intToKey) m

{-# INLINE foldMapWithKey #-}
foldMapWithKey :: (EnumKey k) => (Monoid m) => (k -> a -> m) -> EnumMap k a -> m
foldMapWithKey f (EM m) = IM.foldMapWithKey (f . intToKey) m

{-# INLINE mapToList #-}
mapToList :: (EnumKey k) => EnumMap k a -> [(k, a)]
mapToList (EM m) = first intToKey <$> IM.toList m

{-# INLINE (!) #-}
(!) :: (EnumKey k) => EnumMap k a -> k -> a
(!) (EM m) e = m IM.! keyToInt e

{-# INLINE findMin #-}
findMin :: (EnumKey k) => EnumSet k -> k
findMin (ES s) = intToKey $ IS.findMin s

{-# INLINE traverseSet_ #-}
traverseSet_ ::
  (Applicative f) => (EnumKey k) => (k -> f ()) -> EnumSet k -> f ()
traverseSet_ f (ES s) =
  IS.foldr (\i r -> f (intToKey i) *> r) (pure ()) s

{-# INLINE interverse #-}
interverse ::
  (Applicative f) =>
  (a -> b -> f c) ->
  EnumMap k a ->
  EnumMap k b ->
  f (EnumMap k c)
interverse f (EM l) (EM r) =
  fmap EM . traverse id $ IM.intersectionWith f l r

{-# INLINE traverseWithKey #-}
traverseWithKey ::
  (Applicative f) =>
  (EnumKey k) =>
  (k -> a -> f b) ->
  EnumMap k a ->
  f (EnumMap k b)
traverseWithKey f (EM m) = EM <$> IM.traverseWithKey (f . intToKey) m

{-# INLINE setSize #-}
setSize :: EnumSet k -> Int
setSize (ES s) = IS.size s
