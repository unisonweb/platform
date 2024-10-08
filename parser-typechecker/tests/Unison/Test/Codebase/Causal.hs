{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Test.Codebase.Causal (test) where

import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import EasyTest
import Unison.Codebase.Causal (Causal, one)
import Unison.Codebase.Causal qualified as Causal
import Unison.Hash qualified as Hash
import Unison.Hashing.V2 qualified as Hashing
import Unison.Prelude

-- Dummy instances for this test suite. Would probably be better if they weren't orphans.
instance Hashing.ContentAddressable Int64 where contentHash = Hash.fromByteString . Text.encodeUtf8 . tShow

instance Hashing.ContentAddressable (Set Int64) where contentHash = Hash.fromByteString . Text.encodeUtf8 . tShow

test :: Test ()
test =
  scope "causal"
    . tests
    $ [ scope "threeWayMerge.ex1"
          . expect
          $ Causal.head testThreeWay
            == Set.fromList [3, 4],
        scope "threeWayMerge.idempotent"
          . expect
          $ testIdempotent oneCausal, -- == oneCausal
          --  $  prop_mergeIdempotent
        scope "threeWayMerge.identity"
          . expect
          $ testIdentity oneCausal emptyCausal,
        --  $  prop_mergeIdentity
        scope "threeWayMerge.commutative"
          . expect
          $ testCommutative (Set.fromList [3, 4]) oneRemoved,
        --  $  prop_mergeCommutative
        {- , scope "threeWayMerge.commonAncestor"
        .  expect
        \$  testCommonAncestor
        -- $  prop_mergeCommonAncestor --}
        scope "lca.hasLca" lcaPairTest,
        scope "lca.noLca" noLcaPairTest
      ]

int64 :: Test Int64
int64 = random

extend ::
  Int ->
  Causal Identity Int64 ->
  Test (Causal Identity Int64)
extend 0 ca = pure ca
extend n ca = do
  i <- int64
  extend (n - 1) (Causal.cons i ca)

lcaPair :: Test (Causal Identity Int64, Causal Identity Int64)
lcaPair = do
  base <- one <$> int64
  ll <- int' 0 20
  lr <- int' 0 20
  (,) <$> extend ll base <*> extend lr base

lcaPairTest :: Test ()
lcaPairTest = replicateM_ 50 test >> ok
  where
    test =
      runIdentity . uncurry Causal.lca <$> lcaPair >>= \case
        Just _ -> pure ()
        Nothing -> crash "expected lca"

noLcaPair ::
  Test (Causal Identity Int64, Causal Identity Int64)
noLcaPair = do
  basel <- one <$> int64
  baser <- one <$> int64
  ll <- int' 0 20
  lr <- int' 0 20
  (,) <$> extend ll basel <*> extend lr baser

noLcaPairTest :: Test ()
noLcaPairTest = replicateM_ 50 test >> ok
  where
    test =
      runIdentity . uncurry Causal.lca <$> noLcaPair >>= \case
        Nothing -> pure ()
        Just _ -> crash "expected no lca"

oneRemoved :: Causal Identity (Set Int64)
oneRemoved =
  foldr
    Causal.cons
    (one (Set.singleton 1))
    (Set.fromList <$> [[2, 3, 4], [1, 2, 3, 4], [1, 2]])

twoRemoved :: Causal Identity (Set Int64)
twoRemoved =
  foldr
    Causal.cons
    (one (Set.singleton 1))
    (Set.fromList <$> [[1, 3, 4], [1, 2, 3], [1, 2]])

testThreeWay :: Causal Identity (Set Int64)
testThreeWay =
  runIdentity $
    threeWayMerge' oneRemoved twoRemoved

setCombine :: (Applicative m) => (Ord a) => Set a -> Set a -> m (Set a)
setCombine a b = pure $ a <> b

setDiff :: (Applicative m) => (Ord a) => Set a -> Set a -> m (Set a, Set a)
setDiff old new = pure (Set.difference new old, Set.difference old new)

setPatch :: (Applicative m) => (Ord a) => Set a -> (Set a, Set a) -> m (Set a)
setPatch s (added, removed) = pure (added <> Set.difference s removed)

-- merge x x == x, should not add a new head, and also the value at the head should be the same of course
testIdempotent :: Causal Identity (Set Int64) -> Bool -- Causal Identity (Set Int64)
testIdempotent causal =
  runIdentity (threeWayMerge' causal causal)
    == causal

-- prop_mergeIdempotent :: Bool
-- prop_mergeIdempotent = and (map testIdempotent (take 1000 generateRandomCausals))

oneCausal :: Causal Identity (Set Int64)
oneCausal = Causal.one (Set.fromList [1])

-- generateRandomCausals :: Causal Identity (Set Int64)
-- generateRandomCausals = undefined

easyCombine ::
  (Monad m, Semigroup d) =>
  (e -> e -> m e) ->
  (e -> e -> m d) ->
  (e -> d -> m e) ->
  (Maybe e -> e -> e -> m e)
easyCombine comb _ _ Nothing l r = comb l r
easyCombine _ diff appl (Just ca) l r = do
  dl <- diff ca l
  dr <- diff ca r
  appl ca (dl <> dr)

threeWayMerge' ::
  Causal Identity (Set Int64) ->
  Causal Identity (Set Int64) ->
  Identity (Causal Identity (Set Int64))
threeWayMerge' = Causal.threeWayMerge' Causal.lca (easyCombine setCombine setDiff setPatch)

-- merge x mempty == x, merge mempty x == x
testIdentity :: Causal Identity (Set Int64) -> Causal Identity (Set Int64) -> Bool
testIdentity causal mempty =
  (threeWayMerge' causal mempty)
    == (threeWayMerge' mempty causal)

emptyCausal :: Causal Identity (Set Int64)
emptyCausal = one (Set.empty)

-- merge (cons hd tl) tl == cons hd tl, merge tl (cons hd tl) == cons hd tl
testCommutative :: Set Int64 -> Causal Identity (Set Int64) -> Bool
testCommutative hd tl =
  (threeWayMerge' (Causal.cons hd tl) tl)
    == (threeWayMerge' tl (Causal.cons hd tl))
