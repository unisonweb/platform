-- | An API for merging together two collections of library dependencies.
module Unison.Merge.Libdeps
  ( LibdepDiffOp (..),
    diffLibdeps,
    mergeLibdepsDiffs,
    applyLibdepsDiff,
    getTwoFreshLibdepNames,
  )
where

import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.These (These (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay qualified as EitherWay
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoDiffOps (TwoDiffOps (..))
import Unison.Merge.TwoDiffOps qualified as TwoDiffOps
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.Updated (Updated (..))
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Prelude hiding (catMaybes)
import Unison.Util.Map qualified as Map
import Witherable (catMaybes)

------------------------------------------------------------------------------------------------------------------------
-- Diffing libdeps

data LibdepDiffOp a
  = AddLibdep !a
  | AddBothLibdeps !a !a
  | DeleteLibdep

-- | Perform two two-way diffs on two collections of library dependencies. This is only half of a three-way diff: use
-- 'mergeLibdepsDiffs' to complete it.
diffLibdeps ::
  forall k v.
  (Ord k, Eq v) =>
  -- | Library dependencies.
  ThreeWay (Map k v) ->
  -- | Library dependencies diffs.
  TwoWay (Map k (DiffOp v))
diffLibdeps libdeps =
  f <$> ThreeWay.forgetLca libdeps
  where
    f :: Map k v -> Map k (DiffOp v)
    f =
      Map.merge
        (Map.mapMissing \_ -> DiffOp'Delete)
        (Map.mapMissing \_ -> DiffOp'Add)
        ( Map.zipWithMaybeMatched \_ old new ->
            if old == new
              then Nothing
              else Just (DiffOp'Update Updated {old, new})
        )
        libdeps.lca

-- Merge two library dependency diffs together:
--
--   * Keep all adds/updates (allowing conflicts as necessary, which will be resolved later)
--   * Ignore deletes that only one party makes (because the other party may expect the dep to still be there)
mergeLibdepsDiffs ::
  forall k v.
  (Ord k, Eq v) =>
  -- The LCA->Alice and LCA->Bob library dependencies diffs.
  TwoWay (Map k (DiffOp v)) ->
  -- The merged library dependencies diff.
  Map k (LibdepDiffOp v)
mergeLibdepsDiffs diffs =
  catMaybes (alignWith combineDiffOps diffs.alice diffs.bob)

combineDiffOps :: (Eq a) => These (DiffOp a) (DiffOp a) -> Maybe (LibdepDiffOp a)
combineDiffOps =
  TwoDiffOps.make >>> combineDiffOps1

combineDiffOps1 :: (Eq a) => TwoDiffOps a -> Maybe (LibdepDiffOp a)
combineDiffOps1 = \case
  TwoDiffOps'Add new -> Just (AddLibdep (EitherWay.value new))
  -- If Alice deletes a dep and Bob doesn't touch it, ignore the delete, since Bob may still be using it.
  TwoDiffOps'Delete _old -> Nothing
  -- If Alice updates a dep and Bob doesn't touch it, keep the old one around too, since Bob may still be using it.
  TwoDiffOps'Update x -> Just (AddBothLibdeps (EitherWay.value x).old (EitherWay.value x).new)
  TwoDiffOps'AddAdd TwoWay {alice, bob}
    | alice == bob -> Just (AddLibdep alice)
    | otherwise -> Just (AddBothLibdeps alice bob)
  -- If Alice and Bob both delete something, delete it.
  TwoDiffOps'DeleteDelete _ -> Just DeleteLibdep
  -- If Alice updates a dependency and Bob deletes the old one, ignore the delete and keep Alice's, and vice versa.
  TwoDiffOps'DeleteUpdate bob -> Just (AddLibdep bob.new)
  TwoDiffOps'UpdateDelete alice -> Just (AddLibdep alice.new)
  -- combineDiffOps (Deleted _) (Updated _ bob) = AddLibdep bob
  -- combineDiffOps (Updated _ alice) (Deleted _) = AddLibdep alice
  TwoDiffOps'UpdateUpdate _old TwoWay {alice, bob}
    | alice == bob -> Just (AddLibdep alice)
    | otherwise -> Just (AddBothLibdeps alice bob)

------------------------------------------------------------------------------------------------------------------------
-- Applying libdeps diff

-- Apply a library dependencies diff to the LCA.
applyLibdepsDiff ::
  forall k v.
  (Ord k) =>
  -- | Freshen a name, e.g. "base" -> ("base__4", "base__5").
  (Set k -> k -> (k, k)) ->
  -- | Library dependencies.
  ThreeWay (Map k v) ->
  -- | Library dependencies diff.
  Map k (LibdepDiffOp v) ->
  -- | Merged library dependencies.
  Map k v
applyLibdepsDiff freshen0 libdeps =
  Map.mergeMap Map.singleton f (\name _ -> f name) libdeps.lca
  where
    f :: k -> LibdepDiffOp v -> Map k v
    f k = \case
      AddLibdep v -> Map.singleton k v
      AddBothLibdeps v1 v2 ->
        let (k1, k2) = freshen k
         in Map.fromList [(k1, v1), (k2, v2)]
      DeleteLibdep -> Map.empty

    freshen :: k -> (k, k)
    freshen =
      freshen0 $
        Set.unions
          [ Map.keysSet libdeps.lca,
            Map.keysSet libdeps.alice,
            Map.keysSet libdeps.bob
          ]

------------------------------------------------------------------------------------------------------------------------
-- Getting fresh libdeps names

-- Given a name like "base", try "base__1", then "base__2", etc, until we find a name that doesn't
-- clash with any existing dependencies.
getTwoFreshLibdepNames :: Set NameSegment -> NameSegment -> (NameSegment, NameSegment)
getTwoFreshLibdepNames names name0 =
  go2 0
  where
    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go2 4 = ("base__4", "base__7")
    go2 :: Integer -> (NameSegment, NameSegment)
    go2 !i
      | Set.member name names = go2 (i + 1)
      | otherwise = (name, go1 (i + 1))
      where
        name = mangled i

    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go1 5 = "base__7"
    go1 :: Integer -> NameSegment
    go1 !i
      | Set.member name names = go1 (i + 1)
      | otherwise = name
      where
        name = mangled i

    mangled :: Integer -> NameSegment
    mangled i =
      NameSegment (NameSegment.toUnescapedText name0 <> "__" <> tShow i)
