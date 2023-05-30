{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Branch.Merge
  ( MergeMode (..),
    merge'',
  )
where

import Data.Map qualified as Map
import Data.Map.Merge.Lazy qualified as Map
import U.Codebase.HashTags (PatchHash (..))
import Unison.Codebase.Branch
  ( Branch (..),
    Branch0 (_children, _edits, _terms, _types),
    branch0,
    cons,
    discardHistory0,
    empty0,
    head,
    isEmpty,
    isEmpty0,
  )
import Unison.Codebase.Branch.BranchDiff (BranchDiff (BranchDiff))
import Unison.Codebase.Branch.BranchDiff qualified as BDiff
import Unison.Codebase.Causal qualified as Causal
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Patch qualified as Patch
import Unison.Hashing.V2.Convert qualified as H
import Unison.Prelude hiding (empty)
import Unison.Util.Map (unionWithM)
import Unison.Util.Relation qualified as R
import Unison.Util.Star3 qualified as Star3
import Prelude hiding (head, read, subtract)

data MergeMode = RegularMerge | SquashMerge deriving (Eq, Ord, Show)

merge'' ::
  forall m.
  (Monad m) =>
  (Branch m -> Branch m -> m (Maybe (Branch m))) -> -- lca calculator
  MergeMode ->
  Branch m ->
  Branch m ->
  m (Branch m)
merge'' _ _ b1 b2 | isEmpty b1 = pure b2
merge'' _ mode b1 b2 | isEmpty b2 = case mode of
  RegularMerge -> pure b1
  SquashMerge -> pure $ cons (discardHistory0 (head b1)) b2
merge'' lca mode (Branch x) (Branch y) =
  Branch <$> case mode of
    RegularMerge -> Causal.threeWayMerge' lca' combine x y
    SquashMerge -> Causal.squashMerge' lca' (pure . discardHistory0) combine x y
  where
    lca' c1 c2 = fmap _history <$> lca (Branch c1) (Branch c2)
    combine :: Maybe (Branch0 m) -> Branch0 m -> Branch0 m -> m (Branch0 m)
    combine Nothing l r = merge0 lca mode l r
    combine (Just ca) l r = do
      dl <- BDiff.diff0 ca l
      dr <- BDiff.diff0 ca r
      head0 <- apply ca (dl <> dr)
      children <-
        Map.mergeA
          (Map.traverseMaybeMissing $ combineMissing ca)
          (Map.traverseMaybeMissing $ combineMissing ca)
          (Map.zipWithAMatched $ const (merge'' lca mode))
          (_children l)
          (_children r)
      pure $ branch0 (_terms head0) (_types head0) children (_edits head0)

    combineMissing ca k cur =
      case Map.lookup k (_children ca) of
        Nothing -> pure $ Just cur
        Just old -> do
          nw <- merge'' lca mode (cons empty0 old) cur
          if isEmpty0 $ head nw
            then pure Nothing
            else pure $ Just nw

    apply :: Branch0 m -> BranchDiff -> m (Branch0 m)
    apply b0 (BranchDiff addedTerms removedTerms addedTypes removedTypes changedPatches) = do
      patches <-
        sequenceA $
          Map.differenceWith patchMerge (pure @m <$> _edits b0) changedPatches
      let newPatches = makePatch <$> Map.difference changedPatches (_edits b0)
          makePatch Patch.PatchDiff {..} =
            let p = Patch.Patch _addedTermEdits _addedTypeEdits
             in (PatchHash (H.hashPatch p), pure p)
      pure $
        branch0
          (Star3.difference (_terms b0) removedTerms <> addedTerms)
          (Star3.difference (_types b0) removedTypes <> addedTypes)
          (_children b0)
          (patches <> newPatches)
    patchMerge mhp Patch.PatchDiff {..} = Just $ do
      (_, mp) <- mhp
      p <- mp
      let np =
            Patch.Patch
              { _termEdits =
                  R.difference (Patch._termEdits p) _removedTermEdits
                    <> _addedTermEdits,
                _typeEdits =
                  R.difference (Patch._typeEdits p) _removedTypeEdits
                    <> _addedTypeEdits
              }
      pure (PatchHash (H.hashPatch np), pure np)

merge0 ::
  forall m.
  (Monad m) =>
  (Branch m -> Branch m -> m (Maybe (Branch m))) ->
  MergeMode ->
  Branch0 m ->
  Branch0 m ->
  m (Branch0 m)
merge0 lca mode b1 b2 = do
  c3 <- unionWithM (merge'' lca mode) (_children b1) (_children b2)
  e3 <- unionWithM g (_edits b1) (_edits b2)
  pure $
    branch0
      (_terms b1 <> _terms b2)
      (_types b1 <> _types b2)
      c3
      e3
  where
    g :: (PatchHash, m Patch) -> (PatchHash, m Patch) -> m (PatchHash, m Patch)
    g (h1, m1) (h2, _) | h1 == h2 = pure (h1, m1)
    g (_, m1) (_, m2) = do
      e1 <- m1
      e2 <- m2
      let e3 = e1 <> e2
      pure (PatchHash (H.hashPatch e3), pure e3)
