{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Codebase.Branch
  ( -- * Branch types
    Branch (..),
    UnwrappedBranch,
    Branch0,
    Raw,
    Star,
    NamespaceHash,

    -- * Branch construction
    branch0,
    one,
    cons,
    mergeNode,
    uncons,
    empty,
    empty0,
    discardHistory,
    discardHistory0,
    transform,
    transform0,

    -- * Branch tests
    isEmpty,
    isEmpty0,
    isOne,
    before,
    lca,

    -- * properties
    history,
    head,
    head_,
    headHash,
    children,
    nonEmptyChildren,
    deepEdits',
    namespaceStats,

    -- * step
    step,
    stepManyAt,
    stepManyAtM,
    stepEverywhere,
    batchUpdates,
    batchUpdatesM,
    UpdateStrategy (..),
    addTermName,
    addTypeName,
    deleteTermName,
    annihilateTermName,
    annihilateTypeName,
    deleteTypeName,
    setChildBranch,
    replacePatch,
    deletePatch,
    getMaybePatch,
    getPatch,
    modifyPatches,

    -- ** Children queries
    getAt,
    getAt',
    getAt0,
    modifyAt,
    modifyAtM,
    children0,

    -- *** Libdep manipulations
    withoutLib,
    withoutTransitiveLibs,
    deleteLibdep,
    deleteLibdeps,

    -- * Branch terms/types/edits

    -- ** Term/type/edits lenses
    terms,
    types,
    edits,

    -- ** Term/type queries
    deepTerms,
    deepTypes,
    deepDefns,
    deepEdits,
    deepPaths,
    deepReferents,
    deepTermReferences,
    deepTermReferenceIds,
    deepTypeReferences,
    deepTypeReferenceIds,
    consBranchSnapshot,
  )
where

import Control.Lens hiding (children, cons, transform, uncons)
import Data.Map qualified as Map
import Data.Semialign qualified as Align
import Data.These (These (..))
import U.Codebase.Branch.Type (NamespaceStats (..))
import U.Codebase.HashTags (CausalHash, PatchHash (..))
import Unison.Codebase.Branch.Raw (Raw)
import Unison.Codebase.Branch.Type
  ( Branch (..),
    Branch0,
    NamespaceHash,
    Star,
    UnwrappedBranch,
    branch0,
    children,
    deepDefns,
    deepEdits,
    deepPaths,
    deepTerms,
    deepTypes,
    edits,
    head,
    headHash,
    history,
    isEmpty0,
    nonEmptyChildren,
    terms,
    types,
  )
import Unison.Codebase.Causal (Causal)
import Unison.Codebase.Causal qualified as Causal
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Patch qualified as Patch
import Unison.Codebase.Path (Path)
import Unison.Hashing.V2 qualified as Hashing (ContentAddressable (contentHash))
import Unison.Hashing.V2.Convert qualified as H
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude hiding (empty)
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Util.List qualified as List
import Unison.Util.Recursion (XNor (Both, Neither), cata, project)
import Unison.Util.Relation qualified as R
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.Star2 qualified as Star2
import Witherable (FilterableWithIndex (imapMaybe))
import Prelude hiding (head, read, subtract)

instance AsEmpty (Branch m) where
  _Empty = prism' (const empty) matchEmpty
    where
      matchEmpty b0
        | b0 == empty = Just ()
        | otherwise = Nothing

instance Hashing.ContentAddressable (Branch0 m) where
  contentHash = H.hashBranch0

-- | Remove any lib subtrees reachable within the branch.
-- Note: This DOES affect the hash.
withoutLib :: Branch0 m -> Branch0 m
withoutLib b =
  b
    & children
      %~ imapMaybe
        ( \nameSegment child ->
            if nameSegment == NameSegment.libSegment
              then Nothing
              else Just (child & head_ %~ withoutLib)
        )

-- | Remove any transitive libs reachable within the branch.
-- Note: This DOES affect the hash.
withoutTransitiveLibs :: Branch0 m -> Branch0 m
withoutTransitiveLibs b0 =
  let newChildren =
        (b0 ^. children)
          & imapMaybe
            ( \nameSegment child ->
                if nameSegment == NameSegment.libSegment
                  then Just (child & head_ %~ withoutLib)
                  else Just (child & head_ %~ withoutTransitiveLibs)
            )
   in b0 & children .~ newChildren

-- | @deleteLibdep name branch@ deletes the libdep named @name@ from @branch@, if it exists.
deleteLibdep :: NameSegment -> Branch0 m -> Branch0 m
deleteLibdep dep =
  over (children . ix NameSegment.libSegment . head_ . children) (Map.delete dep)

-- | @deleteLibdeps branch@ deletes all libdeps from @branch@.
deleteLibdeps :: Branch0 m -> Branch0 m
deleteLibdeps =
  over children (Map.delete NameSegment.libSegment)

deepReferents :: Branch0 m -> Set Referent
deepReferents = R.dom . deepTerms

deepTermReferences :: Branch0 m -> Set TermReference
deepTermReferences =
  Set.mapMaybe Referent.toTermReference . deepReferents

deepTermReferenceIds :: Branch0 m -> Set TermReferenceId
deepTermReferenceIds =
  Set.mapMaybe Referent.toTermReferenceId . deepReferents

deepTypeReferences :: Branch0 m -> Set TypeReference
deepTypeReferences = R.dom . deepTypes

deepTypeReferenceIds :: Branch0 m -> Set TypeReferenceId
deepTypeReferenceIds =
  Set.mapMaybe Reference.toId . deepTypeReferences

namespaceStats :: Branch0 m -> NamespaceStats
namespaceStats b =
  NamespaceStats
    { numContainedTerms = Relation.size $ deepTerms b,
      numContainedTypes = Relation.size $ deepTypes b,
      numContainedPatches = Map.size $ deepEdits b
    }

-- | Update the head of the current causal.
-- This re-hashes the current causal head after modifications.
head_ :: Lens' (Branch m) (Branch0 m)
head_ = history . Causal.head_

-- | a version of `deepEdits` that returns the `m Patch` as well.
deepEdits' :: Branch0 m -> Map Name (PatchHash, m Patch)
deepEdits' = go id
  where
    -- can change this to an actual prefix once Name is a [NameSegment]
    go :: (Name -> Name) -> Branch0 m -> Map Name (PatchHash, m Patch)
    go addPrefix b0 =
      Map.mapKeys (addPrefix . Name.fromSegment) (b0 ^. edits)
        <> foldMap f (Map.toList (b0 ^. children))
      where
        f :: (NameSegment, Branch m) -> Map Name (PatchHash, m Patch)
        f (c, b) = go (addPrefix . Name.cons c) (head b)

-- | Discards the history of a Branch0's children, recursively
discardHistory0 :: (Applicative m) => Branch0 m -> Branch0 m
discardHistory0 = over children (fmap tweak)
  where
    tweak b = one (discardHistory0 (head b))

-- | Discards the history of a Branch and its children, recursively
discardHistory :: (Applicative m) => Branch m -> Branch m
discardHistory b =
  one (discardHistory0 (head b))

-- | `before b1 b2` is true if `b2` incorporates all of `b1`
before :: (Monad m) => Branch m -> Branch m -> m Bool
before (Branch b1) (Branch b2) = Causal.before b1 b2

-- returns `Nothing` if no Branch at `path` or if Branch is empty at `path`
getAt :: Path -> Branch m -> Maybe (Branch m)
getAt = cata \case
  Neither -> \root -> if isEmpty root then Nothing else Just root
  Both seg fn -> fn <=< Map.lookup seg . view children . head

getAt' :: Path -> Branch m -> Branch m
getAt' p b = fromMaybe empty $ getAt p b

getAt0 :: Path -> Branch0 m -> Branch0 m
getAt0 = cata \case
  Neither -> id
  Both seg fn -> fn . maybe empty0 head . Map.lookup seg . view children

empty :: Branch m
empty = Branch $ Causal.one empty0

one :: Branch0 m -> Branch m
one = Branch . Causal.one

empty0 :: Branch0 m
empty0 = branch0 mempty mempty mempty mempty

-- | Checks whether a branch is empty AND has no history.
isEmpty :: Branch m -> Bool
isEmpty = (== empty)

-- | Perform an update over the current branch and create a new causal step.
step :: (Applicative m) => (Branch0 m -> Branch0 m) -> Branch m -> Branch m
step f = runIdentity . stepM (Identity . f)

-- | Perform an update over the current branch and create a new causal step.
stepM :: (Monad n, Applicative m) => (Branch0 m -> n (Branch0 m)) -> Branch m -> n (Branch m)
stepM f = \case
  Branch (Causal.One _h _eh e) | e == empty0 -> Branch . Causal.one <$> f empty0
  b -> mapMOf history (Causal.stepDistinctM f) b

cons :: (Applicative m) => Branch0 m -> Branch m -> Branch m
cons = step . const

-- | Construct a two-parent merge node.
mergeNode ::
  forall m.
  (Applicative m) =>
  Branch0 m ->
  (CausalHash, m (Branch m)) ->
  (CausalHash, m (Branch m)) ->
  Branch m
mergeNode child parent1 parent2 =
  Branch (Causal.mergeNode child (Map.fromList [f parent1, f parent2]))
  where
    f (hash, getBranch) =
      (hash, _history <$> getBranch)

isOne :: Branch m -> Bool
isOne (Branch Causal.One {}) = True
isOne _ = False

uncons :: (Applicative m) => Branch m -> m (Maybe (Branch0 m, Branch m))
uncons (Branch b) = go <$> Causal.uncons b
  where
    go = over (_Just . _2) Branch

-- | Run a series of updates at specific locations, aggregating all changes into a single causal step.
-- History is managed according to 'UpdateStrategy'.
stepManyAt ::
  forall m f.
  (Monad m, Foldable f) =>
  f (Path, Branch0 m -> Branch0 m) ->
  Branch m ->
  Branch m
stepManyAt actions startBranch =
  runIdentity $ stepManyAtM actionsIdentity startBranch
  where
    actionsIdentity :: [(Path, Branch0 m -> Identity (Branch0 m))]
    actionsIdentity = coerce (toList actions)

data UpdateStrategy
  = -- | Compress all changes into a single causal cons.
    -- The resulting branch will have at most one new causal cons at each branch.
    --
    -- Note that this does NOT allow updates to add histories at children.
    -- E.g. if the root.editme branch has history: A -> B -> C
    -- and you use 'makeSetBranch' to update it to a new branch with history X -> Y -> Z,
    -- CompressHistory will result in a history for root.editme of: A -> B -> C -> Z.
    -- A 'snapshot' of the most recent state of the updated branch is appended to the existing history,
    -- if the new state is equal to the existing state, no new history nodes are appended.
    CompressHistory
  | -- | Preserves any history changes made within the update.
    --
    -- Note that this allows you to clobber the history child branches if you want.
    -- E.g. if the root.editme branch has history: A -> B -> C
    -- and you use 'makeSetBranch' to update it to a new branch with history X -> Y -> Z,
    -- AllowRewritingHistory will result in a history for root.editme of: X -> Y -> Z.
    -- The history of the updated branch is replaced entirely.
    AllowRewritingHistory

-- | Run a series of updates at specific locations.
-- History is managed according to the 'UpdateStrategy'
stepManyAtM ::
  (Monad m, Monad n, Foldable f) =>
  f (Path, Branch0 m -> n (Branch0 m)) ->
  Branch m ->
  n (Branch m)
stepManyAtM actions startBranch = do
  updatedBranch <- startBranch & head_ %%~ batchUpdatesM actions
  pure $ updatedBranch `consBranchSnapshot` startBranch

-- starting at the leaves, apply `f` to every level of the branch.
stepEverywhere ::
  (Applicative m) => (Branch0 m -> Branch0 m) -> (Branch0 m -> Branch0 m)
stepEverywhere f b0 = f (b0 & children %~ updates)
  where
    updates = fmap (step $ stepEverywhere f)

-- Creates a function to fix up the children field._1
-- If the action emptied a child, then remove the mapping,
-- otherwise update it.
-- Todo: Fix this in hashing & serialization instead of here?
getChildBranch :: NameSegment -> Branch0 m -> Branch m
getChildBranch seg b = fromMaybe empty $ Map.lookup seg (b ^. children)

setChildBranch :: NameSegment -> Branch m -> Branch0 m -> Branch0 m
setChildBranch seg b = over children (updateChildren seg b)

getPatch :: (Applicative m) => NameSegment -> Branch0 m -> m Patch
getPatch seg b = case Map.lookup seg (b ^. edits) of
  Nothing -> pure Patch.empty
  Just (_, p) -> p

getMaybePatch :: (Applicative m) => NameSegment -> Branch0 m -> m (Maybe Patch)
getMaybePatch seg b = case Map.lookup seg (b ^. edits) of
  Nothing -> pure Nothing
  Just (_, p) -> Just <$> p

modifyPatches ::
  (Monad m) => NameSegment -> (Patch -> Patch) -> Branch0 m -> m (Branch0 m)
modifyPatches seg f = mapMOf edits update
  where
    update m = do
      p' <- case Map.lookup seg m of
        Nothing -> pure $ f Patch.empty
        Just (_, p) -> f <$> p
      let h = H.hashPatch p'
      pure $ Map.insert seg (PatchHash h, pure p') m

replacePatch :: (Applicative m) => NameSegment -> Patch -> Branch0 m -> Branch0 m
replacePatch n p = over edits (Map.insert n (PatchHash (H.hashPatch p), pure p))

deletePatch :: NameSegment -> Branch0 m -> Branch0 m
deletePatch n = over edits (Map.delete n)

updateChildren ::
  NameSegment ->
  Branch m ->
  Map NameSegment (Branch m) ->
  Map NameSegment (Branch m)
updateChildren seg updatedChild =
  if isEmpty updatedChild
    then Map.delete seg
    else Map.insert seg updatedChild

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAt ::
  (Applicative m) =>
  Path ->
  (Branch m -> Branch m) ->
  Branch m ->
  Branch m
modifyAt path f = runIdentity . modifyAtM path (pure . f)

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAtM ::
  forall n m.
  (Functor n, Applicative m) =>
  Path ->
  (Branch m -> n (Branch m)) ->
  Branch m ->
  n (Branch m)
modifyAtM path f = flip cata path \case
  Neither -> f
  Both seg fn -> \b ->
    -- step the branch by updating its children according to fixup
    flip step b . setChildBranch seg <$> fn (getChildBranch seg $ head b)

-- | Perform updates over many locations within a branch by batching up operations on
-- sub-branches as much as possible without affecting semantics.
-- This operation does not create any causal conses, the operations are performed directly
-- on the current head of the provided branch and child branches. It's the caller's
-- responsibility to apply updates in history however they choose.
batchUpdates ::
  forall f m.
  (Monad m, Foldable f) =>
  f (Path, Branch0 m -> Branch0 m) ->
  Branch0 m ->
  Branch0 m
batchUpdates actions =
  runIdentity . batchUpdatesM actionsIdentity
  where
    actionsIdentity :: [(Path, Branch0 m -> Identity (Branch0 m))]
    actionsIdentity = coerce $ toList actions

-- | Helper type for grouping up actions according to whether they should be applied at
-- the current branch, or at a child location.
data ActionLocation = HereActions | ChildActions
  deriving (Eq)

-- | Batch many updates. This allows us to apply the updates while minimizing redundant traversals.
-- Semantics of operations are preserved by ensuring that all updates will always see changes
-- by updates before them in the list.
--
-- This method does not 'step' any branches on its own, all causal changes must be performed in the updates themselves,
-- or this batch update must be provided to 'stepManyAt(M)'.
batchUpdatesM ::
  forall m n f.
  (Monad m, Monad n, Foldable f) =>
  f (Path, Branch0 m -> n (Branch0 m)) ->
  Branch0 m ->
  n (Branch0 m)
batchUpdatesM (toList -> actions) curBranch = foldM execActions curBranch (groupActionsByLocation actions)
  where
    groupActionsByLocation :: [(Path, b)] -> [(ActionLocation, [(Path, b)])]
    groupActionsByLocation = List.groupMap \(p, act) -> (pathLocation p, (p, act))

    execActions ::
      ( Branch0 m ->
        (ActionLocation, [(Path, Branch0 m -> n (Branch0 m))]) ->
        n (Branch0 m)
      )
    execActions b = \case
      (HereActions, acts) -> foldM (\b (_, act) -> act b) b acts
      (ChildActions, acts) -> b & children %%~ adjustChildren (groupByNextSegment acts)

    adjustChildren ::
      Map NameSegment [(Path, Branch0 m -> n (Branch0 m))] ->
      Map NameSegment (Branch m) ->
      n (Map NameSegment (Branch m))
    adjustChildren childActions children0 =
      foldM go children0 $ Map.toList childActions
      where
        -- Recursively applies the relevant actions to the child branch
        go ::
          ( Map NameSegment (Branch m) ->
            (NameSegment, [(Path, Branch0 m -> n (Branch0 m))]) ->
            n (Map NameSegment (Branch m))
          )
        go children (seg, acts) = do
          -- 'non empty' creates an empty branch if one is missing,
          -- and similarly deletes a branch if it is empty after modifications.
          -- This is important so that branch actions can create/delete branches.
          children & at seg . non empty . head_ %%~ batchUpdatesM acts
    -- The order of actions across differing keys is irrelevant since those actions can't
    -- affect each other.
    -- The order within a given key is stable.
    groupByNextSegment :: [(Path, x)] -> Map NameSegment [(Path, x)]
    groupByNextSegment =
      Map.unionsWith (<>) . fmap \(p, action) -> case project p of
        Neither -> error "groupByNextSegment called on current path, which shouldn't happen."
        Both seg rest -> Map.singleton seg [(rest, action)]
    pathLocation :: Path -> ActionLocation
    pathLocation p = if p == mempty then HereActions else ChildActions

-- todo: consider inlining these into Actions2
addTermName :: Referent -> NameSegment -> Branch0 m -> Branch0 m
addTermName r new =
  over terms (Star2.insertD1 (r, new))

addTypeName :: TypeReference -> NameSegment -> Branch0 m -> Branch0 m
addTypeName r new =
  over types (Star2.insertD1 (r, new))

deleteTermName :: Referent -> NameSegment -> Branch0 m -> Branch0 m
deleteTermName r n b
  | Star2.memberD1 (r, n) (view terms b) =
      over terms (Star2.deletePrimaryD1 (r, n)) b
deleteTermName _ _ b = b

annihilateTermName :: NameSegment -> Branch0 m -> Branch0 m
annihilateTermName = over terms . Star2.deleteD1

annihilateTypeName :: NameSegment -> Branch0 m -> Branch0 m
annihilateTypeName = over types . Star2.deleteD1

deleteTypeName :: TypeReference -> NameSegment -> Branch0 m -> Branch0 m
deleteTypeName r n b
  | Star2.memberD1 (r, n) (view types b) =
      over types (Star2.deletePrimaryD1 (r, n)) b
deleteTypeName _ _ b = b

lca :: (Monad m) => Branch m -> Branch m -> m (Maybe (Branch m))
lca (Branch a) (Branch b) = fmap Branch <$> Causal.lca a b

transform :: (Functor m) => (forall a. m a -> n a) -> Branch m -> Branch n
transform f b = case _history b of
  causal -> Branch . Causal.transform f $ transformB0s f causal
  where
    transformB0s :: (Functor m) => (forall a. m a -> n a) -> Causal m (Branch0 m) -> Causal m (Branch0 n)
    transformB0s f = Causal.unsafeMapHashPreserving (transform0 f)

transform0 :: (Functor m) => (forall a. m a -> n a) -> Branch0 m -> Branch0 n
transform0 f b =
  branch0 (b ^. terms) (b ^. types) newChildren newEdits
  where
    newChildren = transform f <$> (b ^. children)
    newEdits = second f <$> (b ^. edits)

-- | Traverse the head branch of all direct children.
-- The index of the traversal is the name of that child branch according to the parent.
children0 :: IndexedTraversal' NameSegment (Branch0 m) (Branch0 m)
children0 = children .> itraversed <. (history . Causal.head_)

-- | @head `consBranchSnapshot` base@ Cons's the current state of @head@ onto @base@ as-is.
-- Consider whether you really want this behaviour or the behaviour of 'Causal.squashMerge'
-- That is, it does not perform any common ancestor detection, or change reconciliation, it
-- sets the current state of the base branch to the new state as a new causal step (or returns
-- the existing base if there are no)
consBranchSnapshot ::
  forall m.
  (Monad m) =>
  Branch m ->
  Branch m ->
  Branch m
-- If the target branch is empty we just replace it.
consBranchSnapshot headBranch Empty = discardHistory headBranch
consBranchSnapshot headBranch baseBranch =
  if baseBranch == headBranch
    then baseBranch
    else
      Branch $
        Causal.consDistinct
          (head headBranch & children .~ combinedChildren)
          (_history baseBranch)
  where
    combineChildren :: These (Branch m) (Branch m) -> Maybe (Branch m)
    combineChildren = \case
      -- If we have a matching child in both base and head, squash the child head onto the
      -- child base recursively.
      (These base head) -> Just (head `consBranchSnapshot` base)
      -- This child has been deleted, let it be
      (This _) -> Nothing
      -- This child didn't exist in the base, we add any changes as a single commit
      (That head) -> Just (discardHistory head)
    combinedChildren :: Map NameSegment (Branch m)
    combinedChildren =
      Map.mapMaybe combineChildren $
        Align.align
          (head baseBranch ^. children)
          (head headBranch ^. children)
