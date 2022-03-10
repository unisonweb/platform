{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Codebase.Branch
  ( -- * Branch types
    Branch (..),
    UnwrappedBranch,
    Branch0 (..),
    Raw (..),
    Star,
    Hash,
    EditHash,
    pattern Hash,

    -- * Branch construction
    branch0,
    one,
    cons,
    uncons,
    empty,
    empty0,
    discardHistory,
    discardHistory0,
    transform,

    -- * Branch tests
    isEmpty,
    isEmpty0,
    isOne,
    before,
    lca,

    -- * properties
    history,
    head,
    headHash,
    children,
    nonEmptyChildren,
    deepEdits',
    toList0,

    -- * step
    stepManyAt,
    stepManyAtM,
    stepEverywhere,
    batchUpdates,
    batchUpdatesM,
    UpdateStrategy (..),
    addTermName,
    addTypeName,
    deleteTermName,
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

    -- * Branch terms/types/edits

    -- ** Term/type/edits lenses
    terms,
    types,
    edits,

    -- ** Term/type queries
    deepReferents,
    deepTypeReferences,
    consBranchSnapshot,
  )
where

import Control.Lens hiding (children, cons, transform, uncons)
import Data.Bifunctor (second)
import qualified Data.Map as Map
import qualified Data.Semialign as Align
import qualified Data.Set as Set
import Data.These (These (..))
import Unison.Codebase.Branch.Raw (Raw (Raw))
import Unison.Codebase.Branch.Type
  ( Branch (..),
    Branch0 (..),
    EditHash,
    Hash,
    Star,
    UnwrappedBranch,
    edits,
    head,
    headHash,
    history,
  )
import Unison.Codebase.Causal (Causal)
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Patch (Patch)
import qualified Unison.Codebase.Patch as Patch
import Unison.Codebase.Path (Path (..))
import qualified Unison.Codebase.Path as Path
import qualified Unison.Hashing.V2.Convert as H
import qualified Unison.Hashing.V2.Hashable as H
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude hiding (empty)
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import qualified Unison.Util.List as List
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation as Relation
import qualified Unison.Util.Relation4 as R4
import qualified Unison.Util.Star3 as Star3
import Prelude hiding (head, read, subtract)

instance AsEmpty (Branch m) where
  _Empty = prism' (const empty) matchEmpty
    where
      matchEmpty b0
        | b0 == empty = Just ()
        | otherwise = Nothing

instance H.Hashable (Branch0 m) where
  hash = H.hashBranch0

deepReferents :: Branch0 m -> Set Referent
deepReferents = R.dom . deepTerms

deepTypeReferences :: Branch0 m -> Set TypeReference
deepTypeReferences = R.dom . deepTypes

terms :: Lens' (Branch0 m) (Star Referent NameSegment)
terms =
  lens
    _terms
    \branch terms ->
      branch {_terms = terms}
        & deriveDeepTerms
        & deriveDeepTermMetadata

types :: Lens' (Branch0 m) (Star TypeReference NameSegment)
types =
  lens
    _types
    \branch types ->
      branch {_types = types}
        & deriveDeepTypes
        & deriveDeepTypeMetadata

children :: Lens' (Branch0 m) (Map NameSegment (Branch m))
children = lens _children (\Branch0 {..} x -> branch0 _terms _types x _edits)

nonEmptyChildren :: Branch0 m -> Map NameSegment (Branch m)
nonEmptyChildren b =
  b
    & _children
    & Map.filter (not . isEmpty0 . head)

-- creates a Branch0 from the primary fields and derives the others.
branch0 ::
  forall m.
  Metadata.Star Referent NameSegment ->
  Metadata.Star TypeReference NameSegment ->
  Map NameSegment (Branch m) ->
  Map NameSegment (EditHash, m Patch) ->
  Branch0 m
branch0 terms types children edits =
  Branch0
    { _terms = terms,
      _types = types,
      _children = children,
      _edits = edits,
      -- These are all overwritten immediately
      deepTerms = R.empty,
      deepTypes = R.empty,
      deepTermMetadata = R4.empty,
      deepTypeMetadata = R4.empty,
      deepPaths = Set.empty,
      deepEdits = Map.empty
    }
    & deriveDeepTerms
    & deriveDeepTypes
    & deriveDeepTermMetadata
    & deriveDeepTypeMetadata
    & deriveDeepPaths
    & deriveDeepEdits

-- | Derive the 'deepTerms' field of a branch.
deriveDeepTerms :: Branch0 m -> Branch0 m
deriveDeepTerms branch =
  branch {deepTerms = makeDeepTerms (_terms branch) (nonEmptyChildren branch)}
  where
    makeDeepTerms :: Metadata.Star Referent NameSegment -> Map NameSegment (Branch m) -> Relation Referent Name
    makeDeepTerms terms children =
      R.mapRanMonotonic Name.fromSegment (Star3.d1 terms) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Relation Referent Name
        go n b =
          R.mapRan (Name.cons n) (deepTerms $ head b)

-- | Derive the 'deepTypes' field of a branch.
deriveDeepTypes :: Branch0 m -> Branch0 m
deriveDeepTypes branch =
  branch {deepTypes = makeDeepTypes (_types branch) (nonEmptyChildren branch)}
  where
    makeDeepTypes :: Metadata.Star TypeReference NameSegment -> Map NameSegment (Branch m) -> Relation TypeReference Name
    makeDeepTypes types children =
      R.mapRanMonotonic Name.fromSegment (Star3.d1 types) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Relation TypeReference Name
        go n b =
          R.mapRan (Name.cons n) (deepTypes $ head b)

-- | Derive the 'deepTermMetadata' field of a branch.
deriveDeepTermMetadata :: Branch0 m -> Branch0 m
deriveDeepTermMetadata branch =
  branch {deepTermMetadata = makeDeepTermMetadata (_terms branch) (nonEmptyChildren branch)}
  where
    makeDeepTermMetadata :: Metadata.Star Referent NameSegment -> Map NameSegment (Branch m) -> Metadata.R4 Referent Name
    makeDeepTermMetadata terms children =
      R4.mapD2Monotonic Name.fromSegment (Metadata.starToR4 terms) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Metadata.R4 Referent Name
        go n b =
          R4.mapD2 (Name.cons n) (deepTermMetadata $ head b)

-- | Derive the 'deepTypeMetadata' field of a branch.
deriveDeepTypeMetadata :: Branch0 m -> Branch0 m
deriveDeepTypeMetadata branch =
  branch {deepTypeMetadata = makeDeepTypeMetadata (_types branch) (nonEmptyChildren branch)}
  where
    makeDeepTypeMetadata :: Metadata.Star TypeReference NameSegment -> Map NameSegment (Branch m) -> Metadata.R4 TypeReference Name
    makeDeepTypeMetadata types children =
      R4.mapD2Monotonic Name.fromSegment (Metadata.starToR4 types) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Metadata.R4 TypeReference Name
        go n b =
          R4.mapD2 (Name.cons n) (deepTypeMetadata $ head b)

-- | Derive the 'deepPaths' field of a branch.
deriveDeepPaths :: Branch0 m -> Branch0 m
deriveDeepPaths branch =
  branch {deepPaths = makeDeepPaths (nonEmptyChildren branch)}
  where
    makeDeepPaths :: Map NameSegment (Branch m) -> Set Path
    makeDeepPaths children =
      Set.mapMonotonic Path.singleton (Map.keysSet children) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Set Path
        go n b =
          Set.map (Path.cons n) (deepPaths $ head b)

-- | Derive the 'deepEdits' field of a branch.
deriveDeepEdits :: Branch0 m -> Branch0 m
deriveDeepEdits branch =
  branch {deepEdits = makeDeepEdits (_edits branch) (nonEmptyChildren branch)}
  where
    makeDeepEdits :: Map NameSegment (EditHash, m Patch) -> Map NameSegment (Branch m) -> Map Name EditHash
    makeDeepEdits edits children =
      Map.mapKeysMonotonic Name.fromSegment (Map.map fst edits) <> ifoldMap go children
      where
        go :: NameSegment -> Branch m -> Map Name EditHash
        go n b =
          Map.mapKeys (Name.cons n) (deepEdits $ head b)

-- | Update the head of the current causal.
-- This re-hashes the current causal head after modifications.
head_ :: Lens' (Branch m) (Branch0 m)
head_ = history . Causal.head_

-- | a version of `deepEdits` that returns the `m Patch` as well.
deepEdits' :: Branch0 m -> Map Name (EditHash, m Patch)
deepEdits' = go id
  where
    -- can change this to an actual prefix once Name is a [NameSegment]
    go :: (Name -> Name) -> Branch0 m -> Map Name (EditHash, m Patch)
    go addPrefix Branch0 {_children, _edits} =
      Map.mapKeys (addPrefix . Name.fromSegment) _edits
        <> foldMap f (Map.toList _children)
      where
        f :: (NameSegment, Branch m) -> Map Name (EditHash, m Patch)
        f (c, b) = go (addPrefix . Name.cons c) (head b)

-- | Discards the history of a Branch0's children, recursively
discardHistory0 :: Applicative m => Branch0 m -> Branch0 m
discardHistory0 = over children (fmap tweak)
  where
    tweak b = one (discardHistory0 (head b))

-- | Discards the history of a Branch and its children, recursively
discardHistory :: Applicative m => Branch m -> Branch m
discardHistory b =
  one (discardHistory0 (head b))

-- `before b1 b2` is true if `b2` incorporates all of `b1`
before :: Monad m => Branch m -> Branch m -> m Bool
before (Branch b1) (Branch b2) = Causal.before b1 b2

pattern Hash h = Causal.RawHash h

-- | what does this do? —AI
toList0 :: Branch0 m -> [(Path, Branch0 m)]
toList0 = go Path.empty
  where
    go p b =
      (p, b) :
      ( Map.toList (_children b)
          >>= ( \(seg, cb) ->
                  go (Path.snoc p seg) (head cb)
              )
      )

-- returns `Nothing` if no Branch at `path` or if Branch is empty at `path`
getAt ::
  Path ->
  Branch m ->
  Maybe (Branch m)
getAt path root = case Path.uncons path of
  Nothing -> if isEmpty root then Nothing else Just root
  Just (seg, path) -> case Map.lookup seg (_children $ head root) of
    Just b -> getAt path b
    Nothing -> Nothing

getAt' :: Path -> Branch m -> Branch m
getAt' p b = fromMaybe empty $ getAt p b

getAt0 :: Path -> Branch0 m -> Branch0 m
getAt0 p b = case Path.uncons p of
  Nothing -> b
  Just (seg, path) -> case Map.lookup seg (_children b) of
    Just c -> getAt0 path (head c)
    Nothing -> empty0

empty :: Branch m
empty = Branch $ Causal.one empty0

one :: Branch0 m -> Branch m
one = Branch . Causal.one

empty0 :: Branch0 m
empty0 =
  Branch0 mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

-- | Checks whether a Branch0 is empty, which means that the branch contains no terms or
-- types, and that the heads of all children are empty by the same definition.
-- This is not as easy as checking whether the branch is equal to the `empty0` branch
-- because child branches may be empty, but still have history.
isEmpty0 :: Branch0 m -> Bool
isEmpty0 (Branch0 _terms _types _children _edits deepTerms deepTypes _deepTermMetadata _deepTypeMetadata _deepPaths deepEdits) =
  Relation.null deepTerms
    && Relation.null deepTypes
    && Map.null deepEdits

-- | Checks whether a branch is empty AND has no history.
isEmpty :: Branch m -> Bool
isEmpty = (== empty)

-- | Perform an update over the current branch and create a new causal step.
step :: Applicative m => (Branch0 m -> Branch0 m) -> Branch m -> Branch m
step f = runIdentity . stepM (Identity . f)

-- | Perform an update over the current branch and create a new causal step.
stepM :: (Monad n, Applicative m) => (Branch0 m -> n (Branch0 m)) -> Branch m -> n (Branch m)
stepM f = \case
  Branch (Causal.One _h e) | e == empty0 -> Branch . Causal.one <$> f empty0
  b -> mapMOf history (Causal.stepDistinctM f) b

cons :: Applicative m => Branch0 m -> Branch m -> Branch m
cons = step . const

isOne :: Branch m -> Bool
isOne (Branch Causal.One {}) = True
isOne _ = False

uncons :: Applicative m => Branch m -> m (Maybe (Branch0 m, Branch m))
uncons (Branch b) = go <$> Causal.uncons b
  where
    go = over (_Just . _2) Branch

-- | Run a series of updates at specific locations, aggregating all changes into a single causal step.
-- History is managed according to 'UpdateStrategy'.
stepManyAt ::
  forall m f.
  (Monad m, Foldable f) =>
  UpdateStrategy ->
  f (Path, Branch0 m -> Branch0 m) ->
  Branch m ->
  Branch m
stepManyAt strat actions startBranch =
  runIdentity $ stepManyAtM strat actionsIdentity startBranch
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
  UpdateStrategy ->
  f (Path, Branch0 m -> n (Branch0 m)) ->
  Branch m ->
  n (Branch m)
stepManyAtM strat actions startBranch =
  case strat of
    AllowRewritingHistory -> steppedUpdates
    CompressHistory -> squashedUpdates
  where
    steppedUpdates = do
      stepM (batchUpdatesM actions) startBranch
    squashedUpdates = do
      updatedBranch <- startBranch & head_ %%~ batchUpdatesM actions
      pure $ updatedBranch `consBranchSnapshot` startBranch

-- starting at the leaves, apply `f` to every level of the branch.
stepEverywhere ::
  Applicative m => (Branch0 m -> Branch0 m) -> (Branch0 m -> Branch0 m)
stepEverywhere f Branch0 {..} = f (branch0 _terms _types children _edits)
  where
    children = fmap (step $ stepEverywhere f) _children

-- Creates a function to fix up the children field._1
-- If the action emptied a child, then remove the mapping,
-- otherwise update it.
-- Todo: Fix this in hashing & serialization instead of here?
getChildBranch :: NameSegment -> Branch0 m -> Branch m
getChildBranch seg b = fromMaybe empty $ Map.lookup seg (_children b)

setChildBranch :: NameSegment -> Branch m -> Branch0 m -> Branch0 m
setChildBranch seg b = over children (updateChildren seg b)

getPatch :: Applicative m => NameSegment -> Branch0 m -> m Patch
getPatch seg b = case Map.lookup seg (_edits b) of
  Nothing -> pure Patch.empty
  Just (_, p) -> p

getMaybePatch :: Applicative m => NameSegment -> Branch0 m -> m (Maybe Patch)
getMaybePatch seg b = case Map.lookup seg (_edits b) of
  Nothing -> pure Nothing
  Just (_, p) -> Just <$> p

modifyPatches ::
  Monad m => NameSegment -> (Patch -> Patch) -> Branch0 m -> m (Branch0 m)
modifyPatches seg f = mapMOf edits update
  where
    update m = do
      p' <- case Map.lookup seg m of
        Nothing -> pure $ f Patch.empty
        Just (_, p) -> f <$> p
      let h = H.hashPatch p'
      pure $ Map.insert seg (h, pure p') m

replacePatch :: Applicative m => NameSegment -> Patch -> Branch0 m -> Branch0 m
replacePatch n p = over edits (Map.insert n (H.hashPatch p, pure p))

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
  Applicative m =>
  Path ->
  (Branch m -> Branch m) ->
  Branch m ->
  Branch m
modifyAt path f = runIdentity . modifyAtM path (pure . f)

-- Modify the Branch at `path` with `f`, after creating it if necessary.
-- Because it's a `Branch`, it overwrites the history at `path`.
modifyAtM ::
  forall n m.
  Functor n =>
  Applicative m => -- because `Causal.cons` uses `pure`
  Path ->
  (Branch m -> n (Branch m)) ->
  Branch m ->
  n (Branch m)
modifyAtM path f b = case Path.uncons path of
  Nothing -> f b
  Just (seg, path) -> do
    -- Functor
    let child = getChildBranch seg (head b)
    child' <- modifyAtM path f child
    -- step the branch by updating its children according to fixup
    pure $ step (setChildBranch seg child') b

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
      Map.unionsWith (<>) . fmap \case
        (seg :< rest, action) -> Map.singleton seg [(rest, action)]
        _ -> error "groupByNextSegment called on current path, which shouldn't happen."
    pathLocation :: Path -> ActionLocation
    pathLocation (Path Empty) = HereActions
    pathLocation _ = ChildActions

-- todo: consider inlining these into Actions2
addTermName ::
  Referent -> NameSegment -> Metadata.Metadata -> Branch0 m -> Branch0 m
addTermName r new md =
  over terms (Metadata.insertWithMetadata (r, md) . Star3.insertD1 (r, new))

addTypeName ::
  TypeReference -> NameSegment -> Metadata.Metadata -> Branch0 m -> Branch0 m
addTypeName r new md =
  over types (Metadata.insertWithMetadata (r, md) . Star3.insertD1 (r, new))

deleteTermName :: Referent -> NameSegment -> Branch0 m -> Branch0 m
deleteTermName r n b
  | Star3.memberD1 (r, n) (view terms b) =
      over terms (Star3.deletePrimaryD1 (r, n)) b
deleteTermName _ _ b = b

deleteTypeName :: TypeReference -> NameSegment -> Branch0 m -> Branch0 m
deleteTypeName r n b
  | Star3.memberD1 (r, n) (view types b) =
      over types (Star3.deletePrimaryD1 (r, n)) b
deleteTypeName _ _ b = b

lca :: Monad m => Branch m -> Branch m -> m (Maybe (Branch m))
lca (Branch a) (Branch b) = fmap Branch <$> Causal.lca a b

transform :: Functor m => (forall a. m a -> n a) -> Branch m -> Branch n
transform f b = case _history b of
  causal -> Branch . Causal.transform f $ transformB0s f causal
  where
    transformB0 :: Functor m => (forall a. m a -> n a) -> Branch0 m -> Branch0 n
    transformB0 f b =
      b
        { _children = transform f <$> _children b,
          _edits = second f <$> _edits b
        }

    transformB0s ::
      Functor m =>
      (forall a. m a -> n a) ->
      Causal m Raw (Branch0 m) ->
      Causal m Raw (Branch0 n)
    transformB0s f = Causal.unsafeMapHashPreserving (transformB0 f)

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
  Monad m =>
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
    combineChildren :: These (Branch m) (Branch m) -> Branch m
    combineChildren = \case
      -- If we have a matching child in both base and head, squash the child head onto the
      -- child base recursively.
      (These base head) -> head `consBranchSnapshot` base
      -- This child has been deleted, recursively replace children with an empty branch.
      (This base) -> empty `consBranchSnapshot` base
      -- This child didn't exist in the base, we add any changes as a single commit
      (That head) -> discardHistory head
    combinedChildren :: Map NameSegment (Branch m)
    combinedChildren =
      Align.alignWith
        combineChildren
        (head baseBranch ^. children)
        (head headBranch ^. children)
