module U.Codebase.Branch.Diff
  ( TreeDiff (..),
    NameChanges (..),
    DefinitionDiffs (..),
    Diff (..),
    diffBranches,
    allNameChanges,
    streamNameChanges,
  )
where

import Control.Comonad.Cofree
import Control.Lens (ifoldMap)
import Control.Lens qualified as Lens
import Data.Functor.Compose (Compose (..))
import Data.Map qualified as Map
import Data.Semialign qualified as Align
import Data.Set qualified as Set
import Data.These
import U.Codebase.Branch
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Branch.Type qualified as Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Monoid (ifoldMapM)

data Diff a = Diff
  { adds :: Set a,
    removals :: Set a
  }
  deriving (Show, Eq, Ord)

-- | Represents the changes to definitions at a given path, not including child paths.
--
-- Note: doesn't yet include any info on patch diffs. Feel free to add it.
data DefinitionDiffs = DefinitionDiffs
  { termDiffs :: Map NameSegment (Diff Referent),
    typeDiffs :: Map NameSegment (Diff Reference)
    -- patchDiffs :: Map NameSegment (Diff ())
  }
  deriving stock (Show, Eq, Ord)

instance Semigroup DefinitionDiffs where
  a <> b =
    DefinitionDiffs
      { termDiffs = termDiffs a <> termDiffs b,
        typeDiffs = typeDiffs a <> typeDiffs b
      }

instance Monoid DefinitionDiffs where
  mempty = DefinitionDiffs mempty mempty

-- | A tree of local diffs. Each node of the tree contains the definition diffs at that path.
newtype TreeDiff m = TreeDiff
  { unTreeDiff :: Cofree (Compose (Map NameSegment) m) DefinitionDiffs
  }
  deriving stock (Show, Eq, Ord)

instance (Applicative m) => Semigroup (TreeDiff m) where
  TreeDiff (a :< Compose mas) <> TreeDiff (b :< Compose mbs) =
    TreeDiff $ (a <> b) :< Compose (Map.unionWith mergeCofrees mas mbs)
    where
      mergeCofrees = liftA2 (\x y -> unTreeDiff (TreeDiff x <> TreeDiff y))

instance (Applicative m) => Monoid (TreeDiff m) where
  mempty = TreeDiff (mempty :< Compose mempty)

-- | A summary of a 'TreeDiff', containing all names added and removed.
-- Note that there isn't a clear notion of a name "changing" since conflicts might muddy the notion
-- by having multiple copies of both the from and to names, so we just talk about adds and
-- removals instead.
data NameChanges = NameChanges
  { termNameAdds :: [(Name, Referent)],
    termNameRemovals :: [(Name, Referent)],
    typeNameAdds :: [(Name, Reference)],
    typeNameRemovals :: [(Name, Reference)]
  }
  deriving stock (Show, Eq)

instance Semigroup NameChanges where
  NameChanges a b c d <> NameChanges a2 b2 c2 d2 =
    NameChanges (a <> a2) (b <> b2) (c <> c2) (d <> d2)

instance Monoid NameChanges where
  mempty = NameChanges mempty mempty mempty mempty

-- | Diff two Branches, returning a tree containing all of the changes
diffBranches :: Branch Sqlite.Transaction -> Branch Sqlite.Transaction -> Sqlite.Transaction (TreeDiff Sqlite.Transaction)
diffBranches from to = do
  fromChildren <- V2Branch.nonEmptyChildren from
  toChildren <- V2Branch.nonEmptyChildren to
  let termDiffs = diffMap (Branch.terms from) (Branch.terms to)
  let typeDiffs = diffMap (Branch.types from) (Branch.types to)
  let defDiff = DefinitionDiffs {termDiffs, typeDiffs}
  let childDiff :: Map NameSegment (Sqlite.Transaction (Cofree (Compose (Map NameSegment) Sqlite.Transaction) DefinitionDiffs))
      childDiff =
        Align.align fromChildren toChildren
          & mapMaybe \case
            This ca -> Just do
              -- TODO: For the names index we really don't need to know which exact
              -- names were removed, we just need to delete from the index using a
              -- prefix query, this would be faster than crawling to get all the deletes.
              removedChildBranch <- Causal.value ca
              unTreeDiff <$> diffBranches removedChildBranch Branch.empty
            That ca -> Just do
              newChildBranch <- Causal.value ca
              unTreeDiff <$> diffBranches Branch.empty newChildBranch
            These fromC toC
              | Causal.valueHash fromC == Causal.valueHash toC ->
                  -- This child didn't change.
                  Nothing
              | otherwise -> Just $ do
                  fromChildBranch <- Causal.value fromC
                  toChildBranch <- Causal.value toC
                  diffBranches fromChildBranch toChildBranch >>= \case
                    TreeDiff (defDiffs :< Compose mchildren) -> do
                      pure $ (defDiffs :< Compose mchildren)
  pure $
    TreeDiff (defDiff :< Compose childDiff)
  where
    diffMap :: forall ref. (Ord ref) => Map NameSegment (Map ref (Sqlite.Transaction MdValues)) -> Map NameSegment (Map ref (Sqlite.Transaction MdValues)) -> Map NameSegment (Diff ref)
    diffMap l r =
      Align.align l r
        & fmap \case
          This refs -> Diff {removals = Map.keysSet refs, adds = mempty}
          That refs -> Diff {removals = mempty, adds = Map.keysSet refs}
          These l' r' ->
            let lRefs = Map.keysSet l'
                rRefs = Map.keysSet r'
             in Diff {removals = lRefs `Set.difference` rRefs, adds = rRefs `Set.difference` lRefs}

-- | Get a summary of all of the name adds and removals from a tree diff.
--
-- The provided name will be prepended to all names in the output diff, and can be useful if diffing branches at a
-- specific sub-tree, but you can pass 'Nothing' if you're diffing from the root.
allNameChanges ::
  (Monad m) =>
  Maybe Name ->
  TreeDiff m ->
  m NameChanges
allNameChanges mayPrefix treediff = do
  streamNameChanges mayPrefix treediff \_prefix changes -> pure changes

-- | Stream a summary of all of the name adds and removals from a tree diff.
-- Callback is passed the diff from one namespace level at a time, with the name representing
-- that location.
-- Accumulator is folded strictly, use '()' if you don't need one.
streamNameChanges ::
  (Monad m, Monoid r) =>
  Maybe Name ->
  TreeDiff m ->
  (Maybe Name -> NameChanges -> m r) ->
  m r
streamNameChanges namePrefix (TreeDiff (DefinitionDiffs {termDiffs, typeDiffs} :< Compose children)) f = do
  let (termNameAdds, termNameRemovals) =
        termDiffs
          & ifoldMap \ns diff ->
            let name = appendName ns
             in (listifyNames name $ adds diff, listifyNames name $ removals diff)
  let (typeNameAdds, typeNameRemovals) =
        typeDiffs
          & ifoldMap \ns diff ->
            let name = appendName ns
             in (listifyNames name $ adds diff, listifyNames name $ removals diff)
  let nameChanges = NameChanges {termNameAdds, termNameRemovals, typeNameAdds, typeNameRemovals}
  acc <-
    if nameChanges == mempty
      then pure mempty
      else f namePrefix nameChanges
  childAcc <-
    children
      & ifoldMapM
        ( \ns mchildTree -> do
            childTree <- mchildTree
            streamNameChanges (Just $ appendName ns) (TreeDiff childTree) f
        )
  pure $! acc <> childAcc
  where
    appendName :: NameSegment -> Name
    appendName =
      case namePrefix of
        Nothing -> Name.fromSegment
        Just prefix -> (prefix Lens.|>)
    listifyNames :: (Name -> Set ref -> [(Name, ref)])
    listifyNames name xs =
      xs
        & Set.toList
        & fmap (name,)
