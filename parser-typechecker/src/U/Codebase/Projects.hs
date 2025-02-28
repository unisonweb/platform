module U.Codebase.Projects
  ( inferDependencyMounts,
  )
where

import Control.Lens (ifoldMap)
import Data.Bool (bool)
import Data.Map qualified as Map
import U.Codebase.Branch
import U.Codebase.Branch qualified as Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..))
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.NameSegment (libSegment)
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Monoid (ifoldMapM)

-- | Find all dependency mounts within a branch and the path to those mounts.
--
-- For a typical project this will return something like:
-- @[(lib.base, #abc), (lib.distributed, #def)]@
--
-- For the top-level name lookup of a user codebase it returns the project roots, and will return something like:
-- @[(public.nested.myproject.latest, #abc), (public.other.namespace.otherproject.main, #def)]@
inferDependencyMounts :: Branch Sqlite.Transaction -> Sqlite.Transaction [(Path, BranchHash)]
inferDependencyMounts =
  ifoldMapM
    ( \seg child -> do
        childBranch <- Causal.value child
        if seg == libSegment
          then
            ifoldMap (\depName depBranch -> [(Path.fromList [seg, depName], Causal.valueHash depBranch)])
              <$> Branch.nonEmptyChildren childBranch
          else -- If a given child has a lib child, then it's inferred to be a project root.
          -- This allows us to detect most project roots in loose code.
          -- Note, we only do this on children nested at least one level deep
          -- to avoid treating project roots as their own self-referential dependency
          -- mounts. Mount paths must not be empty.

            bool
              (map (first . Path.resolve $ Path.singleton seg) <$> inferDependencyMounts childBranch)
              (pure [(Path.singleton seg, Causal.valueHash child)])
              . Map.member libSegment
              =<< Branch.nonEmptyChildren childBranch
    )
    <=< Branch.nonEmptyChildren
