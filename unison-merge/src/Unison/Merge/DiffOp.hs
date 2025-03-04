module Unison.Merge.DiffOp
  ( DiffOp (..),
    DiffOp2 (..),
  )
where

import Unison.Merge.Updated (Updated)

-- | A diff operation is one of:
--
--   * An add (where nothing was)
--   * A delete (of the thing that was)
--   * An update (from old to new)
data DiffOp a
  = DiffOp'Add !a
  | DiffOp'Delete !a
  | DiffOp'Update !(Updated a)
  deriving stock (Foldable, Functor, Show, Traversable)

-- | Like 'DiffOp', but updates are tagged as propagated (True) or not (False).
--
-- This could be cleaned up a bit, but eh, it works for now. Historical context: the concept of a propagated upddate was
-- not in the initial version of merge (which was only concerned with the merge algorithm and producing the correct
-- output). However, it is now being incorporated, because when e.g. viewing a diff, we do want to see propagated, not
-- drop them.
data DiffOp2 a
  = DiffOp2'Add !a
  | DiffOp2'Delete !a
  | DiffOp2'Update !(Updated a) !Bool {- is propagated? -}
  deriving stock (Foldable, Functor, Show, Traversable)
