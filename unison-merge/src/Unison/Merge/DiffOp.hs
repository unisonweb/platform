module Unison.Merge.DiffOp
  ( DiffOp (..),
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
