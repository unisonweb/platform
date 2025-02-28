module Unison.Merge.HumanDiffOp
  ( HumanDiffOp (..),
  )
where

import Data.Set.NonEmpty (NESet)
import Unison.Merge.Updated (Updated)
import Unison.Name (Name)

-- | A diff operation is one of:
--
--   * An add (where nothing was)
--   * A delete (of the thing that was)
--   * An update (from old to new)
--   * A propagated update (from old to new)
--   * An alias of some definition(s) on the other side
--   * A rename from some definition(s) on the other side
data HumanDiffOp ref
  = HumanDiffOp'Add !ref
  | HumanDiffOp'Delete !ref
  | HumanDiffOp'Update !(Updated ref)
  | HumanDiffOp'PropagatedUpdate !(Updated ref)
  | HumanDiffOp'AliasOf !ref !(NESet Name {- existing names -})
  | -- The definition at this location was renamed from the given set of names to the current place
    HumanDiffOp'RenamedFrom !ref !(NESet Name)
  | -- The definition at this location was renamed to the given set of names from the current place
    HumanDiffOp'RenamedTo !ref !(NESet Name)
  deriving stock (Show)
