{-# LANGUAGE OverloadedStrings #-}

module Unison.PatternMatchCoverage.GrdTree
  ( GrdTree,
    GrdTreeF (..),
    pattern Leaf,
    pattern Grd,
    pattern Fork,
    prettyGrdTree,
  )
where

import Data.ListLike (ListLike)
import Unison.Prelude
import Unison.Util.Pretty
import Unison.Util.Recursion

-- | A @GrdTree@ is the simple language to desugar matches into. All
-- pattern matching constructs (/e.g./ structural pattern matching,
-- boolean guards, pattern guards, view patterns, etc) are desugared
-- into this simpler structure.
--
-- It is parameterized by the values at guard nodes, @n@, and the
-- values at the leaves, @l@. When desugaring, @n@ is
-- 'Unison.PatternMatchCoverage.PmGrd.PmGrd' and @l@ is the source
-- location. After annotating the @GrdTree@, @n@ is a refinement type
-- representing matching values and the @l@ is pairs of the
-- aforementioned refinement type and source location.
--
-- For example:
--
-- @
-- example : Optional Nat -> Nat
-- example = cases
--   None -> 0
--   Some x
--     | isEven x -> 0
--     | otherwise -> 1
-- @
--
-- is desugared into
--
-- @
--  ──┬─ None <- v0 ── srcloc
--    ├─ Some ( v1 :: ##Nat ) <- v0 ── let v2 = isEven v1 ── True <- v2 ── srcloc
--    └─ Some ( v3 :: ##Nat ) <- v0 ── srcloc
-- @
type GrdTree n l = Fix (GrdTreeF n l)

data GrdTreeF n l a
  = -- | A successful match
    LeafF l
  | -- | A constraint of some kind (structural pattern match, boolan guard, etc)
    GrdF n a
  | -- | A list of alternative matches, tried in order
    ForkF [a]
  deriving stock (Functor, Show)

prettyGrdTree :: forall n l s. (ListLike s Char, IsString s) => (n -> Pretty s) -> (l -> Pretty s) -> GrdTree n l -> Pretty s
prettyGrdTree prettyNode prettyLeaf = cata phi
  where
    phi = \case
      LeafF l -> prettyLeaf l
      GrdF n rest -> sep " " [prettyNode n, "──", rest]
      ForkF xs -> "──" <> group (sep "\n" $ makeTree xs)
    makeTree :: [Pretty s] -> [Pretty s]
    makeTree = \case
      [] -> []
      x : [] -> [sep " " ["──", x]]
      x0 : x1 : xs ->
        sep " " ["┬─", x0]
          : let go y0 = \case
                  [] -> [sep " " ["└─", y0]]
                  y1 : ys -> "├─ " <> y0 : go y1 ys
             in [indent "  " (sep "\n" (go x1 xs))]

pattern Leaf :: l -> GrdTree n l
pattern Leaf x = Fix (LeafF x)

pattern Grd :: n -> GrdTree n l -> GrdTree n l
pattern Grd x rest = Fix (GrdF x rest)

pattern Fork :: [GrdTree n l] -> GrdTree n l
pattern Fork alts = Fix (ForkF alts)

{-# COMPLETE Leaf, Grd, Fork #-}
