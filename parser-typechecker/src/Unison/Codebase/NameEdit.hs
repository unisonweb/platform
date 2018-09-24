module Unison.Codebase.NameEdit where

import Data.Set (Set)
import Data.Foldable (toList)
import Unison.Reference (Reference)
import Unison.Hashable (Hashable, tokens)

data NameEdit =
  NameEdit { added :: Set Reference, removed :: Set Reference }

instance Semigroup NameEdit where
  NameEdit add1 del1 <> NameEdit add2 del2 = NameEdit (add1 <> add2) (del1 <> del2)

instance Hashable NameEdit where
  tokens (NameEdit added removed) = tokens (toList added, toList removed)