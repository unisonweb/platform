{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Parser.Ann where

import Data.List.NonEmpty (NonEmpty)
import Unison.Lexer.Pos qualified as L

data Ann
  = -- Used for things like Builtins which don't have a source position.
    Intrinsic -- { sig :: String, start :: L.Pos, end :: L.Pos }
  | External
  | -- Indicates that the term was generated from something at this location.
    -- E.g. generated record field accessors (get, modify, etc.) are generated from their field definition, so are tagged
    -- with @GeneratedFrom <field position>@
    GeneratedFrom Ann
  | Ann {start :: L.Pos, end :: L.Pos}
  deriving (Eq, Ord, Show)

startingLine :: Ann -> Maybe L.Line
startingLine (Ann (L.line -> line) _) = Just line
startingLine (GeneratedFrom a) = startingLine a
startingLine _ = Nothing

instance Monoid Ann where
  mempty = External

-- | This instance is commutative.
instance Semigroup Ann where
  Ann s1 e1 <> Ann s2 e2 = Ann (min s1 s2) (max e1 e2)
  -- If we have a concrete location from a file, use it
  External <> a = a
  a <> External = a
  Intrinsic <> a = a
  a <> Intrinsic = a
  GeneratedFrom a <> b = a <> b
  a <> GeneratedFrom b = a <> b

-- | Checks whether an annotation contains a given position
-- i.e. pos ∈ [start, end)
--
-- >>> Intrinsic `contains` L.Pos 1 1
-- False
--
-- >>> External `contains` L.Pos 1 1
-- False
--
-- >>> Ann (L.Pos 0 0) (L.Pos 0 10) `contains` L.Pos 0 5
-- True
--
-- >>> Ann (L.Pos 0 0) (L.Pos 0 10) `contains` L.Pos 0 10
-- False
contains :: Ann -> L.Pos -> Bool
contains Intrinsic _ = False
contains External _ = False
contains (Ann start end) p = start <= p && p < end
contains (GeneratedFrom ann) p = contains ann p

class Annotated a where
  ann :: a -> Ann

instance Annotated Ann where
  ann = id

instance (Annotated a) => Annotated [a] where
  ann = foldMap ann

instance (Annotated a) => Annotated (NonEmpty a) where
  ann = foldMap ann
