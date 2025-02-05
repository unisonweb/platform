module Unison.Util.Defn
  ( Defn (..),
  )
where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import GHC.Generics (Generic)

-- | A "definition" is either a term or a type.
data Defn term typ
  = TermDefn term
  | TypeDefn typ
  deriving stock (Generic, Functor, Foldable, Traversable, Show, Eq, Ord)

instance Bifunctor Defn where
  bimap f g = \case
    TermDefn x -> TermDefn (f x)
    TypeDefn y -> TypeDefn (g y)

instance Bifoldable Defn where
  bifoldMap f g = \case
    TermDefn x -> f x
    TypeDefn y -> g y

instance Bitraversable Defn where
  bitraverse f g = \case
    TermDefn x -> TermDefn <$> f x
    TypeDefn y -> TypeDefn <$> g y
