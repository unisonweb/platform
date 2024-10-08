{-# LANGUAGE DeriveGeneric #-}

module Unison.Codebase.Editor.DisplayObject where

import Data.Bifoldable
import Data.Bitraversable
import Unison.Prelude
import Unison.ShortHash (ShortHash)

data DisplayObject b a = BuiltinObject b | MissingObject ShortHash | UserObject a
  deriving (Eq, Ord, Show, Functor, Generic, Foldable, Traversable)

instance Bifunctor DisplayObject where
  bimap _ _ (MissingObject sh) = MissingObject sh
  bimap f _ (BuiltinObject b) = BuiltinObject (f b)
  bimap _ f (UserObject a) = UserObject (f a)

instance Bitraversable DisplayObject where
  bitraverse f _ (BuiltinObject b) = BuiltinObject <$> f b
  bitraverse _ _ (MissingObject sh) = pure (MissingObject sh)
  bitraverse _ g (UserObject a) = UserObject <$> g a

instance Bifoldable DisplayObject where
  bifoldMap = bifoldMapDefault
