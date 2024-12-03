{-# LANGUAGE GADTs #-}

module Unison.Runtime.Vector where

import Data.MemoCombinators qualified as Memo
import Data.Vector.Unboxed qualified as UV
import Unison.Prelude

-- A `Vec a` denotes a `Nat -> Maybe a`
data Vec a where
  Scalar :: a -> Vec a
  Vec :: (UV.Unbox a) => UV.Vector a -> Vec a
  Pair :: Vec a -> Vec b -> Vec (a, b)
  Choose :: Vec Bool -> Vec a -> Vec a -> Vec a
  Mux :: Vec Nat -> Vec (Vec a) -> Vec a

-- todo: maybe make representation `(UV.Vector Nat -> UnboxedMap Nat a, Bound)`
-- `UnboxedMap Nat a = (UV.Vector Nat, UV.Vector a)`
-- UnboxedMap Nat could be implemented as an `UArray`
-- `Bound` is Nat, max possible index
-- then easy to implement `+`, `-`, etc

type Nat = Word64

mu :: Vec a -> Nat -> Maybe a
mu v = case v of
  Scalar a -> const (Just a)
  Vec vs -> \i -> vs UV.!? fromIntegral i
  Choose cond t f ->
    let (condr, tr, tf) = (mu cond, mu t, mu f)
     in \i -> condr i >>= \b -> if b then tr i else tf i
  Mux mux branches ->
    let muxr = mu mux
        branchesr = Memo.integral $ let f = mu branches in \i -> mu <$> f i
     in \i -> do j <- muxr i; b <- branchesr j; b i
  Pair v1 v2 ->
    let (v1r, v2r) = (mu v1, mu v2)
     in \i -> liftA2 (,) (v1r i) (v2r i)

-- Returns the maximum `Nat` for which `mu v` may return `Just`.
bound :: Nat -> Vec a -> Nat
bound width v = case v of
  Scalar _ -> width
  Vec vs -> fromIntegral $ UV.length vs
  Pair v1 v2 -> bound width v1 `min` bound width v2
  Choose cond _ _ -> bound width cond
  Mux mux _ -> bound width mux

toList :: Vec a -> [a]
toList v =
  let n = bound maxBound v
      muv = mu v
   in catMaybes $ muv <$> [0 .. n]
