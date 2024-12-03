{-# LANGUAGE BangPatterns #-}
-- used for unsafe pointer equality
{-# LANGUAGE MagicHash #-}

module Unison.Runtime.SparseVector where

import Control.Monad.ST (ST)
import Data.Bits ((.&.), (.|.))
import Data.Bits qualified as B
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as MUV
import GHC.Exts qualified as Exts
import Prelude hiding (unzip)

-- Denotes a `Nat -> Maybe a`.
-- Representation is a `Vector a` along with a bitset
-- that encodes the index of each element.
-- Ex: `[(1,a), (5,b)]` is encoded as (100010, [a,b])
data SparseVector bits a = SparseVector
  { indices :: !bits,
    elements :: !(UV.Vector a)
  }

-- todo: instance (UV.Unbox a, B.FiniteBits bits, Num n)
--   => Num (SparseVector bits n)

-- Denotationally: `map f v n = f <$> v n`
map :: (UV.Unbox a, UV.Unbox b) => (a -> b) -> SparseVector bits a -> SparseVector bits b
map f v = v {elements = UV.map f (elements v)}

-- Denotationally, a mask is a `Nat -> Bool`, so this implementation
-- means: `mask ok v n = if ok n then v n else Nothing`
mask ::
  forall a bits.
  (UV.Unbox a, B.FiniteBits bits) =>
  bits ->
  SparseVector bits a ->
  SparseVector bits a
mask bits a =
  if indices' == bits
    then a -- check if mask is a superset
    else SparseVector indices' $
      UV.create $ do
        vec <- MUV.new (B.popCount indices')
        go vec (indices a) bits 0 0
  where
    indices' = indices a .&. bits
    eas = elements a
    go :: MUV.STVector s a -> bits -> bits -> Int -> Int -> ST s (MUV.STVector s a)
    go !out !indAs !indBs !i !k =
      if indAs == B.zeroBits || indBs == B.zeroBits
        then pure out
        else
          let (!a1, !b1) = (B.countTrailingZeros indAs, B.countTrailingZeros indBs)
           in if a1 == b1
                then do
                  MUV.write out k (eas UV.! (i + a1))
                  go
                    out
                    (indAs `B.shiftR` (a1 + 1))
                    (indBs `B.shiftR` (b1 + 1))
                    (i + 1)
                    (k + 1)
                else
                  if a1 < b1
                    then
                      go
                        out
                        (indAs `B.shiftR` (a1 + 1))
                        indBs
                        (i + 1)
                        k
                    else go out indAs (indBs `B.shiftR` (b1 + 1)) i k

-- Denotationally: `zipWith f a b n = f <$> a n <*> b n`, in other words,
-- this takes the intersection of the two shapes.
zipWith ::
  (UV.Unbox a, UV.Unbox b, UV.Unbox c, B.FiniteBits bits) =>
  (a -> b -> c) ->
  SparseVector bits a ->
  SparseVector bits b ->
  SparseVector bits c
zipWith f a b =
  if indices a `eq` indices b || indices a == indices b
    then SparseVector (indices a) (UV.zipWith f (elements a) (elements b))
    else
      let indices' = indices a .&. indices b
          a' = mask indices' a
          b' = mask indices' b
       in SparseVector indices' (UV.zipWith f (elements a') (elements b'))

_1 :: (UV.Unbox a, UV.Unbox b) => SparseVector bits (a, b) -> SparseVector bits a
_1 = fst . unzip

_2 :: (UV.Unbox a, UV.Unbox b) => SparseVector bits (a, b) -> SparseVector bits b
_2 = snd . unzip

-- Denotationally: `unzip p = (\n -> fst <$> p n, \n -> snd <$> p n)`
unzip ::
  (UV.Unbox a, UV.Unbox b) =>
  SparseVector bits (a, b) ->
  (SparseVector bits a, SparseVector bits b)
unzip (SparseVector inds ps) =
  let (as, bs) = UV.unzip ps
   in (SparseVector inds as, SparseVector inds bs)

-- Denotationally: `choose bs a b n = if bs n then a n else b n`
choose ::
  (B.FiniteBits bits, UV.Unbox a) =>
  bits ->
  SparseVector bits a ->
  SparseVector bits a ->
  SparseVector bits a
choose bits t f
  | B.zeroBits == bits = f
  | B.complement bits == B.zeroBits = t
  | otherwise -- it's a mix of true and false
    =
      merge (mask bits t) (mask (B.complement bits) f)

-- Denotationally: `merge a b n = a n <|> b n`
merge ::
  forall a bits.
  (B.FiniteBits bits, UV.Unbox a) =>
  SparseVector bits a ->
  SparseVector bits a ->
  SparseVector bits a
merge a b = SparseVector indices' tricky
  where
    indices' = indices a .|. indices b
    tricky = UV.create $ do
      vec <- MUV.new (B.popCount indices')
      go vec (indices a) (indices b) 0 0 0
    (!eas, !ebs) = (elements a, elements b)
    go :: MUV.STVector s a -> bits -> bits -> Int -> Int -> Int -> ST s (MUV.STVector s a)
    go !out !indAs !indBs !i !j !k =
      if indAs == B.zeroBits || indBs == B.zeroBits
        then pure out
        else
          let (!a1, !b1) = (B.countTrailingZeros indAs, B.countTrailingZeros indBs)
           in if a1 == b1
                then do
                  MUV.write out k (eas UV.! (i + a1))
                  go
                    out
                    (indAs `B.shiftR` (a1 + 1))
                    (indBs `B.shiftR` (b1 + 1))
                    (i + 1)
                    (j + 1)
                    (k + 1)
                else
                  if a1 < b1
                    then do
                      MUV.write out k (eas UV.! (i + a1))
                      go
                        out
                        (indAs `B.shiftR` (a1 + 1))
                        indBs
                        (i + 1)
                        j
                        (k + 1)
                    else do
                      MUV.write out k (ebs UV.! (j + a1))
                      go out indAs (indBs `B.shiftR` (b1 + 1)) i (j + 1) (k + 1)

-- Pointer equality a la Scala.
eq :: a -> a -> Bool
eq x y = Exts.isTrue# (Exts.reallyUnsafePtrEquality# x y Exts.==# 1#)
{-# INLINE eq #-}
