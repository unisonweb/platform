
module Unison.Runtime.Util where

import Data.Primitive.ByteArray qualified as BA

doubleToInt :: Double -> Int
doubleToInt d = BA.indexByteArray (BA.byteArrayFromList [d]) 0
{-# INLINE doubleToInt #-}

intToDouble :: Int -> Double
intToDouble w = BA.indexByteArray (BA.byteArrayFromList [w]) 0
{-# INLINE intToDouble #-}

