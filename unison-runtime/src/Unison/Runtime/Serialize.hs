{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime.Serialize where

import Control.Monad (replicateM)
import Data.Bits (Bits)
import Data.ByteString qualified as B
import Data.Bytes.Get hiding (getBytes)
import Data.Bytes.Get qualified as Ser
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Bytes.Signed (Unsigned)
import Data.Bytes.VarInt
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Map.Strict as Map (Map, fromList, toList)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector.Primitive qualified as BA
import Data.Word (Word64, Word8)
import GHC.Exts as IL (IsList (..))
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Reference (Id' (..), Reference, Reference' (Builtin, DerivedId), pattern Derived)
import Unison.Referent (Referent, pattern Con, pattern Ref)
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Exception
import Unison.Runtime.MCode
  ( BPrim1 (..),
    BPrim2 (..),
    UPrim1 (..),
    UPrim2 (..),
  )
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.EnumContainers as EC

unknownTag :: (MonadGet m) => String -> Word8 -> m a
unknownTag t w =
  remaining >>= \r ->
    exn $
      "unknown "
        ++ t
        ++ " word: "
        ++ show w
        ++ " ("
        ++ show (fromIntegral @_ @Int r)
        ++ " bytes remaining)"

class Tag t where
  tag2word :: t -> Word8
  word2tag :: (MonadGet m) => Word8 -> m t

putTag :: (MonadPut m) => (Tag t) => t -> m ()
putTag = putWord8 . tag2word

getTag :: (MonadGet m) => (Tag t) => m t
getTag = word2tag =<< getWord8

-- Some basics, moved over from V1 serialization
putChar :: (MonadPut m) => Char -> m ()
putChar = serialize . VarInt . fromEnum

getChar :: (MonadGet m) => m Char
getChar = toEnum . unVarInt <$> deserialize

putFloat :: (MonadPut m) => Double -> m ()
putFloat = serializeBE

getFloat :: (MonadGet m) => m Double
getFloat = deserializeBE

putBool :: (MonadPut m) => Bool -> m ()
putBool b = putWord8 (if b then 1 else 0)

getBool :: (MonadGet m) => m Bool
getBool = d =<< getWord8
  where
    d 0 = pure False
    d 1 = pure True
    d n = exn $ "getBool: bad tag: " ++ show n

putNat :: (MonadPut m) => Word64 -> m ()
putNat = putWord64be

getNat :: (MonadGet m) => m Word64
getNat = getWord64be

putInt :: (MonadPut m) => Int64 -> m ()
putInt = serializeBE

getInt :: (MonadGet m) => m Int64
getInt = deserializeBE

putLength ::
  ( MonadPut m,
    Integral n,
    Integral (Unsigned n),
    Bits n,
    Bits (Unsigned n)
  ) =>
  n ->
  m ()
putLength = serialize . VarInt

getLength ::
  ( MonadGet m,
    Integral n,
    Integral (Unsigned n),
    Bits n,
    Bits (Unsigned n)
  ) =>
  m n
getLength = unVarInt <$> deserialize

-- Checks for negatives, in case you put an Integer, which does not
-- behave properly for negative numbers.
putPositive ::
  (MonadPut m, Bits n, Bits (Unsigned n), Integral n, Integral (Unsigned n)) =>
  n ->
  m ()
putPositive n
  | n < 0 = exn $ "putPositive: negative number: " ++ show (toInteger n)
  | otherwise = serialize (VarInt n)

-- Reads as an Integer, then checks that the result will fit in the
-- result type.
getPositive :: forall m n. (Bounded n, Integral n, MonadGet m) => m n
getPositive = validate . unVarInt =<< deserialize
  where
    mx0 :: n
    mx0 = maxBound
    mx :: Integer
    mx = fromIntegral mx0

    validate :: Integer -> m n
    validate n
      | n <= mx = pure $ fromIntegral n
      | otherwise = fail $ "getPositive: overflow: " ++ show n

putFoldable ::
  (Foldable f, MonadPut m) => (a -> m ()) -> f a -> m ()
putFoldable putA as = do
  putLength (length as)
  traverse_ putA as

putMap :: (MonadPut m) => (a -> m ()) -> (b -> m ()) -> Map a b -> m ()
putMap putA putB m = putFoldable (putPair putA putB) (Map.toList m)

getList :: (MonadGet m) => m a -> m [a]
getList a = getLength >>= (`replicateM` a)

getMap :: (MonadGet m, Ord a) => m a -> m b -> m (Map a b)
getMap getA getB = Map.fromList <$> getList (getPair getA getB)

putEnumMap ::
  (MonadPut m) =>
  (EnumKey k) =>
  (k -> m ()) ->
  (v -> m ()) ->
  EnumMap k v ->
  m ()
putEnumMap pk pv m = putFoldable (putPair pk pv) (mapToList m)

getEnumMap :: (MonadGet m) => (EnumKey k) => m k -> m v -> m (EnumMap k v)
getEnumMap gk gv = mapFromList <$> getList (getPair gk gv)

putEnumSet :: (MonadPut m) => (EnumKey k) => (k -> m ()) -> EnumSet k -> m ()
putEnumSet pk s = putLength (setSize s) *> traverseSet_ pk s

getEnumSet :: (MonadGet m) => (EnumKey k) => m k -> m (EnumSet k)
getEnumSet gk = setFromList <$> getList gk

putMaybe :: (MonadPut m) => Maybe a -> (a -> m ()) -> m ()
putMaybe Nothing _ = putWord8 0
putMaybe (Just a) putA = putWord8 1 *> putA a

getMaybe :: (MonadGet m) => m a -> m (Maybe a)
getMaybe getA =
  getWord8 >>= \tag -> case tag of
    0 -> pure Nothing
    1 -> Just <$> getA
    _ -> unknownTag "Maybe" tag

putPair :: (MonadPut m) => (a -> m ()) -> (b -> m ()) -> (a, b) -> m ()
putPair putA putB (a, b) = putA a *> putB b

getPair :: (MonadGet m) => m a -> m b -> m (a, b)
getPair = liftA2 (,)

getBytes :: (MonadGet m) => m Bytes.Bytes
getBytes = Bytes.fromChunks <$> getList getBlock

putBytes :: (MonadPut m) => Bytes.Bytes -> m ()
putBytes = putFoldable putBlock . Bytes.chunks

getByteArray :: (MonadGet m) => m PA.ByteArray
getByteArray = PA.byteArrayFromList <$> getList getWord8

putByteArray :: (MonadPut m) => PA.ByteArray -> m ()
putByteArray a = putFoldable putWord8 (IL.toList a)

getArray :: (MonadGet m) => m a -> m (PA.Array a)
getArray getThing = PA.arrayFromList <$> getList getThing

putArray :: (MonadPut m) => (a -> m ()) -> PA.Array a -> m ()
putArray putThing a = putFoldable putThing (IL.toList a)

getBlock :: (MonadGet m) => m Bytes.Chunk
getBlock = getLength >>= fmap Bytes.byteStringToChunk . getByteString

putBlock :: (MonadPut m) => Bytes.Chunk -> m ()
putBlock b = putLength (BA.length b) *> putByteString (Bytes.chunkToByteString b)

putHash :: (MonadPut m) => Hash -> m ()
putHash h = do
  let bs = Hash.toByteString h
  putLength (B.length bs)
  putByteString bs

getHash :: (MonadGet m) => m Hash
getHash = do
  len <- getLength
  bs <- B.copy <$> Ser.getBytes len
  pure $ Hash.fromByteString bs

putReferent :: (MonadPut m) => Referent -> m ()
putReferent = \case
  Ref r -> do
    putWord8 0
    putReference r
  Con r ct -> do
    putWord8 1
    putConstructorReference r
    putConstructorType ct

getReferent :: (MonadGet m) => m Referent
getReferent = do
  tag <- getWord8
  case tag of
    0 -> Ref <$> getReference
    1 -> Con <$> getConstructorReference <*> getConstructorType
    _ -> unknownTag "getReferent" tag

getConstructorType :: (MonadGet m) => m CT.ConstructorType
getConstructorType =
  getWord8 >>= \case
    0 -> pure CT.Data
    1 -> pure CT.Effect
    t -> unknownTag "getConstructorType" t

putConstructorType :: (MonadPut m) => CT.ConstructorType -> m ()
putConstructorType = \case
  CT.Data -> putWord8 0
  CT.Effect -> putWord8 1

putText :: (MonadPut m) => Text -> m ()
putText text = do
  let bs = encodeUtf8 text
  putLength $ B.length bs
  putByteString bs

getText :: (MonadGet m) => m Text
getText = do
  len <- getLength
  bs <- B.copy <$> Ser.getBytes len
  pure $ decodeUtf8 bs

putReference :: (MonadPut m) => Reference -> m ()
putReference r = case r of
  Builtin name -> do
    putWord8 0
    putText name
  Derived hash i -> do
    putWord8 1
    putHash hash
    putLength i

getReference :: (MonadGet m) => m Reference
getReference = do
  tag <- getWord8
  case tag of
    0 -> Builtin <$> getText
    1 -> DerivedId <$> (Id <$> getHash <*> getLength)
    _ -> unknownTag "Reference" tag

putConstructorReference :: (MonadPut m) => ConstructorReference -> m ()
putConstructorReference (ConstructorReference r i) = do
  putReference r
  putLength i

getConstructorReference :: (MonadGet m) => m ConstructorReference
getConstructorReference =
  ConstructorReference <$> getReference <*> getLength

instance Tag UPrim1 where
  tag2word DECI = 0
  tag2word DECN = 1
  tag2word INCI = 2
  tag2word INCN = 3
  tag2word NEGI = 4
  tag2word SGNI = 5
  tag2word LZRO = 6
  tag2word TZRO = 7
  tag2word COMN = 8
  tag2word COMI = 9
  tag2word POPC = 10
  tag2word ABSF = 11
  tag2word EXPF = 12
  tag2word LOGF = 13
  tag2word SQRT = 14
  tag2word COSF = 15
  tag2word ACOS = 16
  tag2word COSH = 17
  tag2word ACSH = 18
  tag2word SINF = 19
  tag2word ASIN = 20
  tag2word SINH = 21
  tag2word ASNH = 22
  tag2word TANF = 23
  tag2word ATAN = 24
  tag2word TANH = 25
  tag2word ATNH = 26
  tag2word ITOF = 27
  tag2word NTOF = 28
  tag2word CEIL = 29
  tag2word FLOR = 30
  tag2word TRNF = 31
  tag2word RNDF = 32
  tag2word TRNC = 33

  word2tag 0 = pure DECI
  word2tag 1 = pure DECN
  word2tag 2 = pure INCI
  word2tag 3 = pure INCN
  word2tag 4 = pure NEGI
  word2tag 5 = pure SGNI
  word2tag 6 = pure LZRO
  word2tag 7 = pure TZRO
  word2tag 8 = pure COMN
  word2tag 9 = pure COMI
  word2tag 10 = pure POPC
  word2tag 11 = pure ABSF
  word2tag 12 = pure EXPF
  word2tag 13 = pure LOGF
  word2tag 14 = pure SQRT
  word2tag 15 = pure COSF
  word2tag 16 = pure ACOS
  word2tag 17 = pure COSH
  word2tag 18 = pure ACSH
  word2tag 19 = pure SINF
  word2tag 20 = pure ASIN
  word2tag 21 = pure SINH
  word2tag 22 = pure ASNH
  word2tag 23 = pure TANF
  word2tag 24 = pure ATAN
  word2tag 25 = pure TANH
  word2tag 26 = pure ATNH
  word2tag 27 = pure ITOF
  word2tag 28 = pure NTOF
  word2tag 29 = pure CEIL
  word2tag 30 = pure FLOR
  word2tag 31 = pure TRNF
  word2tag 32 = pure RNDF
  word2tag 33 = pure TRNC
  word2tag n = unknownTag "UPrim1" n

instance Tag UPrim2 where
  tag2word ADDI = 0
  tag2word ADDN = 1
  tag2word SUBI = 2
  tag2word SUBN = 3
  tag2word MULI = 4
  tag2word MULN = 5
  tag2word DIVI = 6
  tag2word MODI = 7
  tag2word DIVN = 8
  tag2word MODN = 9
  tag2word SHLI = 10
  tag2word SHLN = 11
  tag2word SHRI = 12
  tag2word SHRN = 13
  tag2word POWI = 14
  tag2word POWN = 15
  tag2word EQLI = 16
  tag2word EQLN = 17
  tag2word LEQI = 18
  tag2word LEQN = 19
  tag2word ANDN = 20
  tag2word ANDI = 21
  tag2word IORN = 22
  tag2word IORI = 23
  tag2word XORN = 24
  tag2word XORI = 25
  tag2word EQLF = 26
  tag2word LEQF = 27
  tag2word ADDF = 28
  tag2word SUBF = 29
  tag2word MULF = 30
  tag2word DIVF = 31
  tag2word ATN2 = 32
  tag2word POWF = 33
  tag2word LOGB = 34
  tag2word MAXF = 35
  tag2word MINF = 36
  tag2word CAST = 37
  tag2word DRPN = 38

  word2tag 0 = pure ADDI
  word2tag 1 = pure ADDN
  word2tag 2 = pure SUBI
  word2tag 3 = pure SUBN
  word2tag 4 = pure MULI
  word2tag 5 = pure MULN
  word2tag 6 = pure DIVI
  word2tag 7 = pure MODI
  word2tag 8 = pure DIVN
  word2tag 9 = pure MODN
  word2tag 10 = pure SHLI
  word2tag 11 = pure SHLN
  word2tag 12 = pure SHRI
  word2tag 13 = pure SHRN
  word2tag 14 = pure POWI
  word2tag 15 = pure POWN
  word2tag 16 = pure EQLI
  word2tag 17 = pure EQLN
  word2tag 18 = pure LEQI
  word2tag 19 = pure LEQN
  word2tag 20 = pure ANDN
  word2tag 21 = pure ANDI
  word2tag 22 = pure IORN
  word2tag 23 = pure IORI
  word2tag 24 = pure XORN
  word2tag 25 = pure XORI
  word2tag 26 = pure EQLF
  word2tag 27 = pure LEQF
  word2tag 28 = pure ADDF
  word2tag 29 = pure SUBF
  word2tag 30 = pure MULF
  word2tag 31 = pure DIVF
  word2tag 32 = pure ATN2
  word2tag 33 = pure POWF
  word2tag 34 = pure LOGB
  word2tag 35 = pure MAXF
  word2tag 36 = pure MINF
  word2tag 37 = pure CAST
  word2tag 38 = pure DRPN
  word2tag n = unknownTag "UPrim2" n

instance Tag BPrim1 where
  tag2word SIZT = 0
  tag2word USNC = 1
  tag2word UCNS = 2
  tag2word ITOT = 3
  tag2word NTOT = 4
  tag2word FTOT = 5
  tag2word TTOI = 6
  tag2word TTON = 7
  tag2word TTOF = 8
  tag2word PAKT = 9
  tag2word UPKT = 10
  tag2word VWLS = 11
  tag2word VWRS = 12
  tag2word SIZS = 13
  tag2word PAKB = 14
  tag2word UPKB = 15
  tag2word SIZB = 16
  tag2word FLTB = 17
  tag2word MISS = 18
  tag2word CACH = 19
  tag2word LKUP = 20
  tag2word LOAD = 21
  tag2word CVLD = 22
  tag2word VALU = 23
  tag2word TLTT = 24
  tag2word DBTX = 25
  tag2word SDBL = 26
  tag2word REFN = 27
  tag2word REFR = 28
  tag2word RRFC = 29
  tag2word TIKR = 30

  word2tag 0 = pure SIZT
  word2tag 1 = pure USNC
  word2tag 2 = pure UCNS
  word2tag 3 = pure ITOT
  word2tag 4 = pure NTOT
  word2tag 5 = pure FTOT
  word2tag 6 = pure TTOI
  word2tag 7 = pure TTON
  word2tag 8 = pure TTOF
  word2tag 9 = pure PAKT
  word2tag 10 = pure UPKT
  word2tag 11 = pure VWLS
  word2tag 12 = pure VWRS
  word2tag 13 = pure SIZS
  word2tag 14 = pure PAKB
  word2tag 15 = pure UPKB
  word2tag 16 = pure SIZB
  word2tag 17 = pure FLTB
  word2tag 18 = pure MISS
  word2tag 19 = pure CACH
  word2tag 20 = pure LKUP
  word2tag 21 = pure LOAD
  word2tag 22 = pure CVLD
  word2tag 23 = pure VALU
  word2tag 24 = pure TLTT
  word2tag 25 = pure DBTX
  word2tag 26 = pure SDBL
  word2tag 27 = pure REFN
  word2tag 28 = pure REFR
  word2tag 29 = pure RRFC
  word2tag 30 = pure TIKR
  word2tag n = unknownTag "BPrim1" n

instance Tag BPrim2 where
  tag2word EQLU = 0
  tag2word CMPU = 1
  tag2word DRPT = 2
  tag2word CATT = 3
  tag2word TAKT = 4
  tag2word EQLT = 5
  tag2word LEQT = 6
  tag2word LEST = 7
  tag2word DRPS = 8
  tag2word CATS = 9
  tag2word TAKS = 10
  tag2word CONS = 11
  tag2word SNOC = 12
  tag2word IDXS = 13
  tag2word SPLL = 14
  tag2word SPLR = 15
  tag2word TAKB = 16
  tag2word DRPB = 17
  tag2word IDXB = 18
  tag2word CATB = 19
  tag2word THRO = 20
  tag2word TRCE = 21
  tag2word SDBX = 22
  tag2word IXOT = 23
  tag2word IXOB = 24
  tag2word SDBV = 25
  tag2word REFW = 26

  word2tag 0 = pure EQLU
  word2tag 1 = pure CMPU
  word2tag 2 = pure DRPT
  word2tag 3 = pure CATT
  word2tag 4 = pure TAKT
  word2tag 5 = pure EQLT
  word2tag 6 = pure LEQT
  word2tag 7 = pure LEST
  word2tag 8 = pure DRPS
  word2tag 9 = pure CATS
  word2tag 10 = pure TAKS
  word2tag 11 = pure CONS
  word2tag 12 = pure SNOC
  word2tag 13 = pure IDXS
  word2tag 14 = pure SPLL
  word2tag 15 = pure SPLR
  word2tag 16 = pure TAKB
  word2tag 17 = pure DRPB
  word2tag 18 = pure IDXB
  word2tag 19 = pure CATB
  word2tag 20 = pure THRO
  word2tag 21 = pure TRCE
  word2tag 22 = pure SDBX
  word2tag 23 = pure IXOT
  word2tag 24 = pure IXOB
  word2tag 25 = pure SDBV
  word2tag 26 = pure REFW
  word2tag n = unknownTag "BPrim2" n
