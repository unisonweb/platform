{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime.ANF.Serialize where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Bytes.Get hiding (getBytes)
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Bytes.VarInt
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Map as Map (Map, fromList, lookup)
import Data.Maybe (mapMaybe)
import Data.Sequence qualified as Seq
import Data.Serialize.Put (runPutLazy)
import Data.Text (Text)
import Data.Word (Word16, Word32, Word64)
import GHC.IsList qualified (fromList)
import GHC.Stack
import Unison.ABT.Normalized (Term (..))
import Unison.Reference (Reference, Reference' (Builtin), pattern Derived)
import Unison.Runtime.ANF as ANF hiding (Tag)
import Unison.Runtime.Exception
import Unison.Runtime.Serialize
import Unison.Util.EnumContainers qualified as EC
import Unison.Util.Text qualified as Util.Text
import Unison.Var (Type (ANFBlank), Var (..))
import Prelude hiding (getChar, putChar)

-- Version information is threaded through to allow handling
-- different formats. Transfer means that it is for saving
-- code/values to be restored later. Hash means we're just getting
-- bytes for hashing, so we don't need perfect information.
data Version = Transfer Word32 | Hash Word32

data TmTag
  = VarT
  | ForceT
  | AppT
  | HandleT
  | ShiftT
  | MatchT
  | LitT
  | NameRefT
  | NameVarT
  | LetDirT
  | LetIndT
  | BxLitT

data FnTag
  = FVarT
  | FCombT
  | FContT
  | FConT
  | FReqT
  | FPrimT
  | FForeignT

data MtTag
  = MIntT
  | MTextT
  | MReqT
  | MEmptyT
  | MDataT
  | MSumT
  | MNumT

data LtTag
  = IT
  | NT
  | FT
  | TT
  | CT
  | LMT
  | LYT

data BLTag
  = TextT
  | ListT
  | TmLinkT
  | TyLinkT
  | BytesT
  | QuoteT
  | CodeT
  | BArrT
  | PosT
  | NegT
  | CharT
  | FloatT
  | ArrT
  | CachedCodeT

data VaTag = PartialT | DataT | ContT | BLitT

data CoTag = KET | MarkT | PushT

instance Tag TmTag where
  tag2word = \case
    VarT -> 1
    ForceT -> 2
    AppT -> 3
    HandleT -> 4
    ShiftT -> 5
    MatchT -> 6
    LitT -> 7
    NameRefT -> 8
    NameVarT -> 9
    LetDirT -> 10
    LetIndT -> 11
    BxLitT -> 12
  word2tag = \case
    1 -> pure VarT
    2 -> pure ForceT
    3 -> pure AppT
    4 -> pure HandleT
    5 -> pure ShiftT
    6 -> pure MatchT
    7 -> pure LitT
    8 -> pure NameRefT
    9 -> pure NameVarT
    10 -> pure LetDirT
    11 -> pure LetIndT
    12 -> pure BxLitT
    n -> unknownTag "TmTag" n

instance Tag FnTag where
  tag2word = \case
    FVarT -> 0
    FCombT -> 1
    FContT -> 2
    FConT -> 3
    FReqT -> 4
    FPrimT -> 5
    FForeignT -> 6

  word2tag = \case
    0 -> pure FVarT
    1 -> pure FCombT
    2 -> pure FContT
    3 -> pure FConT
    4 -> pure FReqT
    5 -> pure FPrimT
    6 -> pure FForeignT
    n -> unknownTag "FnTag" n

instance Tag MtTag where
  tag2word = \case
    MIntT -> 0
    MTextT -> 1
    MReqT -> 2
    MEmptyT -> 3
    MDataT -> 4
    MSumT -> 5
    MNumT -> 6

  word2tag = \case
    0 -> pure MIntT
    1 -> pure MTextT
    2 -> pure MReqT
    3 -> pure MEmptyT
    4 -> pure MDataT
    5 -> pure MSumT
    6 -> pure MNumT
    n -> unknownTag "MtTag" n

instance Tag LtTag where
  tag2word = \case
    IT -> 0
    NT -> 1
    FT -> 2
    TT -> 3
    CT -> 4
    LMT -> 5
    LYT -> 6

  word2tag = \case
    0 -> pure IT
    1 -> pure NT
    2 -> pure FT
    3 -> pure TT
    4 -> pure CT
    5 -> pure LMT
    6 -> pure LYT
    n -> unknownTag "LtTag" n

instance Tag BLTag where
  tag2word = \case
    TextT -> 0
    ListT -> 1
    TmLinkT -> 2
    TyLinkT -> 3
    BytesT -> 4
    QuoteT -> 5
    CodeT -> 6
    BArrT -> 7
    PosT -> 8
    NegT -> 9
    CharT -> 10
    FloatT -> 11
    ArrT -> 12
    CachedCodeT -> 13

  word2tag = \case
    0 -> pure TextT
    1 -> pure ListT
    2 -> pure TmLinkT
    3 -> pure TyLinkT
    4 -> pure BytesT
    5 -> pure QuoteT
    6 -> pure CodeT
    7 -> pure BArrT
    8 -> pure PosT
    9 -> pure NegT
    10 -> pure CharT
    11 -> pure FloatT
    12 -> pure ArrT
    13 -> pure CachedCodeT
    t -> unknownTag "BLTag" t

instance Tag VaTag where
  tag2word = \case
    PartialT -> 0
    DataT -> 1
    ContT -> 2
    BLitT -> 3

  word2tag = \case
    0 -> pure PartialT
    1 -> pure DataT
    2 -> pure ContT
    3 -> pure BLitT
    t -> unknownTag "VaTag" t

instance Tag CoTag where
  tag2word = \case
    KET -> 0
    MarkT -> 1
    PushT -> 2
  word2tag = \case
    0 -> pure KET
    1 -> pure MarkT
    2 -> pure PushT
    t -> unknownTag "CoTag" t

index :: (Eq v) => [v] -> v -> Maybe Word64
index ctx u = go 0 ctx
  where
    go !_ [] = Nothing
    go n (v : vs)
      | v == u = Just n
      | otherwise = go (n + 1) vs

deindex :: (HasCallStack) => [v] -> Word64 -> v
deindex [] _ = exn "deindex: bad index"
deindex (v : vs) n
  | n == 0 = v
  | otherwise = deindex vs (n - 1)

pushCtx :: [v] -> [v] -> [v]
pushCtx us vs = reverse us ++ vs

putIndex :: (MonadPut m) => Word64 -> m ()
putIndex = serialize . VarInt

getIndex :: (MonadGet m) => m Word64
getIndex = unVarInt <$> deserialize

putVar :: (MonadPut m) => (Eq v) => [v] -> v -> m ()
putVar ctx v
  | Just i <- index ctx v = putIndex i
  | otherwise = exn "putVar: variable not in context"

getVar :: (MonadGet m) => [v] -> m v
getVar ctx = deindex ctx <$> getIndex

putArgs :: (MonadPut m) => (Eq v) => [v] -> [v] -> m ()
putArgs ctx is = putFoldable (putVar ctx) is

getArgs :: (MonadGet m) => [v] -> m [v]
getArgs ctx = getList (getVar ctx)

putCCs :: (MonadPut m) => [Mem] -> m ()
putCCs ccs = putLength n *> traverse_ putCC ccs
  where
    n = length ccs
    putCC UN = putWord8 0
    putCC BX = putWord8 1

getCCs :: (MonadGet m) => m [Mem]
getCCs =
  getList $
    getWord8 <&> \case
      0 -> UN
      1 -> BX
      _ -> exn "getCCs: bad calling convention"

-- Serializes a `SuperGroup`.
--
-- The Reference map allows certain term references to be switched out
-- for a given 64 bit word. This is used when re-hashing intermediate
-- code. For actual serialization, the empty map should be used, so
-- that the process is reversible. The purpose of this is merely to
-- strip out (mutual/)self-references when producing a byte sequence
-- to recompute a hash of a connected component of intermediate
-- definitons, since it is infeasible to
--
-- The EnumMap associates 'foreign' operations with a textual name
-- that is used as the serialized representation. Since they are
-- generated somewhat dynamically, it is not easy to associate them
-- with a fixed numbering like we can with POps.
putGroup ::
  (MonadPut m) =>
  (Var v) =>
  Map Reference Word64 ->
  EC.EnumMap FOp Text ->
  SuperGroup v ->
  m ()
putGroup refrep fops (Rec bs e) =
  putLength n
    *> traverse_ (putComb refrep fops ctx) cs
    *> putComb refrep fops ctx e
  where
    n = length us
    (us, cs) = unzip bs
    ctx = pushCtx us []

getGroup :: (MonadGet m) => (Var v) => m (SuperGroup v)
getGroup = do
  l <- getLength
  let n = fromIntegral l
      vs = getFresh <$> take l [0 ..]
      ctx = pushCtx vs []
  cs <- replicateM l (getComb ctx n)
  Rec (zip vs cs) <$> getComb ctx n

putCode :: (MonadPut m) => EC.EnumMap FOp Text -> Code -> m ()
putCode fops (CodeRep g c) = putGroup mempty fops g *> putCacheability c

getCode :: (MonadGet m) => Word32 -> m Code
getCode v = CodeRep <$> getGroup <*> getCache
  where
    getCache
      | v == 3 = getCacheability
      | otherwise = pure Uncacheable

putCacheability :: (MonadPut m) => Cacheability -> m ()
putCacheability Uncacheable = putWord8 0
putCacheability Cacheable = putWord8 1

getCacheability :: (MonadGet m) => m Cacheability
getCacheability =
  getWord8 >>= \case
    0 -> pure Uncacheable
    1 -> pure Cacheable
    n -> exn $ "getBLit: unrecognized cacheability byte: " ++ show n

putComb ::
  (MonadPut m) =>
  (Var v) =>
  Map Reference Word64 ->
  EC.EnumMap FOp Text ->
  [v] ->
  SuperNormal v ->
  m ()
putComb refrep fops ctx (Lambda ccs (TAbss us e)) =
  putCCs ccs *> putNormal refrep fops (pushCtx us ctx) e

getFresh :: (Var v) => Word64 -> v
getFresh n = freshenId n $ typed ANFBlank

getComb :: (MonadGet m) => (Var v) => [v] -> Word64 -> m (SuperNormal v)
getComb ctx frsh0 = do
  ccs <- getCCs
  let us = zipWith (\_ -> getFresh) ccs [frsh0 ..]
      frsh = frsh0 + fromIntegral (length ccs)
  Lambda ccs . TAbss us <$> getNormal (pushCtx us ctx) frsh

putNormal ::
  (MonadPut m) =>
  (Var v) =>
  Map Reference Word64 ->
  EC.EnumMap FOp Text ->
  [v] ->
  ANormal v ->
  m ()
putNormal refrep fops ctx tm = case tm of
  TVar v -> putTag VarT *> putVar ctx v
  TFrc v -> putTag ForceT *> putVar ctx v
  TApp f as -> putTag AppT *> putFunc refrep fops ctx f *> putArgs ctx as
  THnd rs h e ->
    putTag HandleT
      *> putRefs rs
      *> putVar ctx h
      *> putNormal refrep fops ctx e
  TShift r v e ->
    putTag ShiftT *> putReference r *> putNormal refrep fops (v : ctx) e
  TMatch v bs ->
    putTag MatchT
      *> putVar ctx v
      *> putBranches refrep fops ctx bs
  TLit l -> putTag LitT *> putLit l
  TBLit l -> putTag BxLitT *> putLit l
  TName v (Left r) as e ->
    putTag NameRefT
      *> pr
      *> putArgs ctx as
      *> putNormal refrep fops (v : ctx) e
    where
      pr
        | Just w <- Map.lookup r refrep = putWord64be w
        | otherwise = putReference r
  TName v (Right u) as e ->
    putTag NameVarT
      *> putVar ctx u
      *> putArgs ctx as
      *> putNormal refrep fops (v : ctx) e
  TLets Direct us ccs l e ->
    putTag LetDirT
      *> putCCs ccs
      *> putNormal refrep fops ctx l
      *> putNormal refrep fops (pushCtx us ctx) e
  TLets (Indirect w) us ccs l e ->
    putTag LetIndT
      *> putWord16be w
      *> putCCs ccs
      *> putNormal refrep fops ctx l
      *> putNormal refrep fops (pushCtx us ctx) e
  _ -> exn "putNormal: malformed term"

getNormal :: (MonadGet m) => (Var v) => [v] -> Word64 -> m (ANormal v)
getNormal ctx frsh0 =
  getTag >>= \case
    VarT -> TVar <$> getVar ctx
    ForceT -> TFrc <$> getVar ctx
    AppT -> TApp <$> getFunc ctx <*> getArgs ctx
    HandleT -> THnd <$> getRefs <*> getVar ctx <*> getNormal ctx frsh0
    ShiftT ->
      flip TShift v <$> getReference <*> getNormal (v : ctx) (frsh0 + 1)
      where
        v = getFresh frsh0
    MatchT -> TMatch <$> getVar ctx <*> getBranches ctx frsh0
    LitT -> TLit <$> getLit
    BxLitT -> TBLit <$> getLit
    NameRefT ->
      TName v . Left
        <$> getReference
        <*> getArgs ctx
        <*> getNormal (v : ctx) (frsh0 + 1)
      where
        v = getFresh frsh0
    NameVarT ->
      TName v . Right
        <$> getVar ctx
        <*> getArgs ctx
        <*> getNormal (v : ctx) (frsh0 + 1)
      where
        v = getFresh frsh0
    LetDirT -> do
      ccs <- getCCs
      let l = length ccs
          frsh = frsh0 + fromIntegral l
          us = getFresh <$> take l [frsh0 ..]
      TLets Direct us ccs
        <$> getNormal ctx frsh0
        <*> getNormal (pushCtx us ctx) frsh
    LetIndT -> do
      w <- getWord16be
      ccs <- getCCs
      let l = length ccs
          frsh = frsh0 + fromIntegral l
          us = getFresh <$> take l [frsh0 ..]
      TLets (Indirect w) us ccs
        <$> getNormal ctx frsh0
        <*> getNormal (pushCtx us ctx) frsh

putFunc ::
  (MonadPut m) =>
  (Var v) =>
  Map Reference Word64 ->
  EC.EnumMap FOp Text ->
  [v] ->
  Func v ->
  m ()
putFunc refrep fops ctx f = case f of
  FVar v -> putTag FVarT *> putVar ctx v
  FComb r
    | Just w <- Map.lookup r refrep -> putTag FCombT *> putWord64be w
    | otherwise -> putTag FCombT *> putReference r
  FCont v -> putTag FContT *> putVar ctx v
  FCon r c -> putTag FConT *> putReference r *> putCTag c
  FReq r c -> putTag FReqT *> putReference r *> putCTag c
  FPrim (Left p) -> putTag FPrimT *> putPOp p
  FPrim (Right f)
    | Just nm <- EC.lookup f fops ->
        putTag FForeignT *> putText nm
    | otherwise ->
        exn $ "putFunc: could not serialize foreign operation: " ++ show f

getFunc :: (MonadGet m) => (Var v) => [v] -> m (Func v)
getFunc ctx =
  getTag >>= \case
    FVarT -> FVar <$> getVar ctx
    FCombT -> FComb <$> getReference
    FContT -> FCont <$> getVar ctx
    FConT -> FCon <$> getReference <*> getCTag
    FReqT -> FReq <$> getReference <*> getCTag
    FPrimT -> FPrim . Left <$> getPOp
    FForeignT -> exn "getFunc: can't deserialize a foreign func"

putPOp :: (MonadPut m) => POp -> m ()
putPOp op
  | Just w <- Map.lookup op pop2word = putWord16be w
  | otherwise = exn $ "putPOp: unknown POp: " ++ show op

getPOp :: (MonadGet m) => m POp
getPOp =
  getWord16be >>= \w -> case Map.lookup w word2pop of
    Just op -> pure op
    Nothing -> exn "getPOp: unknown enum code"

pOpCode :: POp -> Word16
pOpCode op = case op of
  ADDI -> 0
  SUBI -> 1
  MULI -> 2
  DIVI -> 3
  SGNI -> 4
  NEGI -> 5
  MODI -> 6
  POWI -> 7
  SHLI -> 8
  SHRI -> 9
  INCI -> 10
  DECI -> 11
  LEQI -> 12
  EQLI -> 13
  ADDN -> 14
  SUBN -> 15
  MULN -> 16
  DIVN -> 17
  MODN -> 18
  TZRO -> 19
  LZRO -> 20
  POWN -> 21
  SHLN -> 22
  SHRN -> 23
  ANDN -> 24
  IORN -> 25
  XORN -> 26
  COMN -> 27
  INCN -> 28
  DECN -> 29
  LEQN -> 30
  EQLN -> 31
  ADDF -> 32
  SUBF -> 33
  MULF -> 34
  DIVF -> 35
  MINF -> 36
  MAXF -> 37
  LEQF -> 38
  EQLF -> 39
  POWF -> 40
  EXPF -> 41
  SQRT -> 42
  LOGF -> 43
  LOGB -> 44
  ABSF -> 45
  CEIL -> 46
  FLOR -> 47
  TRNF -> 48
  RNDF -> 49
  COSF -> 50
  ACOS -> 51
  COSH -> 52
  ACSH -> 53
  SINF -> 54
  ASIN -> 55
  SINH -> 56
  ASNH -> 57
  TANF -> 58
  ATAN -> 59
  TANH -> 60
  ATNH -> 61
  ATN2 -> 62
  CATT -> 63
  TAKT -> 64
  DRPT -> 65
  SIZT -> 66
  UCNS -> 67
  USNC -> 68
  EQLT -> 69
  LEQT -> 70
  PAKT -> 71
  UPKT -> 72
  CATS -> 73
  TAKS -> 74
  DRPS -> 75
  SIZS -> 76
  CONS -> 77
  SNOC -> 78
  IDXS -> 79
  BLDS -> 80
  VWLS -> 81
  VWRS -> 82
  SPLL -> 83
  SPLR -> 84
  PAKB -> 85
  UPKB -> 86
  TAKB -> 87
  DRPB -> 88
  IDXB -> 89
  SIZB -> 90
  FLTB -> 91
  CATB -> 92
  ITOF -> 93
  NTOF -> 94
  ITOT -> 95
  NTOT -> 96
  TTOI -> 97
  TTON -> 98
  TTOF -> 99
  FTOT -> 100
  FORK -> 101
  EQLU -> 102
  CMPU -> 103
  EROR -> 104
  PRNT -> 105
  INFO -> 106
  POPC -> 107
  MISS -> 108
  CACH -> 109
  LKUP -> 110
  LOAD -> 111
  CVLD -> 112
  SDBX -> 113
  VALU -> 114
  TLTT -> 115
  TRCE -> 116
  ATOM -> 117
  TFRC -> 118
  DBTX -> 119
  IXOT -> 120
  IXOB -> 121
  SDBL -> 122
  SDBV -> 123

pOpAssoc :: [(POp, Word16)]
pOpAssoc = map (\op -> (op, pOpCode op)) [minBound .. maxBound]

pop2word :: Map POp Word16
pop2word = fromList pOpAssoc

word2pop :: Map Word16 POp
word2pop = fromList $ swap <$> pOpAssoc
  where
    swap (x, y) = (y, x)

putLit :: (MonadPut m) => Lit -> m ()
putLit (I i) = putTag IT *> putInt i
putLit (N n) = putTag NT *> putNat n
putLit (F f) = putTag FT *> putFloat f
putLit (T t) = putTag TT *> putText (Util.Text.toText t)
putLit (C c) = putTag CT *> putChar c
putLit (LM r) = putTag LMT *> putReferent r
putLit (LY r) = putTag LYT *> putReference r

getLit :: (MonadGet m) => m Lit
getLit =
  getTag >>= \case
    IT -> I <$> getInt
    NT -> N <$> getNat
    FT -> F <$> getFloat
    TT -> T . Util.Text.fromText <$> getText
    CT -> C <$> getChar
    LMT -> LM <$> getReferent
    LYT -> LY <$> getReference

putBLit :: (MonadPut m) => Version -> BLit -> m ()
putBLit _ (Text t) = putTag TextT *> putText (Util.Text.toText t)
putBLit v (List s) = putTag ListT *> putFoldable (putValue v) s
putBLit _ (TmLink r) = putTag TmLinkT *> putReferent r
putBLit _ (TyLink r) = putTag TyLinkT *> putReference r
putBLit _ (Bytes b) = putTag BytesT *> putBytes b
putBLit v (Quote vl) = putTag QuoteT *> putValue v vl
putBLit v (Code (CodeRep sg ch)) =
  putTag tag *> putGroup mempty mempty sg
  where
    -- Hashing treats everything as uncacheable for consistent
    -- results.
    tag
      | Cacheable <- ch,
        Transfer _ <- v =
          CachedCodeT
      | otherwise = CodeT
putBLit _ (BArr a) = putTag BArrT *> putByteArray a
putBLit _ (Pos n) = putTag PosT *> putPositive n
putBLit _ (Neg n) = putTag NegT *> putPositive n
putBLit _ (Char c) = putTag CharT *> putChar c
putBLit _ (Float d) = putTag FloatT *> putFloat d
putBLit v (Arr a) = putTag ArrT *> putFoldable (putValue v) a

getBLit :: (MonadGet m) => Version -> m BLit
getBLit v =
  getTag >>= \case
    TextT -> Text . Util.Text.fromText <$> getText
    ListT -> List . Seq.fromList <$> getList (getValue v)
    TmLinkT -> TmLink <$> getReferent
    TyLinkT -> TyLink <$> getReference
    BytesT -> Bytes <$> getBytes
    QuoteT -> Quote <$> getValue v
    CodeT -> Code . flip CodeRep Uncacheable <$> getGroup
    BArrT -> BArr <$> getByteArray
    PosT -> Pos <$> getPositive
    NegT -> Neg <$> getPositive
    CharT -> Char <$> getChar
    FloatT -> Float <$> getFloat
    ArrT -> Arr . GHC.IsList.fromList <$> getList (getValue v)
    CachedCodeT -> Code . flip CodeRep Cacheable <$> getGroup

putRefs :: (MonadPut m) => [Reference] -> m ()
putRefs rs = putFoldable putReference rs

getRefs :: (MonadGet m) => m [Reference]
getRefs = getList getReference

putBranches ::
  (MonadPut m) =>
  (Var v) =>
  Map Reference Word64 ->
  EC.EnumMap FOp Text ->
  [v] ->
  Branched (ANormal v) ->
  m ()
putBranches refrep fops ctx bs = case bs of
  MatchEmpty -> putTag MEmptyT
  MatchIntegral m df -> do
    putTag MIntT
    putEnumMap putWord64be (putNormal refrep fops ctx) m
    putMaybe df $ putNormal refrep fops ctx
  MatchText m df -> do
    putTag MTextT
    putMap (putText . Util.Text.toText) (putNormal refrep fops ctx) m
    putMaybe df $ putNormal refrep fops ctx
  MatchRequest m (TAbs v df) -> do
    putTag MReqT
    putMap putReference (putEnumMap putCTag (putCase refrep fops ctx)) m
    putNormal refrep fops (v : ctx) df
  MatchData r m df -> do
    putTag MDataT
    putReference r
    putEnumMap putCTag (putCase refrep fops ctx) m
    putMaybe df $ putNormal refrep fops ctx
  MatchSum m -> do
    putTag MSumT
    putEnumMap putWord64be (putCase refrep fops ctx) m
  MatchNumeric r m df -> do
    putTag MNumT
    putReference r
    putEnumMap putWord64be (putNormal refrep fops ctx) m
    putMaybe df $ putNormal refrep fops ctx
  _ -> exn "putBranches: malformed intermediate term"

getBranches ::
  (MonadGet m) => (Var v) => [v] -> Word64 -> m (Branched (ANormal v))
getBranches ctx frsh0 =
  getTag >>= \case
    MEmptyT -> pure MatchEmpty
    MIntT ->
      MatchIntegral
        <$> getEnumMap getWord64be (getNormal ctx frsh0)
        <*> getMaybe (getNormal ctx frsh0)
    MTextT ->
      MatchText
        <$> getMap (Util.Text.fromText <$> getText) (getNormal ctx frsh0)
        <*> getMaybe (getNormal ctx frsh0)
    MReqT ->
      MatchRequest
        <$> getMap getReference (getEnumMap getCTag (getCase ctx frsh0))
        <*> (TAbs v <$> getNormal (v : ctx) (frsh0 + 1))
      where
        v = getFresh frsh0
    MDataT ->
      MatchData
        <$> getReference
        <*> getEnumMap getCTag (getCase ctx frsh0)
        <*> getMaybe (getNormal ctx frsh0)
    MSumT -> MatchSum <$> getEnumMap getWord64be (getCase ctx frsh0)
    MNumT ->
      MatchNumeric
        <$> getReference
        <*> getEnumMap getWord64be (getNormal ctx frsh0)
        <*> getMaybe (getNormal ctx frsh0)

putCase ::
  (MonadPut m) =>
  (Var v) =>
  Map Reference Word64 ->
  EC.EnumMap FOp Text ->
  [v] ->
  ([Mem], ANormal v) ->
  m ()
putCase refrep fops ctx (ccs, (TAbss us e)) =
  putCCs ccs *> putNormal refrep fops (pushCtx us ctx) e

getCase :: (MonadGet m) => (Var v) => [v] -> Word64 -> m ([Mem], ANormal v)
getCase ctx frsh0 = do
  ccs <- getCCs
  let l = length ccs
      frsh = frsh0 + fromIntegral l
      us = getFresh <$> take l [frsh0 ..]
  (,) ccs . TAbss us <$> getNormal (pushCtx us ctx) frsh

putCTag :: (MonadPut m) => CTag -> m ()
putCTag c = serialize (VarInt $ fromEnum c)

getCTag :: (MonadGet m) => m CTag
getCTag = toEnum . unVarInt <$> deserialize

putGroupRef :: (MonadPut m) => GroupRef -> m ()
putGroupRef (GR r i) =
  putReference r *> putWord64be i

getGroupRef :: (MonadGet m) => m GroupRef
getGroupRef = GR <$> getReference <*> getWord64be

-- Notes
--
-- Starting with version 4 of the value format, it is expected that
-- unboxed data does not actually occur in the values being sent. For
-- most values this was not a problem:
--
--   - Partial applications had no way of directly including unboxed
--     values, because they all result from surface level unison
--     applications
--   - Unboxed values in Data only occurred to represent certain
--     builtin types. Those have been replaced by BLits.
--
-- However, some work was required to make sure no unboxed data ended
-- up in Cont. The runtime has been modified to avoid using the
-- unboxed stack in generated code, so now only builtins use it,
-- effectively. Since continuations are never captured inside builtins
-- (and even if we wanted to do that, we could arrange for a clean
-- unboxed stack), this is no longer a problem, either.
--
-- So, unboxed data is completely absent from the format. We are now
-- exchanging unison surface values, effectively.
putValue :: (MonadPut m) => Version -> Value -> m ()
putValue v (Partial gr vs) =
  putTag PartialT
    *> putGroupRef gr
    *> putFoldable (putUBValue v) vs
putValue v (Data r t vs) =
  putTag DataT
    *> putReference r
    *> putWord64be t
    *> putFoldable (putUBValue v) vs
putValue v (Cont bs k) =
  putTag ContT
    *> putFoldable (putUBValue v) bs
    *> putCont v k
putValue v (BLit l) =
  putTag BLitT *> putBLit v l

putUBValue :: (MonadPut m) => Version -> UBValue -> m ()
putUBValue _v Left {} = exn "putUBValue: Unboxed values no longer supported"
putUBValue v (Right a) = putValue v a

getValue :: (MonadGet m) => Version -> m Value
getValue v =
  getTag >>= \case
    PartialT
      | Transfer vn <- v,
        vn < 4 -> do
          gr <- getGroupRef
          getList getWord64be >>= assertEmptyUnboxed
          bs <- getList getUBValue
          pure $ Partial gr bs
      | otherwise -> do
          gr <- getGroupRef
          vs <- getList getUBValue
          pure $ Partial gr vs
    DataT
      | Transfer vn <- v,
        vn < 4 -> do
          r <- getReference
          w <- getWord64be
          getList getWord64be >>= assertEmptyUnboxed
          vs <- getList getUBValue
          pure $ Data r w vs
      | otherwise -> do
          r <- getReference
          w <- getWord64be
          vs <- getList getUBValue
          pure $ Data r w vs
    ContT
      | Transfer vn <- v,
        vn < 4 -> do
          getList getWord64be >>= assertEmptyUnboxed
          bs <- getList getUBValue
          k <- getCont v
          pure $ Cont bs k
      | otherwise -> do
          bs <- getList getUBValue
          k <- getCont v
          pure $ Cont bs k
    BLitT -> BLit <$> getBLit v
  where
    -- Only Boxed values are supported.
    getUBValue :: (MonadGet m) => m UBValue
    getUBValue = Right <$> getValue v
    assertEmptyUnboxed :: (MonadGet m) => [a] -> m ()
    assertEmptyUnboxed [] = pure ()
    assertEmptyUnboxed _ = exn "getValue: unboxed values no longer supported"

putCont :: (MonadPut m) => Version -> Cont -> m ()
putCont _ KE = putTag KET
putCont v (Mark a rs ds k) =
  putTag MarkT
    *> putWord64be a
    *> putFoldable putReference rs
    *> putMap putReference (putValue v) ds
    *> putCont v k
putCont v (Push f n gr k) =
  putTag PushT
    *> putWord64be f
    *> putWord64be n
    *> putGroupRef gr
    *> putCont v k

getCont :: (MonadGet m) => Version -> m Cont
getCont v =
  getTag >>= \case
    KET -> pure KE
    MarkT
      | Transfer vn <- v,
        vn < 4 -> do
          getWord64be >>= assert0 "unboxed arg size"
          ba <- getWord64be
          refs <- getList getReference
          vals <- getMap getReference (getValue v)
          cont <- getCont v
          pure $ Mark ba refs vals cont
      | otherwise ->
          Mark
            <$> getWord64be
            <*> getList getReference
            <*> getMap getReference (getValue v)
            <*> getCont v
    PushT
      | Transfer vn <- v,
        vn < 4 -> do
          getWord64be >>= assert0 "unboxed frame size"
          bf <- getWord64be
          getWord64be >>= assert0 "unboxed arg size"
          ba <- getWord64be
          gr <- getGroupRef
          cont <- getCont v
          pure $ Push bf ba gr cont
      | otherwise ->
          Push
            <$> getWord64be
            <*> getWord64be
            <*> getGroupRef
            <*> getCont v
  where
    assert0 _name 0 = pure ()
    assert0 name n = exn $ "getCont: malformed intermediate term. Expected " <> name <> " to be 0, but got " <> show n

deserializeCode :: ByteString -> Either String Code
deserializeCode bs = runGetS (getVersion >>= getCode) bs
  where
    getVersion =
      getWord32be >>= \case
        n | 1 <= n && n <= 3 -> pure n
        n -> fail $ "deserializeGroup: unknown version: " ++ show n

serializeCode :: EC.EnumMap FOp Text -> Code -> ByteString
serializeCode fops co = runPutS (putVersion *> putCode fops co)
  where
    putVersion = putWord32be codeVersion

-- | Serializes a `SuperGroup` for rehashing.
--
-- Expected as arguments are some code, and the `Reference` that
-- refers to it. In particular, if the code refers to itself by
-- reference, or if the code is part of a mututally-recursive set of
-- definitions (which have a common hash), the reference used as part
-- of that (mutual) recursion must be supplied.
--
-- Using that reference, we find all references in the code to that
-- connected component. In the resulting byte string, those references
-- are instead replaced by positions in a listing of the connected
-- component. This means that the byte string is independent of the
-- hash used for the self reference. Only the order matters (which is
-- determined by the `Reference`). Then the bytes can be re-hashed to
-- establish a new hash for the connected component. This operation
-- should be idempotent as long as the indexing is preserved.
--
-- Supplying a `Builtin` reference is not supported. Such code
-- shouldn't be subject to rehashing.
serializeGroupForRehash ::
  (Var v) =>
  EC.EnumMap FOp Text ->
  Reference ->
  SuperGroup v ->
  L.ByteString
serializeGroupForRehash _ (Builtin _) _ =
  error "serializeForRehash: builtin reference"
serializeGroupForRehash fops (Derived h _) sg =
  runPutLazy $ putGroup refrep fops sg
  where
    f r@(Derived h' i) | h == h' = Just (r, i)
    f _ = Nothing
    refrep = Map.fromList . mapMaybe f $ groupTermLinks sg

getVersionedValue :: (MonadGet m) => m Value
getVersionedValue = getVersion >>= getValue . Transfer
  where
    getVersion =
      getWord32be >>= \case
        n
          | n < 1 -> fail $ "deserializeValue: unknown version: " ++ show n
          | n < 3 -> fail $ "deserializeValue: unsupported version: " ++ show n
          | n <= 4 -> pure n
          | otherwise -> fail $ "deserializeValue: unknown version: " ++ show n

deserializeValue :: ByteString -> Either String Value
deserializeValue bs = runGetS getVersionedValue bs

serializeValue :: Value -> ByteString
serializeValue v =
  runPutS (putVersion *> putValue (Transfer valueVersion) v)
  where
    putVersion = putWord32be valueVersion

-- This serializer is used exclusively for hashing unison values.
-- For this reason, it doesn't prefix the string with the current
-- version, so that only genuine changes in the way things are
-- serialized will change hashes.
--
-- The 4 prefix is used because we were previously including the
-- version in the hash, so to maintain the same hashes, we need to
-- include the extra bytes that were previously there.
--
-- Additionally, any major serialization changes should consider
-- retaining this representation as much as possible, even if it
-- becomes a separate format, because there is no need to parse from
-- the hash serialization, just generate and hash it.
serializeValueForHash :: Value -> L.ByteString
serializeValueForHash v = runPutLazy (putPrefix *> putValue (Hash 4) v)
  where
    putPrefix = putWord32be 4

valueVersion :: Word32
valueVersion = 4

codeVersion :: Word32
codeVersion = 3
