{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime.MCode.Serialize
  ( putComb,
    getComb,
    putCombIx,
    getCombIx,
  )
where

import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Bytes.VarInt
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Exts (IsList (..))
import Unison.Runtime.ANF (PackedTag (..))
import Unison.Runtime.Array (Array)
import Unison.Runtime.Foreign.Function.Type (ForeignFunc)
import Unison.Runtime.MCode hiding (MatchT)
import Unison.Runtime.Serialize
import Unison.Util.Text qualified as Util.Text
import Prelude hiding (getChar, putChar)

data CombT = LamT | CachedClosureT

instance Tag CombT where
  tag2word LamT = 0
  tag2word CachedClosureT = 1

  word2tag 0 = pure LamT
  word2tag 1 = pure CachedClosureT
  word2tag n = unknownTag "CombT" n

putPackedTag :: (MonadPut m) => PackedTag -> m ()
putPackedTag (PackedTag w) = pWord w

getPackedTag :: (MonadGet m) => m PackedTag
getPackedTag = PackedTag <$> gWord

putComb :: (MonadPut m) => (clos -> m ()) -> GComb clos comb -> m ()
putComb pClos = \case
  (Lam a f body) ->
    putTag LamT *> pInt a *> pInt f *> putSection body
  (CachedVal w v) ->
    putTag CachedClosureT *> putNat w *> pClos v

getComb :: (MonadGet m) => m (GComb Void CombIx)
getComb =
  getTag >>= \case
    LamT ->
      Lam <$> gInt <*> gInt <*> getSection
    CachedClosureT -> error "getComb: Unexpected serialized Cached Closure"

getMForeignFunc :: (MonadGet m) => m ForeignFunc
getMForeignFunc = do
  toEnum <$> gInt

putMForeignFunc :: (MonadPut m) => ForeignFunc -> m ()
putMForeignFunc = pInt . fromEnum

data SectionT
  = AppT
  | CallT
  | JumpT
  | MatchT
  | YieldT
  | InsT
  | LetT
  | DieT
  | ExitT
  | DMatchT
  | NMatchT
  | RMatchT

instance Tag SectionT where
  tag2word AppT = 0
  tag2word CallT = 1
  tag2word JumpT = 2
  tag2word MatchT = 3
  tag2word YieldT = 4
  tag2word InsT = 5
  tag2word LetT = 6
  tag2word DieT = 7
  tag2word ExitT = 8
  tag2word DMatchT = 9
  tag2word NMatchT = 10
  tag2word RMatchT = 11

  word2tag 0 = pure AppT
  word2tag 1 = pure CallT
  word2tag 2 = pure JumpT
  word2tag 3 = pure MatchT
  word2tag 4 = pure YieldT
  word2tag 5 = pure InsT
  word2tag 6 = pure LetT
  word2tag 7 = pure DieT
  word2tag 8 = pure ExitT
  word2tag 9 = pure DMatchT
  word2tag 10 = pure NMatchT
  word2tag 11 = pure RMatchT
  word2tag i = unknownTag "SectionT" i

putSection :: (MonadPut m) => GSection cix -> m ()
putSection = \case
  App b r a -> putTag AppT *> serialize b *> putRef r *> putArgs a
  Call b cix _comb a -> putTag CallT *> serialize b *> putCombIx cix *> putArgs a
  Jump i a -> putTag JumpT *> pInt i *> putArgs a
  Match i b -> putTag MatchT *> pInt i *> putBranch b
  Yield a -> putTag YieldT *> putArgs a
  Ins i s -> putTag InsT *> putInstr i *> putSection s
  Let s ci f bd ->
    putTag LetT
      *> putSection s
      *> putCombIx ci
      *> pInt f
      *> putSection bd
  Die s -> putTag DieT *> serialize s
  Exit -> putTag ExitT
  DMatch mr i b -> putTag DMatchT *> putMaybe mr putReference *> pInt i *> putBranch b
  NMatch mr i b -> putTag NMatchT *> putMaybe mr putReference *> pInt i *> putBranch b
  RMatch i pu bs ->
    putTag RMatchT
      *> pInt i
      *> putSection pu
      *> putEnumMap pWord putBranch bs

getSection :: (MonadGet m) => m Section
getSection =
  getTag >>= \case
    AppT -> App <$> deserialize <*> getRef <*> getArgs
    CallT -> do
      skipCheck <- deserialize
      cix <- getCombIx
      args <- getArgs
      pure $ Call skipCheck cix cix args
    JumpT -> Jump <$> gInt <*> getArgs
    MatchT -> Match <$> gInt <*> getBranch
    YieldT -> Yield <$> getArgs
    InsT -> Ins <$> getInstr <*> getSection
    LetT ->
      Let <$> getSection <*> getCombIx <*> gInt <*> getSection
    DieT -> Die <$> deserialize
    ExitT -> pure Exit
    DMatchT -> DMatch <$> getMaybe getReference <*> gInt <*> getBranch
    NMatchT -> NMatch <$> getMaybe getReference <*> gInt <*> getBranch
    RMatchT ->
      RMatch <$> gInt <*> getSection <*> getEnumMap gWord getBranch

data InstrT
  = Prim1T
  | PrimLT
  | PrimXXT
  | PrimLLT
  | PrimIXT
  | PrimXIT
  | PrimDXT
  | PrimXDT
  | PrimTXT
  | PrimXTT
  | PrimMXT
  | PrimXMT
  | PrimYXT
  | PrimXYT
  | ForeignCallT
  | SetDynT
  | CaptureT
  | NameT
  | InfoT
  | PackT
  | LitT
  | PrintT
  | ResetT
  | ForkT
  | AtomicallyT
  | SeqT
  | TryForceT
  | RefCAST
  | SandboxingFailureT

instance Tag InstrT where
  tag2word Prim1T = 0
  tag2word PrimLT = 1
  tag2word PrimXXT = 2
  tag2word PrimLLT = 3
  tag2word PrimIXT = 4
  tag2word PrimXIT = 5
  tag2word PrimDXT = 6
  tag2word PrimXDT = 7
  tag2word PrimTXT = 8
  tag2word PrimXTT = 9
  tag2word PrimMXT = 10
  tag2word PrimXMT = 11
  tag2word PrimYXT = 12
  tag2word PrimXYT = 13
  tag2word ForeignCallT = 14
  tag2word SetDynT = 15
  tag2word CaptureT = 16
  tag2word NameT = 17
  tag2word InfoT = 18
  tag2word PackT = 19
  tag2word LitT = 20
  tag2word PrintT = 21
  tag2word ResetT = 22
  tag2word ForkT = 23
  tag2word AtomicallyT = 24
  tag2word SeqT = 25
  tag2word TryForceT = 26
  tag2word RefCAST = 27
  tag2word SandboxingFailureT = 28

  word2tag 0 = pure Prim1T
  word2tag 1 = pure PrimLT
  word2tag 2 = pure PrimXXT
  word2tag 3 = pure PrimLLT
  word2tag 4 = pure PrimIXT
  word2tag 5 = pure PrimXIT
  word2tag 6 = pure PrimDXT
  word2tag 7 = pure PrimXDT
  word2tag 8 = pure PrimTXT
  word2tag 9 = pure PrimXTT
  word2tag 10 = pure PrimMXT
  word2tag 11 = pure PrimXMT
  word2tag 12 = pure PrimYXT
  word2tag 13 = pure PrimXYT
  word2tag 14 = pure ForeignCallT
  word2tag 15 = pure SetDynT
  word2tag 16 = pure CaptureT
  word2tag 17 = pure NameT
  word2tag 18 = pure InfoT
  word2tag 19 = pure PackT
  word2tag 20 = pure LitT
  word2tag 21 = pure PrintT
  word2tag 22 = pure ResetT
  word2tag 23 = pure ForkT
  word2tag 24 = pure AtomicallyT
  word2tag 25 = pure SeqT
  word2tag 26 = pure TryForceT
  word2tag 27 = pure RefCAST
  word2tag 28 = pure SandboxingFailureT
  word2tag n = unknownTag "InstrT" n

putInstr :: (MonadPut m) => GInstr cix -> m ()
putInstr = \case
  (Prim1 p i) -> putTag Prim1T *> putTag p *> pInt i
  (PrimL p l) -> putTag PrimLT *> putTag p *> putLit l
  (PrimXX bp i j) -> putTag PrimXXT *> putTag bp *> pInt i *> pInt j
  (PrimLL bp l r) -> putTag PrimLLT *> putTag bp *> putLit l *> putLit r
  (PrimIX bp tt i j) ->
    putTag PrimIXT *> putTag bp *> putUTypeTag tt *> pInt i *> pInt j
  (PrimXI bp i tt j) ->
    putTag PrimXIT *> putTag bp *> pInt i *> putUTypeTag tt *> pInt j
  (PrimDX bp i j) ->
    putTag PrimDXT *> putTag bp *> putFloat i *> pInt j
  (PrimXD bp i j) ->
    putTag PrimXDT *> putTag bp *> pInt i *> putFloat j
  (PrimTX bp i j) ->
    putTag PrimMXT *> putTag bp *> putUText i *> pInt j
  (PrimXT bp i j) ->
    putTag PrimXMT *> putTag bp *> pInt i *> putUText j
  (PrimMX bp i j) ->
    putTag PrimMXT *> putTag bp *> putReferent i *> pInt j
  (PrimXM bp i j) ->
    putTag PrimXMT *> putTag bp *> pInt i *> putReferent j
  (PrimYX bp i j) ->
    putTag PrimYXT *> putTag bp *> putReference i *> pInt j
  (PrimXY bp i j) ->
    putTag PrimXYT *> putTag bp *> pInt i *> putReference j
  (RefCAS i j k) -> putTag RefCAST *> pInt i *> pInt j *> putArg k
  (ForeignCall b ff a) -> putTag ForeignCallT *> serialize b *> putMForeignFunc ff *> putArgs a
  (SetDyn w i) -> putTag SetDynT *> pWord w *> pInt i
  (Capture w) -> putTag CaptureT *> pWord w
  (Name r a) -> putTag NameT *> putRef r *> putArgs a
  (Info s) -> putTag InfoT *> serialize s
  (Pack r w a) -> putTag PackT *> putReference r *> putPackedTag w *> putArgs a
  (Lit l) -> putTag LitT *> putLit l
  (Print i) -> putTag PrintT *> pInt i
  (Reset s) -> putTag ResetT *> putEnumSet pWord s
  (Fork i) -> putTag ForkT *> pInt i
  (Atomically i) -> putTag AtomicallyT *> pInt i
  (Seq a) -> putTag SeqT *> putArgs a
  (TryForce i) -> putTag TryForceT *> pInt i
  (SandboxingFailure {}) ->
    -- Sandboxing failures should only exist in code we're actively running, it shouldn't be serialized.
    error "putInstr: Unexpected serialized Sandboxing Failure"

getInstr :: (MonadGet m) => m Instr
getInstr =
  getTag >>= \case
    Prim1T -> Prim1 <$> getTag <*> gInt
    PrimLT -> PrimL <$> getTag <*> getLit
    PrimXXT -> PrimXX <$> getTag <*> gInt <*> gInt
    PrimLLT -> PrimLL <$> getTag <*> getLit <*> getLit
    PrimIXT -> PrimIX <$> getTag <*> getUTypeTag <*> gInt <*> gInt
    PrimXIT -> PrimXI <$> getTag <*> gInt <*> getUTypeTag <*> gInt
    PrimDXT -> PrimDX <$> getTag <*> getFloat <*> gInt
    PrimXDT -> PrimXD <$> getTag <*> gInt <*> getFloat
    PrimTXT -> PrimTX <$> getTag <*> getUText <*> gInt
    PrimXTT -> PrimXT <$> getTag <*> gInt <*> getUText
    PrimMXT -> PrimMX <$> getTag <*> getReferent <*> gInt
    PrimXMT -> PrimXM <$> getTag <*> gInt <*> getReferent
    PrimYXT -> PrimYX <$> getTag <*> getReference <*> gInt
    PrimXYT -> PrimXY <$> getTag <*> gInt <*> getReference
    RefCAST -> RefCAS <$> gInt <*> gInt <*> getArg
    ForeignCallT -> ForeignCall <$> deserialize <*> getMForeignFunc <*> getArgs
    SetDynT -> SetDyn <$> gWord <*> gInt
    CaptureT -> Capture <$> gWord
    NameT -> Name <$> getRef <*> getArgs
    InfoT -> Info <$> deserialize
    PackT -> Pack <$> getReference <*> getPackedTag <*> getArgs
    LitT -> Lit <$> getLit
    PrintT -> Print <$> gInt
    ResetT -> Reset <$> getEnumSet gWord
    ForkT -> Fork <$> gInt
    AtomicallyT -> Atomically <$> gInt
    SeqT -> Seq <$> getArgs
    TryForceT -> TryForce <$> gInt
    SandboxingFailureT -> error "getInstr: Unexpected serialized Sandboxing Failure"

data ArgsT
  = ZArgsT
  | Arg1T
  | Arg2T
  | ArgRT
  | ArgNT
  | ArgVT

instance Tag ArgsT where
  tag2word ZArgsT = 0
  tag2word Arg1T = 1
  tag2word Arg2T = 2
  tag2word ArgRT = 3
  tag2word ArgNT = 4
  tag2word ArgVT = 5

  word2tag 0 = pure ZArgsT
  word2tag 1 = pure Arg1T
  word2tag 2 = pure Arg2T
  word2tag 3 = pure ArgRT
  word2tag 4 = pure ArgNT
  word2tag 5 = pure ArgVT
  word2tag n = unknownTag "ArgsT" n

data ArgT = MLitT | IxT

instance Tag ArgT where
  tag2word MLitT = 0
  tag2word IxT = 1

  word2tag 0 = pure MLitT
  word2tag 1 = pure IxT
  word2tag n = unknownTag "ArgT" n

putArg :: (MonadPut m) => Arg -> m ()
putArg (MLit l) = putTag MLitT *> putLit l
putArg (Ix i) = putTag IxT *> pInt i

getArg :: (MonadGet m) => m Arg
getArg = getTag >>= \case
  MLitT -> MLit <$> getLit
  IxT -> Ix <$> gInt

putArgs :: (MonadPut m) => Args -> m ()
putArgs ZArgs = putTag ZArgsT
putArgs (VArg1 i) = putTag Arg1T *> putArg i
putArgs (VArg2 i j) = putTag Arg2T *> putArg i *> putArg j
putArgs (VArgR i j) = putTag ArgRT *> pInt i *> pInt j
putArgs (VArgN pa) = putTag ArgNT *> putArgArr pa
putArgs (VArgV i) = putTag ArgVT *> pInt i

getArgs :: (MonadGet m) => m Args
getArgs =
  getTag >>= \case
    ZArgsT -> pure ZArgs
    Arg1T -> VArg1 <$> getArg
    Arg2T -> VArg2 <$> getArg <*> getArg
    ArgRT -> VArgR <$> gInt <*> gInt
    ArgNT -> VArgN <$> getArgArr
    ArgVT -> VArgV <$> gInt

data RefT = StkT | EnvT | DynT

instance Tag RefT where
  tag2word StkT = 0
  tag2word EnvT = 1
  tag2word DynT = 2

  word2tag 0 = pure StkT
  word2tag 1 = pure EnvT
  word2tag 2 = pure DynT
  word2tag n = unknownTag "RefT" n

putRef :: (MonadPut m) => GRef cix -> m ()
putRef (Stk i) = putTag StkT *> pInt i
putRef (Env cix _) = putTag EnvT *> putCombIx cix
putRef (Dyn i) = putTag DynT *> pWord i

getRef :: (MonadGet m) => m Ref
getRef =
  getTag >>= \case
    StkT -> Stk <$> gInt
    EnvT -> do
      cix <- getCombIx
      pure $ Env cix cix
    DynT -> Dyn <$> gWord

putCombIx :: (MonadPut m) => CombIx -> m ()
putCombIx (CIx r n i) = putReference r *> pWord n *> pWord i

getCombIx :: (MonadGet m) => m CombIx
getCombIx = CIx <$> getReference <*> gWord <*> gWord

data MLitT = MIT | MNT | MCT | MDT | MTT | MMT | MYT

instance Tag MLitT where
  tag2word MIT = 0
  tag2word MNT = 1
  tag2word MCT = 2
  tag2word MDT = 3
  tag2word MTT = 4
  tag2word MMT = 5
  tag2word MYT = 6

  word2tag 0 = pure MIT
  word2tag 1 = pure MNT
  word2tag 2 = pure MCT
  word2tag 3 = pure MDT
  word2tag 4 = pure MTT
  word2tag 5 = pure MMT
  word2tag 6 = pure MYT
  word2tag n = unknownTag "MLitT" n

getUText :: (MonadGet m) => m Util.Text.Text
getUText = Util.Text.fromText <$> getText

putUText :: (MonadPut m) => Util.Text.Text -> m ()
putUText = putText . Util.Text.toText

putLit :: (MonadPut m) => MLit -> m ()
putLit (MI i) = putTag MIT *> pInt i
putLit (MN n) = putTag MNT *> pWord n
putLit (MC c) = putTag MCT *> putChar c
putLit (MD d) = putTag MDT *> putFloat d
putLit (MT t) = putTag MTT *> putUText t
putLit (MM r) = putTag MMT *> putReferent r
putLit (MY r) = putTag MYT *> putReference r

getLit :: (MonadGet m) => m MLit
getLit =
  getTag >>= \case
    MIT -> MI <$> gInt
    MNT -> MN <$> gWord
    MCT -> MC <$> getChar
    MDT -> MD <$> getFloat
    MTT -> MT <$> getUText
    MMT -> MM <$> getReferent
    MYT -> MY <$> getReference

data BranchT = Test1T | Test2T | TestWT | TestTT

instance Tag BranchT where
  tag2word Test1T = 0
  tag2word Test2T = 1
  tag2word TestWT = 2
  tag2word TestTT = 3

  word2tag 0 = pure Test1T
  word2tag 1 = pure Test2T
  word2tag 2 = pure TestWT
  word2tag 3 = pure TestTT
  word2tag n = unknownTag "BranchT" n

putBranch :: (MonadPut m) => GBranch cix -> m ()
putBranch (Test1 w s d) =
  putTag Test1T *> pWord w *> putSection s *> putSection d
putBranch (Test2 a sa b sb d) =
  putTag Test2T
    *> pWord a
    *> putSection sa
    *> pWord b
    *> putSection sb
    *> putSection d
putBranch (TestW d m) =
  putTag TestWT *> putSection d *> putEnumMap pWord putSection m
putBranch (TestT d m) =
  putTag TestTT *> putSection d *> putMap putUText putSection m

getBranch :: (MonadGet m) => m Branch
getBranch =
  getTag >>= \case
    Test1T -> Test1 <$> gWord <*> getSection <*> getSection
    Test2T ->
      Test2
        <$> gWord
        <*> getSection
        <*> gWord
        <*> getSection
        <*> getSection
    TestWT -> TestW <$> getSection <*> getEnumMap gWord getSection
    TestTT -> TestT <$> getSection <*> getMap (Util.Text.fromText <$> getText) getSection

gInt :: (MonadGet m) => m Int
gInt = unVarInt <$> deserialize

pInt :: (MonadPut m) => Int -> m ()
pInt i = serialize (VarInt i)

gWord :: (MonadGet m) => m Word64
gWord = unVarInt <$> deserialize

pWord :: (MonadPut m) => Word64 -> m ()
pWord w = serialize (VarInt w)

putArgArr :: (MonadPut m) => Array Arg -> m ()
putArgArr a = putFoldable putArg a

getArgArr :: (MonadGet m) => m (Array Arg)
getArgArr = fromList <$> getList getArg

putUTypeTag :: (MonadPut m) => UnboxedTypeTag -> m ()
putUTypeTag = pInt . unboxedTypeTagToInt

getUTypeTag :: (MonadGet m) => m UnboxedTypeTag
getUTypeTag = unboxedTypeTagFromInt <$> gInt
