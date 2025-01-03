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
import Unison.Runtime.Array (PrimArray)
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
  | LetT
  | DieT
  | ExitT
  | DMatchT
  | NMatchT
  | RMatchT
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
  | UPrim1T
  | UPrim2T
  | BPrim1T
  | BPrim2T

instance Tag SectionT where
  tag2word AppT = 0
  tag2word CallT = 1
  tag2word JumpT = 2
  tag2word MatchT = 3
  tag2word YieldT = 4
  tag2word LetT = 5
  tag2word DieT = 6
  tag2word ExitT = 7
  tag2word DMatchT = 8
  tag2word NMatchT = 9
  tag2word RMatchT = 10
  tag2word ForeignCallT = 11
  tag2word SetDynT = 12
  tag2word CaptureT = 13
  tag2word NameT = 14
  tag2word InfoT = 15
  tag2word PackT = 16
  tag2word LitT = 17
  tag2word PrintT = 18
  tag2word ResetT = 19
  tag2word ForkT = 20
  tag2word AtomicallyT = 21
  tag2word SeqT = 22
  tag2word TryForceT = 23
  tag2word RefCAST = 24
  tag2word SandboxingFailureT = 25
  tag2word UPrim1T = 26
  tag2word UPrim2T = 27
  tag2word BPrim1T = 28
  tag2word BPrim2T = 29

  word2tag 0 = pure AppT
  word2tag 1 = pure CallT
  word2tag 2 = pure JumpT
  word2tag 3 = pure MatchT
  word2tag 4 = pure YieldT
  word2tag 5 = pure LetT
  word2tag 6 = pure DieT
  word2tag 7 = pure ExitT
  word2tag 8 = pure DMatchT
  word2tag 9 = pure NMatchT
  word2tag 10 = pure RMatchT
  word2tag 11 = pure ForeignCallT
  word2tag 12 = pure SetDynT
  word2tag 13 = pure CaptureT
  word2tag 14 = pure NameT
  word2tag 15 = pure InfoT
  word2tag 16 = pure PackT
  word2tag 17 = pure LitT
  word2tag 18 = pure PrintT
  word2tag 19 = pure ResetT
  word2tag 20 = pure ForkT
  word2tag 21 = pure AtomicallyT
  word2tag 22 = pure SeqT
  word2tag 23 = pure TryForceT
  word2tag 24 = pure RefCAST
  word2tag 25 = pure SandboxingFailureT
  word2tag 26 = pure UPrim1T
  word2tag 27 = pure UPrim2T
  word2tag 28 = pure BPrim1T
  word2tag 29 = pure BPrim2T
  word2tag i = unknownTag "SectionT" i

putSection :: (MonadPut m) => GSection cix -> m ()
putSection = \case
  App b r a -> putTag AppT *> serialize b *> putRef r *> putArgs a
  Call b cix _comb a -> putTag CallT *> serialize b *> putCombIx cix *> putArgs a
  Jump i a -> putTag JumpT *> pInt i *> putArgs a
  Match i b -> putTag MatchT *> pInt i *> putBranch b
  Yield a -> putTag YieldT *> putArgs a
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
  RefCAS i j k nx -> putTag RefCAST *> pInt i *> pInt j *> pInt k *> putSection nx
  ForeignCall b ff a nx -> putTag ForeignCallT *> serialize b *> putMForeignFunc ff *> putArgs a *> putSection nx
  SetDyn w i nx -> putTag SetDynT *> pWord w *> pInt i *> putSection nx
  Capture w nx -> putTag CaptureT *> pWord w *> putSection nx
  Name r a nx -> putTag NameT *> putRef r *> putArgs a *> putSection nx
  Info s nx -> putTag InfoT *> serialize s *> putSection nx
  Pack r w a nx -> putTag PackT *> putReference r *> putPackedTag w *> putArgs a *> putSection nx
  Lit l nx -> putTag LitT *> putLit l *> putSection nx
  Print i nx -> putTag PrintT *> pInt i *> putSection nx
  Reset s nx -> putTag ResetT *> putEnumSet pWord s *> putSection nx
  Fork i nx -> putTag ForkT *> pInt i *> putSection nx
  Atomically i nx -> putTag AtomicallyT *> pInt i *> putSection nx
  Seq a nx -> putTag SeqT *> putArgs a *> putSection nx
  TryForce i nx -> putTag TryForceT *> pInt i *> putSection nx
  SandboxingFailure {} ->
    -- Sandboxing failures should only exist in code we're actively running, it shouldn't be serialized.
    error "putInstr: Unexpected serialized Sandboxing Failure"
  UPrim1 up i nx -> putTag UPrim1T *> putTag up *> pInt i *> putSection nx
  UPrim2 up i j nx -> putTag UPrim2T *> putTag up *> pInt i *> pInt j *> putSection nx
  BPrim1 bp i nx -> putTag BPrim1T *> putTag bp *> pInt i *> putSection nx
  BPrim2 bp i j nx -> putTag BPrim2T *> putTag bp *> pInt i *> pInt j *> putSection nx

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
    LetT ->
      Let <$> getSection <*> getCombIx <*> gInt <*> getSection
    DieT -> Die <$> deserialize
    ExitT -> pure Exit
    DMatchT -> DMatch <$> getMaybe getReference <*> gInt <*> getBranch
    NMatchT -> NMatch <$> getMaybe getReference <*> gInt <*> getBranch
    RMatchT ->
      RMatch <$> gInt <*> getSection <*> getEnumMap gWord getBranch
    RefCAST -> RefCAS <$> gInt <*> gInt <*> gInt <*> getSection
    ForeignCallT -> ForeignCall <$> deserialize <*> getMForeignFunc <*> getArgs <*> getSection
    SetDynT -> SetDyn <$> gWord <*> gInt <*> getSection
    CaptureT -> Capture <$> gWord <*> getSection
    NameT -> Name <$> getRef <*> getArgs <*> getSection
    InfoT -> Info <$> deserialize <*> getSection
    PackT -> Pack <$> getReference <*> getPackedTag <*> getArgs <*> getSection
    LitT -> Lit <$> getLit <*> getSection
    PrintT -> Print <$> gInt <*> getSection
    ResetT -> Reset <$> getEnumSet gWord <*> getSection
    ForkT -> Fork <$> gInt <*> getSection
    AtomicallyT -> Atomically <$> gInt <*> getSection
    SeqT -> Seq <$> getArgs <*> getSection
    TryForceT -> TryForce <$> gInt <*> getSection
    SandboxingFailureT -> error "getInstr: Unexpected serialized Sandboxing Failure"
    UPrim1T -> UPrim1 <$> getTag <*> gInt <*> getSection
    UPrim2T -> UPrim2 <$> getTag <*> gInt <*> gInt <*> getSection
    BPrim1T -> BPrim1 <$> getTag <*> gInt <*> getSection
    BPrim2T -> BPrim2 <$> getTag <*> gInt <*> gInt <*> getSection

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

putArgs :: (MonadPut m) => Args -> m ()
putArgs ZArgs = putTag ZArgsT
putArgs (VArg1 i) = putTag Arg1T *> pInt i
putArgs (VArg2 i j) = putTag Arg2T *> pInt i *> pInt j
putArgs (VArgR i j) = putTag ArgRT *> pInt i *> pInt j
putArgs (VArgN pa) = putTag ArgNT *> putIntArr pa
putArgs (VArgV i) = putTag ArgVT *> pInt i

getArgs :: (MonadGet m) => m Args
getArgs =
  getTag >>= \case
    ZArgsT -> pure ZArgs
    Arg1T -> VArg1 <$> gInt
    Arg2T -> VArg2 <$> gInt <*> gInt
    ArgRT -> VArgR <$> gInt <*> gInt
    ArgNT -> VArgN <$> getIntArr
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

putLit :: (MonadPut m) => MLit -> m ()
putLit (MI i) = putTag MIT *> pInt i
putLit (MN n) = putTag MNT *> pWord n
putLit (MC c) = putTag MCT *> putChar c
putLit (MD d) = putTag MDT *> putFloat d
putLit (MT t) = putTag MTT *> putText (Util.Text.toText t)
putLit (MM r) = putTag MMT *> putReferent r
putLit (MY r) = putTag MYT *> putReference r

getLit :: (MonadGet m) => m MLit
getLit =
  getTag >>= \case
    MIT -> MI <$> gInt
    MNT -> MN <$> gWord
    MCT -> MC <$> getChar
    MDT -> MD <$> getFloat
    MTT -> MT . Util.Text.fromText <$> getText
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
  putTag TestTT *> putSection d *> putMap (putText . Util.Text.toText) putSection m

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

putIntArr :: (MonadPut m) => PrimArray Int -> m ()
putIntArr pa = putFoldable pInt $ toList pa

getIntArr :: (MonadGet m) => m (PrimArray Int)
getIntArr = fromList <$> getList gInt
