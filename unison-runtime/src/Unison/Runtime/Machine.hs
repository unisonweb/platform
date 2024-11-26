{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Unison.Runtime.Machine where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM as STM
import Control.Exception
import Control.Lens
import Data.Bits
import Data.Functor.Classes (Eq1 (..), Ord1 (..))
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as DTx
import Data.Text.IO qualified as Tx
import Data.Traversable
import GHC.Conc as STM (unsafeIOToSTM)
import Unison.Builtin.Decls (exceptionRef, ioFailureRef)
import Unison.Builtin.Decls qualified as Rf
import Unison.ConstructorReference qualified as CR
import Unison.Prelude hiding (Text)
import Unison.Reference
  ( Reference,
    Reference' (Builtin),
    isBuiltin,
    toShortHash,
  )
import Unison.Referent (Referent, pattern Con, pattern Ref)
import Unison.Runtime.ANF as ANF
  ( Cacheability (..),
    Code (..),
    CompileExn (..),
    PackedTag (..),
    SuperGroup,
    codeGroup,
    foldGroup,
    foldGroupLinks,
    maskTags,
    packTags,
    valueLinks,
  )
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.Array as PA
import Unison.Runtime.Builtin
import Unison.Runtime.Exception
import Unison.Runtime.Foreign
import Unison.Runtime.Foreign.Function
import Unison.Runtime.MCode
import Unison.Runtime.Stack
import Unison.Runtime.TypeTags qualified as TT
import Unison.ShortHash qualified as SH
import Unison.Symbol (Symbol)
import Unison.Type qualified as Rf
import Unison.Util.Bytes qualified as By
import Unison.Util.EnumContainers as EC
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Pretty (toPlainUnbroken)
import Unison.Util.Text qualified as Util.Text
import UnliftIO (IORef)
import UnliftIO qualified
import UnliftIO.Concurrent qualified as UnliftIO

{- ORMOLU_DISABLE -}
#ifdef STACK_CHECK
import Unison.Debug qualified as Debug
import System.IO.Unsafe (unsafePerformIO)
#endif
{- ORMOLU_ENABLE -}

-- | A ref storing every currently active thread.
-- This is helpful for cleaning up orphaned threads when the main process
-- completes.
--
-- We track threads when running in a host process like UCM,
-- otherwise, in one-off environments 'Nothing' is used and we don't bother tracking forked threads since they'll be
-- cleaned up automatically on process termination.
type ActiveThreads = Maybe (IORef (Set ThreadId))

type Tag = Word64

-- dynamic environment
type DEnv = EnumMap Word64 Val

type MCombs = RCombs Val

type Combs = GCombs Void CombIx

type MSection = RSection Val

type MBranch = RBranch Val

type MInstr = RInstr Val

type MComb = RComb Val

type MRef = RRef Val

data Tracer
  = NoTrace
  | MsgTrace String String String
  | SimpleTrace String

-- code caching environment
data CCache = CCache
  { foreignFuncs :: EnumMap Word64 ForeignFunc,
    sandboxed :: Bool,
    tracer :: Bool -> Val -> Tracer,
    -- Combinators in their original form, where they're easier to serialize into SCache
    srcCombs :: TVar (EnumMap Word64 Combs),
    combs :: TVar (EnumMap Word64 MCombs),
    combRefs :: TVar (EnumMap Word64 Reference),
    -- Combs which we're allowed to cache after evaluating
    cacheableCombs :: TVar (EnumSet Word64),
    tagRefs :: TVar (EnumMap Word64 Reference),
    freshTm :: TVar Word64,
    freshTy :: TVar Word64,
    intermed :: TVar (M.Map Reference (SuperGroup Symbol)),
    refTm :: TVar (M.Map Reference Word64),
    refTy :: TVar (M.Map Reference Word64),
    sandbox :: TVar (M.Map Reference (Set Reference))
  }

refNumsTm :: CCache -> IO (M.Map Reference Word64)
refNumsTm cc = readTVarIO (refTm cc)

refNumsTy :: CCache -> IO (M.Map Reference Word64)
refNumsTy cc = readTVarIO (refTy cc)

refNumTm :: CCache -> Reference -> IO Word64
refNumTm cc r =
  refNumsTm cc >>= \case
    (M.lookup r -> Just w) -> pure w
    _ -> die $ "refNumTm: unknown reference: " ++ show r

refNumTy :: CCache -> Reference -> IO Word64
refNumTy cc r =
  refNumsTy cc >>= \case
    (M.lookup r -> Just w) -> pure w
    _ -> die $ "refNumTy: unknown reference: " ++ show r

refNumTy' :: CCache -> Reference -> IO (Maybe Word64)
refNumTy' cc r = M.lookup r <$> refNumsTy cc

baseCCache :: Bool -> IO CCache
baseCCache sandboxed = do
  CCache ffuncs sandboxed noTrace
    <$> newTVarIO srcCombs
    <*> newTVarIO combs
    <*> newTVarIO builtinTermBackref
    <*> newTVarIO cacheableCombs
    <*> newTVarIO builtinTypeBackref
    <*> newTVarIO ftm
    <*> newTVarIO fty
    <*> newTVarIO mempty
    <*> newTVarIO builtinTermNumbering
    <*> newTVarIO builtinTypeNumbering
    <*> newTVarIO baseSandboxInfo
  where
    cacheableCombs = mempty
    ffuncs | sandboxed = sandboxedForeigns | otherwise = builtinForeigns
    noTrace _ _ = NoTrace
    ftm = 1 + maximum builtinTermNumbering
    fty = 1 + maximum builtinTypeNumbering

    rns = emptyRNs {dnum = refLookup "ty" builtinTypeNumbering}

    srcCombs :: EnumMap Word64 Combs
    srcCombs =
      numberedTermLookup
        & mapWithKey
          (\k v -> let r = builtinTermBackref ! k in emitComb @Symbol rns r k mempty (0, v))
    combs :: EnumMap Word64 MCombs
    combs =
      srcCombs
        & absurdCombs
        & resolveCombs Nothing

info :: (Show a) => String -> a -> IO ()
info ctx x = infos ctx (show x)

infos :: String -> String -> IO ()
infos ctx s = putStrLn $ ctx ++ ": " ++ s

stk'info :: Stack -> IO ()
stk'info s@(Stack _ _ sp _ _) = do
  let prn i
        | i < 0 = return ()
        | otherwise = bpeekOff s i >>= print >> prn (i - 1)
  prn sp

-- Entry point for evaluating a section
eval0 :: CCache -> ActiveThreads -> MSection -> IO ()
eval0 !env !activeThreads !co = do
  stk <- alloc
  cmbs <- readTVarIO $ combs env
  (denv, k) <-
    topDEnv cmbs <$> readTVarIO (refTy env) <*> readTVarIO (refTm env)
  eval env denv activeThreads stk (k KE) dummyRef co

mCombVal :: CombIx -> MComb -> Val
mCombVal cix (RComb (Comb comb)) =
  BoxedVal (PAp cix comb nullSeg)
mCombVal _ (RComb (CachedVal _ clo)) = clo

topDEnv ::
  EnumMap Word64 MCombs ->
  M.Map Reference Word64 ->
  M.Map Reference Word64 ->
  (DEnv, K -> K)
topDEnv combs rfTy rfTm
  | Just n <- M.lookup exceptionRef rfTy,
    rcrf <- Builtin (DTx.pack "raise"),
    Just j <- M.lookup rcrf rfTm,
    cix <- CIx rcrf j 0,
    clo <- mCombVal cix $ rCombSection combs cix =
      ( EC.mapSingleton n clo,
        Mark 0 (EC.setSingleton n) mempty
      )
topDEnv _ _ _ = (mempty, id)

-- Entry point for evaluating a numbered combinator.
-- An optional callback for the base of the stack may be supplied.
--
-- This is the entry point actually used in the interactive
-- environment currently.
apply0 ::
  Maybe (Stack -> IO ()) ->
  CCache ->
  ActiveThreads ->
  Word64 ->
  IO ()
apply0 !callback !env !threadTracker !i = do
  stk <- alloc
  cmbrs <- readTVarIO $ combRefs env
  cmbs <- readTVarIO $ combs env
  (denv, kf) <-
    topDEnv cmbs <$> readTVarIO (refTy env) <*> readTVarIO (refTm env)
  r <- case EC.lookup i cmbrs of
    Just r -> pure r
    Nothing -> die "apply0: missing reference to entry point"
  let entryCix = (CIx r i 0)
  case unRComb $ rCombSection cmbs entryCix of
    Comb entryComb -> do
      apply env denv threadTracker stk (kf k0) True ZArgs . BoxedVal $
        PAp entryCix entryComb nullSeg
    -- if it's cached, we can just finish
    CachedVal _ val -> bump stk >>= \stk -> poke stk val
  where
    k0 = maybe KE (CB . Hook) callback

-- Apply helper currently used for forking. Creates the new stacks
-- necessary to evaluate a closure with the provided information.
apply1 ::
  (Stack -> IO ()) ->
  CCache ->
  ActiveThreads ->
  Val ->
  IO ()
apply1 callback env threadTracker clo = do
  stk <- alloc
  apply env mempty threadTracker stk k0 True ZArgs $ clo
  where
    k0 = CB $ Hook callback

-- Entry point for evaluating a saved continuation.
--
-- The continuation must be from an evaluation context expecting a
-- unit value.
jump0 ::
  (Stack -> IO ()) ->
  CCache ->
  ActiveThreads ->
  Closure ->
  IO ()
jump0 !callback !env !activeThreads !clo = do
  stk <- alloc
  cmbs <- readTVarIO $ combs env
  (denv, kf) <-
    topDEnv cmbs <$> readTVarIO (refTy env) <*> readTVarIO (refTm env)
  stk <- bump stk
  bpoke stk (Enum Rf.unitRef TT.unitTag)
  jump env denv activeThreads stk (kf k0) (VArg1 0) clo
  where
    k0 = CB (Hook callback)

unitValue :: Closure
unitValue = Enum Rf.unitRef TT.unitTag

lookupDenv :: Word64 -> DEnv -> Val
lookupDenv p denv = fromMaybe (BoxedVal BlackHole) $ EC.lookup p denv

litToVal :: MLit -> Val
litToVal = \case
  MT t -> BoxedVal $ Foreign (Wrap Rf.textRef t)
  MM r -> BoxedVal $ Foreign (Wrap Rf.termLinkRef r)
  MY r -> BoxedVal $ Foreign (Wrap Rf.typeLinkRef r)
  MI i -> IntVal i
  MN n -> NatVal n
  MC c -> CharVal c
  MD d -> DoubleVal d
{-# INLINE litToVal #-}

{- ORMOLU_DISABLE -}
#ifdef STACK_CHECK
debugger :: (Show a) => Stack -> String -> a -> Bool
debugger stk msg a = unsafePerformIO $ do
  dumpStack stk
  Debug.debugLogM Debug.Interpreter (msg ++ ": " ++ show a)
  pure False

dumpStack :: Stack -> IO ()
dumpStack stk@(Stack ap fp sp _ustk _bstk)
  | sp - fp < 0 = Debug.debugLogM Debug.Interpreter "Stack before 👇: Empty"
  | otherwise = do
      stkLocals <- for [0 .. ((sp - fp) - 1)] $ \i -> do
        peekOff stk i
      Debug.debugM Debug.Interpreter "Stack frame locals 👇:" stkLocals
      stkArgs <- for [0 .. ((fp - ap) - 1)] $ \i -> do
        peekOff stk (i + (sp - fp))
      Debug.debugM Debug.Interpreter "Stack args 👇:" stkArgs
#endif
{- ORMOLU_ENABLE -}

-- | Execute an instruction
exec ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Reference ->
  MInstr ->
  IO (DEnv, Stack, K)
{- ORMOLU_DISABLE -}
#ifdef STACK_CHECK
exec !_ !_ !_ !stk !_ !_ instr
  | debugger stk "exec" instr = undefined
#endif
{- ORMOLU_ENABLE -}
exec !_ !denv !_activeThreads !stk !k _ (Info tx) = do
  info tx stk
  info tx k
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (Name r args) = do
  v <- resolve env denv stk r
  stk <- name stk args v
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (SetDyn p i) = do
  val <- peekOff stk i
  pure (EC.mapInsert p val denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Capture p) = do
  (cap, denv, stk, k) <- splitCont denv stk k p
  stk <- bump stk
  poke stk cap
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (UPrim1 op i) = do
  stk <- uprim1 stk op i
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (UPrim2 op i j) = do
  stk <- uprim2 stk op i j
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 MISS i)
  | sandboxed env = die "attempted to use sandboxed operation: isMissing"
  | otherwise = do
      clink <- bpeekOff stk i
      let link = case unwrapForeign $ marshalToForeign clink of
            Ref r -> r
            _ -> error "exec:BPrim1:MISS: Expected Ref"
      m <- readTVarIO (intermed env)
      stk <- bump stk
      pokeTag stk $ if (link `M.member` m) then 1 else 0
      pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 CACH i)
  | sandboxed env = die "attempted to use sandboxed operation: cache"
  | otherwise = do
      arg <- peekOffS stk i
      news <- decodeCacheArgument arg
      unknown <- cacheAdd news env
      stk <- bump stk
      pokeS
        stk
        (Sq.fromList $ boxedVal . Foreign . Wrap Rf.termLinkRef . Ref <$> unknown)
      pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 CVLD i)
  | sandboxed env = die "attempted to use sandboxed operation: validate"
  | otherwise = do
      arg <- peekOffS stk i
      news <- decodeCacheArgument arg
      codeValidate (second codeGroup <$> news) env >>= \case
        Nothing -> do
          stk <- bump stk
          pokeTag stk 0
          pure (denv, stk, k)
        Just (Failure ref msg clo) -> do
          stk <- bumpn stk 3
          bpoke stk (Foreign $ Wrap Rf.typeLinkRef ref)
          pokeOffBi stk 1 msg
          bpokeOff stk 2 clo
          stk <- bump stk
          pokeTag stk 1
          pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 LKUP i)
  | sandboxed env = die "attempted to use sandboxed operation: lookup"
  | otherwise = do
      clink <- bpeekOff stk i
      let link = case unwrapForeign $ marshalToForeign clink of
            Ref r -> r
            _ -> error "exec:BPrim1:LKUP: Expected Ref"
      m <- readTVarIO (intermed env)
      rfn <- readTVarIO (refTm env)
      cach <- readTVarIO (cacheableCombs env)
      stk <- bump stk
      stk <- case M.lookup link m of
        Nothing
          | Just w <- M.lookup link builtinTermNumbering,
            Just sn <- EC.lookup w numberedTermLookup -> do
              pokeBi stk (CodeRep (ANF.Rec [] sn) Uncacheable)
              stk <- bump stk
              stk <$ pokeTag stk 1
          | otherwise -> stk <$ pokeTag stk 0
        Just sg -> do
          let ch
                | Just n <- M.lookup link rfn,
                  EC.member n cach =
                    Cacheable
                | otherwise = Uncacheable
          pokeBi stk (CodeRep sg ch)
          stk <- bump stk
          stk <$ pokeTag stk 1
      pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (BPrim1 TLTT i) = do
  clink <- bpeekOff stk i
  let shortHash = case unwrapForeign $ marshalToForeign clink of
        Ref r -> toShortHash r
        Con r _ -> CR.toShortHash r
  let sh = Util.Text.fromText . SH.toText $ shortHash
  stk <- bump stk
  pokeBi stk sh
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 LOAD i)
  | sandboxed env = die "attempted to use sandboxed operation: load"
  | otherwise = do
      v <- peekOffBi stk i
      stk <- bumpn stk 2
      reifyValue env v >>= \case
        Left miss -> do
          pokeOffS stk 1 $
            Sq.fromList $
              boxedVal . Foreign . Wrap Rf.termLinkRef . Ref <$> miss
          pokeTag stk 0
        Right x -> do
          pokeOff stk 1 x
          pokeTag stk 1
      pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 VALU i) = do
  m <- readTVarIO (tagRefs env)
  c <- peekOff stk i
  stk <- bump stk
  pokeBi stk =<< reflectValue m c
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 DBTX i)
  | sandboxed env =
      die "attempted to use sandboxed operation: Debug.toText"
  | otherwise = do
      val <- peekOff stk i
      stk <- bump stk
      stk <- case tracer env False val of
        NoTrace -> stk <$ pokeTag stk 0
        MsgTrace _ _ tx -> do
          pokeBi stk (Util.Text.pack tx)
          stk <- bump stk
          stk <$ pokeTag stk 1
        SimpleTrace tx -> do
          pokeBi stk (Util.Text.pack tx)
          stk <- bump stk
          stk <$ pokeTag stk 2
      pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim1 SDBL i)
  | sandboxed env =
      die "attempted to use sandboxed operation: sandboxLinks"
  | otherwise = do
      tl <- peekOffBi stk i
      stk <- bump stk
      pokeS stk . encodeSandboxListResult =<< sandboxList env tl
      pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (BPrim1 op i) = do
  stk <- bprim1 stk op i
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim2 SDBX i j) = do
  s <- peekOffS stk i
  c <- bpeekOff stk j
  l <- decodeSandboxArgument s
  b <- checkSandboxing env l c
  stk <- bump stk
  pokeBool stk $ b
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (BPrim2 SDBV i j)
  | sandboxed env =
      die "attempted to use sandboxed operation: Value.validateSandboxed"
  | otherwise = do
      s <- peekOffS stk i
      v <- peekOffBi stk j
      l <- decodeSandboxArgument s
      res <- checkValueSandboxing env l v
      stk <- bump stk
      bpoke stk $ encodeSandboxResult res
      pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (BPrim2 EQLU i j) = do
  x <- peekOff stk i
  y <- peekOff stk j
  stk <- bump stk
  pokeBool stk $ universalEq (==) x y
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (BPrim2 CMPU i j) = do
  x <- peekOff stk i
  y <- peekOff stk j
  stk <- bump stk
  pokeI stk . fromEnum $ universalCompare compare x y
  pure (denv, stk, k)
exec !_ !_ !_activeThreads !stk !k r (BPrim2 THRO i j) = do
  name <- peekOffBi @Util.Text.Text stk i
  x <- peekOff stk j
  throwIO (BU (traceK r k) (Util.Text.toText name) x)
exec !env !denv !_activeThreads !stk !k _ (BPrim2 TRCE i j)
  | sandboxed env = die "attempted to use sandboxed operation: trace"
  | otherwise = do
      tx <- peekOffBi stk i
      clo <- peekOff stk j
      case tracer env True clo of
        NoTrace -> pure ()
        SimpleTrace str -> do
          putStrLn $ "trace: " ++ Util.Text.unpack tx
          putStrLn str
        MsgTrace msg ugl pre -> do
          putStrLn $ "trace: " ++ Util.Text.unpack tx
          putStrLn ""
          putStrLn msg
          putStrLn "\nraw structure:\n"
          putStrLn ugl
          putStrLn "partial decompilation:\n"
          putStrLn pre
      pure (denv, stk, k)
exec !_ !denv !_trackThreads !stk !k _ (BPrim2 op i j) = do
  stk <- bprim2 stk op i j
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Pack r t args) = do
  clo <- buildData stk r t args
  stk <- bump stk
  bpoke stk clo
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Print i) = do
  t <- peekOffBi stk i
  Tx.putStrLn (Util.Text.toText t)
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Lit ml) = do
  stk <- bump stk
  poke stk $ litToVal ml
  pure (denv, stk, k)
exec !_ !denv !_activeThreads !stk !k _ (Reset ps) = do
  (stk, a) <- saveArgs stk
  pure (denv, stk, Mark a ps clos k)
  where
    clos = EC.restrictKeys denv ps
exec !_ !denv !_activeThreads !stk !k _ (Seq as) = do
  l <- closureArgs stk as
  stk <- bump stk
  pokeS stk $ Sq.fromList l
  pure (denv, stk, k)
exec !env !denv !_activeThreads !stk !k _ (ForeignCall _ w args)
  | Just (FF arg res ev) <- EC.lookup w (foreignFuncs env) =
      (denv,,k)
        <$> (arg stk args >>= ev >>= res stk)
  | otherwise =
      die $ "reference to unknown foreign function: " ++ show w
exec !env !denv !activeThreads !stk !k _ (Fork i)
  | sandboxed env = die "attempted to use sandboxed operation: fork"
  | otherwise = do
      tid <- forkEval env activeThreads =<< peekOff stk i
      stk <- bump stk
      bpoke stk . Foreign . Wrap Rf.threadIdRef $ tid
      pure (denv, stk, k)
exec !env !denv !activeThreads !stk !k _ (Atomically i)
  | sandboxed env = die $ "attempted to use sandboxed operation: atomically"
  | otherwise = do
      v <- peekOff stk i
      stk <- bump stk
      atomicEval env activeThreads (poke stk) v
      pure (denv, stk, k)
exec !env !denv !activeThreads !stk !k _ (TryForce i)
  | sandboxed env = die $ "attempted to use sandboxed operation: tryForce"
  | otherwise = do
      v <- peekOff stk i
      stk <- bump stk -- Bump the boxed stack to make a slot for the result, which will be written in the callback if we succeed.
      ev <- Control.Exception.try $ nestEval env activeThreads (poke stk) v
      stk <- encodeExn stk ev
      pure (denv, stk, k)
{-# INLINE exec #-}

encodeExn ::
  Stack ->
  Either SomeException () ->
  IO Stack
encodeExn stk exc = do
  case exc of
    Right () -> do
      stk <- bump stk
      stk <$ pokeTag stk 1
    Left exn -> do
      -- If we hit an exception, we have one unused slot on the stack
      -- from where the result _would_ have been placed.
      -- So here we bump one less than it looks like we should, and re-use
      -- that slot.
      stk <- bumpn stk 3
      pokeTag stk 0
      bpokeOff stk 1 $ Foreign (Wrap Rf.typeLinkRef link)
      pokeOffBi stk 2 msg
      stk <$ pokeOff stk 3 extra
      where
        disp e = Util.Text.pack $ show e
        (link, msg, extra)
          | Just (ioe :: IOException) <- fromException exn =
              (Rf.ioFailureRef, disp ioe, boxedVal unitValue)
          | Just re <- fromException exn = case re of
              PE _stk msg ->
                (Rf.runtimeFailureRef, Util.Text.pack $ toPlainUnbroken msg, boxedVal unitValue)
              BU _ tx val -> (Rf.runtimeFailureRef, Util.Text.fromText tx, val)
          | Just (ae :: ArithException) <- fromException exn =
              (Rf.arithmeticFailureRef, disp ae, boxedVal unitValue)
          | Just (nae :: NestedAtomically) <- fromException exn =
              (Rf.stmFailureRef, disp nae, boxedVal unitValue)
          | Just (be :: BlockedIndefinitelyOnSTM) <- fromException exn =
              (Rf.stmFailureRef, disp be, boxedVal unitValue)
          | Just (be :: BlockedIndefinitelyOnMVar) <- fromException exn =
              (Rf.ioFailureRef, disp be, boxedVal unitValue)
          | Just (ie :: AsyncException) <- fromException exn =
              (Rf.threadKilledFailureRef, disp ie, boxedVal unitValue)
          | otherwise = (Rf.miscFailureRef, disp exn, boxedVal unitValue)

numValue :: Maybe Reference -> Val -> IO Word64
numValue _ (UnboxedVal v _) = pure (fromIntegral @Int @Word64 v)
numValue mr clo =
  die $
    "numValue: bad closure: "
      ++ show clo
      ++ maybe "" (\r -> "\nexpected type: " ++ show r) mr

-- | Evaluate a section
eval ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Reference ->
  MSection ->
  IO ()
{- ORMOLU_DISABLE -}
#ifdef STACK_CHECK
eval !_ !_ !_ !stk !_ !_ section
  | debugger stk "eval" section = undefined
#endif
{- ORMOLU_ENABLE -}
eval !env !denv !activeThreads !stk !k r (Match i (TestT df cs)) = do
  t <- peekOffBi stk i
  eval env denv activeThreads stk k r $ selectTextBranch t df cs
eval !env !denv !activeThreads !stk !k r (Match i br) = do
  n <- peekOffN stk i
  eval env denv activeThreads stk k r $ selectBranch n br
eval !env !denv !activeThreads !stk !k r (DMatch mr i br) = do
  (t, stk) <- dumpDataNoTag mr stk =<< peekOff stk i
  eval env denv activeThreads stk k r $
    selectBranch (maskTags t) br
eval !env !denv !activeThreads !stk !k r (NMatch _mr i br) = do
  n <- peekOffN stk i
  eval env denv activeThreads stk k r $ selectBranch n br
eval !env !denv !activeThreads !stk !k r (RMatch i pu br) = do
  (t, stk) <- dumpDataNoTag Nothing stk =<< peekOff stk i
  if t == PackedTag 0
    then eval env denv activeThreads stk k r pu
    else case ANF.unpackTags t of
      (ANF.rawTag -> e, ANF.rawTag -> t)
        | Just ebs <- EC.lookup e br ->
            eval env denv activeThreads stk k r $ selectBranch t ebs
        | otherwise -> unhandledErr "eval" env e
eval !env !denv !activeThreads !stk !k _ (Yield args)
  | asize stk > 0,
    VArg1 i <- args =
      peekOff stk i >>= apply env denv activeThreads stk k False ZArgs
  | otherwise = do
      stk <- moveArgs stk args
      stk <- frameArgs stk
      yield env denv activeThreads stk k
eval !env !denv !activeThreads !stk !k _ (App ck r args) =
  resolve env denv stk r
    >>= apply env denv activeThreads stk k ck args
eval !env !denv !activeThreads !stk !k _ (Call ck combIx rcomb args) =
  enter env denv activeThreads stk k (combRef combIx) ck args rcomb
eval !env !denv !activeThreads !stk !k _ (Jump i args) =
  bpeekOff stk i >>= jump env denv activeThreads stk k args
eval !env !denv !activeThreads !stk !k r (Let nw cix f sect) = do
  (stk, fsz, asz) <- saveFrame stk
  eval
    env
    denv
    activeThreads
    stk
    (Push fsz asz cix f sect k)
    r
    nw
eval !env !denv !activeThreads !stk !k r (Ins i nx) = do
  (denv, stk, k) <- exec env denv activeThreads stk k r i
  eval env denv activeThreads stk k r nx
eval !_ !_ !_ !_activeThreads !_ _ Exit = pure ()
eval !_ !_ !_ !_activeThreads !_ _ (Die s) = die s
{-# NOINLINE eval #-}

forkEval :: CCache -> ActiveThreads -> Val -> IO ThreadId
forkEval env activeThreads clo =
  do
    threadId <-
      UnliftIO.forkFinally
        (apply1 err env activeThreads clo)
        (const cleanupThread)
    trackThread threadId
    pure threadId
  where
    err :: Stack -> IO ()
    err _ = pure ()
    trackThread :: ThreadId -> IO ()
    trackThread threadID = do
      case activeThreads of
        Nothing -> pure ()
        Just activeThreads -> UnliftIO.atomicModifyIORef' activeThreads (\ids -> (Set.insert threadID ids, ()))
    cleanupThread :: IO ()
    cleanupThread = do
      case activeThreads of
        Nothing -> pure ()
        Just activeThreads -> do
          myThreadId <- UnliftIO.myThreadId
          UnliftIO.atomicModifyIORef' activeThreads (\ids -> (Set.delete myThreadId ids, ()))
{-# INLINE forkEval #-}

nestEval :: CCache -> ActiveThreads -> (Val -> IO ()) -> Val -> IO ()
nestEval env activeThreads write val = apply1 readBack env activeThreads val
  where
    readBack stk = peek stk >>= write
{-# INLINE nestEval #-}

atomicEval :: CCache -> ActiveThreads -> (Val -> IO ()) -> Val -> IO ()
atomicEval env activeThreads write val =
  atomically . unsafeIOToSTM $ nestEval env activeThreads write val
{-# INLINE atomicEval #-}

-- fast path application
enter ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Reference ->
  Bool ->
  Args ->
  MComb ->
  IO ()
enter !env !denv !activeThreads !stk !k !cref !sck !args = \case
  (RComb (Lam a f entry)) -> do
    -- check for stack check _skip_
    stk <- if sck then pure stk else ensure stk f
    stk <- moveArgs stk args
    stk <- acceptArgs stk a
    eval env denv activeThreads stk k cref entry
  (RComb (CachedVal _ val)) -> do
    stk <- discardFrame stk
    stk <- bump stk
    poke stk val
    yield env denv activeThreads stk k
{-# INLINE enter #-}

-- fast path by-name delaying
name :: Stack -> Args -> Val -> IO Stack
name !stk !args = \case
  BoxedVal (PAp cix comb seg) -> do
    seg <- closeArgs I stk seg args
    stk <- bump stk
    bpoke stk $ PAp cix comb seg
    pure stk
  v -> die $ "naming non-function: " ++ show v
{-# INLINE name #-}

-- slow path application
apply ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Bool ->
  Args ->
  Val ->
  IO ()
{- ORMOLU_DISABLE -}
#ifdef STACK_CHECK
apply !_env !_denv !_activeThreads !stk !_k !_ck !args !val
  | debugger stk "apply" (args, val) = undefined
#endif
{- ORMOLU_ENABLE -}
apply !env !denv !activeThreads !stk !k !ck !args !val =
  case val of
    BoxedVal (PAp cix@(CIx combRef _ _) comb seg) ->
      case comb of
        LamI a f entry
          | ck || a <= ac -> do
              stk <- ensure stk f
              stk <- moveArgs stk args
              stk <- dumpSeg stk seg A
              stk <- acceptArgs stk a
              eval env denv activeThreads stk k combRef entry
          | otherwise -> do
              seg <- closeArgs C stk seg args
              stk <- discardFrame =<< frameArgs stk
              stk <- bump stk
              bpoke stk $ PAp cix comb seg
              yield env denv activeThreads stk k
      where
        ac = asize stk + countArgs args + scount seg
    v -> zeroArgClosure v
  where
    zeroArgClosure :: Val -> IO ()
    zeroArgClosure v
      | ZArgs <- args,
        asize stk == 0 = do
          stk <- discardFrame stk
          stk <- bump stk
          poke stk v
          yield env denv activeThreads stk k
      | otherwise = die $ "applying non-function: " ++ show v
{-# INLINE apply #-}

jump ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  Args ->
  Closure ->
  IO ()
jump !env !denv !activeThreads !stk !k !args clo = case clo of
  Captured sk0 a seg -> do
    let (p, sk) = adjust sk0
    seg <- closeArgs K stk seg args
    stk <- discardFrame stk
    stk <- dumpSeg stk seg $ F (countArgs args) a
    stk <- adjustArgs stk p
    repush env activeThreads stk denv sk k
  _ -> die "jump: non-cont"
  where
    -- Adjusts a repushed continuation to account for pending arguments. If
    -- there are any frames in the pushed continuation, the nearest one needs to
    -- record the additional pending arguments.
    --
    -- If the repushed continuation has no frames, then the arguments are still
    -- pending, and the result stacks need to be adjusted.
    adjust :: K -> (SZ, K)
    adjust (Mark a rs denv k) =
      (0, Mark (a + asize stk) rs denv k)
    adjust (Push n a cix f rsect k) =
      (0, Push n (a + asize stk) cix f rsect k)
    adjust k = (asize stk, k)
{-# INLINE jump #-}

repush ::
  CCache ->
  ActiveThreads ->
  Stack ->
  DEnv ->
  K ->
  K ->
  IO ()
repush !env !activeThreads !stk = go
  where
    go !denv KE !k = yield env denv activeThreads stk k
    go !denv (Mark a ps cs sk) !k = go denv' sk $ Mark a ps cs' k
      where
        denv' = cs <> EC.withoutKeys denv ps
        cs' = EC.restrictKeys denv ps
    go !denv (Push n a cix f rsect sk) !k =
      go denv sk $ Push n a cix f rsect k
    go !_ (CB _) !_ = die "repush: impossible"
{-# INLINE repush #-}

moveArgs ::
  Stack ->
  Args ->
  IO Stack
moveArgs !stk ZArgs = do
  stk <- discardFrame stk
  pure stk
moveArgs !stk (VArg1 i) = do
  stk <- prepareArgs stk (Arg1 i)
  pure stk
moveArgs !stk (VArg2 i j) = do
  stk <- prepareArgs stk (Arg2 i j)
  pure stk
moveArgs !stk (VArgR i l) = do
  stk <- prepareArgs stk (ArgR i l)
  pure stk
moveArgs !stk (VArgN as) = do
  stk <- prepareArgs stk (ArgN as)
  pure stk
moveArgs !stk (VArgV i) = do
  stk <-
    if l > 0
      then prepareArgs stk (ArgR 0 l)
      else discardFrame stk
  pure stk
  where
    l = fsize stk - i
{-# INLINE moveArgs #-}

closureArgs :: Stack -> Args -> IO [Val]
closureArgs !_ ZArgs = pure []
closureArgs !stk (VArg1 i) = do
  x <- peekOff stk i
  pure [x]
closureArgs !stk (VArg2 i j) = do
  x <- peekOff stk i
  y <- peekOff stk j
  pure [x, y]
closureArgs !stk (VArgR i l) =
  for (take l [i ..]) (peekOff stk)
closureArgs !stk (VArgN bs) =
  for (PA.primArrayToList bs) (peekOff stk)
closureArgs !_ _ =
  error "closure arguments can only be boxed."
{-# INLINE closureArgs #-}

-- | Pack some number of args into a data type of the provided ref/tag type.
buildData ::
  Stack -> Reference -> PackedTag -> Args -> IO Closure
buildData !_ !r !t ZArgs = pure $ Enum r t
buildData !stk !r !t (VArg1 i) = do
  v <- peekOff stk i
  pure $ Data1 r t v
buildData !stk !r !t (VArg2 i j) = do
  v1 <- peekOff stk i
  v2 <- peekOff stk j
  pure $ Data2 r t v1 v2
buildData !stk !r !t (VArgR i l) = do
  seg <- augSeg I stk nullSeg (Just $ ArgR i l)
  pure $ DataG r t seg
buildData !stk !r !t (VArgN as) = do
  seg <- augSeg I stk nullSeg (Just $ ArgN as)
  pure $ DataG r t seg
buildData !stk !r !t (VArgV i) = do
  seg <-
    if l > 0
      then augSeg I stk nullSeg (Just $ ArgR 0 l)
      else pure nullSeg
  pure $ DataG r t seg
  where
    l = fsize stk - i
{-# INLINE buildData #-}

-- Dumps a data type closure to the stack without writing its tag.
-- Instead, the tag is returned for direct case analysis.
dumpDataNoTag ::
  Maybe Reference ->
  Stack ->
  Val ->
  IO (PackedTag, Stack)
dumpDataNoTag !mr !stk = \case
  -- Normally we want to avoid dumping unboxed values since it's unnecessary, but sometimes we don't know the type of
  -- the incoming value and end up dumping unboxed values, so we just push them back to the stack as-is. e.g. in type-casts/coercions
  val@(UnboxedVal _ t) -> do
    stk <- bump stk
    poke stk val
    pure (unboxedPackedTag t, stk)
  BoxedVal clos -> case clos of
    (Enum _ t) -> pure (t, stk)
    (Data1 _ t x) -> do
      stk <- bump stk
      poke stk x
      pure (t, stk)
    (Data2 _ t x y) -> do
      stk <- bumpn stk 2
      pokeOff stk 1 y
      poke stk x
      pure (t, stk)
    (DataG _ t seg) -> do
      stk <- dumpSeg stk seg S
      pure (t, stk)
    clo ->
      die $
        "dumpDataNoTag: bad closure: "
          ++ show clo
          ++ maybe "" (\r -> "\nexpected type: " ++ show r) mr
  where
    unboxedPackedTag :: UnboxedTypeTag -> PackedTag
    unboxedPackedTag = \case
      CharTag -> TT.charTag
      FloatTag -> TT.floatTag
      IntTag -> TT.intTag
      NatTag -> TT.natTag
{-# INLINE dumpDataNoTag #-}

-- Note: although the representation allows it, it is impossible
-- to under-apply one sort of argument while over-applying the
-- other. Thus, it is unnecessary to worry about doing tricks to
-- only grab a certain number of arguments.
closeArgs ::
  Augment ->
  Stack ->
  Seg ->
  Args ->
  IO Seg
closeArgs mode !stk !seg args = augSeg mode stk seg as
  where
    as = case args of
      ZArgs -> Nothing
      VArg1 i -> Just $ Arg1 i
      VArg2 i j -> Just $ Arg2 i j
      VArgR i l -> Just $ ArgR i l
      VArgN as -> Just $ ArgN as
      VArgV i -> a
        where
          a
            | l > 0 = Just $ ArgR 0 l
            | otherwise = Nothing
          l = fsize stk - i

peekForeign :: Stack -> Int -> IO a
peekForeign stk i =
  bpeekOff stk i >>= \case
    Foreign x -> pure $ unwrapForeign x
    _ -> die "bad foreign argument"
{-# INLINE peekForeign #-}

uprim1 :: Stack -> UPrim1 -> Int -> IO Stack
uprim1 !stk DECI !i = do
  m <- peekOffI stk i
  stk <- bump stk
  pokeI stk (m - 1)
  pure stk
uprim1 !stk DECN !i = do
  m <- peekOffN stk i
  stk <- bump stk
  pokeN stk (m - 1)
  pure stk
uprim1 !stk INCI !i = do
  m <- peekOffI stk i
  stk <- bump stk
  pokeI stk (m + 1)
  pure stk
uprim1 !stk INCN !i = do
  m <- peekOffN stk i
  stk <- bump stk
  pokeN stk (m + 1)
  pure stk
uprim1 !stk NEGI !i = do
  m <- upeekOff stk i
  stk <- bump stk
  pokeI stk (-m)
  pure stk
uprim1 !stk SGNI !i = do
  m <- upeekOff stk i
  stk <- bump stk
  pokeI stk (signum m)
  pure stk
uprim1 !stk ABSF !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (abs d)
  pure stk
uprim1 !stk CEIL !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeI stk (ceiling d)
  pure stk
uprim1 !stk FLOR !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeI stk (floor d)
  pure stk
uprim1 !stk TRNF !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeI stk (truncate d)
  pure stk
uprim1 !stk RNDF !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeI stk (round d)
  pure stk
uprim1 !stk EXPF !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (exp d)
  pure stk
uprim1 !stk LOGF !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (log d)
  pure stk
uprim1 !stk SQRT !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (sqrt d)
  pure stk
uprim1 !stk COSF !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (cos d)
  pure stk
uprim1 !stk SINF !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (sin d)
  pure stk
uprim1 !stk TANF !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (tan d)
  pure stk
uprim1 !stk COSH !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (cosh d)
  pure stk
uprim1 !stk SINH !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (sinh d)
  pure stk
uprim1 !stk TANH !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (tanh d)
  pure stk
uprim1 !stk ACOS !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (acos d)
  pure stk
uprim1 !stk ASIN !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (asin d)
  pure stk
uprim1 !stk ATAN !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (atan d)
  pure stk
uprim1 !stk ASNH !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (asinh d)
  pure stk
uprim1 !stk ACSH !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (acosh d)
  pure stk
uprim1 !stk ATNH !i = do
  d <- peekOffD stk i
  stk <- bump stk
  pokeD stk (atanh d)
  pure stk
uprim1 !stk ITOF !i = do
  n <- upeekOff stk i
  stk <- bump stk
  pokeD stk (fromIntegral n)
  pure stk
uprim1 !stk NTOF !i = do
  n <- peekOffN stk i
  stk <- bump stk
  pokeD stk (fromIntegral n)
  pure stk
uprim1 !stk LZRO !i = do
  n <- peekOffN stk i
  stk <- bump stk
  unsafePokeIasN stk (countLeadingZeros n)
  pure stk
uprim1 !stk TZRO !i = do
  n <- peekOffN stk i
  stk <- bump stk
  unsafePokeIasN stk (countTrailingZeros n)
  pure stk
uprim1 !stk POPC !i = do
  n <- peekOffN stk i
  stk <- bump stk
  unsafePokeIasN stk (popCount n)
  pure stk
uprim1 !stk COMN !i = do
  n <- peekOffN stk i
  stk <- bump stk
  pokeN stk (complement n)
  pure stk
uprim1 !stk COMI !i = do
  n <- peekOffI stk i
  stk <- bump stk
  pokeI stk (complement n)
  pure stk
{-# INLINE uprim1 #-}

uprim2 :: Stack -> UPrim2 -> Int -> Int -> IO Stack
uprim2 !stk ADDI !i !j = do
  m <- upeekOff stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeI stk (m + n)
  pure stk
uprim2 !stk ADDN !i !j = do
  m <- peekOffN stk i
  n <- peekOffN stk j
  stk <- bump stk
  pokeN stk (m + n)
  pure stk
uprim2 !stk SUBI !i !j = do
  m <- upeekOff stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeI stk (m - n)
  pure stk
uprim2 !stk SUBN !i !j = do
  m <- peekOffI stk i
  n <- peekOffI stk j
  stk <- bump stk
  pokeI stk (m - n)
  pure stk
uprim2 !stk MULI !i !j = do
  m <- upeekOff stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeI stk (m * n)
  pure stk
uprim2 !stk MULN !i !j = do
  m <- peekOffN stk i
  n <- peekOffN stk j
  stk <- bump stk
  pokeN stk (m * n)
  pure stk
uprim2 !stk DIVI !i !j = do
  m <- upeekOff stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeI stk (m `div` n)
  pure stk
uprim2 !stk MODI !i !j = do
  m <- upeekOff stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeI stk (m `mod` n)
  pure stk
uprim2 !stk SHLI !i !j = do
  m <- upeekOff stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeI stk (m `shiftL` n)
  pure stk
uprim2 !stk SHLN !i !j = do
  m <- peekOffN stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeN stk (m `shiftL` n)
  pure stk
uprim2 !stk SHRI !i !j = do
  m <- upeekOff stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeI stk (m `shiftR` n)
  pure stk
uprim2 !stk SHRN !i !j = do
  m <- peekOffN stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeN stk (m `shiftR` n)
  pure stk
uprim2 !stk POWI !i !j = do
  m <- upeekOff stk i
  n <- peekOffN stk j
  stk <- bump stk
  pokeI stk (m ^ n)
  pure stk
uprim2 !stk POWN !i !j = do
  m <- peekOffN stk i
  n <- peekOffN stk j
  stk <- bump stk
  pokeN stk (m ^ n)
  pure stk
uprim2 !stk EQLI !i !j = do
  m <- upeekOff stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeBool stk $ m == n
  pure stk
uprim2 !stk EQLN !i !j = do
  m <- peekOffN stk i
  n <- peekOffN stk j
  stk <- bump stk
  pokeBool stk $ m == n
  pure stk
uprim2 !stk LEQI !i !j = do
  m <- upeekOff stk i
  n <- upeekOff stk j
  stk <- bump stk
  pokeBool stk $ m <= n
  pure stk
uprim2 !stk LEQN !i !j = do
  m <- peekOffN stk i
  n <- peekOffN stk j
  stk <- bump stk
  pokeBool stk $ m <= n
  pure stk
uprim2 !stk DIVN !i !j = do
  m <- peekOffN stk i
  n <- peekOffN stk j
  stk <- bump stk
  pokeN stk (m `div` n)
  pure stk
uprim2 !stk MODN !i !j = do
  m <- peekOffN stk i
  n <- peekOffN stk j
  stk <- bump stk
  pokeN stk (m `mod` n)
  pure stk
uprim2 !stk ADDF !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeD stk (x + y)
  pure stk
uprim2 !stk SUBF !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeD stk (x - y)
  pure stk
uprim2 !stk MULF !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeD stk (x * y)
  pure stk
uprim2 !stk DIVF !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeD stk (x / y)
  pure stk
uprim2 !stk LOGB !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeD stk (logBase x y)
  pure stk
uprim2 !stk POWF !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeD stk (x ** y)
  pure stk
uprim2 !stk MAXF !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeD stk (max x y)
  pure stk
uprim2 !stk MINF !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeD stk (min x y)
  pure stk
uprim2 !stk EQLF !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeBool stk $ x == y
  pure stk
uprim2 !stk LEQF !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeBool stk $ x <= y
  pure stk
uprim2 !stk ATN2 !i !j = do
  x <- peekOffD stk i
  y <- peekOffD stk j
  stk <- bump stk
  pokeD stk (atan2 x y)
  pure stk
uprim2 !stk ANDN !i !j = do
  x <- peekOffN stk i
  y <- peekOffN stk j
  stk <- bump stk
  pokeN stk (x .&. y)
  pure stk
uprim2 !stk ANDI !i !j = do
  x <- peekOffI stk i
  y <- peekOffI stk j
  stk <- bump stk
  pokeI stk (x .&. y)
  pure stk
uprim2 !stk IORN !i !j = do
  x <- peekOffN stk i
  y <- peekOffN stk j
  stk <- bump stk
  pokeN stk (x .|. y)
  pure stk
uprim2 !stk IORI !i !j = do
  x <- peekOffI stk i
  y <- peekOffI stk j
  stk <- bump stk
  pokeI stk (x .|. y)
  pure stk
uprim2 !stk XORN !i !j = do
  x <- peekOffN stk i
  y <- peekOffN stk j
  stk <- bump stk
  pokeN stk (xor x y)
  pure stk
uprim2 !stk XORI !i !j = do
  x <- peekOffI stk i
  y <- peekOffI stk j
  stk <- bump stk
  pokeI stk (xor x y)
  pure stk
uprim2 !stk CAST !vi !ti = do
  newTypeTag <- peekOffI stk ti
  v <- upeekOff stk vi
  stk <- bump stk
  poke stk $ UnboxedVal v (unboxedTypeTagFromInt newTypeTag)
  pure stk
{-# INLINE uprim2 #-}

bprim1 ::
  Stack ->
  BPrim1 ->
  Int ->
  IO Stack
bprim1 !stk SIZT i = do
  t <- peekOffBi stk i
  stk <- bump stk
  unsafePokeIasN stk $ Util.Text.size t
  pure stk
bprim1 !stk SIZS i = do
  s <- peekOffS stk i
  stk <- bump stk
  unsafePokeIasN stk $ Sq.length s
  pure stk
bprim1 !stk ITOT i = do
  n <- upeekOff stk i
  stk <- bump stk
  pokeBi stk . Util.Text.pack $ show n
  pure stk
bprim1 !stk NTOT i = do
  n <- peekOffN stk i
  stk <- bump stk
  pokeBi stk . Util.Text.pack $ show n
  pure stk
bprim1 !stk FTOT i = do
  f <- peekOffD stk i
  stk <- bump stk
  pokeBi stk . Util.Text.pack $ show f
  pure stk
bprim1 !stk USNC i =
  peekOffBi stk i >>= \t -> case Util.Text.unsnoc t of
    Nothing -> do
      stk <- bump stk
      pokeTag stk 0
      pure stk
    Just (t, c) -> do
      stk <- bumpn stk 3
      pokeOffC stk 2 $ c -- char value
      pokeOffBi stk 1 t -- remaining text
      pokeTag stk 1 -- 'Just' tag
      pure stk
bprim1 !stk UCNS i =
  peekOffBi stk i >>= \t -> case Util.Text.uncons t of
    Nothing -> do
      stk <- bump stk
      pokeTag stk 0
      pure stk
    Just (c, t) -> do
      stk <- bumpn stk 3
      pokeOffBi stk 2 t -- remaining text
      pokeOffC stk 1 $ c -- char value
      pokeTag stk 1 -- 'Just' tag
      pure stk
bprim1 !stk TTOI i =
  peekOffBi stk i >>= \t -> case readm $ Util.Text.unpack t of
    Just n
      | fromIntegral (minBound :: Int) <= n,
        n <= fromIntegral (maxBound :: Int) -> do
          stk <- bumpn stk 2
          pokeTag stk 1
          pokeOffI stk 1 (fromInteger n)
          pure stk
    _ -> do
      stk <- bump stk
      pokeTag stk 0
      pure stk
  where
    readm ('+' : s) = readMaybe s
    readm s = readMaybe s
bprim1 !stk TTON i =
  peekOffBi stk i >>= \t -> case readMaybe $ Util.Text.unpack t of
    Just n
      | 0 <= n,
        n <= fromIntegral (maxBound :: Word) -> do
          stk <- bumpn stk 2
          pokeTag stk 1
          pokeOffN stk 1 (fromInteger n)
          pure stk
    _ -> do
      stk <- bump stk
      pokeTag stk 0
      pure stk
bprim1 !stk TTOF i =
  peekOffBi stk i >>= \t -> case readMaybe $ Util.Text.unpack t of
    Nothing -> do
      stk <- bump stk
      pokeTag stk 0
      pure stk
    Just f -> do
      stk <- bumpn stk 2
      pokeTag stk 1
      pokeOffD stk 1 f
      pure stk
bprim1 !stk VWLS i =
  peekOffS stk i >>= \case
    Sq.Empty -> do
      stk <- bump stk
      pokeTag stk 0 -- 'Empty' tag
      pure stk
    x Sq.:<| xs -> do
      stk <- bumpn stk 3
      pokeOffS stk 2 xs -- remaining seq
      pokeOff stk 1 x -- head
      pokeTag stk 1 -- ':<|' tag
      pure stk
bprim1 !stk VWRS i =
  peekOffS stk i >>= \case
    Sq.Empty -> do
      stk <- bump stk
      pokeTag stk 0 -- 'Empty' tag
      pure stk
    xs Sq.:|> x -> do
      stk <- bumpn stk 3
      pokeOff stk 2 x -- last
      pokeOffS stk 1 xs -- remaining seq
      pokeTag stk 1 -- ':|>' tag
      pure stk
bprim1 !stk PAKT i = do
  s <- peekOffS stk i
  stk <- bump stk
  pokeBi stk . Util.Text.pack . toList $ val2char <$> s
  pure stk
  where
    val2char :: Val -> Char
    val2char (CharVal c) = c
    val2char c = error $ "pack text: non-character closure: " ++ show c
bprim1 !stk UPKT i = do
  t <- peekOffBi stk i
  stk <- bump stk
  pokeS stk
    . Sq.fromList
    . fmap CharVal
    . Util.Text.unpack
    $ t
  pure stk
bprim1 !stk PAKB i = do
  s <- peekOffS stk i
  stk <- bump stk
  pokeBi stk . By.fromWord8s . fmap val2w8 $ toList s
  pure stk
  where
    -- TODO: Should we have a tag for bytes specifically?
    val2w8 :: Val -> Word8
    val2w8 (NatVal n) = toEnum . fromEnum $ n
    val2w8 c = error $ "pack bytes: non-natural closure: " ++ show c
bprim1 !stk UPKB i = do
  b <- peekOffBi stk i
  stk <- bump stk
  pokeS stk . Sq.fromList . fmap (NatVal . toEnum @Word64 . fromEnum @Word8) $
    By.toWord8s b
  pure stk
bprim1 !stk SIZB i = do
  b <- peekOffBi stk i
  stk <- bump stk
  unsafePokeIasN stk $ By.size b
  pure stk
bprim1 !stk FLTB i = do
  b <- peekOffBi stk i
  stk <- bump stk
  pokeBi stk $ By.flatten b
  pure stk
-- impossible
bprim1 !stk MISS _ = pure stk
bprim1 !stk CACH _ = pure stk
bprim1 !stk LKUP _ = pure stk
bprim1 !stk CVLD _ = pure stk
bprim1 !stk TLTT _ = pure stk
bprim1 !stk LOAD _ = pure stk
bprim1 !stk VALU _ = pure stk
bprim1 !stk DBTX _ = pure stk
bprim1 !stk SDBL _ = pure stk
{-# INLINE bprim1 #-}

bprim2 ::
  Stack ->
  BPrim2 ->
  Int ->
  Int ->
  IO Stack
bprim2 !stk IXOT i j = do
  x <- peekOffBi stk i
  y <- peekOffBi stk j
  case Util.Text.indexOf x y of
    Nothing -> do
      stk <- bump stk
      pokeTag stk 0
      pure stk
    Just i -> do
      stk <- bumpn stk 2
      pokeTag stk 1
      pokeOffN stk 1 i
      pure stk
bprim2 !stk IXOB i j = do
  x <- peekOffBi stk i
  y <- peekOffBi stk j
  case By.indexOf x y of
    Nothing -> do
      stk <- bump stk
      pokeTag stk 0
      pure stk
    Just i -> do
      stk <- bumpn stk 2
      pokeTag stk 1
      pokeOffN stk 1 i
      pure stk
bprim2 !stk DRPT i j = do
  n <- upeekOff stk i
  t <- peekOffBi stk j
  stk <- bump stk
  -- Note; if n < 0, the Nat argument was greater than the maximum
  -- signed integer. As an approximation, just return the empty
  -- string, as a string larger than this would require an absurd
  -- amount of memory.
  pokeBi stk $ if n < 0 then Util.Text.empty else Util.Text.drop n t
  pure stk
bprim2 !stk CATT i j = do
  x <- peekOffBi stk i
  y <- peekOffBi stk j
  stk <- bump stk
  pokeBi stk $ (x <> y :: Util.Text.Text)
  pure stk
bprim2 !stk TAKT i j = do
  n <- upeekOff stk i
  t <- peekOffBi stk j
  stk <- bump stk
  -- Note: if n < 0, the Nat argument was greater than the maximum
  -- signed integer. As an approximation, we just return the original
  -- string, because it's unlikely such a large string exists.
  pokeBi stk $ if n < 0 then t else Util.Text.take n t
  pure stk
bprim2 !stk EQLT i j = do
  x <- peekOffBi @Util.Text.Text stk i
  y <- peekOffBi stk j
  stk <- bump stk
  pokeBool stk $ x == y
  pure stk
bprim2 !stk LEQT i j = do
  x <- peekOffBi @Util.Text.Text stk i
  y <- peekOffBi stk j
  stk <- bump stk
  pokeBool stk $ x <= y
  pure stk
bprim2 !stk LEST i j = do
  x <- peekOffBi @Util.Text.Text stk i
  y <- peekOffBi stk j
  stk <- bump stk
  pokeBool stk $ x < y
  pure stk
bprim2 !stk DRPS i j = do
  n <- upeekOff stk i
  s <- peekOffS stk j
  stk <- bump stk
  -- Note: if n < 0, then the Nat argument was larger than the largest
  -- signed integer. Seq actually doesn't handle this well, despite it
  -- being possible to build (lazy) sequences this large. So,
  -- approximate by yielding the empty sequence.
  pokeS stk $ if n < 0 then Sq.empty else Sq.drop n s
  pure stk
bprim2 !stk TAKS i j = do
  n <- upeekOff stk i
  s <- peekOffS stk j
  stk <- bump stk
  -- Note: if n < 0, then the Nat argument was greater than the
  -- largest signed integer. It is possible to build such large
  -- sequences, but the internal size will actually be wrong then. So,
  -- we just return the original sequence as an approximation.
  pokeS stk $ if n < 0 then s else Sq.take n s
  pure stk
bprim2 !stk CONS i j = do
  x <- peekOff stk i
  s <- peekOffS stk j
  stk <- bump stk
  pokeS stk $ x Sq.<| s
  pure stk
bprim2 !stk SNOC i j = do
  s <- peekOffS stk i
  x <- peekOff stk j
  stk <- bump stk
  pokeS stk $ s Sq.|> x
  pure stk
bprim2 !stk CATS i j = do
  x <- peekOffS stk i
  y <- peekOffS stk j
  stk <- bump stk
  pokeS stk $ x Sq.>< y
  pure stk
bprim2 !stk IDXS i j = do
  n <- upeekOff stk i
  s <- peekOffS stk j
  case Sq.lookup n s of
    Nothing -> do
      stk <- bump stk
      pokeTag stk 0
      pure stk
    Just x -> do
      stk <- bump stk
      poke stk x
      stk <- bump stk
      pokeTag stk 1
      pure stk
bprim2 !stk SPLL i j = do
  n <- upeekOff stk i
  s <- peekOffS stk j
  if Sq.length s < n
    then do
      stk <- bump stk
      pokeTag stk 0
      pure stk
    else do
      stk <- bumpn stk 2
      let (l, r) = Sq.splitAt n s
      pokeOffS stk 1 r
      pokeS stk l
      stk <- bump stk
      pokeTag stk 1
      pure stk
bprim2 !stk SPLR i j = do
  n <- upeekOff stk i
  s <- peekOffS stk j
  if Sq.length s < n
    then do
      stk <- bump stk
      pokeTag stk 0
      pure stk
    else do
      stk <- bumpn stk 2
      let (l, r) = Sq.splitAt (Sq.length s - n) s
      pokeOffS stk 1 r
      pokeS stk l
      stk <- bump stk
      pokeTag stk 1
      pure stk
bprim2 !stk TAKB i j = do
  n <- upeekOff stk i
  b <- peekOffBi stk j
  stk <- bump stk
  -- If n < 0, the Nat argument was larger than the maximum signed
  -- integer. Building a value this large would reuire an absurd
  -- amount of memory, so just assume n is larger.
  pokeBi stk $ if n < 0 then b else By.take n b
  pure stk
bprim2 !stk DRPB i j = do
  n <- upeekOff stk i
  b <- peekOffBi stk j
  stk <- bump stk
  -- See above for n < 0
  pokeBi stk $ if n < 0 then By.empty else By.drop n b
  pure stk
bprim2 !stk IDXB i j = do
  n <- upeekOff stk i
  b <- peekOffBi stk j
  stk <- bump stk
  stk <- case By.at n b of
    Nothing -> stk <$ pokeTag stk 0
    Just x -> do
      pokeByte stk x
      stk <- bump stk
      stk <$ pokeTag stk 1
  pure stk
bprim2 !stk CATB i j = do
  l <- peekOffBi stk i
  r <- peekOffBi stk j
  stk <- bump stk
  pokeBi stk (l <> r :: By.Bytes)
  pure stk
bprim2 !stk THRO _ _ = pure stk -- impossible
bprim2 !stk TRCE _ _ = pure stk -- impossible
bprim2 !stk EQLU _ _ = pure stk -- impossible
bprim2 !stk CMPU _ _ = pure stk -- impossible
bprim2 !stk SDBX _ _ = pure stk -- impossible
bprim2 !stk SDBV _ _ = pure stk -- impossible
{-# INLINE bprim2 #-}

yield ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  IO ()
yield !env !denv !activeThreads !stk !k = leap denv k
  where
    leap !denv0 (Mark a ps cs k) = do
      let denv = cs <> EC.withoutKeys denv0 ps
          val = denv0 EC.! EC.findMin ps
      v <- peek stk
      stk <- bump stk
      bpoke stk $ Data1 Rf.effectRef (PackedTag 0) v
      stk <- adjustArgs stk a
      apply env denv activeThreads stk k False (VArg1 0) val
    leap !denv (Push fsz asz (CIx ref _ _) f nx k) = do
      stk <- restoreFrame stk fsz asz
      stk <- ensure stk f
      eval env denv activeThreads stk k ref nx
    leap _ (CB (Hook f)) = f stk
    leap _ KE = pure ()
{-# INLINE yield #-}

selectTextBranch ::
  Util.Text.Text -> MSection -> M.Map Util.Text.Text MSection -> MSection
selectTextBranch t df cs = M.findWithDefault df t cs
{-# INLINE selectTextBranch #-}

selectBranch :: Tag -> MBranch -> MSection
selectBranch t (Test1 u y n)
  | t == u = y
  | otherwise = n
selectBranch t (Test2 u cu v cv e)
  | t == u = cu
  | t == v = cv
  | otherwise = e
selectBranch t (TestW df cs) = lookupWithDefault df t cs
selectBranch _ (TestT {}) = error "impossible"
{-# INLINE selectBranch #-}

-- Splits off a portion of the continuation up to a given prompt.
--
-- The main procedure walks along the 'code' stack `k`, keeping track of how
-- many cells of the data stacks need to be captured. Then the `finish` function
-- performs the actual splitting of the data stacks together with some tweaking.
--
-- Some special attention is required for pending arguments for over-applied
-- functions. They are part of the continuation, so how many there are at the
-- time of capture is recorded in the `Captured` closure, so that information
-- can be restored later. Also, the `Mark` frame that is popped off as part of
-- this operation potentially exposes pending arguments beyond the delimited
-- region, so those are restored in the `finish` function.
splitCont ::
  DEnv ->
  Stack ->
  K ->
  Word64 ->
  IO (Val, DEnv, Stack, K)
splitCont !denv !stk !k !p =
  walk denv asz KE k
  where
    asz = asize stk
    walk :: EnumMap Word64 Val -> SZ -> K -> K -> IO (Val, EnumMap Word64 Val, Stack, K)
    walk !denv !sz !ck KE =
      die "fell off stack" >> finish denv sz 0 ck KE
    walk !denv !sz !ck (CB _) =
      die "fell off stack" >> finish denv sz 0 ck KE
    walk !denv !sz !ck (Mark a ps cs k)
      | EC.member p ps = finish denv' sz a ck k
      | otherwise = walk denv' (sz + a) (Mark a ps cs' ck) k
      where
        denv' = cs <> EC.withoutKeys denv ps
        cs' = EC.restrictKeys denv ps
    walk !denv !sz !ck (Push n a br p brSect k) =
      walk
        denv
        (sz + n + a)
        (Push n a br p brSect ck)
        k

    finish :: EnumMap Word64 Val -> SZ -> SZ -> K -> K -> (IO (Val, EnumMap Word64 Val, Stack, K))
    finish !denv !sz !a !ck !k = do
      (seg, stk) <- grab stk sz
      stk <- adjustArgs stk a
      return (BoxedVal $ Captured ck asz seg, denv, stk, k)
{-# INLINE splitCont #-}

discardCont ::
  DEnv ->
  Stack ->
  K ->
  Word64 ->
  IO (DEnv, Stack, K)
discardCont denv stk k p =
  splitCont denv stk k p
    <&> \(_, denv, stk, k) -> (denv, stk, k)
{-# INLINE discardCont #-}

resolve :: CCache -> DEnv -> Stack -> MRef -> IO Val
resolve _ _ _ (Env cix mcomb) = pure $ mCombVal cix mcomb
resolve _ _ stk (Stk i) = peekOff stk i
resolve env denv _ (Dyn i) = case EC.lookup i denv of
  Just val -> pure val
  Nothing -> unhandledErr "resolve" env i

unhandledErr :: String -> CCache -> Word64 -> IO a
unhandledErr fname env i =
  readTVarIO (tagRefs env) >>= \rs -> case EC.lookup i rs of
    Just r -> bomb (show r)
    Nothing -> bomb (show i)
  where
    bomb sh = die $ fname ++ ": unhandled ability request: " ++ sh

rCombSection :: EnumMap Word64 MCombs -> CombIx -> MComb
rCombSection combs (CIx r n i) =
  case EC.lookup n combs of
    Just cmbs -> case EC.lookup i cmbs of
      Just cmb -> RComb cmb
      Nothing -> error $ "unknown section `" ++ show i ++ "` of combinator `" ++ show n ++ "`. Reference: " ++ show r
    Nothing -> error $ "unknown combinator `" ++ show n ++ "`. Reference: " ++ show r

resolveSection :: CCache -> Section -> IO MSection
resolveSection cc section = do
  rcombs <- readTVarIO (combs cc)
  pure $ rCombSection rcombs <$> section

dummyRef :: Reference
dummyRef = Builtin (DTx.pack "dummy")

reserveIds :: Word64 -> TVar Word64 -> IO Word64
reserveIds n free = atomically . stateTVar free $ \i -> (i, i + n)

updateMap :: (Semigroup s) => s -> TVar s -> STM s
updateMap new0 r = do
  new <- evaluateSTM new0
  stateTVar r $ \old ->
    let total = new <> old in (total, total)

refLookup :: String -> M.Map Reference Word64 -> Reference -> Word64
refLookup s m r
  | Just w <- M.lookup r m = w
  | otherwise =
      error $ "refLookup:" ++ s ++ ": unknown reference: " ++ show r

decodeCacheArgument ::
  USeq -> IO [(Reference, Code)]
decodeCacheArgument s = for (toList s) $ \case
  (Val _unboxed (Data2 _ _ (BoxedVal (Foreign x)) (BoxedVal (Data2 _ _ (BoxedVal (Foreign y)) _)))) ->
    case unwrapForeign x of
      Ref r -> pure (r, unwrapForeign y)
      _ -> die "decodeCacheArgument: Con reference"
  _ -> die "decodeCacheArgument: unrecognized value"

decodeSandboxArgument :: USeq -> IO [Reference]
decodeSandboxArgument s = fmap join . for (toList s) $ \case
  Val _ (Foreign x) -> case unwrapForeign x of
    Ref r -> pure [r]
    _ -> pure [] -- constructor
  _ -> die "decodeSandboxArgument: unrecognized value"

encodeSandboxListResult :: [Reference] -> Sq.Seq Val
encodeSandboxListResult =
  Sq.fromList . fmap (boxedVal . Foreign . Wrap Rf.termLinkRef . Ref)

encodeSandboxResult :: Either [Reference] [Reference] -> Closure
encodeSandboxResult (Left rfs) =
  encodeLeft . boxedVal . Foreign . Wrap Rf.listRef $ encodeSandboxListResult rfs
encodeSandboxResult (Right rfs) =
  encodeRight . boxedVal . Foreign . Wrap Rf.listRef $ encodeSandboxListResult rfs

encodeLeft :: Val -> Closure
encodeLeft = Data1 Rf.eitherRef TT.leftTag

encodeRight :: Val -> Closure
encodeRight = Data1 Rf.eitherRef TT.rightTag

addRefs ::
  TVar Word64 ->
  TVar (M.Map Reference Word64) ->
  TVar (EnumMap Word64 Reference) ->
  S.Set Reference ->
  STM (M.Map Reference Word64)
addRefs vfrsh vfrom vto rs = do
  from0 <- readTVar vfrom
  let new = S.filter (`M.notMember` from0) rs
      sz = fromIntegral $ S.size new
  frsh <- stateTVar vfrsh $ \i -> (i, i + sz)
  let newl = S.toList new
      from = M.fromList (zip newl [frsh ..]) <> from0
      nto = mapFromList (zip [frsh ..] newl)
  writeTVar vfrom from
  modifyTVar vto (nto <>)
  pure from

codeValidate ::
  [(Reference, SuperGroup Symbol)] ->
  CCache ->
  IO (Maybe (Failure Closure))
codeValidate tml cc = do
  rty0 <- readTVarIO (refTy cc)
  fty <- readTVarIO (freshTy cc)
  let f b r
        | b, M.notMember r rty0 = S.singleton r
        | otherwise = mempty
      ntys0 = (foldMap . foldMap) (foldGroupLinks f) tml
      ntys = M.fromList $ zip (S.toList ntys0) [fty ..]
      rty = ntys <> rty0
  ftm <- readTVarIO (freshTm cc)
  rtm0 <- readTVarIO (refTm cc)
  let rs = fst <$> tml
      rtm = rtm0 `M.withoutKeys` S.fromList rs
      rns = RN (refLookup "ty" rty) (refLookup "tm" rtm) (const Nothing)
      combinate (n, (r, g)) = evaluate $ emitCombs rns r n g
  (Nothing <$ traverse_ combinate (zip [ftm ..] tml))
    `catch` \(CE cs perr) ->
      let msg = Util.Text.pack $ toPlainUnbroken perr
          extra = Foreign . Wrap Rf.textRef . Util.Text.pack $ show cs
       in pure . Just $ Failure ioFailureRef msg extra

sandboxList :: CCache -> Referent -> IO [Reference]
sandboxList cc (Ref r) = do
  sands <- readTVarIO $ sandbox cc
  pure . maybe [] S.toList $ M.lookup r sands
sandboxList _ _ = pure []

checkSandboxing ::
  CCache ->
  [Reference] ->
  Closure ->
  IO Bool
checkSandboxing cc allowed0 c = do
  sands <- readTVarIO $ sandbox cc
  let f r
        | Just rs <- M.lookup r sands =
            rs `S.difference` allowed
        | otherwise = mempty
  pure $ S.null (closureTermRefs f c)
  where
    allowed = S.fromList allowed0

-- Checks a Value for sandboxing. A Left result indicates that some
-- dependencies of the Value are unknown. A Right result indicates
-- builtins transitively referenced by the Value that are disallowed.
checkValueSandboxing ::
  CCache ->
  [Reference] ->
  ANF.Value ->
  IO (Either [Reference] [Reference])
checkValueSandboxing cc allowed0 v = do
  sands <- readTVarIO $ sandbox cc
  have <- readTVarIO $ intermed cc
  let f False r
        | Nothing <- M.lookup r have,
          not (isBuiltin r) =
            (S.singleton r, mempty)
        | Just rs <- M.lookup r sands =
            (mempty, rs `S.difference` allowed)
      f _ _ = (mempty, mempty)
  case valueLinks f v of
    (miss, sbx)
      | S.null miss -> pure . Right $ S.toList sbx
      | otherwise -> pure . Left $ S.toList miss
  where
    allowed = S.fromList allowed0

-- Just evaluating to force exceptions. Shouldn't actually be that
-- unsafe.
evaluateSTM :: a -> STM a
evaluateSTM x = unsafeIOToSTM (evaluate x)

cacheAdd0 ::
  S.Set Reference ->
  [(Reference, Code)] ->
  [(Reference, Set Reference)] ->
  CCache ->
  IO ()
cacheAdd0 ntys0 termSuperGroups sands cc = do
  let toAdd = M.fromList (termSuperGroups <&> second codeGroup)
  (unresolvedCacheableCombs, unresolvedNonCacheableCombs) <- atomically $ do
    have <- readTVar (intermed cc)
    let new = M.difference toAdd have
    let sz = fromIntegral $ M.size new
    let rgs = M.toList new
    let rs = fst <$> rgs
    int <- updateMap new (intermed cc)
    rty <- addRefs (freshTy cc) (refTy cc) (tagRefs cc) ntys0
    ntm <- stateTVar (freshTm cc) $ \i -> (i, i + sz)
    rtm <- updateMap (M.fromList $ zip rs [ntm ..]) (refTm cc)
    -- check for missing references
    let arities = fmap (head . ANF.arities) int <> builtinArities
        rns = RN (refLookup "ty" rty) (refLookup "tm" rtm) (flip M.lookup arities)
        combinate :: Word64 -> (Reference, SuperGroup Symbol) -> (Word64, EnumMap Word64 Comb)
        combinate n (r, g) =
          (n, emitCombs rns r n g)
    let combRefUpdates = (mapFromList $ zip [ntm ..] rs)
    let combIdFromRefMap = (M.fromList $ zip rs [ntm ..])
    let newCacheableCombs =
          termSuperGroups
            & mapMaybe
              ( \case
                  (ref, CodeRep _ Cacheable) ->
                    M.lookup ref combIdFromRefMap
                  _ -> Nothing
              )
            & EC.setFromList
    newCombRefs <- updateMap combRefUpdates (combRefs cc)
    (unresolvedNewCombs, unresolvedCacheableCombs, unresolvedNonCacheableCombs, updatedCombs) <- stateTVar (combs cc) \oldCombs ->
      let unresolvedNewCombs :: EnumMap Word64 (GCombs any CombIx)
          unresolvedNewCombs = absurdCombs . mapFromList $ zipWith combinate [ntm ..] rgs
          (unresolvedCacheableCombs, unresolvedNonCacheableCombs) =
            EC.mapToList unresolvedNewCombs & foldMap \(w, gcombs) ->
              if EC.member w newCacheableCombs
                then (EC.mapSingleton w gcombs, mempty)
                else (mempty, EC.mapSingleton w gcombs)
          newCombs :: EnumMap Word64 MCombs
          newCombs = resolveCombs (Just oldCombs) $ unresolvedNewCombs
          updatedCombs = newCombs <> oldCombs
       in ((unresolvedNewCombs, unresolvedCacheableCombs, unresolvedNonCacheableCombs, updatedCombs), updatedCombs)
    nsc <- updateMap unresolvedNewCombs (srcCombs cc)
    nsn <- updateMap (M.fromList sands) (sandbox cc)
    ncc <- updateMap newCacheableCombs (cacheableCombs cc)
    -- Now that the code cache is primed with everything we need,
    -- we can pre-evaluate the top-level constants.
    pure $ int `seq` rtm `seq` newCombRefs `seq` updatedCombs `seq` nsn `seq` ncc `seq` nsc `seq` (unresolvedCacheableCombs, unresolvedNonCacheableCombs)
  preEvalTopLevelConstants unresolvedCacheableCombs unresolvedNonCacheableCombs cc

preEvalTopLevelConstants :: (EnumMap Word64 (GCombs Val CombIx)) -> (EnumMap Word64 (GCombs Val CombIx)) -> CCache -> IO ()
preEvalTopLevelConstants cacheableCombs newCombs cc = do
  activeThreads <- Just <$> UnliftIO.newIORef mempty
  evaluatedCacheableCombsVar <- newTVarIO mempty
  for_ (EC.mapToList cacheableCombs) \(w, _) -> do
    let hook stk = do
          val <- peek stk
          atomically $ do
            modifyTVar evaluatedCacheableCombsVar $ EC.mapInsert w (EC.mapSingleton 0 $ CachedVal w val)
    apply0 (Just hook) cc activeThreads w

  evaluatedCacheableCombs <- readTVarIO evaluatedCacheableCombsVar
  let allNew = evaluatedCacheableCombs <> newCombs
  -- Rewrite all the inlined combinator references to point to the
  -- new cached versions.
  atomically $ modifyTVar (combs cc) (\existingCombs -> (resolveCombs (Just $ EC.mapDifference existingCombs allNew) allNew) <> existingCombs)

expandSandbox ::
  Map Reference (Set Reference) ->
  [(Reference, SuperGroup Symbol)] ->
  [(Reference, Set Reference)]
expandSandbox sand0 groups = fixed mempty
  where
    f sand False r = fromMaybe mempty $ M.lookup r sand
    f _ True _ = mempty

    h sand (r, foldGroupLinks (f sand) -> s)
      | S.null s = Nothing
      | otherwise = Just (r, s)

    fixed extra
      | extra == extra' = new
      | otherwise = fixed extra'
      where
        new = mapMaybe (h $ extra <> sand0) groups
        extra' = M.fromList new

cacheAdd ::
  [(Reference, Code)] ->
  CCache ->
  IO [Reference]
cacheAdd l cc = do
  rtm <- readTVarIO (refTm cc)
  rty <- readTVarIO (refTy cc)
  sand <- readTVarIO (sandbox cc)
  let known = M.keysSet rtm <> S.fromList (view _1 <$> l)
      f b r
        | not b, S.notMember r known = Const (S.singleton r, mempty)
        | b, M.notMember r rty = Const (mempty, S.singleton r)
        | otherwise = Const (mempty, mempty)
      (missing, tys) =
        getConst $ (foldMap . foldMap . foldGroup) (foldGroupLinks f) l
      l'' = filter (\(r, _) -> M.notMember r rtm) l
      l' = map (second codeGroup) l''
  if S.null missing
    then [] <$ cacheAdd0 tys l'' (expandSandbox sand l') cc
    else pure $ S.toList missing

reflectValue :: EnumMap Word64 Reference -> Val -> IO ANF.Value
reflectValue rty = goV
  where
    err s = "reflectValue: cannot prepare value for serialization: " ++ s
    refTy w
      | Just r <- EC.lookup w rty = pure r
      | otherwise =
          die $ err "unknown type reference"

    goIx (CIx r _ i) = ANF.GR r i

    goV :: Val -> IO ANF.Value
    goV = \case
      -- For back-compatibility we reflect all Unboxed values into boxed literals, we could change this in the future,
      -- but there's not much of a big reason to.

      NatVal n -> pure . ANF.BLit $ ANF.Pos n
      IntVal n
        | n >= 0 -> pure . ANF.BLit $ ANF.Pos (fromIntegral n)
        | otherwise -> pure . ANF.BLit $ ANF.Neg (fromIntegral (abs n))
      DoubleVal f -> pure . ANF.BLit $ ANF.Float f
      CharVal c -> pure . ANF.BLit $ ANF.Char c
      val@(Val _ clos) ->
        case clos of
          (PApV cix _rComb args) ->
            ANF.Partial (goIx cix) <$> traverse goV args
          (DataC r t segs) ->
            ANF.Data r (maskTags t) <$> traverse goV segs
          (CapV k _ segs) ->
            ANF.Cont <$> traverse goV segs <*> goK k
          (Foreign f) -> ANF.BLit <$> goF f
          BlackHole -> die $ err "black hole"
          UnboxedTypeTag {} -> die $ err $ "unknown unboxed value" <> show val

    goK (CB _) = die $ err "callback continuation"
    goK KE = pure ANF.KE
    goK (Mark a ps de k) = do
      ps <- traverse refTy (EC.setToList ps)
      de <- traverse (\(k, v) -> (,) <$> refTy k <*> goV v) (mapToList de)
      ANF.Mark (fromIntegral a) ps (M.fromList de) <$> goK k
    goK (Push f a cix _ _rsect k) =
      ANF.Push
        (fromIntegral f)
        (fromIntegral a)
        (goIx cix)
        <$> goK k

    goF f
      | Just t <- maybeUnwrapBuiltin f =
          pure (ANF.Text t)
      | Just b <- maybeUnwrapBuiltin f =
          pure (ANF.Bytes b)
      | Just s <- maybeUnwrapForeign Rf.listRef f =
          ANF.List <$> traverse goV s
      | Just l <- maybeUnwrapForeign Rf.termLinkRef f =
          pure (ANF.TmLink l)
      | Just l <- maybeUnwrapForeign Rf.typeLinkRef f =
          pure (ANF.TyLink l)
      | Just v <- maybeUnwrapForeign Rf.valueRef f =
          pure (ANF.Quote v)
      | Just g <- maybeUnwrapForeign Rf.codeRef f =
          pure (ANF.Code g)
      | Just a <- maybeUnwrapForeign Rf.ibytearrayRef f =
          pure (ANF.BArr a)
      | Just a <- maybeUnwrapForeign Rf.iarrayRef f =
          ANF.Arr <$> traverse goV a
      | otherwise = die $ err $ "foreign value: " <> (show f)

reifyValue :: CCache -> ANF.Value -> IO (Either [Reference] Val)
reifyValue cc val = do
  erc <-
    atomically $ do
      combs <- readTVar (combs cc)
      rtm <- readTVar (refTm cc)
      case S.toList $ S.filter (`M.notMember` rtm) tmLinks of
        [] -> do
          newTy <- addRefs (freshTy cc) (refTy cc) (tagRefs cc) tyLinks
          pure . Right $ (combs, newTy, rtm)
        l -> pure (Left l)
  traverse (\rfs -> reifyValue0 rfs val) erc
  where
    f False r = (mempty, S.singleton r)
    f True r = (S.singleton r, mempty)
    (tyLinks, tmLinks) = valueLinks f val

reifyValue0 ::
  (EnumMap Word64 MCombs, M.Map Reference Word64, M.Map Reference Word64) ->
  ANF.Value ->
  IO Val
reifyValue0 (combs, rty, rtm) = goV
  where
    err s = "reifyValue: cannot restore value: " ++ s
    refTy r
      | Just w <- M.lookup r rty = pure w
      | otherwise = die . err $ "unknown type reference: " ++ show r
    refTm r
      | Just w <- M.lookup r rtm = pure w
      | otherwise = die . err $ "unknown term reference: " ++ show r
    goIx :: ANF.GroupRef -> IO (CombIx, MComb)
    goIx (ANF.GR r i) =
      refTm r <&> \n ->
        let cix = (CIx r n i)
         in (cix, rCombSection combs cix)

    goV :: ANF.Value -> IO Val
    goV (ANF.Partial gr vs) =
      goIx gr >>= \case
        (cix, RComb (Comb rcomb)) -> boxedVal . PApV cix rcomb <$> traverse goV vs
        (_, RComb (CachedVal _ val))
          | [] <- vs -> pure val
          | otherwise -> die . err $ msg
          where
            msg = "reifyValue0: non-trivial partial application to cached value"
    goV (ANF.Data r t0 vs) = do
      t <- flip packTags (fromIntegral t0) . fromIntegral <$> refTy r
      boxedVal . DataC r t <$> traverse goV vs
    goV (ANF.Cont vs k) = do
      k' <- goK k
      vs' <- traverse goV vs
      pure . boxedVal $ cv k' vs'
      where
        cv k s = CapV k a s
          where
            ksz = frameDataSize k
            a = fromIntegral $ length s - ksz
    goV (ANF.BLit l) = goL l

    goK ANF.KE = pure KE
    goK (ANF.Mark a ps de k) =
      mrk
        <$> traverse refTy ps
        <*> traverse (\(k, v) -> (,) <$> refTy k <*> (goV v)) (M.toList de)
        <*> goK k
      where
        mrk ps de k =
          Mark (fromIntegral a) (setFromList ps) (mapFromList de) k
    goK (ANF.Push f a gr k) =
      goIx gr >>= \case
        (cix, RComb (Lam _ fr sect)) ->
          Push
            (fromIntegral f)
            (fromIntegral a)
            cix
            fr
            sect
            <$> goK k
        (CIx r _ _, _) ->
          die . err $
            "tried to reify a continuation with a cached value resumption"
              ++ show r

    goL :: ANF.BLit -> IO Val
    goL (ANF.Text t) = pure . boxedVal . Foreign $ Wrap Rf.textRef t
    goL (ANF.List l) = boxedVal . Foreign . Wrap Rf.listRef <$> traverse goV l
    goL (ANF.TmLink r) = pure . boxedVal . Foreign $ Wrap Rf.termLinkRef r
    goL (ANF.TyLink r) = pure . boxedVal . Foreign $ Wrap Rf.typeLinkRef r
    goL (ANF.Bytes b) = pure . boxedVal . Foreign $ Wrap Rf.bytesRef b
    goL (ANF.Quote v) = pure . boxedVal . Foreign $ Wrap Rf.valueRef v
    goL (ANF.Code g) = pure . boxedVal . Foreign $ Wrap Rf.codeRef g
    goL (ANF.BArr a) = pure . boxedVal . Foreign $ Wrap Rf.ibytearrayRef a
    goL (ANF.Char c) = pure $ CharVal c
    goL (ANF.Pos w) =
      -- TODO: Should this be a Nat or an Int?
      pure $ NatVal w
    goL (ANF.Neg w) = pure $ IntVal (negate (fromIntegral w :: Int))
    goL (ANF.Float d) = pure $ DoubleVal d
    goL (ANF.Arr a) = boxedVal . Foreign . Wrap Rf.iarrayRef <$> traverse goV a

-- Universal comparison functions

closureNum :: Closure -> Int
closureNum PAp {} = 0
closureNum DataC {} = 1
closureNum Captured {} = 2
closureNum Foreign {} = 3
closureNum UnboxedTypeTag {} = 4
closureNum BlackHole {} = 5

universalEq ::
  (Foreign -> Foreign -> Bool) ->
  Val ->
  Val ->
  Bool
universalEq frn = eqVal
  where
    eql :: (a -> b -> Bool) -> [a] -> [b] -> Bool
    eql cm l r = length l == length r && and (zipWith cm l r)
    eqVal :: Val -> Val -> Bool
    eqVal (UnboxedVal v1 t1) (UnboxedVal v2 t2) = matchUnboxedTypes t1 t2 && v1 == v2
    eqVal (BoxedVal x) (BoxedVal y) = eqc x y
    eqVal _ _ = False
    eqc :: Closure -> Closure -> Bool
    eqc (DataC _ ct1 [w1]) (DataC _ ct2 [w2]) =
      matchTags ct1 ct2 && eqVal w1 w2
    eqc (DataC _ ct1 vs1) (DataC _ ct2 vs2) =
      ct1 == ct2
        && eqValList vs1 vs2
    eqc (PApV cix1 _ segs1) (PApV cix2 _ segs2) =
      cix1 == cix2
        && eqValList segs1 segs2
    eqc (CapV k1 a1 vs1) (CapV k2 a2 vs2) =
      eqK k1 k2
        && a1 == a2
        && eqValList vs1 vs2
    eqc (Foreign fl) (Foreign fr)
      | Just al <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fl,
        Just ar <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fr =
          arrayEq eqVal al ar
      | Just sl <- maybeUnwrapForeign @(Seq Val) Rf.listRef fl,
        Just sr <- maybeUnwrapForeign @(Seq Val) Rf.listRef fr =
          length sl == length sr && and (Sq.zipWith eqVal sl sr)
      | otherwise = frn fl fr
    eqc c d = closureNum c == closureNum d

    eqValList :: [Val] -> [Val] -> Bool
    eqValList vs1 vs2 = eql eqVal vs1 vs2

    eqK :: K -> K -> Bool
    eqK KE KE = True
    eqK (CB cb) (CB cb') = cb == cb'
    eqK (Mark a ps m k) (Mark a' ps' m' k') =
      a == a' && ps == ps' && liftEq eqVal m m' && eqK k k'
    eqK (Push f a ci _ _sect k) (Push f' a' ci' _ _sect' k') =
      f == f' && a == a' && ci == ci' && eqK k k'
    eqK _ _ = False

-- serialization doesn't necessarily preserve Int tags, so be
-- more accepting for those.
matchTags :: PackedTag -> PackedTag -> Bool
matchTags ct1 ct2 =
  ct1 == ct2
    || (ct1 == TT.intTag && ct2 == TT.natTag)
    || (ct1 == TT.natTag && ct2 == TT.intTag)

-- serialization doesn't necessarily preserve Int tags, so be
-- more accepting for those.
matchUnboxedTypes :: UnboxedTypeTag -> UnboxedTypeTag -> Bool
matchUnboxedTypes ct1 ct2 =
  ct1 == ct2
    || (ct1 == IntTag && ct2 == NatTag)
    || (ct1 == NatTag && ct2 == IntTag)

arrayEq :: (a -> a -> Bool) -> PA.Array a -> PA.Array a -> Bool
arrayEq eqc l r
  | PA.sizeofArray l /= PA.sizeofArray r = False
  | otherwise = go (PA.sizeofArray l - 1)
  where
    go i
      | i < 0 = True
      | otherwise = eqc (PA.indexArray l i) (PA.indexArray r i) && go (i - 1)

-- IEEE floating point layout is such that comparison as integers
-- somewhat works. Positive floating values map to positive integers
-- and negatives map to negatives. The corner cases are:
--
--   1. If both numbers are negative, ordering is flipped.
--   2. There is both +0 and -0, with -0 being represented as the
--      minimum signed integer.
--   3. NaN does weird things.
--
-- So, the strategy here is to compare normally if one argument is
-- positive, since positive numbers compare normally to others.
-- Otherwise, the sign bit is cleared and the numbers are compared
-- backwards. Clearing the sign bit maps -0 to +0 and maps a negative
-- number to its absolute value (including infinities). The multiple
-- NaN values are just handled according to bit patterns, rather than
-- IEEE specified behavior.
--
-- Transitivity is somewhat non-obvious for this implementation.
--
--   if i <= j and j <= k
--     if j > 0 then k > 0, so all 3 comparisons use `compare`
--     if k > 0 then k > i, since i <= j <= 0
--     if all 3 are <= 0, all 3 comparisons use the alternate
--       comparison, which is transitive via `compare`
compareAsFloat :: Int -> Int -> Ordering
compareAsFloat i j
  | i > 0 || j > 0 = compare i j
  | otherwise = compare (clear j) (clear i)
  where
    clear k = clearBit k 64

universalCompare ::
  (Foreign -> Foreign -> Ordering) ->
  Val ->
  Val ->
  Ordering
universalCompare frn = cmpVal False
  where
    cmpVal :: Bool -> Val -> Val -> Ordering
    cmpVal tyEq = \cases
      (BoxedVal c1) (BoxedVal c2) -> cmpc tyEq c1 c2
      (UnboxedVal {}) (BoxedVal {}) -> LT
      (BoxedVal {}) (UnboxedVal {}) -> GT
      (NatVal i) (NatVal j) -> compare i j
      (UnboxedVal v1 t1) (UnboxedVal v2 t2) -> cmpUnboxed tyEq (t1, v1) (t2, v2)
    cmpl :: (a -> b -> Ordering) -> [a] -> [b] -> Ordering
    cmpl cm l r =
      compare (length l) (length r) <> fold (zipWith cm l r)
    cmpc :: Bool -> Closure -> Closure -> Ordering
    cmpc tyEq = \cases
      (DataC rf1 ct1 vs1) (DataC rf2 ct2 vs2) ->
        (if tyEq && ct1 /= ct2 then compare rf1 rf2 else EQ)
          <> compare (maskTags ct1) (maskTags ct2)
          -- when comparing corresponding `Any` values, which have
          -- existentials inside check that type references match
          <> cmpValList (tyEq || rf1 == Rf.anyRef) vs1 vs2
      (PApV cix1 _ segs1) (PApV cix2 _ segs2) ->
        compare cix1 cix2
          <> cmpValList tyEq segs1 segs2
      (CapV k1 a1 vs1) (CapV k2 a2 vs2) ->
        cmpK tyEq k1 k2
          <> compare a1 a2
          <> cmpValList True vs1 vs2
      (Foreign fl) (Foreign fr)
        | Just sl <- maybeUnwrapForeign @(Seq Val) Rf.listRef fl,
          Just sr <- maybeUnwrapForeign @(Seq Val) Rf.listRef fr ->
            fold (Sq.zipWith (cmpVal tyEq) sl sr)
              <> compare (length sl) (length sr)
        | Just al <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fl,
          Just ar <- maybeUnwrapForeign @(PA.Array Val) Rf.iarrayRef fr ->
            arrayCmp (cmpVal tyEq) al ar
        | otherwise -> frn fl fr
      (UnboxedTypeTag t1) (UnboxedTypeTag t2) -> compare t1 t2
      (BlackHole) (BlackHole) -> EQ
      c d -> comparing closureNum c d

    cmpUnboxed :: Bool -> (UnboxedTypeTag, Int) -> (UnboxedTypeTag, Int) -> Ordering
    cmpUnboxed tyEq = \cases
      -- Need to cast to Nat or else maxNat == -1 and it flips comparisons of large Nats.
      -- TODO: Investigate whether bit-twiddling is faster than using Haskell's fromIntegral.
      (IntTag, n1) (IntTag, n2) -> compare n1 n2
      (NatTag, n1) (NatTag, n2) -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (NatTag, n1) (IntTag, n2)
        | n2 < 0 -> GT
        | otherwise -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (IntTag, n1) (NatTag, n2)
        | n1 < 0 -> LT
        | otherwise -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (FloatTag, n1) (FloatTag, n2) -> compareAsFloat n1 n2
      (t1, v1) (t2, v2) ->
        Monoid.whenM tyEq (compare t1 t2)
          <> compare v1 v2

    cmpValList :: Bool -> [Val] -> [Val] -> Ordering
    cmpValList tyEq vs1 vs2 = cmpl (cmpVal tyEq) vs1 vs2

    cmpK :: Bool -> K -> K -> Ordering
    cmpK tyEq = \cases
      KE KE -> EQ
      (CB cb) (CB cb') -> compare cb cb'
      (Mark a ps m k) (Mark a' ps' m' k') ->
        compare a a'
          <> compare ps ps'
          <> liftCompare (cmpVal tyEq) m m'
          <> cmpK tyEq k k'
      (Push f a ci _ _sect k) (Push f' a' ci' _ _sect' k') ->
        compare f f'
          <> compare a a'
          <> compare ci ci'
          <> cmpK tyEq k k'
      KE _ -> LT
      _ KE -> GT
      (CB {}) _ -> LT
      _ (CB {}) -> GT
      (Mark {}) _ -> LT
      _ (Mark {}) -> GT

arrayCmp ::
  (a -> a -> Ordering) ->
  PA.Array a ->
  PA.Array a ->
  Ordering
arrayCmp cmpVal l r =
  comparing PA.sizeofArray l r <> go (PA.sizeofArray l - 1)
  where
    go i
      | i < 0 = EQ
      | otherwise = cmpVal (PA.indexArray l i) (PA.indexArray r i) <> go (i - 1)
