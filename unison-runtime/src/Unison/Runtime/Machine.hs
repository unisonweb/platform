{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Unison.Runtime.Machine
  ( ActiveThreads,
    CCache (..),
    Combs,
    Tracer (..),
    apply0,
    baseCCache,
    cacheAdd,
    cacheAdd0,
    eval0,
    expandSandbox,
    preEvalTopLevelConstants,
    refLookup,
    refNumTm,
    refNumsTm,
    refNumsTy,
    reifyValue,
    resolveSection,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM as STM
import Control.Exception
import Control.Lens
import Data.Atomics qualified as Atomic
import Data.List qualified as List
import Data.IORef (IORef)
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as DTx
import Data.Text.IO qualified as Tx
import Data.Traversable
import GHC.Conc as STM (unsafeIOToSTM)
import GHC.Stack
import Unison.Builtin.Decls (exceptionRef)
import Unison.Builtin.Decls qualified as Rf
import Unison.Prelude hiding (Text)
import Unison.Reference
  ( Reference,
    Reference' (Builtin),
  )
import Unison.Referent (pattern Ref)
import Unison.Runtime.ANF as ANF
  ( Cacheability (..),
    Code (..),
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
import Unison.Runtime.Builtin hiding (unitValue)
import Unison.Runtime.Exception hiding (die)
import Unison.Runtime.Foreign
import Unison.Runtime.Foreign.Function (foreignCall)
import Unison.Runtime.Machine.Types
import Unison.Runtime.Machine.Primops
import Unison.Runtime.MCode
import Unison.Runtime.Stack
import Unison.Runtime.TypeTags qualified as TT
import Unison.Symbol (Symbol)
import Unison.Type qualified as Rf
import Unison.Util.EnumContainers as EC
import Unison.Util.Pretty (toPlainUnbroken)
import Unison.Util.Pretty qualified as P
import Unison.Util.Text qualified as Util.Text
import UnliftIO qualified
import UnliftIO.Concurrent qualified as UnliftIO

{- ORMOLU_DISABLE -}
#ifdef STACK_CHECK
import Unison.Debug qualified as Debug
import System.IO.Unsafe (unsafePerformIO)
#endif

#ifdef OPT_CHECK
import Test.Inspection qualified as TI
#endif
{- ORMOLU_ENABLE -}

info :: (Show a) => String -> a -> IO ()
info ctx x = infos ctx (show x)

infos :: String -> String -> IO ()
infos ctx s = putStrLn $ ctx ++ ": " ++ s

-- Entry point for evaluating a section
eval0 :: CCache -> ActiveThreads -> MSection -> IO ()
eval0 env !activeThreads !co = do
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
  Maybe (XStack -> IO ()) ->
  CCache ->
  ActiveThreads ->
  Word64 ->
  IO ()
apply0 !callback env !threadTracker !i = do
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
    k0 = fromMaybe KE (callback <&> \cb -> CB . Hook $ \stk -> cb stk)

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
    k0 = CB $ Hook (\stk -> callback $ packXStack stk)
{-# inline apply1 #-}

unitValue :: Val
unitValue = BoxedVal $ unitClosure
{-# NOINLINE unitValue #-}

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
  IO (Bool, DEnv, Stack, K)
{- ORMOLU_DISABLE -}
#ifdef STACK_CHECK
exec _ !_ !_ !stk !_ !_ instr
  | debugger stk "exec" instr = undefined
#endif
{- ORMOLU_ENABLE -}
exec _ !denv !_activeThreads !stk !k _ (Info tx) = do
  info tx stk
  info tx k
  pure (False, denv, stk, k)
exec env !denv !_activeThreads !stk !k _ (Name r args) = do
  v <- resolve env denv stk r
  stk <- name stk args v
  pure (False, denv, stk, k)
exec _ !denv !_activeThreads !stk !k _ (SetDyn p i) = do
  val <- peekOff stk i
  pure (False, EC.mapInsert p val denv, stk, k)
exec _ !denv !_activeThreads !stk !k _ (Capture p) = do
  (cap, denv, stk, k) <- splitCont denv stk k p
  stk <- bump stk
  poke stk cap
  pure (False, denv, stk, k)
exec env !denv !_activeThreads !stk !k _ (Prim1 CACH i)
  | sandboxed env = die "attempted to use sandboxed operation: cache"
  | otherwise = do
      arg <- peekOffS stk i
      news <- decodeCacheArgument arg
      unknown <- cacheAdd news env
      stk <- bump stk
      pokeS
        stk
        (Sq.fromList $ boxedVal . Foreign . Wrap Rf.termLinkRef . Ref <$> unknown)
      pure (False, denv, stk, k)
exec env !denv !_activeThreads !stk !k _ (Prim1 LOAD i)
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
      pure (False, denv, stk, k)
exec env !denv !_activeThreads !stk !k _ (Prim1 VALU i) = do
  m <- readTVarIO (tagRefs env)
  c <- peekOff stk i
  stk <- bump stk
  pokeBi stk =<< reflectValue m c
  pure (False, denv, stk, k)
exec env !denv !_activeThreads !stk !k _ (Prim1 op i) = do
  stk <- prim1 env stk op i
  pure (False, denv, stk, k)
exec _ !_ !_activeThreads !stk !k r (Prim2 THRO i j) = do
  name <- peekOffBi @Util.Text.Text stk i
  x <- peekOff stk j
  () <- throwIO (BU (traceK r k) (Util.Text.toText name) x)
  error "throwIO should never return"
exec env !denv !_activeThreads !stk !k _ (Prim2 TRCE i j)
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
      pure (False, denv, stk, k)
exec env !denv !_trackThreads !stk !k _ (Prim2 op i j) = do
  stk <- primxx env stk op i j
  pure (False, denv, stk, k)
exec env !denv !_activeThreads !stk !k _ (RefCAS refI ticketI valI)
  | sandboxed env = die "attempted to use sandboxed operation: Ref.cas"
  | otherwise = do
      (ref :: IORef Val) <- peekOffBi stk refI
      -- Note that the CAS machinery is extremely fussy w/r to whether things are forced because it
      -- uses unsafe pointer equality. The only way we've gotten it to work as expected is with liberal
      -- forcing of the values and tickets.
      !(ticket :: Atomic.Ticket Val) <- peekOffBi stk ticketI
      v <- peekOff stk valI
      (r, _) <- Atomic.casIORef ref ticket v
      stk <- bump stk
      pokeBool stk r
      pure (False, denv, stk, k)
exec _ !denv !_activeThreads !stk !k _ (Pack r t args) = do
  clo <- buildData stk r t args
  stk <- bump stk
  bpoke stk clo
  pure (False, denv, stk, k)
exec _ !denv !_activeThreads !stk !k _ (Print i) = do
  t <- peekOffBi stk i
  Tx.putStrLn (Util.Text.toText t)
  pure (False, denv, stk, k)
exec _ !denv !_activeThreads !stk !k _ (Lit ml) = do
  stk <- bump stk
  poke stk $ litToVal ml
  pure (False, denv, stk, k)
exec _ !denv !_activeThreads !stk !k _ (Reset ps) = do
  (stk, a) <- saveArgs stk
  pure (False, denv, stk, Mark a ps clos k)
  where
    clos = EC.restrictKeys denv ps
exec _ !denv !_activeThreads !stk !k _ (Seq as) = do
  l <- closureArgs stk as
  stk <- bump stk
  pokeS stk $ Sq.fromList l
  pure (False, denv, stk, k)
exec _env !denv !_activeThreads !stk !k _ (ForeignCall _ func args) = do
  (b, stk) <- exStackIOToIO $ foreignCall func args (unpackXStack stk)
  pure (b, denv, stk, k)
exec env !denv !activeThreads !stk !k _ (Fork i)
  | sandboxed env = die "attempted to use sandboxed operation: fork"
  | otherwise = do
      tid <- forkEval env activeThreads =<< peekOff stk i
      stk <- bump stk
      bpoke stk . Foreign . Wrap Rf.threadIdRef $ tid
      pure (False, denv, stk, k)
exec env !denv !activeThreads !stk !k _ (Atomically i)
  | sandboxed env = die $ "attempted to use sandboxed operation: atomically"
  | otherwise = do
      v <- peekOff stk i
      stk <- bump stk
      atomicEval env activeThreads (poke stk) v
      pure (False, denv, stk, k)
exec env !denv !activeThreads !stk !k _ (TryForce i)
  | sandboxed env = die $ "attempted to use sandboxed operation: tryForce"
  | otherwise = do
      v <- peekOff stk i
      stk <- bump stk -- Bump the boxed stack to make a slot for the result, which will be written in the callback if we succeed.
      ev <- Control.Exception.try $ nestEval env activeThreads (poke stk) v
      stk <- encodeExn stk ev
      pure (False, denv, stk, k)
exec !_ !_ !_ !_ !_ _ (SandboxingFailure t) = do
  die $ "Attempted to use disallowed builtin in sandboxed environment: " <> DTx.unpack t
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
              (Rf.ioFailureRef, disp ioe, unitValue)
          | Just re <- fromException exn = case re of
              PE _stk msg ->
                (Rf.runtimeFailureRef, Util.Text.pack $ toPlainUnbroken msg, unitValue)
              BU _ tx val -> (Rf.runtimeFailureRef, Util.Text.fromText tx, val)
          | Just (ae :: ArithException) <- fromException exn =
              (Rf.arithmeticFailureRef, disp ae, unitValue)
          | Just (nae :: NestedAtomically) <- fromException exn =
              (Rf.stmFailureRef, disp nae, unitValue)
          | Just (be :: BlockedIndefinitelyOnSTM) <- fromException exn =
              (Rf.stmFailureRef, disp be, unitValue)
          | Just (be :: BlockedIndefinitelyOnMVar) <- fromException exn =
              (Rf.ioFailureRef, disp be, unitValue)
          | Just (ie :: AsyncException) <- fromException exn =
              (Rf.threadKilledFailureRef, disp ie, unitValue)
          | otherwise = (Rf.miscFailureRef, disp exn, unitValue)

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
eval _ !_ !_ !stk !_ !_ section
  | debugger stk "eval" section = undefined
#endif
{- ORMOLU_ENABLE -}
eval env !denv !activeThreads !stk !k r (Match i (TestT df cs)) = do
  t <- peekOffBi stk i
  eval env denv activeThreads stk k r $ selectTextBranch t df cs
eval env !denv !activeThreads !stk !k r (Match i br) = do
  n <- peekOffN stk i
  eval env denv activeThreads stk k r $ selectBranch n br
eval env !denv !activeThreads !stk !k r (DMatch mr i br) = do
  (nx, stk) <- dataBranch mr stk br =<< bpeekOff stk i
  eval env denv activeThreads stk k r nx
eval env !denv !activeThreads !stk !k r (NMatch _mr i br) = do
  n <- peekOffN stk i
  eval env denv activeThreads stk k r $ selectBranch n br
eval env !denv !activeThreads !stk !k r (RMatch i pu br) = do
  (t, stk) <- dumpDataValNoTag stk =<< peekOff stk i
  if t == TT.pureEffectTag
    then eval env denv activeThreads stk k r pu
    else case ANF.unpackTags t of
      (ANF.rawTag -> e, ANF.rawTag -> t)
        | Just ebs <- EC.lookup e br ->
            eval env denv activeThreads stk k r $ selectBranch t ebs
        | otherwise -> unhandledAbilityRequest
eval env !denv !activeThreads !stk !k _ (Yield args)
  | asize stk > 0,
    VArg1 i <- args =
      peekOff stk i >>= apply env denv activeThreads stk k False ZArgs
  | otherwise = do
      stk <- moveArgs stk args
      stk <- frameArgs stk
      yield env denv activeThreads stk k
eval env !denv !activeThreads !stk !k _ (App ck r args) =
  resolve env denv stk r
    >>= apply env denv activeThreads stk k ck args
eval env !denv !activeThreads !stk !k _ (Call ck combIx rcomb args) =
  enter env denv activeThreads stk k (combRef combIx) ck args rcomb
eval env !denv !activeThreads !stk !k _ (Jump i args) =
  bpeekOff stk i >>= jump env denv activeThreads stk k args
eval env !denv !activeThreads !stk !k r (Let nw cix f sect) = do
  (stk, fsz, asz) <- saveFrame stk
  eval
    env
    denv
    activeThreads
    stk
    (Push fsz asz cix f sect k)
    r
    nw
eval env !denv !activeThreads !stk !k r (Ins i nx) = do
  exec env denv activeThreads stk k r i >>= \case
    (exception, denv, stk, k)
      -- In this case, the instruction indicated an exception to
      -- be handled by the current {Exception} handler. The stack
      -- currently points to an appropriate `Failure` value, and
      -- we must handle the rest.
      | exception -> case EC.lookup TT.exceptionTag denv of
        Just eh -> do
          -- wrap the failure in an exception raise box
          fv <- peek stk
          bpoke stk $ Data1 exceptionRef TT.exceptionRaiseTag fv
          (stk, fsz, asz) <- saveFrame stk
          let kk = Push fsz asz fakeCix 10 nx k
          apply env denv activeThreads stk kk False (VArg1 0) eh
        Nothing -> -- should be impossible
          unhandledAbilityRequest
      | otherwise -> eval env denv activeThreads stk k r nx
eval _ !_ !_ !_activeThreads !_ _ Exit = pure ()
eval _ !_ !_ !_activeThreads !_ _ (Die s) = die s
{-# NOINLINE eval #-}

fakeCix :: CombIx
fakeCix = CIx exceptionRef maxBound maxBound

unhandledAbilityRequest :: (HasCallStack) => IO a
unhandledAbilityRequest = error . show . PE callStack . P.lit . fromString $ "eval: unhandled ability request"

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
enter env !denv !activeThreads !stk !k !cref !sck !args = \case
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
apply _env !_denv !_activeThreads !stk !_k !_ck !args !val
  | debugger stk "apply" (args, val) = undefined
#endif
{- ORMOLU_ENABLE -}
apply env !denv !activeThreads !stk !k !ck !args !val =
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
jump env !denv !activeThreads !stk !k !args clo = case clo of
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
repush env !activeThreads !stk = go
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

dumpDataValNoTag ::
  Stack ->
  Val ->
  IO (PackedTag, Stack)
dumpDataValNoTag stk (BoxedVal c) =
  (closureTag c,) <$> dumpDataNoTag Nothing stk c
dumpDataValNoTag _ v =
  die $ "dumpDataValNoTag: unboxed val: " ++ show v
{-# inline dumpDataValNoTag #-}

-- Dumps a data type closure to the stack without writing its tag.
-- Instead, the tag is returned for direct case analysis.
dumpDataNoTag ::
  Maybe Reference ->
  Stack ->
  Closure ->
  IO Stack
dumpDataNoTag !mr !stk = \case
  -- Normally we want to avoid dumping unboxed values since it's unnecessary, but sometimes we don't know the type of
  -- the incoming value and end up dumping unboxed values, so we just push them back to the stack as-is. e.g. in type-casts/coercions
  Enum _ _ -> pure stk
  Data1 _ _ x -> do
    stk <- bump stk
    poke stk x
    pure stk
  Data2 _ _ x y -> do
    stk <- bumpn stk 2
    pokeOff stk 1 y
    stk <$ poke stk x
  DataG _ _ seg -> dumpSeg stk seg S
  clo ->
    die $
      "dumpDataNoTag: bad closure: "
        ++ show clo
        ++ maybe "" (\r -> "\nexpected type: " ++ show r) mr
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

yield ::
  CCache ->
  DEnv ->
  ActiveThreads ->
  Stack ->
  K ->
  IO ()
yield env !denv !activeThreads !stk !k = leap denv k
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
    leap _ (CB (Hook f)) = f (unpackXStack stk)
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

-- Combined branch selection and field dumping function for data types.
-- Fields should only be dumped on _matches_, not default cases, because
-- default cases potentially cover many constructors which could result
-- in a variable number of values being put on the stack. Default cases
-- uniformly expect _no_ values to be added to the stack.
dataBranch
  :: Maybe Reference -> Stack -> MBranch -> Closure -> IO (MSection, Stack)
dataBranch mrf stk (Test1 u cu df) = \case
  Enum _ t
    | maskTags t == u -> pure (cu, stk)
    | otherwise -> pure (df, stk)
  Data1 _ t x
    | maskTags t == u -> do
      stk <- bump stk
      (cu, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  Data2 _ t x y
    | maskTags t == u -> do
      stk <- bumpn stk 2
      pokeOff stk 1 y
      (cu, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  DataG _ t seg
    | maskTags t == u -> (cu,) <$> dumpSeg stk seg S
    | otherwise -> pure (df, stk)
  clo -> dataBranchClosureError mrf clo
dataBranch mrf stk (Test2 u cu v cv df) = \case
  Enum _ t
    | maskTags t == u -> pure (cu, stk)
    | maskTags t == v -> pure (cv, stk)
    | otherwise -> pure (df, stk)
  Data1 _ t x
    | maskTags t == u -> do
      stk <- bump stk
      (cu, stk) <$ poke stk x
    | maskTags t == v -> do
      stk <- bump stk
      (cv, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  Data2 _ t x y
    | maskTags t == u -> do
      stk <- bumpn stk 2
      pokeOff stk 1 y
      (cu, stk) <$ poke stk x
    | maskTags t == v -> do
      stk <- bumpn stk 2
      pokeOff stk 1 y
      (cv, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  DataG _ t seg
    | maskTags t == u -> (cu,) <$> dumpSeg stk seg S
    | maskTags t == v -> (cv,) <$> dumpSeg stk seg S
    | otherwise -> pure (df, stk)
  clo -> dataBranchClosureError mrf clo
dataBranch mrf stk (TestW df bs) = \case
  Enum _ t
    | Just ca <- EC.lookup (maskTags t) bs -> pure (ca, stk)
    | otherwise -> pure (df, stk)
  Data1 _ t x
    | Just ca <- EC.lookup (maskTags t) bs -> do
      stk <- bump stk
      (ca, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  Data2 _ t x y
    | Just ca <- EC.lookup (maskTags t) bs -> do
      stk <- bumpn stk 2
      pokeOff stk 1 y
      (ca, stk) <$ poke stk x
    | otherwise -> pure (df, stk)
  DataG _ t seg
    | Just ca <- EC.lookup (maskTags t) bs ->
      (ca,) <$> dumpSeg stk seg S
    | otherwise -> pure (df, stk)
  clo -> dataBranchClosureError mrf clo
dataBranch _ _ br = \_ ->
  dataBranchBranchError br
{-# inline dataBranch #-}

dataBranchClosureError :: Maybe Reference -> Closure -> IO a
dataBranchClosureError mrf clo =
  die $ "dataBranch: bad closure: "
    ++ show clo
    ++ maybe "" (\ r -> "\nexpected type: " ++ show r) mrf

dataBranchBranchError :: MBranch -> IO a
dataBranchBranchError br =
  die $ "dataBranch: unexpected branch: " ++ show br

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

updateMap :: (Semigroup s) => s -> TVar s -> STM s
updateMap new0 r = do
  new <- evaluateSTM new0
  stateTVar r $ \old ->
    let total = new <> old in (total, total)

decodeCacheArgument ::
  USeq -> IO [(Reference, Code)]
decodeCacheArgument s = for (toList s) $ \case
  (Val _unboxed (Data2 _ _ (BoxedVal (Foreign x)) (BoxedVal (Data2 _ _ (BoxedVal (Foreign y)) _)))) ->
    case unwrapForeign x of
      Ref r -> pure (r, unwrapForeign y)
      _ -> die "decodeCacheArgument: Con reference"
  _ -> die "decodeCacheArgument: unrecognized value"

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
        inlinfo = ANF.buildInlineMap int <> builtinInlineInfo
        rns = RN (refLookup "ty" rty) (refLookup "tm" rtm) (flip M.lookup arities)
        combinate :: Word64 -> (Reference, SuperGroup Symbol) -> (Word64, EnumMap Word64 Comb)
        combinate n (r, g) =
          (n, emitCombs rns r n $ ANF.inline inlinfo g)
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
          unresolvedNewCombs =
            absurdCombs . sanitizeCombsOfForeignFuncs (sandboxed cc) sandboxedForeignFuncs . mapFromList $ zipWith combinate [ntm ..] rgs
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
    let hook xstk = do
          val <- peek (packXStack xstk)
          atomically $ do
            modifyTVar evaluatedCacheableCombsVar $ EC.mapInsert w (EC.mapSingleton 0 $ CachedVal w val)
    apply0 (Just hook) cc activeThreads w
      `catch` \e ->
        -- ignore sandboxing exceptions during pre-eval, in case they
        -- don't matter for the final result.
        if isSandboxingException e
          then pure ()
          else throwIO e

  evaluatedCacheableCombs <- readTVarIO evaluatedCacheableCombsVar
  let allNew = evaluatedCacheableCombs <> newCombs
  -- Rewrite all the inlined combinator references to point to the
  -- new cached versions.
  atomically $ modifyTVar (combs cc) (\existingCombs -> (resolveCombs (Just $ EC.mapDifference existingCombs allNew) allNew) <> existingCombs)

-- Checks if a runtime exception is due to sandboxing.
--
-- This is used above during pre-evaluation, to ignore sandboxing
-- exceptions for top-level constant dependencies of docs and such, in
-- case the docs don't actually evaluate them.
isSandboxingException :: RuntimeExn -> Bool
isSandboxingException (PE _ (P.toPlainUnbroken -> msg)) =
  List.isPrefixOf sdbx1 msg || List.isPrefixOf sdbx2 msg
  where
    sdbx1 = "attempted to use sandboxed operation"
    sdbx2 = "Attempted to use disallowed builtin in sandboxed"
isSandboxingException _ = False

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

{- ORMOLU_DISABLE -}
#ifdef OPT_CHECK
-- Assert that we don't allocate any 'Stack' objects in 'eval', since we expect GHC to always
-- trigger the worker/wrapper optimization and unbox it fully, and if it fails to do so, we want to
-- know about it.
--
-- Note: this must remain in this module, it can't be moved to a testing module, this is a requirement of the inspection
-- testing library.
--
-- Note: We _must_ check 'eval0' instead of 'eval' here because if you simply check 'eval', you'll be
-- testing the 'wrapper' part of the worker/wrapper, which will always mention the 'Stack' object as part of its
-- unwrapping, and since there's  no way to refer to the generated wrapper directly, we instead refer to 'eval0'
-- which allocates its own stack to pass in, meaning it's one level above the wrapper, and GHC should always detect that
-- it can call the worker directly without using the wrapper.
-- See: https://github.com/nomeata/inspection-testing/issues/50 for more information.
--
-- If this test starts failing, here are some things you can check.
--
-- 1. Are 'Stack's being passed to dynamic functions? If so, try changing those functions to take an 'XStack' instead,
--    and manually unpack/pack the 'Stack' where necessary.
-- 2. Are there calls to 'die' or 'throwIO' or something similar in which a fully polymorphic type variable is being
--    specialized to 'Stack'? Sometimes this trips up the optimization, you can try using an 'error' instead, or even
--    following the 'throwIO' with a useless call to @error "unreachable"@, this seems to help for some reason.
--    See this page for more info on precise exceptions: https://gitlab.haskell.org/ghc/ghc/-/wikis/exceptions/precise-exceptions
--
-- Best of luck!
TI.inspect $ 'eval0 `TI.hasNoType` ''Stack
#endif
{- ORMOLU_ENABLE -}
