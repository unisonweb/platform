
module Unison.Runtime.Machine.Types where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM as STM
import Control.Exception
import Data.IORef (IORef)
import Data.Word
import Data.Map.Strict qualified as M
import GHC.Stack
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent, pattern Ref)
import Unison.Runtime.ANF (SuperGroup (..), Cacheability (..), Code (..))
import Unison.Runtime.Builtin
import Unison.Runtime.Exception hiding (die)
import Unison.Runtime.MCode
import Unison.Runtime.Stack
import Unison.Symbol
import Unison.Util.EnumContainers as EC
import Unison.Util.Pretty qualified as P

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
  { sandboxed :: Bool,
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

baseCCache :: Bool -> IO CCache
baseCCache sandboxed = do
  CCache sandboxed noTrace
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
        & sanitizeCombsOfForeignFuncs sandboxed sandboxedForeignFuncs
        & absurdCombs
        & resolveCombs Nothing

refLookup :: String -> M.Map Reference Word64 -> Reference -> Word64
refLookup s m r
  | Just w <- M.lookup r m = w
  | otherwise =
      error $ "refLookup:" ++ s ++ ": unknown reference: " ++ show r

die :: (HasCallStack) => String -> IO a
die s = do
  void . throwIO . PE callStack . P.lit . fromString $ s
  -- This is unreachable, but we need it to fix some quirks in GHC's
  -- worker/wrapper optimization, specifically, it seems that when throwIO's polymorphic return
  -- value is specialized to a type like 'Stack' which we want GHC to unbox, it will sometimes
  -- fail to unbox it, possibly because it can't unbox it when it's strictly a type application.
  -- For whatever reason, this seems to fix it while still allowing us to throw exceptions in IO
  -- like we prefer.
  error "unreachable"
{-# INLINE die #-}

lookupCode :: CCache -> Referent -> IO (Maybe Code)
lookupCode env (Ref link) =
  resolveCode link <$>
    readTVarIO (intermed env) <*>
    readTVarIO (refTm env) <*>
    readTVarIO (cacheableCombs env)
lookupCode _ _ = die "lookupCode: Expected Ref"

resolveCode ::
  Reference ->
  Map Reference (SuperGroup Symbol) ->
  Map Reference Word64 ->
  EnumSet Word64 ->
  Maybe Code
resolveCode link m rfn cach
  | Just sg <- M.lookup link m,
    ch <- cacheability rfn cach link =
      Just $ CodeRep sg ch
  | Just w <- M.lookup link builtinTermNumbering,
    Just sn <- EC.lookup w numberedTermLookup =
      Just $ CodeRep (Rec [] sn) Uncacheable
  | otherwise = Nothing

cacheability ::
  Map Reference Word64 ->
  EnumSet Word64 ->
  Reference ->
  Cacheability
cacheability rfn cach link
  | Just n <- M.lookup link rfn,
    EC.member n cach =
      Cacheable
  | otherwise = Uncacheable
