{-# LANGUAGE OverloadedStrings #-}
-- pTrace
{-# OPTIONS_GHC -Wno-deprecations #-}

module Unison.Debug
  ( debug,
    debugM,
    whenDebug,
    debugLog,
    debugLogM,
    shouldDebug,
    DebugFlag (..),
  )
where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Debug.Pretty.Simple (pTrace, pTraceM)
import System.IO.Unsafe (unsafePerformIO)
import Text.Pretty.Simple (pShow)
import Unison.Prelude
import UnliftIO.Environment (lookupEnv)

data DebugFlag
  = Auth
  | Codebase
  | Integrity
  | Merge
  | Migration
  | Sqlite
  | Sync
  | -- Language server
    LSP
  | -- | Timing how long things take
    Timing
  | -- | Useful for adding temporary debugging statements during development.
    -- Remove uses of Debug.Temp before merging to keep things clean for the next person :)
    Temp
  | -- | Debugging the interpreter
    Interpreter
  | -- | Shows Annotations when printing terms
    Annotations
  | -- | Debug endpoints of the local UI (or Share) server
    Server
  | PatternCoverage
  | PatternCoverageConstraintSolver
  | KindInference
  | Update
  deriving (Eq, Ord, Show, Bounded, Enum)

debugFlags :: Set DebugFlag
debugFlags = case (unsafePerformIO (lookupEnv "UNISON_DEBUG")) of
  Nothing -> Set.empty
  -- Enable all debugging flags for bare UNISON_DEBUG declarations like:
  -- UNISON_DEBUG= ucm
  Just "" -> Set.fromList [minBound .. maxBound]
  Just s -> Set.fromList $ do
    w <- (Text.splitOn "," . Text.pack $ s)
    case Text.toUpper . Text.strip $ w of
      "AUTH" -> pure Auth
      "CODEBASE" -> pure Codebase
      "INTEGRITY" -> pure Integrity
      "MERGE" -> pure Merge
      "MIGRATION" -> pure Migration
      "SQLITE" -> pure Sqlite
      "SYNC" -> pure Sync
      "LSP" -> pure LSP
      "TIMING" -> pure Timing
      "TEMP" -> pure Temp
      "INTERPRETER" -> pure Interpreter
      "ANNOTATIONS" -> pure Annotations
      "SERVER" -> pure Server
      "PATTERN_COVERAGE" -> pure PatternCoverage
      "PATTERN_COVERAGE_CONSTRAINT_SOLVER" -> pure PatternCoverageConstraintSolver
      "KIND_INFERENCE" -> pure KindInference
      "UPDATE" -> pure Update
      _ -> empty
{-# NOINLINE debugFlags #-}

debugSqlite :: Bool
debugSqlite = Sqlite `Set.member` debugFlags
{-# NOINLINE debugSqlite #-}

debugCodebase :: Bool
debugCodebase = Codebase `Set.member` debugFlags
{-# NOINLINE debugCodebase #-}

debugAuth :: Bool
debugAuth = Auth `Set.member` debugFlags
{-# NOINLINE debugAuth #-}

debugMerge :: Bool
debugMerge = Merge `Set.member` debugFlags
{-# NOINLINE debugMerge #-}

debugMigration :: Bool
debugMigration = Migration `Set.member` debugFlags
{-# NOINLINE debugMigration #-}

debugIntegrity :: Bool
debugIntegrity = Integrity `Set.member` debugFlags
{-# NOINLINE debugIntegrity #-}

debugSync :: Bool
debugSync = Sync `Set.member` debugFlags
{-# NOINLINE debugSync #-}

debugLSP :: Bool
debugLSP = LSP `Set.member` debugFlags
{-# NOINLINE debugLSP #-}

debugTiming :: Bool
debugTiming = Timing `Set.member` debugFlags
{-# NOINLINE debugTiming #-}

debugTemp :: Bool
debugTemp = Temp `Set.member` debugFlags
{-# NOINLINE debugTemp #-}

debugInterpreter :: Bool
debugInterpreter = Interpreter `Set.member` debugFlags
{-# NOINLINE debugInterpreter #-}

debugAnnotations :: Bool
debugAnnotations = Annotations `Set.member` debugFlags
{-# NOINLINE debugAnnotations #-}

debugServer :: Bool
debugServer = Server `Set.member` debugFlags
{-# NOINLINE debugServer #-}

debugKindInference :: Bool
debugKindInference = KindInference `Set.member` debugFlags
{-# NOINLINE debugKindInference #-}

debugUpdate :: Bool
debugUpdate = Update `Set.member` debugFlags
{-# NOINLINE debugUpdate #-}

debugPatternCoverage :: Bool
debugPatternCoverage = PatternCoverage `Set.member` debugFlags
{-# NOINLINE debugPatternCoverage #-}

debugPatternCoverageConstraintSolver :: Bool
debugPatternCoverageConstraintSolver = PatternCoverageConstraintSolver `Set.member` debugFlags
{-# NOINLINE debugPatternCoverageConstraintSolver #-}

-- | Use for trace-style selective debugging.
-- E.g. 1 + (debug Sync "The second number" 2)
--
-- Or, use in pattern matching to view arguments.
-- E.g.
-- myFunc (debug Sync "argA" -> argA) = ...
debug :: (Show a) => DebugFlag -> String -> a -> a
debug flag msg a =
  if shouldDebug flag
    then (trace (msg <> ":\n" <> into @String (pShow a)) a)
    else a

-- | Use for selective debug logging in monadic contexts.
-- E.g.
-- do
--   debugM Sync "source repo" srcRepo
--   ...
debugM :: (Show a, Monad m) => DebugFlag -> String -> a -> m ()
debugM flag msg a =
  whenDebug flag do
    traceM (msg <> ":\n" <> into @String (pShow a))

debugLog :: DebugFlag -> String -> a -> a
debugLog flag msg =
  if shouldDebug flag
    then pTrace msg
    else id

debugLogM :: (Monad m) => DebugFlag -> String -> m ()
debugLogM flag msg =
  whenDebug flag $ pTraceM msg

-- | A 'when' block which is triggered if the given flag is being debugged.
whenDebug :: (Monad m) => DebugFlag -> m () -> m ()
whenDebug flag action = do
  when (shouldDebug flag) action

shouldDebug :: DebugFlag -> Bool
shouldDebug = \case
  Auth -> debugAuth
  Codebase -> debugCodebase
  Integrity -> debugIntegrity
  Merge -> debugMerge
  Migration -> debugMigration
  Sqlite -> debugSqlite
  Sync -> debugSync
  LSP -> debugLSP
  Timing -> debugTiming
  Temp -> debugTemp
  Interpreter -> debugInterpreter
  Annotations -> debugAnnotations
  Server -> debugServer
  PatternCoverage -> debugPatternCoverage
  PatternCoverageConstraintSolver -> debugPatternCoverageConstraintSolver
  KindInference -> debugKindInference
  Update -> debugUpdate
