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

import Control.Applicative (empty)
import Control.Monad (when)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Debug.Pretty.Simple (pTrace, pTraceM, pTraceShowId, pTraceShowM)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Environment (lookupEnv)

data DebugFlag
  = Auth
  | Codebase
  | Git
  | Integrity
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
  | -- | Shows Annotations when printing terms
    Annotations
  | -- | Debug endpoints of the local UI (or Share) server
    Server
  | PatternCoverage
  | PatternCoverageConstraintSolver
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
      "GIT" -> pure Git
      "INTEGRITY" -> pure Integrity
      "MIGRATION" -> pure Migration
      "SQLITE" -> pure Sqlite
      "SYNC" -> pure Sync
      "LSP" -> pure LSP
      "TIMING" -> pure Timing
      "TEMP" -> pure Temp
      "ANNOTATIONS" -> pure Annotations
      "SERVER" -> pure Server
      "PATTERN_COVERAGE" -> pure PatternCoverage
      "PATTERN_COVERAGE_CONSTRAINT_SOLVER" -> pure PatternCoverageConstraintSolver
      _ -> empty
{-# NOINLINE debugFlags #-}

debugGit :: Bool
debugGit = Git `Set.member` debugFlags
{-# NOINLINE debugGit #-}

debugSqlite :: Bool
debugSqlite = Sqlite `Set.member` debugFlags
{-# NOINLINE debugSqlite #-}

debugCodebase :: Bool
debugCodebase = Codebase `Set.member` debugFlags
{-# NOINLINE debugCodebase #-}

debugAuth :: Bool
debugAuth = Auth `Set.member` debugFlags
{-# NOINLINE debugAuth #-}

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

debugAnnotations :: Bool
debugAnnotations = Annotations `Set.member` debugFlags
{-# NOINLINE debugAnnotations #-}

debugServer :: Bool
debugServer = Server `Set.member` debugFlags
{-# NOINLINE debugServer #-}

debugPatternCoverage :: Bool
debugPatternCoverage = PatternCoverage `Set.member` debugFlags
{-# NOINLINE debugPatternCoverage #-}

debugPatternCoverageConstraintSolver :: Bool
debugPatternCoverageConstraintSolver = PatternCoverageConstraintSolver `Set.member` debugFlags
{-# NOINLINE debugPatternCoverageConstraintSolver #-}

-- | Use for trace-style selective debugging.
-- E.g. 1 + (debug Git "The second number" 2)
--
-- Or, use in pattern matching to view arguments.
-- E.g.
-- myFunc (debug Git "argA" -> argA) = ...
debug :: (Show a) => DebugFlag -> String -> a -> a
debug flag msg a =
  if shouldDebug flag
    then pTraceShowId (pTrace (msg <> ":\n") a)
    else a

-- | Use for selective debug logging in monadic contexts.
-- E.g.
-- do
--   debugM Git "source repo" srcRepo
--   ...
debugM :: (Show a, Monad m) => DebugFlag -> String -> a -> m ()
debugM flag msg a =
  whenDebug flag do
    pTraceM (msg <> ":\n")
    pTraceShowM a

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
  Git -> debugGit
  Integrity -> debugIntegrity
  Migration -> debugMigration
  Sqlite -> debugSqlite
  Sync -> debugSync
  LSP -> debugLSP
  Timing -> debugTiming
  Temp -> debugTemp
  Annotations -> debugAnnotations
  Server -> debugServer
  PatternCoverage -> debugPatternCoverage
  PatternCoverageConstraintSolver -> debugPatternCoverageConstraintSolver
