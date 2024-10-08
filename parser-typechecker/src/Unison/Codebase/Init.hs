{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Init
  ( Init (..),
    DebugName,
    InitError (..),
    CodebaseInitOptions (..),
    CodebaseLockOption (..),
    InitResult (..),
    SpecifiedCodebase (..),
    MigrationStrategy (..),
    BackupStrategy (..),
    VacuumStrategy (..),
    Pretty,
    createCodebase,
    withOpenOrCreateCodebase,
    withNewUcmCodebaseOrExit,
    withTemporaryUcmCodebase,
  )
where

import System.Exit (exitFailure)
import Unison.Codebase (Codebase, CodebasePath)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.FileCodebase qualified as FCC
import Unison.Codebase.Init.CreateCodebaseError
import Unison.Codebase.Init.OpenCodebaseError
import Unison.Codebase.Verbosity (Verbosity, isSilent)
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.PrettyTerminal qualified as PT
import Unison.Symbol (Symbol)
import Unison.Util.Pretty qualified as P
import UnliftIO qualified
import UnliftIO.Directory (canonicalizePath)

-- CodebaseInitOptions is used to help pass around a Home directory that isn't the
-- actual home directory of the user. Useful in tests.
data CodebaseInitOptions
  = Home CodebasePath
  | Specified SpecifiedCodebase

data SpecifiedCodebase
  = CreateWhenMissing CodebasePath
  | DontCreateWhenMissing CodebasePath

data CodebaseLockOption
  = DoLock
  | DontLock

data BackupStrategy
  = -- Create a backup of the codebase in the same directory as the codebase,
    -- see 'backupCodebasePath'.
    Backup
  | -- Don't create a backup when migrating, this might be used if the caller has
    -- already created a copy of the codebase for instance.
    NoBackup
  deriving stock (Show, Eq, Ord)

data VacuumStrategy
  = -- Vacuum after migrating. Takes a bit longer but keeps the codebase clean and maybe reduces size.
    Vacuum
  | -- Don't vacuum after migrating. Vacuuming is time consuming on large codebases,
    -- so we don't want to do it during server migrations.
    NoVacuum
  deriving stock (Show, Eq, Ord)

data MigrationStrategy
  = -- | Perform a migration immediately if one is required.
    MigrateAutomatically BackupStrategy VacuumStrategy
  | -- | Prompt the user that a migration is about to occur, continue after acknownledgment
    MigrateAfterPrompt BackupStrategy VacuumStrategy
  | -- | Triggers an 'OpenCodebaseRequiresMigration' error instead of migrating
    DontMigrate
  deriving stock (Show, Eq, Ord)

initOptionsToDir :: CodebaseInitOptions -> CodebasePath
initOptionsToDir (Home dir) = dir
initOptionsToDir (Specified (CreateWhenMissing dir)) = dir
initOptionsToDir (Specified (DontCreateWhenMissing dir)) = dir

type DebugName = String

data Init m v a = Init
  { -- | open an existing codebase
    withOpenCodebase :: forall r. DebugName -> CodebasePath -> CodebaseLockOption -> MigrationStrategy -> (Codebase m v a -> m r) -> m (Either OpenCodebaseError r),
    -- | create a new codebase
    withCreatedCodebase :: forall r. DebugName -> CodebasePath -> CodebaseLockOption -> (Codebase m v a -> m r) -> m (Either CreateCodebaseError r),
    -- | given a codebase root, and given that the codebase root may have other junk in it,
    -- give the path to the "actual" files; e.g. what a forked transcript should clone.
    codebasePath :: CodebasePath -> CodebasePath
  }

-- | An error that occurred while initializing a codebase.
data InitError
  = FoundV1Codebase
  | InitErrorOpen OpenCodebaseError
  | CouldntCreateCodebase Pretty
  deriving (Show, Eq)

data InitResult
  = OpenedCodebase
  | CreatedCodebase
  deriving (Show, Eq)

createCodebaseWithResult ::
  (MonadIO m) =>
  Init m v a ->
  DebugName ->
  CodebasePath ->
  CodebaseLockOption ->
  (Codebase m v a -> m r) ->
  m (Either (CodebasePath, InitError) r)
createCodebaseWithResult cbInit debugName dir lockOption action =
  createCodebase cbInit debugName dir lockOption action <&> mapLeft \case
    errorMessage -> (dir, (CouldntCreateCodebase errorMessage))

withOpenOrCreateCodebase ::
  (MonadIO m) =>
  Init m v a ->
  DebugName ->
  CodebaseInitOptions ->
  CodebaseLockOption ->
  MigrationStrategy ->
  ((InitResult, CodebasePath, Codebase m v a) -> m r) ->
  m (Either (CodebasePath, InitError) r)
withOpenOrCreateCodebase cbInit debugName initOptions lockOption migrationStrategy action = do
  let resolvedPath = initOptionsToDir initOptions
  result <- withOpenCodebase cbInit debugName resolvedPath lockOption migrationStrategy \codebase -> do
    action (OpenedCodebase, resolvedPath, codebase)
  case result of
    Right r -> pure $ Right r
    Left err -> case err of
      OpenCodebaseDoesntExist ->
        case initOptions of
          Home homeDir -> do
            ifM
              (FCC.codebaseExists homeDir)
              (do pure (Left (homeDir, FoundV1Codebase)))
              ( do
                  -- Create V2 codebase if neither a V1 or V2 exists
                  createCodebaseWithResult cbInit debugName homeDir lockOption (\codebase -> action (CreatedCodebase, homeDir, codebase))
              )
          Specified specified ->
            ifM
              (FCC.codebaseExists resolvedPath)
              (pure $ Left (resolvedPath, FoundV1Codebase))
              case specified of
                DontCreateWhenMissing dir ->
                  pure (Left (dir, (InitErrorOpen OpenCodebaseDoesntExist)))
                CreateWhenMissing dir ->
                  createCodebaseWithResult cbInit debugName dir lockOption (\codebase -> action (CreatedCodebase, dir, codebase))
      OpenCodebaseUnknownSchemaVersion {} -> pure (Left (resolvedPath, InitErrorOpen err))
      OpenCodebaseRequiresMigration {} -> pure (Left (resolvedPath, InitErrorOpen err))
      OpenCodebaseFileLockFailed {} -> pure (Left (resolvedPath, InitErrorOpen err))

createCodebase :: (MonadIO m) => Init m v a -> DebugName -> CodebasePath -> CodebaseLockOption -> (Codebase m v a -> m r) -> m (Either Pretty r)
createCodebase cbInit debugName path lockOption action = do
  prettyDir <- P.string <$> canonicalizePath path
  withCreatedCodebase cbInit debugName path lockOption action <&> mapLeft \case
    CreateCodebaseAlreadyExists ->
      P.wrap $
        "It looks like there's already a codebase in: "
          <> prettyDir

-- * compatibility stuff

-- previously: initCodebaseOrExit :: CodebasePath -> m (m (), Codebase m v a)
-- previously: FileCodebase.initCodebase :: CodebasePath -> m (m (), Codebase m v a)
withNewUcmCodebaseOrExit :: (MonadIO m) => Init m Symbol Ann -> Verbosity -> DebugName -> CodebasePath -> CodebaseLockOption -> (Codebase m Symbol Ann -> m r) -> m r
withNewUcmCodebaseOrExit cbInit verbosity debugName path lockOption action = do
  prettyDir <- P.string <$> canonicalizePath path
  let codebaseSetup codebase = do
        unless (isSilent verbosity) . liftIO $ PT.putPrettyLn' . P.wrap $ "Initializing a new codebase in: " <> prettyDir
        Codebase.runTransaction codebase (Codebase.installUcmDependencies codebase)
  createCodebase cbInit debugName path lockOption (\cb -> codebaseSetup cb *> action cb)
    >>= \case
      Left error -> liftIO $ PT.putPrettyLn' error >> exitFailure
      Right result -> pure result

withTemporaryUcmCodebase ::
  (MonadUnliftIO m) =>
  Init m Symbol Ann ->
  Verbosity ->
  DebugName ->
  CodebaseLockOption ->
  ((CodebasePath, Codebase m Symbol Ann) -> m r) ->
  m r
withTemporaryUcmCodebase cbInit verbosity debugName lockOption action = do
  UnliftIO.withSystemTempDirectory debugName $ \tempDir -> do
    withNewUcmCodebaseOrExit cbInit verbosity debugName tempDir lockOption $ \codebase -> do
      action (tempDir, codebase)
