{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.CodebaseInit where

import EasyTest
import System.IO.Temp qualified as Temp
import Unison.Codebase.Init
  ( CodebaseInitOptions (..),
    Init (..),
    SpecifiedCodebase (..),
  )
import Unison.Codebase.Init qualified as CI
import Unison.Codebase.Init.OpenCodebaseError (OpenCodebaseError (..))

test :: Test ()
test =
  scope "Codebase.Init" $
    tests
      [ scope "*without* a --codebase flag" $
          tests
            [ scope "a v2 codebase should be opened" do
                tmp <- io (Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "ucm-test")
                cbInit <- io initMockWithCodebase
                r <- io $ CI.withOpenOrCreateCodebase cbInit "ucm-test" (Home tmp) CI.DontLock CI.DontMigrate \case
                  (CI.OpenedCodebase, _, _) -> pure True
                  _ -> pure False
                case r of
                  Left _ -> expect False
                  Right b -> expect b,
              scope "a v2 codebase should be created when one does not exist" do
                tmp <- io (Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "ucm-test")
                cbInit <- io initMockWithoutCodebase
                r <- io $ CI.withOpenOrCreateCodebase cbInit "ucm-test" (Home tmp) CI.DontLock CI.DontMigrate \case
                  (CI.CreatedCodebase, _, _) -> pure True
                  _ -> pure False
                case r of
                  Left _ -> expect False
                  Right b -> expect b
            ],
        scope "*with* a --codebase flag" $
          tests
            [ scope "a v2 codebase should be opened" do
                tmp <- io (Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "ucm-test")
                cbInit <- io initMockWithCodebase
                res <- io $
                  CI.withOpenOrCreateCodebase cbInit "ucm-test" (Specified (DontCreateWhenMissing tmp)) CI.DontLock CI.DontMigrate $ \case
                    (CI.OpenedCodebase, _, _) -> pure True
                    _ -> pure False
                case res of
                  Left _ -> expect False
                  Right b -> expect b,
              scope "a v2 codebase should be *not* created when one does not exist at the Specified dir" do
                tmp <- io (Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "ucm-test")
                cbInit <- io initMockWithoutCodebase
                res <- io $
                  CI.withOpenOrCreateCodebase cbInit "ucm-test" (Specified (DontCreateWhenMissing tmp)) CI.DontLock CI.DontMigrate $ \case
                    _ -> pure False
                case res of
                  Left (_, CI.InitErrorOpen OpenCodebaseDoesntExist) -> expect True
                  _ -> expect False
            ],
        scope "*with* a --codebase-create flag" $
          tests
            [ scope "a v2 codebase should be created when one does not exist at the Specified dir" do
                tmp <- io (Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "ucm-test")
                cbInit <- io initMockWithoutCodebase
                res <- io $ CI.withOpenOrCreateCodebase cbInit "ucm-test" (Specified (CreateWhenMissing tmp)) CI.DontLock CI.DontMigrate \case
                  (CI.CreatedCodebase, _, _) -> pure True
                  _ -> pure False
                case res of
                  Left _ -> expect False
                  Right b -> expect b,
              scope "a v2 codebase should be opened when one exists" do
                tmp <- io (Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "ucm-test")
                cbInit <- io initMockWithCodebase
                res <- io $ CI.withOpenOrCreateCodebase cbInit "ucm-test" (Specified (CreateWhenMissing tmp)) CI.DontLock CI.DontMigrate \case
                  (CI.OpenedCodebase, _, _) -> pure True
                  _ -> pure False
                case res of
                  Left _ -> expect False
                  Right b -> expect b
            ]
      ]

-- Test helpers

initMockWithCodebase :: IO (Init IO v a)
initMockWithCodebase = do
  let codebase = error "did we /actually/ need a Codebase?"
  pure $
    Init
      { -- withOpenCodebase :: forall r. DebugName -> CodebasePath -> (Codebase m v a -> m r) -> m (Either Pretty r),
        withOpenCodebase = \_ _ _ _ action -> Right <$> action codebase,
        -- withCreatedCodebase :: forall r. DebugName -> CodebasePath -> (Codebase m v a -> m r) -> m (Either CreateCodebaseError r),
        withCreatedCodebase = \_ _ _ action -> Right <$> action codebase,
        -- CodebasePath -> CodebasePath
        codebasePath = id
      }

initMockWithoutCodebase :: IO (Init IO v a)
initMockWithoutCodebase = do
  let codebase = error "did we /actually/ need a Codebase?"
  pure $
    Init
      { withOpenCodebase = \_ _ _ _ _ -> pure (Left OpenCodebaseDoesntExist),
        -- withCreatedCodebase :: forall r. DebugName -> CodebasePath -> (Codebase m v a -> m r) -> m (Either CreateCodebaseError r),
        withCreatedCodebase = \_ _ _ action -> Right <$> action codebase,
        -- CodebasePath -> CodebasePath
        codebasePath = id
      }
