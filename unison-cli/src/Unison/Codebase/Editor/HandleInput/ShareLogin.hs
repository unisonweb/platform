{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.HandleInput.ShareLogin (shareLogin) where

import Unison.Auth
import Unison.Codebase.Editor.Command (Command (UCMVersion))
import Unison.Codebase.Editor.HandleInput.LoopState
import Unison.Prelude

shareLogin :: MonadIO m => Action m i v ()
shareLogin = do
  liftIO $ obtainAccessToken >>= print
