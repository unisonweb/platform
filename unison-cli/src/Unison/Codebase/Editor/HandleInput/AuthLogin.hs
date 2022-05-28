module Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin) where

import Control.Monad.Reader
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Auth.OAuth (authenticateCodeserver)
import Unison.Codebase.Editor.HandleInput.LoopState
import Unison.Codebase.Editor.Output (Output (CredentialFailureMsg, Success))
import Unison.Prelude
import Unison.Share.Types
import qualified UnliftIO
import UnliftIO.Environment (lookupEnv)

-- | This is the URI where the share API is based.
defaultShareURI :: CodeserverURI
defaultShareURI = unsafePerformIO $ do
  lookupEnv "UNISON_SHARE_HOST" <&> \case
    -- TODO: swap to production share before release.
    Nothing ->
      CodeserverURI . fromJust . parseURI $ "https://share-next.us-west-2.unison-lang.org/api"
    Just shareHost ->
      case parseURI shareHost of
        Nothing -> error $ "Share Host is not a valid URI: " <> shareHost
        Just uri -> CodeserverURI uri
{-# NOINLINE defaultShareURI #-}

authLogin :: UnliftIO.MonadUnliftIO m => Maybe CodeserverURI -> Action m i v ()
authLogin mayURI = do
  let host = fromMaybe defaultShareURI mayURI
  credsMan <- asks credentialManager
  (Action . lift . lift . lift $ authenticateCodeserver credsMan host) >>= \case
    Left err -> respond (CredentialFailureMsg err)
    Right () -> respond Success
