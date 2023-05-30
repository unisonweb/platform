module Unison.Share.Codeserver where

import Network.URI (parseURI)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Codebase.Editor.RemoteRepo qualified as RemoteRepo
import Unison.Prelude
import Unison.Share.Types
import Unison.Share.Types qualified as Share
import UnliftIO.Environment (lookupEnv)

-- | This is the URI where the share API is based.
defaultCodeserver :: CodeserverURI
defaultCodeserver = unsafePerformIO $ do
  lookupEnv "UNISON_SHARE_HOST" <&> \case
    Nothing ->
      CodeserverURI
        { codeserverScheme = Share.Https,
          codeserverUserInfo = "",
          codeserverRegName = "api.unison-lang.org",
          codeserverPort = Nothing,
          codeserverPath = []
        }
    Just shareHost ->
      fromMaybe (error $ "Share Host is not a valid URI: " <> shareHost) $ do
        uri <- parseURI shareHost
        codeserverFromURI uri
{-# NOINLINE defaultCodeserver #-}

resolveCodeserver :: RemoteRepo.ShareCodeserver -> CodeserverURI
resolveCodeserver = \case
  RemoteRepo.DefaultCodeserver -> defaultCodeserver
  RemoteRepo.CustomCodeserver cs -> cs
