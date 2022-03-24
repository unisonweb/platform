{-# LANGUAGE RecordWildCards #-}

module Unison.Auth.OAuth (authWithAudience) where

import qualified Crypto.Hash as Crypto
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray.Encoding as BE
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (urlEncodedBody)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types
import Network.URI
import Network.Wai
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Unison.Auth.CredentialManager (CredentialManager, saveTokens)
import Unison.Auth.Discovery (discoveryForAudience)
import Unison.Auth.Types
import Unison.Prelude
import qualified UnliftIO
import qualified Web.Browser as Web

ucmOAuthClientID :: ByteString
ucmOAuthClientID = "ucm"

-- | A server in the format expected for a Wai Application
authTransferServer :: (Code -> IO Response) -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
authTransferServer callback req respond =
  case (requestMethod req, pathInfo req, getCodeQuery req) of
    ("GET", ["redirect"], Just code) -> do
      callback code >>= respond
    _ -> respond (responseLBS status404 [] "Not Found")
  where
    getCodeQuery req = do
      code <- join $ Prelude.lookup "code" (queryString req)
      pure $ Text.decodeUtf8 code

authWithAudience :: MonadIO m => CredentialManager -> Audience -> m (Either CredentialFailure ())
authWithAudience credsManager aud = liftIO . UnliftIO.try @_ @CredentialFailure $ do
  httpClient <- HTTP.getGlobalManager
  (DiscoveryDoc {authorizationEndpoint, tokenEndpoint}) <- throwCredFailure $ discoveryForAudience httpClient aud
  authResult <- UnliftIO.newEmptyMVar @_ @(Either CredentialFailure Tokens)
  -- Clean up this hack
  redirectURIVar <- UnliftIO.newEmptyMVar
  (verifier, challenge, state) <- generateParams
  let codeHandler code = do
        redirectURI <- UnliftIO.readMVar redirectURIVar
        result <- exchangeCode httpClient tokenEndpoint code verifier redirectURI
        UnliftIO.putMVar authResult result
        pure $ case result of
          Left _ -> Wai.responseLBS internalServerError500 [] "Something went wrong, please try again."
          Right _ -> Wai.responseLBS ok200 [] "Authorization successful. You may close this page and return to UCM."
  Warp.withApplication (pure $ authTransferServer codeHandler) $ \port -> do
    let redirectURI = "http://localhost:" <> show port <> "/redirect"
    UnliftIO.putMVar redirectURIVar redirectURI
    let authorizationKickoff = authURI authorizationEndpoint redirectURI state challenge
    void $ Web.openBrowser (show authorizationKickoff)
    putStrLn $ "Please navigate to " <> show authorizationKickoff <> " to initiate authorization."
    tokens <- throwCredFailure $ UnliftIO.readMVar authResult
    saveTokens credsManager aud tokens
  where
    throwCredFailure :: IO (Either CredentialFailure a) -> IO a
    throwCredFailure = throwEitherM

authURI :: URI -> String -> OAuthState -> PKCEChallenge -> URI
authURI authEndpoint redirectURI state challenge =
  authEndpoint
    & addQueryParam "state" state
    & addQueryParam "redirect_uri" (BSC.pack redirectURI)
    & addQueryParam "response_type" "code"
    & addQueryParam "scope" "openid"
    & addQueryParam "client_id" ucmOAuthClientID
    & addQueryParam "code_challenge" challenge
    & addQueryParam "code_challenge_method" "S256"

exchangeCode :: MonadIO m => HTTP.Manager -> URI -> Code -> PKCEVerifier -> String -> m (Either CredentialFailure Tokens)
exchangeCode httpClient tokenEndpoint code verifier redirectURI = liftIO $ do
  req <- HTTP.requestFromURI tokenEndpoint
  let addFormData =
        urlEncodedBody
          [ ("code", Text.encodeUtf8 code),
            ("code_verifier", verifier),
            ("grant_type", "authorization_code"),
            ("redirect_uri", BSC.pack redirectURI),
            ("client_id", ucmOAuthClientID)
          ]
  let fullReq = addFormData $ req {HTTP.method = "POST", HTTP.requestHeaders = [("Accept", "application/json")]}
  -- TODO: handle failure better
  resp <- HTTP.httpLbs fullReq httpClient
  let respBytes = HTTP.responseBody resp
  pure $ case Aeson.eitherDecode @Tokens respBytes of
    Left err -> Left (InvalidTokenResponse tokenEndpoint (Text.pack err))
    Right a -> Right a

addQueryParam :: ByteString -> ByteString -> URI -> URI
addQueryParam key val uri =
  let existingQuery = parseQuery $ BSC.pack (uriQuery uri)
      newParam = (key, Just val)
   in uri {uriQuery = BSC.unpack $ renderQuery True (existingQuery <> [newParam])}

generateParams :: MonadIO m => m (PKCEVerifier, PKCEChallenge, OAuthState)
generateParams = liftIO $ do
  verifier <- BE.convertToBase @ByteString BE.Base64URLUnpadded <$> getRandomBytes 50
  BSC.unpack verifier
  let digest = Crypto.hashWith Crypto.SHA256 verifier
  let challenge = BE.convertToBase BE.Base64URLUnpadded digest
  state <- BE.convertToBase @ByteString BE.Base64URLUnpadded <$> getRandomBytes 12
  pure (verifier, challenge, state)
