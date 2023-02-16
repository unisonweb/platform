{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Auth.Types
  ( DiscoveryDoc (..),
    Tokens (..),
    Credentials (..),
    Code,
    AccessToken,
    RefreshToken,
    IDToken,
    OAuthState,
    PKCEVerifier,
    PKCEChallenge,
    ProfileName,
    CredentialFailure (..),
    CodeserverCredentials (..),
    UserInfo (..),
    getCodeserverCredentials,
    setCodeserverCredentials,
    codeserverCredentials,
    emptyCredentials,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON (..), KeyValue ((.=)), ToJSON (..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Time (NominalDiffTime)
import Network.URI
import qualified Network.URI as URI
import Unison.Prelude
import Unison.Share.Types

defaultProfileName :: ProfileName
defaultProfileName = "default"

data CredentialFailure
  = ReauthRequired CodeserverId
  | CredentialParseFailure FilePath Text
  | InvalidDiscoveryDocument URI Text
  | InvalidJWT Text
  | RefreshFailure Text
  | InvalidTokenResponse URI Text
  | InvalidHost CodeserverURI
  | FailedToFetchUserInfo URI Text
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

type Code = Text

type OAuthState = ByteString

type PKCEVerifier = ByteString

type PKCEChallenge = ByteString

type AccessToken = Text

type RefreshToken = Text

type IDToken = Text

type TokenType = Text

newtype Scopes = Scopes [Text]
  deriving stock (Show, Eq, Ord)

instance ToJSON Scopes where
  toJSON (Scopes scopes) = Aeson.String $ Text.unwords scopes

instance FromJSON Scopes where
  parseJSON = Aeson.withText "Scopes" $ \txt -> do
    pure . Scopes $ Text.words txt

data DiscoveryDoc = DiscoveryDoc
  { issuer :: URI,
    authorizationEndpoint :: URI,
    tokenEndpoint :: URI,
    userInfoEndpoint :: URI
  }
  deriving (Show)

data Tokens = Tokens
  { accessToken :: AccessToken,
    idToken :: Maybe IDToken,
    refreshToken :: Maybe RefreshToken,
    tokenType :: TokenType,
    expiresIn :: NominalDiffTime,
    scopes :: Scopes
  }
  deriving (Eq, Show)

instance Aeson.FromJSON Tokens where
  parseJSON =
    Aeson.withObject "Tokens" $ \obj -> do
      accessToken <- obj .: "access_token"
      idToken <- obj .:? "id_token"
      refreshToken <- obj .: "refresh_token"
      tokenType <- obj .: "token_type"
      expiresIn <- obj .: "expires_in"
      scopes <- obj .: "scope"
      pure (Tokens {..})

instance Aeson.ToJSON Tokens where
  toJSON (Tokens accessToken idToken refreshToken tokenType expiresIn scopes) =
    Aeson.object
      [ "access_token" .= accessToken,
        "id_token" .= idToken,
        "refresh_token" .= refreshToken,
        "token_type" .= tokenType,
        "expires_in" .= expiresIn,
        "scope" .= scopes
      ]

newtype URIParam = URIParam URI

instance Aeson.FromJSON URIParam where
  parseJSON = Aeson.withText "URI" $ \txt ->
    maybe (fail "Invalid URI") (pure . URIParam) $ URI.parseURI (Text.unpack txt)

instance Aeson.FromJSON DiscoveryDoc where
  parseJSON = Aeson.withObject "Discovery Document" $ \obj -> do
    URIParam issuer <- obj .: "issuer"
    URIParam authorizationEndpoint <- obj .: "authorization_endpoint"
    URIParam tokenEndpoint <- obj .: "token_endpoint"
    URIParam userInfoEndpoint <- obj .: "userinfo_endpoint"
    pure (DiscoveryDoc {..})

type ProfileName = Text

data Credentials = Credentials
  { credentials :: Map ProfileName (Map CodeserverId CodeserverCredentials),
    activeProfile :: ProfileName
  }
  deriving (Eq)

instance Aeson.ToJSON Credentials where
  toJSON (Credentials credMap activeProfile) =
    Aeson.object
      [ "credentials" .= credMap,
        "active_profile" .= activeProfile
      ]

instance Aeson.FromJSON Credentials where
  parseJSON = Aeson.withObject "Credentials" $ \obj -> do
    credentials <- obj .: "credentials"
    activeProfile <- obj .: "active_profile"
    pure Credentials {..}

data UserInfo = UserInfo
  { userId :: Text, -- E.g. U-1234-5678
    name :: Maybe Text,
    handle :: Text -- The user's handle, no @ sign, e.g. "JohnSmith"
  }
  deriving (Show, Eq)

instance ToJSON UserInfo where
  toJSON (UserInfo userId name handle) =
    Aeson.object
      [ "user_id" .= userId,
        "name" .= name,
        "handle" .= handle
      ]

instance FromJSON UserInfo where
  parseJSON = Aeson.withObject "UserInfo" $ \obj -> do
    userId <- obj .: "user_id"
    name <- obj .:? "name"
    handle <- obj .: "handle"
    pure (UserInfo {..})

-- | Credentials for a specific codeserver
data CodeserverCredentials = CodeserverCredentials
  { -- The most recent set of authentication tokens
    tokens :: Tokens,
    -- URI where the discovery document for this codeserver can be fetched.
    discoveryURI :: URI,
    userInfo :: UserInfo
  }
  deriving (Eq)

instance ToJSON CodeserverCredentials where
  toJSON (CodeserverCredentials tokens discoveryURI userInfo) =
    Aeson.object
      [ "tokens" .= tokens,
        "discovery_uri" .= show discoveryURI,
        "user_info" .= userInfo
      ]

instance FromJSON CodeserverCredentials where
  parseJSON =
    Aeson.withObject "CodeserverCredentials" $ \v ->
      do
        tokens <- v .: "tokens"
        discoveryURIString <- v .: "discovery_uri"
        discoveryURI <- case parseURI discoveryURIString of
          Nothing -> fail "discovery_uri is not a valid URI"
          Just uri -> pure uri
        userInfo <- v .: "user_info"
        pure $ CodeserverCredentials {..}

emptyCredentials :: Credentials
emptyCredentials = Credentials mempty defaultProfileName

codeserverCredentials :: URI -> Tokens -> UserInfo -> CodeserverCredentials
codeserverCredentials discoveryURI tokens userInfo = CodeserverCredentials {discoveryURI, tokens, userInfo}

getCodeserverCredentials :: CodeserverId -> Credentials -> Either CredentialFailure CodeserverCredentials
getCodeserverCredentials host (Credentials {credentials, activeProfile}) =
  maybeToEither (ReauthRequired host) $
    credentials ^? ix activeProfile . ix host

setCodeserverCredentials :: CodeserverId -> CodeserverCredentials -> Credentials -> Credentials
setCodeserverCredentials host codeserverCreds creds@(Credentials {credentials, activeProfile}) =
  let newCredMap =
        credentials
          & at activeProfile . non Map.empty . at host .~ Just codeserverCreds
   in creds {credentials = newCredMap}
