{-# LANGUAGE RecordWildCards #-}
-- Manipulating JWT claims with addClaim etc. directly is deprecated, so we'll need to fix that eventually.
-- The new way appears to be to define custom types with JSON instances and use those to encode/decode the JWT;
-- see https://github.com/frasertweedale/hs-jose/issues/116
-- https://github.com/unisonweb/unison/issues/5153
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Hash-related types in the Share API.
module Unison.Share.API.Hash
  ( -- * Hash types
    HashJWT (..),
    hashJWTHash,
    HashJWTClaims (..),
    DecodedHashJWT (..),
    decodeHashJWT,
    decodeHashJWTClaims,
    decodedHashJWTHash,
  )
where

import Control.Lens (folding, ix, (^?))
import Crypto.JWT qualified as Jose
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Servant.Auth.JWT qualified as Servant.Auth
import Unison.Hash32 (Hash32)
import Unison.Hash32.Orphans.Aeson ()
import Unison.Prelude
import Web.JWT qualified as JWT

newtype HashJWT = HashJWT {unHashJWT :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

-- | Grab the hash out of a hash JWT.
--
-- This decodes the whole JWT, then throws away the claims; use it if you really only need the hash!
hashJWTHash :: HashJWT -> Hash32
hashJWTHash =
  decodedHashJWTHash . decodeHashJWT

data HashJWTClaims = HashJWTClaims
  { hash :: Hash32,
    userId :: Maybe Text
  }
  deriving stock (Show, Eq, Ord)

-- | Adding a type tag to the jwt prevents users from using jwts we issue for other things
-- in this spot. All of our jwts should have a type parameter of some kind.
hashJWTType :: String
hashJWTType = "hj"

instance Servant.Auth.ToJWT HashJWTClaims where
  encodeJWT (HashJWTClaims h u) =
    Jose.emptyClaimsSet
      & Jose.addClaim "h" (toJSON h)
      & Jose.addClaim "u" (toJSON u)
      & Jose.addClaim "t" (toJSON hashJWTType)

instance Servant.Auth.FromJWT HashJWTClaims where
  decodeJWT claims = maybe (Left "Invalid HashJWTClaims") pure $ do
    hash <- claims ^? Jose.unregisteredClaims . ix "h" . folding fromJSON
    userId <- claims ^? Jose.unregisteredClaims . ix "u" . folding fromJSON
    case claims ^? Jose.unregisteredClaims . ix "t" . folding fromJSON of
      Just t | t == hashJWTType -> pure ()
      _ -> empty
    pure HashJWTClaims {..}

instance ToJSON HashJWTClaims where
  toJSON (HashJWTClaims hash userId) =
    object
      [ "h" .= hash,
        "u" .= userId
      ]

instance FromJSON HashJWTClaims where
  parseJSON = Aeson.withObject "HashJWTClaims" \obj -> do
    hash <- obj .: "h"
    userId <- obj .: "u"
    pure HashJWTClaims {..}

-- | A decoded hash JWT that retains the original encoded JWT.
data DecodedHashJWT = DecodedHashJWT
  { claims :: HashJWTClaims,
    hashJWT :: HashJWT
  }
  deriving (Eq, Ord, Show)

-- | Decode a hash JWT.
decodeHashJWT :: HashJWT -> DecodedHashJWT
decodeHashJWT hashJWT =
  DecodedHashJWT
    { claims = decodeHashJWTClaims hashJWT,
      hashJWT
    }

-- | Decode the claims out of a hash JWT.
decodeHashJWTClaims :: HashJWT -> HashJWTClaims
decodeHashJWTClaims (HashJWT text) =
  case JWT.decode text of
    Nothing -> error "bad JWT"
    Just jwt ->
      let object =
            jwt
              & JWT.claims
              & JWT.unregisteredClaims
              & JWT.unClaimsMap
              & Aeson.KeyMap.fromMapText
              & Aeson.Object
       in case Aeson.fromJSON object of
            Aeson.Error err -> error ("bad JWT: " ++ err)
            Aeson.Success claims -> claims

-- | Grab the hash out of a decoded hash JWT.
decodedHashJWTHash :: DecodedHashJWT -> Hash32
decodedHashJWTHash DecodedHashJWT {claims = HashJWTClaims {hash}} =
  hash
