-- | Servant configuration for the CBOR media type
--
-- Adapted from https://hackage.haskell.org/package/servant-serialization-0.3/docs/Servant-API-ContentTypes-SerialiseCBOR.html via MIT license
module Unison.Util.Servant.CBOR
  ( CBOR,
    UnknownCBORBytes,
    CBORBytes (..),
    CBORStream (..),
    deserialiseOrFailCBORBytes,
    serialiseCBORBytes,
    decodeCBORBytes,
    decodeUnknownCBORBytes,
    serialiseUnknownCBORBytes,
  )
where

import Codec.CBOR.Read (DeserialiseFailure (..))
import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Codec.Serialise qualified as CBOR
import Codec.Serialise.Decoding qualified as CBOR
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty qualified as NonEmpty
import Network.HTTP.Media.MediaType qualified as MediaType
import Servant

-- | Content-type for encoding and decoding objects as their CBOR representations
data CBOR

-- | Mime-type for CBOR and additional ones using the word "hackage" and the
-- name of the package "serialise".
instance Accept CBOR where
  contentTypes Proxy =
    NonEmpty.singleton ("application" MediaType.// "cbor")

-- |
--
-- >>> mimeRender (Proxy :: Proxy CBOR) ("Hello" :: String)
-- "eHello"
instance (Serialise a) => MimeRender CBOR a where
  mimeRender Proxy = serialise

-- |
--
-- >>> let bsl = mimeRender (Proxy :: Proxy CBOR) (3.14 :: Float)
-- >>> mimeUnrender (Proxy :: Proxy CBOR) bsl :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy CBOR) (bsl <> "trailing garbage") :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy CBOR) ("preceding garbage" <> bsl) :: Either String Float
-- Left "Codec.Serialise.deserialiseOrFail: expected float at byte-offset 0"
instance (Serialise a) => MimeUnrender CBOR a where
  mimeUnrender Proxy = mapLeft prettyErr . deserialiseOrFail
    where
      mapLeft f = either (Left . f) Right
      prettyErr (DeserialiseFailure offset err) =
        "Codec.Serialise.deserialiseOrFail: " ++ err ++ " at byte-offset " ++ show offset

-- | Wrapper for CBOR data that has already been serialized.
-- In our case, we use this because we may load pre-serialized CBOR directly from the database,
-- but it's also useful in allowing us to more quickly seek through a CBOR stream, since we only need to decode the CBOR when/if we actually need to use it, and can skip past it using a byte offset otherwise.
--
-- The 't' phantom type is the type of the data encoded in the bytestring.
newtype CBORBytes t = CBORBytes BL.ByteString
  deriving (Serialise) via (BL.ByteString)
  deriving (Eq, Show, Ord)

-- | Deserialize a 'CBORBytes' value into its tagged type, throwing an error if the deserialization fails.
deserialiseOrFailCBORBytes :: (Serialise t) => CBORBytes t -> Either CBOR.DeserialiseFailure t
deserialiseOrFailCBORBytes (CBORBytes bs) = CBOR.deserialiseOrFail bs

decodeCBORBytes :: (Serialise t) => CBORBytes t -> CBOR.Decoder s t
decodeCBORBytes (CBORBytes bs) = decodeUnknownCBORBytes (CBORBytes bs)

decodeUnknownCBORBytes :: (Serialise t) => UnknownCBORBytes -> CBOR.Decoder s t
decodeUnknownCBORBytes (CBORBytes bs) = case deserialiseOrFailCBORBytes (CBORBytes bs) of
  Left err -> fail (show err)
  Right t -> pure t

serialiseCBORBytes :: (Serialise t) => t -> CBORBytes t
serialiseCBORBytes = CBORBytes . CBOR.serialise

serialiseUnknownCBORBytes :: (Serialise t) => t -> UnknownCBORBytes
serialiseUnknownCBORBytes = CBORBytes . CBOR.serialise

data Unknown

type UnknownCBORBytes = CBORBytes Unknown

-- | Wrapper for a stream of CBOR data. Each chunk may not be a complete CBOR value, but the concatenation of all the chunks is a valid CBOR stream.
newtype CBORStream a = CBORStream BL.ByteString
  deriving (Serialise) via (BL.ByteString)
  deriving (Eq, Show, Ord)

instance MimeRender OctetStream (CBORStream a) where
  mimeRender Proxy (CBORStream bs) = bs

instance MimeUnrender OctetStream (CBORStream a) where
  mimeUnrender Proxy bs = Right (CBORStream bs)
