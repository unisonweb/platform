module Unison.Runtime.Crypto.Rsa
  ( parseRsaPublicKey,
    parseRsaPrivateKey,
    rsaErrorToText,
  )
where

import Crypto.Number.Basic qualified as Crypto
import Crypto.PubKey.RSA qualified as RSA
import Data.ASN1.BinaryEncoding qualified as ASN1
import Data.ASN1.BitArray qualified as ASN1
import Data.ASN1.Encoding qualified as ASN1
import Data.ASN1.Error qualified as ASN1
import Data.ASN1.Types qualified as ASN1
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Unison.Util.Text (Text)
import Unison.Util.Text qualified as Util.Text

-- | Parse a RSA public key from a ByteString
--   The input bytestring is a hex-encoded string of the DER file for the public key.
--   It can be generated with those commands:
--     # generate a RSA key of a given size
--     openssl genrsa -out private_key.pem <size>
--     # output the DER format as a hex string
--     openssl rsa -in private_key.pem -outform DER -pubout | xxd -p
parseRsaPublicKey :: BS.ByteString -> Either Text RSA.PublicKey
parseRsaPublicKey bs = case ASN1.decodeASN1 ASN1.DER (BSL.fromStrict bs) of
  Left err -> Left $ "rsa: cannot decode as an ASN.1 structure. " <> asn1ErrorToText err
  Right asn1 ->
    case asn1 of
      [ ASN1.Start ASN1.Sequence,
        ASN1.Start ASN1.Sequence,
        ASN1.OID _,
        ASN1.Null,
        ASN1.End ASN1.Sequence,
        ASN1.BitString (ASN1.BitArray _ bits),
        ASN1.End ASN1.Sequence
        ] -> case ASN1.decodeASN1 ASN1.DER (BSL.fromStrict bits) of
          Left err -> Left $ "rsa: cannot decode as an ASN.1 inner structure. " <> asn1ErrorToText err
          Right asn1 -> case asn1 of
            [ASN1.Start ASN1.Sequence, ASN1.IntVal n, ASN1.IntVal e, ASN1.End ASN1.Sequence] ->
              Right
                RSA.PublicKey
                  { public_size = Crypto.numBytes n,
                    public_n = n,
                    public_e = e
                  }
            other -> Left ("rsa: unexpected ASN.1 inner structure for a RSA public key" <> Util.Text.pack (show other))
      other -> Left ("rsa: unexpected ASN.1 outer structure for a RSA public key" <> Util.Text.pack (show other))

-- | Parse a RSA private key from a ByteString
--   The input bytestring is a hex-encoded string of the DER file for the private key.
--   It can be generated with those commands:
--     # generate a RSA key of a given size
--     openssl genrsa -out private_key.pem <size>
--     # output the DER format as a hex string
--     openssl rsa -in private_key.pem -outform DER | xxd -p
parseRsaPrivateKey :: BS.ByteString -> Either Text RSA.PrivateKey
parseRsaPrivateKey bs = case ASN1.decodeASN1 ASN1.DER (BSL.fromStrict bs) of
  Left err -> Left $ "Error decoding ASN.1: " <> asn1ErrorToText err
  Right asn1 ->
    case asn1 of
      [ ASN1.Start ASN1.Sequence,
        ASN1.IntVal 0,
        ASN1.Start ASN1.Sequence,
        ASN1.OID _,
        ASN1.Null,
        ASN1.End ASN1.Sequence,
        ASN1.OctetString bits,
        ASN1.End ASN1.Sequence
        ] ->
          case ASN1.decodeASN1 ASN1.DER (BSL.fromStrict bits) of
            Left err -> Left $ "Error decoding inner ASN.1: " <> asn1ErrorToText err
            Right asn1 ->
              case asn1 of
                [ ASN1.Start ASN1.Sequence,
                  ASN1.IntVal _,
                  ASN1.IntVal n,
                  ASN1.IntVal e,
                  ASN1.IntVal d,
                  ASN1.IntVal p,
                  ASN1.IntVal q,
                  ASN1.IntVal dP,
                  ASN1.IntVal dQ,
                  ASN1.IntVal qinv,
                  ASN1.End ASN1.Sequence
                  ] ->
                    Right
                      RSA.PrivateKey
                        { private_pub = RSA.PublicKey {public_size = Crypto.numBytes n, public_n = n, public_e = e},
                          private_d = d,
                          private_p = p,
                          private_q = q,
                          private_dP = dP,
                          private_dQ = dQ,
                          private_qinv = qinv
                        }
                other -> Left ("rsa: unexpected inner ASN.1 structure for a RSA private key" <> Util.Text.pack (show other))
      other -> Left ("rsa: unexpected outer ASN.1 structure for a RSA private key" <> Util.Text.pack (show other))

-- | Display an ASN1 Error
asn1ErrorToText :: ASN1.ASN1Error -> Text
asn1ErrorToText = \case
  ASN1.StreamUnexpectedEOC -> "Unexpected EOC in the stream"
  ASN1.StreamInfinitePrimitive -> "Invalid primitive with infinite length in a stream"
  ASN1.StreamConstructionWrongSize -> "A construction goes over the size specified in the header"
  ASN1.StreamUnexpectedSituation s -> "An unexpected situation has come up parsing an ASN1 event stream: " <> Util.Text.pack s
  ASN1.ParsingHeaderFail s -> "Parsing an invalid header: " <> Util.Text.pack s
  ASN1.ParsingPartial -> "Parsing is not finished, the key is not complete"
  ASN1.TypeNotImplemented s -> "Decoding of a type that is not implemented: " <> Util.Text.pack s
  ASN1.TypeDecodingFailed s -> "Decoding of a known type failed: " <> Util.Text.pack s
  ASN1.TypePrimitiveInvalid s -> "Invalid primitive type: " <> Util.Text.pack s
  ASN1.PolicyFailed s1 s2 -> "Policy failed. Policy name: " <> Util.Text.pack s1 <> ", reason:" <> Util.Text.pack s2

-- | Display a RSA Error
rsaErrorToText :: RSA.Error -> Text
rsaErrorToText = \case
  RSA.MessageSizeIncorrect ->
    "rsa: The message to decrypt is not of the correct size (need to be == private_size)"
  RSA.MessageTooLong ->
    "rsa: The message to encrypt is too long"
  RSA.MessageNotRecognized ->
    "rsa: The message decrypted doesn't have a PKCS15 structure (0 2 .. 0 msg)"
  RSA.SignatureTooLong ->
    "rsa: The message's digest is too long"
  RSA.InvalidParameters ->
    "rsa: Some parameters lead to breaking assumptions"
