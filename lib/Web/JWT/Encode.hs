module Web.JWT.Encode (encodeVerifier, decodeVerifier) where

import Crypto.PubKey.RSA.Types qualified as RSA
import Data.Aeson
import Data.ByteString.Lazy as LB
import Web.JWT qualified as JWT

data RSAPK = RSAPK Int Integer Integer
  deriving stock (Show, Generic)

repToPublic :: RSAPK -> RSA.PublicKey
repToPublic (RSAPK s n e) = RSA.PublicKey s n e

instance ToJSON RSAPK

instance FromJSON RSAPK

-- encodes a verifier for transfer, returns Nothing if you try to encode
-- a secret key.
encodeVerifier :: JWT.VerifySigner -> Maybe LB.ByteString
encodeVerifier (JWT.VerifyRSAPublicKey (RSA.PublicKey s n e)) =
  Just $ encode $ RSAPK s n e
encodeVerifier _ = Nothing

decodeVerifier :: LB.ByteString -> Maybe JWT.VerifySigner
decodeVerifier s = JWT.VerifyRSAPublicKey . repToPublic <$> decode s
