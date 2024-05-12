{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Data.Challenge where

import ByteString.Aeson.Orphans ()
import Control.Arrow ((***))
import Crypto.Error
import Crypto.Error.Utils
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson
import Data.ByteArray (ByteArrayAccess, withByteArray)
import Data.ByteArray qualified as BA
import Data.Time.Clock.POSIX

data Challenge = Challenge
  { ownerSignature :: Maybe ByteString
  , providerSignature :: ByteString
  , token :: ByteString
  , callback :: ByteString
  , issued :: POSIXTime
  }
  deriving stock (Generic, Show)

newChallenge ::
  Ed.SecretKey -> Ed.PublicKey -> ByteString -> ByteString -> IO Challenge
newChallenge s p t c = do
  iss <- getPOSIXTime
  return $ fix $ \ch -> Challenge Nothing (BA.convert $ Ed.sign s p ch) t c iss

instance ByteArrayAccess Challenge where
  length Challenge {..} = BA.length $ fromString (show issued) <> callback <> token
  withByteArray Challenge {..} = withByteArray $ fromString (show issued) <> callback <> token

-- bytestrings get encoded in base64 as per bytestring-aeson-orphans
instance ToJSON Challenge where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Challenge

verify :: Ed.PublicKey -> Ed.PublicKey -> Challenge -> Bool
verify providerKey ownerKey c =
  maybe
    False
    (and . (Ed.verify providerKey c *** Ed.verify ownerKey c))
    $ do
      o <- rightToMaybe . cfToEither . Ed.signature =<< ownerSignature c
      p <- rightToMaybe . cfToEither . Ed.signature $ providerSignature c
      return (o, p)

solve :: Ed.SecretKey -> Ed.PublicKey -> Challenge -> Challenge
solve s p c = c {ownerSignature = Just $ BA.convert $ Ed.sign s p c}

fromCryptoPassed :: CryptoFailable a -> a
fromCryptoPassed (CryptoPassed a) = a
fromCryptoPassed _ = undefined
