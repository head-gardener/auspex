module Crypto.PubKey.Ed25519.OpenSSH (readEdSecret, readEdPublic, readKeys) where

import ByteString.Aeson.Orphans ()
import Crypto.Error.Utils
import Crypto.PubKey.Ed25519
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BC8
import Text.Appar.ByteString

sequencePair :: (Maybe a, Maybe b) -> Maybe (a, b)
sequencePair (Just a, Just b) = Just (a, b)
sequencePair _ = Nothing

readEdSecret :: FilePath -> IO (Maybe SecretKey)
readEdSecret f = parse secretKeyParser <$> readFileBS f

readEdPublic :: FilePath -> IO (Maybe PublicKey)
readEdPublic f = parse publicKeyParser <$> readFileBS f

readKeys :: FilePath -> IO (Maybe (SecretKey, PublicKey))
readKeys f = do
  s <- readEdSecret f
  p <- readEdPublic (f <> ".pub")
  return $ sequencePair (s, p)

secretKeyParser :: Parser SecretKey
secretKeyParser = do
  _ <- string "-----BEGIN OPENSSH PRIVATE KEY-----"
  enc <- manyTill anyChar (char '-')
  let dec = BS.take 32 $ BS.drop 161 $ B64.decodeLenient $ BC8.pack enc
  either fail return $ first show $ cfToEither $ secretKey dec

publicKeyParser :: Parser PublicKey
publicKeyParser = do
  _ <- string "ssh-ed25519 "
  enc <- manyTill anyChar space
  dec <- either fail (return . BS.drop 19) (B64.decode $ BC8.pack enc)
  either fail return $ first show $ cfToEither $ publicKey dec
