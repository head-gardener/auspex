{-# LANGUAGE RecordWildCards #-}

module Main where

import Crypto.PubKey.Ed25519.OpenSSH (readKeys)
import Data.Aeson
import Data.ByteArray qualified as BA
import Data.Challenge
import Network.HTTP.Simple
import Network.HTTP.Types
import Options.Applicative

data AppArgs = AppArgs
  { username :: Text
  , provider :: String
  , keyPath :: String
  , register :: Bool
  }
  deriving stock (Show)

parser :: Parser AppArgs
parser =
  AppArgs
    <$> argument str (metavar "username")
    <*> argument str (metavar "provider")
    <*> argument str (metavar "key-path")
    <*> switch (short 'r' <> long "register")

main :: IO ()
main = do
  AppArgs {..} <- execParser $ info (parser <**> helper) (header "Auspex client")

  (secret, public) <-
    readKeys keyPath >>= maybe (fail "Can't parse") return

  -- register
  when register $
    httpLBS
      ( setRequestBodyJSON
          (username, BA.convert public :: ByteString)
          . setRequestMethod "POST"
          $ fromString (provider <> "/register")
      )
      >>= void . from200

  -- receive challenge
  challenge <- httpLBS (fromString provider) >>= from200

  -- solve challenge
  chl <- maybe (die "Can't parse challenge") return $ decode challenge
  let resp = solve secret public chl

  let params =
        setRequestHeader hAuthorization [encodeUtf8 username]
          . setRequestBodyLBS (encode resp)
          . setRequestMethod "POST"
  response <- httpLBS (params $ fromString provider) >>= from200
  putLBS response

from200 :: (Show a) => Response a -> IO a
from200 response = case getResponseStatusCode response of
  200 -> return $ getResponseBody response
  code ->
    die $
      "Unexpected code. Code: "
        <> show code
        <> ". Reason: "
        <> show (getResponseBody response)
