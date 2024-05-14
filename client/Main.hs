module Main where

import Crypto.PubKey.Ed25519.OpenSSH (readKeys)
import Data.Aeson
import Data.ByteArray qualified as BA
import Data.Challenge
import Network.HTTP.Simple
import Network.HTTP.Types

main :: IO ()
main = do
  (secret, public) <-
    readKeys "./data/ed25519" >>= maybe (fail "Can't parse") return

  -- register
  _ <-
    httpLBS
      ( setRequestBodyJSON
          ("user" :: Text, BA.convert public :: ByteString)
          "POST http://localhost:8080/register"
      )
      >>= from200

  -- receive challenge
  challenge <- httpLBS "http://localhost:8080/" >>= from200

  -- solve challenge
  chl <- maybe (die "Can't parse challenge") return $ decode challenge
  let resp = solve secret public chl

  let params = setRequestHeader hAuthorization ["user"] . setRequestBodyLBS (encode resp)
  response <- httpLBS (params "POST http://localhost:8080") >>= from200
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
