module Main where

import Crypto.PubKey.Ed25519.OpenSSH (readKeys)
import Data.Aeson
import Data.Challenge
import Network.HTTP.Simple
import Network.HTTP.Types (hAuthorization)

main :: IO ()
main = do
  (secret, public) <-
    readKeys "./data/ed25519" >>= maybe (fail "Can't parse") return

  -- receive challenge
  challenge <-
    httpLBS
      "http://localhost:8080/?callback=http%3A%2F%2Flocalhost%3A3000%2Fauth%2Fpage%2Fauspex%2Flogin"
      >>= from200

  -- solve challenge
  chl <- maybe (die "Can't parse challenge") return $ decode challenge
  let resp = solve secret public chl
  print $ toJSON resp

  let params = setRequestHeader hAuthorization ["user"] . setRequestBodyLBS (encode resp)
  response <- httpLBS $ params "POST http://localhost:8080"

  putStrLn $
    "The status code was: "
      ++ show (getResponseStatusCode response)
  print $ getResponseBody response

from200 :: (Show a) => Response a -> IO a
from200 response = case getResponseStatusCode response of
  200 -> return $ getResponseBody response
  code ->
    die $
      "Unexpected code. Code: "
        <> show code
        <> ". Reason: "
        <> show (getResponseBody response)
