module Main (main) where

import Crypto.PubKey.Ed25519.OpenSSH (readKeys)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.HealthCheckEndpoint (healthCheck)
import Network.Wai.Middleware.RequestLogger
import Web.Auspex.Provider
import Web.JWT qualified as JWT

main :: IO ()
main = do
  let edPath = "./data/ed25519"
      rsaPPath = "./data/rsa.pub"
      rsaSPath = "./data/rsa"
  rsaS <-
    ( maybe
        (die $ "Can't parse " <> rsaPPath)
        (return . JWT.EncodeRSAPrivateKey)
        . JWT.readRsaSecret
      )
      =<< readFileBS rsaSPath
  rsaV <-
    ( maybe
        (die $ "Can't parse " <> rsaPPath)
        (return . JWT.VerifyRSAPublicKey)
        . JWT.readRsaPublicKey
      )
      =<< readFileBS rsaPPath
  (sec, pub) <- readKeys edPath >>= maybe (die $ "Can't parse" <> edPath) return
  st <- newTVarIO $ AppState' mempty
  let cfg = AppConfig sec pub rsaS rsaV
  run 8080 $ logStdout $ healthCheck $ auspexServer cfg st
