module Main (main) where

import Crypto.PubKey.Ed25519.OpenSSH
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.HealthCheckEndpoint (healthCheck)
import Network.Wai.Middleware.RealIp (realIp)
import Network.Wai.Middleware.RequestLogger
import Web.Auspex.Provider
import Web.JWT qualified as JWT

main :: IO ()
main = do
  edPPath <- fromMaybe "./data/ed25519.pub" <$> lookupEnv "ED_PUBLIC"
  edSPath <- fromMaybe "./data/ed25519" <$> lookupEnv "ED_SECRET"
  rsaPPath <- fromMaybe "./data/rsa.pub" <$> lookupEnv "RSA_PUBLIC"
  rsaSPath <- fromMaybe "./data/rsa" <$> lookupEnv "RSA_SECRET"
  port <- fromMaybe 8080 . (>>= readMaybe) <$> lookupEnv "PORT"
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
  sec <- readEdSecret edSPath >>= maybe (die $ "Can't parse" <> edSPath) return
  pub <- readEdPublic edPPath >>= maybe (die $ "Can't parse" <> edPPath) return
  st <- newTVarIO $ AppState' mempty
  let cfg = AppConfig sec pub rsaS rsaV
  putStrLn $ "Serving on " <> show port
  run port $ realIp $ logStdout $ healthCheck $ auspexServer cfg st
