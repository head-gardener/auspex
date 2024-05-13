module Web.Auspex.ProviderSpec (main, spec) where

-- import Test.QuickCheck

import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.PubKey.Ed25519.OpenSSH
import Data.Aeson
import Data.Challenge
import Data.Maybe (fromJust)
import GHC.IO (unsafePerformIO)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test hiding (request)
import Test.Hspec
import Test.Hspec.Wai
import Web.Auspex.Provider
import Web.JWT hiding (decode)
import Web.JWT.Encode (encodeVerifier)
import Prelude hiding (get)

main :: IO ()
main = hspec spec

{-# NOINLINE edPub #-}
edPub :: Ed.PublicKey
edPub = unsafePerformIO (fromJust <$> readEdPublic "./data/ed25519.pub")

{-# NOINLINE edSec #-}
edSec :: Ed.SecretKey
edSec = unsafePerformIO (fromJust <$> readEdSecret "./data/ed25519")

{-# NOINLINE rsaPub #-}
rsaPub :: VerifySigner
rsaPub =
  unsafePerformIO
    ( VerifyRSAPublicKey . fromJust . readRsaPublicKey <$> readFileBS "./data/rsa.pub"
    )

{-# NOINLINE rsaSec #-}
rsaSec :: EncodeSigner
rsaSec =
  unsafePerformIO
    (EncodeRSAPrivateKey . fromJust . readRsaSecret <$> readFileBS "./data/rsa")

newApp :: IO Application
newApp = do
  st <- newTVarIO $ AppState' (fromList [("user", User edPub)])
  let cfg = AppConfig edSec edPub rsaSec rsaPub
  return $ auspexServer cfg st

spec :: Spec
spec = with newApp $ do
  describe "Provider" $ do
    it "serves key on /key" $ do
      get "/key" `shouldRespondWith` withJSON rsaPub

    it "can authorize a known user" $ do
      ch <-
        solve edSec edPub . fromJust . decode . simpleBody
          <$> get "/?callback=callback"
      request methodPost "/" [(hAuthorization, "user")] (encode ch)
        `shouldRespondWith` "" {matchStatus = 303}

    it "can detect challenge tampering" $ do
      ch <-
        solve edSec edPub . fromJust . decode . simpleBody
          <$> get "/?callback=callback"
      request methodPost "/" [(hAuthorization, "user")] (encode ch {issued = 0})
        `shouldRespondWith` "BAD" {matchStatus = 400}
  where
    withJSON =
      fromString
        . decodeUtf8 @String @ByteString
        . toStrict
        . fromJust
        . encodeVerifier

-- it "is idempotent" $
--   property $
--     \str -> str === (reverse str :: String)
