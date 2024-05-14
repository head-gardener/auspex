module Web.Auspex.ProviderSpec (main, spec) where

-- import Test.QuickCheck

import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.PubKey.Ed25519.OpenSSH
import Data.Aeson
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as BL
import Data.Challenge
import Data.IP qualified as IP
import Data.Maybe (fromJust)
import GHC.IO (unsafePerformIO)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test hiding (request)
import Network.Wai.Test qualified as T
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Internal
import Web.Auspex.Provider
import Web.JWT hiding (decode)
import Web.JWT qualified as JWT
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
    describe "/" $ do
      it "can authorize a known user" $ do
        ch <- getChallenge "/"
        tok <- request methodPost "/" [(hAuthorization, "user")] (encode ch)
        liftIO $ simpleStatus tok `shouldBe` status200
        liftIO $
          (isJust . JWT.decodeAndVerifySignature rsaPub . decodeUtf8 . simpleBody) tok
            `shouldBe` True

      it "can redirect after authorization" $ do
        ch <- getChallenge "/?callback=callback"
        request methodPost "/" [(hAuthorization, "user")] (encode ch)
          `shouldRespondWith` "" {matchStatus = 303}

      it "can register and authorize a new user" $ do
        post "/register" "qew"
          `shouldRespondWith` "Can't parse response" {matchStatus = 400}
        post "/register" (encode ("new-user" :: Text, BA.convert edPub :: ByteString))
          `shouldRespondWith` "" {matchStatus = 200}
        ch <- getChallenge "/?callback=callback"
        request methodPost "/" [(hAuthorization, "new-user")] (encode ch)
          `shouldRespondWith` "" {matchStatus = 303}

      it "can detect challenge tampering" $ do
        ch <- getChallenge "/?callback=callback"
        request methodPost "/" [(hAuthorization, "user")] (encode ch {issued = 0})
          `shouldRespondWith` "BAD" {matchStatus = 400}

    describe "/key" $ do
      it "serves key on" $ do
        get "/key" `shouldRespondWith` withJSON rsaPub

    describe "/status" $ do
      it "should only be accessible from private addresses" $ do
        let path = ["status", "users"]
        getFrom "127.0.0.1" [("X-Forwarded-For", "1.1.1.1")] path
          `shouldRespondWith` "Forbidden" {matchStatus = 403}
        r <- getFrom "127.0.0.1" [("X-Forwarded-For", "127.0.0.1")] path
        liftIO $ simpleStatus r `shouldBe` status200
  where
    withJSON =
      fromString
        . decodeUtf8 @String @ByteString
        . toStrict
        . fromJust
        . encodeVerifier

    getChallenge :: ByteString -> WaiSession () Challenge
    getChallenge t = solve edSec edPub . fromJust . decode . simpleBody <$> get t

    request' :: Request -> BL.ByteString -> WaiSession st SResponse
    request' r = WaiSession . lift . T.srequest . SRequest r

    getFrom r hs ps =
      request'
        ( defaultRequest
            { pathInfo = ps
            , remoteHost = IP.toSockAddr (r, 80)
            , requestHeaders = hs
            }
        )
        ""

-- it "is idempotent" $
--   property $
--     \str -> str === (reverse str :: String)
