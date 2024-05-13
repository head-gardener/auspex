{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.PubKey.Ed25519.OpenSSH (readKeys)
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson
import Data.ByteString.Base64 qualified as B64
import Data.Challenge
import Data.List (lookup)
import Data.Time.Clock.POSIX
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.HealthCheckEndpoint (healthCheck)
import Network.Wai.Middleware.RequestLogger
import Web.JWT qualified as JWT
import Web.JWT.Encode

newtype User = User {_ownerPublic :: Ed.PublicKey}
  deriving stock (Show)

type App = AppConfig -> AppState -> Application

type AppState = TVar AppState'

data AppConfig = AppConfig
  { edSecretKey :: Ed.SecretKey
  , edPublicKey :: Ed.PublicKey
  , rsaEncoder :: JWT.EncodeSigner
  , rsaVerifier :: JWT.VerifySigner
  }

newtype AppState' = AppState'
  { _users :: Map ByteString User
  }

$(makeLenses ''AppState')
$(makeLenses ''User)

app :: App
app cfg st request respond = do
  case pathInfo request of
    [] -> case requestMethod request of
      "GET" -> handleGet cfg st request respond
      "POST" -> handlePost cfg st request respond
      _ -> respond405 respond
    ["key"] -> case requestMethod request of
      "GET" -> case encodeVerifier $ rsaVerifier cfg of
        Just s -> respond $ responseLBS status200 [] s
        Nothing -> respond500 respond
      _ -> respond405 respond
    _ -> respond404 respond

respond404, respond405, respond500 :: forall {b}. (Response -> b) -> b
respond404 respond = respond $ responseLBS status404 [] "Not found"
respond405 respond = respond $ responseLBS status405 [] "Unsupported method"
respond500 respond = respond $ responseLBS status500 [] "Internal Error"

handleGet :: App
handleGet cfg _ request respond = do
  resp <- fmap (either id id) $ runExceptT $ do
    cb <-
      hoistMaybe' (responseLBS status400 [] "Missing callback parameter")
        . join
        . lookup "callback"
        $ queryString request
    -- we don't need a CSPRNG here, right?
    tok <- B64.encode <$> liftIO (getRandomBytes 63)
    challenge <- liftIO $ newChallenge (edSecretKey cfg) (edPublicKey cfg) tok cb
    return $ responseLBS status200 [] $ encode challenge
  respond resp

hoistMaybe' :: (Applicative m) => e -> Maybe a -> ExceptT e m a
hoistMaybe' e m = hoistEither $ maybeToRight e m

handlePost :: App
handlePost cfg st request respond = do
  resp <- fmap (either id id) $ runExceptT $ do
    providedName <-
      hoistMaybe' (responseLBS status400 [] "Missing Authorization header")
        . lookup hAuthorization
        $ requestHeaders request
    bod <- liftIO $ lazyRequestBody request
    chl <-
      hoistMaybe' (responseLBS status400 [] "Can't parse response") $ decode bod
    clientKey <-
      hoistMaybe' (responseLBS status400 [] "You are gay")
        . fmap (view ownerPublic)
        . view (users . at providedName)
        =<< liftIO (readTVarIO st)
    currentTime <- liftIO getPOSIXTime
    let expirationTime = currentTime + 3600
    let claims =
          mempty
            { JWT.sub = JWT.stringOrURI $ decodeUtf8 providedName
            , JWT.exp = JWT.numericDate expirationTime
            }
    let tok = JWT.encodeSigned (rsaEncoder cfg) mempty claims
    let cb = callback chl <> "/?token=" <> encodeUtf8 tok
    if verify (edPublicKey cfg) clientKey chl
      then return (responseLBS status303 [("Location", cb)] "")
      else return (responseLBS status400 [] "BAD")
  respond resp

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
  st <- newTVarIO $ AppState' (fromList [("user", User pub)])
  let cfg = AppConfig sec pub rsaS rsaV
  run 8080 $ logStdout $ healthCheck $ app cfg st
