{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Lens
import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.PubKey.Ed25519.OpenSSH (readKeys)
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson (decode, encode)
import Data.ByteString.Base64.Lazy qualified as B64
import Data.Challenge
import Data.List (lookup)
import Data.Time.Clock.POSIX
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.HealthCheckEndpoint (healthCheck)
import Network.Wai.Middleware.RequestLogger
import Web.JWT qualified as JWT

newtype User = User {_ownerPublic :: Ed.PublicKey}
  deriving stock (Show)

type App = AppConfig -> AppState -> Application

type AppState = TVar AppState'

data AppConfig = AppConfig
  { edSecretKey :: Ed.SecretKey
  , edPublicKey :: Ed.PublicKey
  , hmacSecretKey :: JWT.EncodeSigner
  }

newtype AppState' = AppState'
  { _users :: Map ByteString User
  }

$(makeLenses ''AppState')
$(makeLenses ''User)

app :: App
app cfg st request respond = do
  case requestMethod request of
    "GET" -> handleGet cfg st request respond
    "POST" -> handlePost cfg st request respond
    _ -> respond $ responseLBS status400 [] "Unsupported method"

handleGet :: App
handleGet cfg _ request respond = do
  resp <- fmap (either id id) $ runExceptT $ do
    cb <-
      hoistMaybe' (responseLBS status400 [] "Missing callback parameter")
        . join
        . lookup "callback"
        $ queryString request
    -- we don't need a CSPRNG here, right?
    tok <- toStrict . B64.encode . toLazy <$> liftIO (getRandomBytes 63)
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
    let tok = JWT.encodeSigned (hmacSecretKey cfg) mempty claims
    let cb = callback chl <> "/?token=" <> encodeUtf8 tok
    if verify (edPublicKey cfg) clientKey chl
      then return (responseLBS status303 [("Location", cb)] "")
      else return (responseLBS status400 [] "BAD")
  respond resp

main :: IO ()
main = do
  let hmac = JWT.hmacSecret "erer"
  (sec, pub) <- readKeys "./data/ed25519" >>= maybe (fail "Can't parse") return
  st <- newTVarIO $ AppState' (fromList [("user", User pub)])
  let cfg = AppConfig sec pub hmac
  run 8080 $ logStdout $ healthCheck $ app cfg st
