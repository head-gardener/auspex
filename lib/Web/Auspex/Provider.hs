{-# LANGUAGE TemplateHaskell #-}

module Web.Auspex.Provider
  ( auspexServer,
    User (..),
    AuspexConfig (..),
    AppState,
    AppState' (..),
  )
where

import Control.Lens
import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson
import Data.ByteString.Base64 qualified as B64
import Data.Challenge
import Data.List (lookup)
import Data.Time.Clock.POSIX
import Network.HTTP.Types
import Network.Wai
import Web.JWT qualified as JWT
import Web.JWT.Encode

newtype User = User {_ownerPublic :: Ed.PublicKey}
  deriving stock (Show)

type App = AuspexConfig -> AppState -> Application

type AppState = TVar AppState'

data AuspexConfig = AppConfig
  { edSecretKey :: Ed.SecretKey,
    edPublicKey :: Ed.PublicKey,
    rsaEncoder :: JWT.EncodeSigner,
    rsaVerifier :: JWT.VerifySigner
  }

newtype AppState' = AppState'
  { _users :: Map ByteString User
  }

$(makeLenses ''AppState')
$(makeLenses ''User)

auspexServer :: App
auspexServer cfg st request respond = do
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
            { JWT.sub = JWT.stringOrURI $ decodeUtf8 providedName,
              JWT.exp = JWT.numericDate expirationTime
            }
    let tok = JWT.encodeSigned (rsaEncoder cfg) mempty claims
    let cb = callback chl <> "/?token=" <> encodeUtf8 tok
    if verify (edPublicKey cfg) clientKey chl
      then return (responseLBS status303 [("Location", cb)] "")
      else return (responseLBS status400 [] "BAD")
  respond resp