{-# LANGUAGE TemplateHaskell #-}

module Web.Auspex.Provider (
  auspexServer,
  User (..),
  AuspexConfig (..),
  AppState,
  AppState' (..),
)
where

import ByteString.Aeson.Orphans ()
import Control.Lens
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
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
  { edSecretKey :: Ed.SecretKey
  , edPublicKey :: Ed.PublicKey
  , rsaEncoder :: JWT.EncodeSigner
  , rsaVerifier :: JWT.VerifySigner
  }

newtype AppState' = AppState'
  { _users :: Map ByteString User
  }
  deriving stock (Show)

$(makeLenses ''AppState')
$(makeLenses ''User)

auspexServer :: App
auspexServer cfg st request respond = do
  case pathInfo request of
    [] -> case requestMethod request of
      "GET" -> handleGet cfg st request respond
      "POST" -> handlePost cfg st request respond
      _ -> respond405 respond
    ["register"] -> case requestMethod request of
      "GET" -> respond $ responseLBS status200 [] "Please Register :)"
      "POST" -> handleRegistration cfg st request respond
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

handleRegistration :: App
handleRegistration _ st request respond = do
  bod <- lazyRequestBody request
  let creds = do
        (n :: Text, key :: ByteString) <- decode bod
        k <- case Ed.publicKey key of
          CryptoPassed a -> Just a
          CryptoFailed _ -> Nothing
        return (encodeUtf8 n, k)
  case creds of
    Nothing -> respond $ responseLBS status400 [] "Can't parse response"
    Just (n, k) -> do
      exists <- isJust . view (users . at n) <$> readTVarIO st
      if exists
        then respond $ responseLBS status400 [] "Name taken"
        else do
          atomically $ modifyTVar' st $ set (users . at n) (Just $ User k)
          respond $ responseLBS status200 [] ""

handleGet :: App
handleGet cfg _ request respond = do
  resp <- fmap (either id id) $ runExceptT $ do
    let cb =
          join
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
      hoistMaybe' (responseLBS status400 [] "User not found")
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
    let cb = (<> "/?token=" <> encodeUtf8 tok) <$> callback chl
    case (verify (edPublicKey cfg) clientKey chl, cb) of
      (False, _) -> return (responseLBS status400 [] "BAD")
      (True, Just cb') -> return (responseLBS status303 [(hLocation, cb')] "")
      (True, Nothing) -> return (responseLBS status200 [] $ encodeUtf8 tok)
  respond resp
