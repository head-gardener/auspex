{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Foundation where

import Network.HTTP.Simple
import Network.HTTP.Types
import Web.JWT qualified as JWT
import Web.JWT.Encode (decodeVerifier)
import Yesod.Auth
import Yesod.Auth.Message qualified as Msg
import Yesod.Core
import Yesod.Form (FormMessage, defaultFormMessage)

data App = App

mkYesodData "App" $(parseRoutesFile "example-app/routes.yesodroutes")

instance Yesod App where
  authRoute _ = Just $ AuthR LoginR

  isAuthorized HomeR _ = isLoggedIn
  isAuthorized _ _ = return Authorized

isLoggedIn :: HandlerFor App AuthResult
isLoggedIn = do
  mu <- maybeAuthId
  return $ case mu of
    Nothing -> AuthenticationRequired
    Just _ -> Authorized

from200 :: (Show a, MonadFail m) => Response a -> m a
from200 response = case getResponseStatusCode response of
  200 -> return $ getResponseBody response
  code ->
    fail $
      "Unexpected code. Code: "
        <> show code
        <> ". Reason: "
        <> show (getResponseBody response)

-- | Site that can store and retrieve (Text, VerifySigner) pairs,
-- i.e. provider addressess paired with their credentials.
-- Default implementation stores nothing, forcing authenticator to
-- request provider credentials every time.
class (YesodAuth site) => YesodAuthAuspex site where
  storeVerifier :: Text -> JWT.VerifySigner -> AuthHandler site ()
  getVerifier :: Text -> AuthHandler site (Maybe JWT.VerifySigner)

  storeVerifier _ _ = pass
  getVerifier _ = return Nothing

acquireVerifier :: (YesodAuthAuspex site) => Text -> AuthHandler site (Maybe JWT.VerifySigner)
acquireVerifier p =
  runMaybeT $
    MaybeT (getVerifier p) <|> do
      req <- hoistMaybe $ parseRequest $ toString $ p <> "/key"
      let action = MaybeT (from200 <$> httpLBS req)
      resp <- action <|> action <|> action
      v <- hoistMaybe $ decodeVerifier resp
      lift $ storeVerifier p v
      return v

authAuspex :: (RenderRoute m, YesodAuth m, YesodAuthAuspex m) => Text -> AuthPlugin m
authAuspex provider = AuthPlugin "auspex" dispatch loginWidget
  where
    dispatch :: (YesodAuthAuspex m) => Text -> [Text] -> AuthHandler m TypedContent
    dispatch "GET" ["login"] = verifyJWT
    dispatch _ _ = notFound

    loginR = PluginR "auspex" ["login"]

    loginWidget toMaster = do
      render <- getUrlRender
      let rdr :: Text = decodeUtf8 $ urlEncode True $ encodeUtf8 $ render $ toMaster loginR
      [whamlet|
        $newline never
          <a href="#{provider}/?callback=#{rdr}">
            <button .btn .btn-success> _{Msg.LoginTitle}
        |]

    verifyJWT :: (YesodAuthAuspex m) => AuthHandler m TypedContent
    verifyJWT = do
      v <- acquireVerifier provider
      key <-
        maybe
          (sendResponseStatus status500 ("Couldn't acquire provider's credentials" :: Text))
          return
          v
      token <-
        maybe (sendResponseStatus status400 ("No token" :: Text)) return
          =<< lookupGetParam "token"
      case JWT.sub . JWT.claims =<< JWT.verify key =<< JWT.decode token of
        Just subj -> setCredsRedirect $ Creds "auspex" (JWT.stringOrURIToText subj) []
        Nothing -> sendResponseStatus status401 ("Invalid token" :: Text)

postLoginR :: (YesodAuth site) => AuthHandler site TypedContent
postLoginR = return $ toTypedContent ("hey" :: String)

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
  type AuthId App = Text
  authenticate = return . Authenticated . credsIdent
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins _ = [authAuspex "http://localhost:8080"]
  maybeAuthId = lookupSession "_ID"

instance YesodAuthAuspex App where
