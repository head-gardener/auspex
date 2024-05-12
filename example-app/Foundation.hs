{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Foundation where

import Network.HTTP.Types
import Web.JWT qualified as JWT
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

authAuspex :: (RenderRoute m, YesodAuth m) => Text -> AuthPlugin m
authAuspex provider = AuthPlugin "auspex" dispatch loginWidget
  where
    dispatch :: Text -> [Text] -> AuthHandler m TypedContent
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

    verifyJWT :: AuthHandler m TypedContent
    verifyJWT = do
      token <-
        maybe (sendResponseStatus status400 ("No token" :: Text)) return
          =<< lookupGetParam "token"
      let key = JWT.toVerify $ JWT.hmacSecret "erer"
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
