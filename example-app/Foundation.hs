{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Foundation where

import GHC.IO (unsafePerformIO)
import Yesod.Auth
import Yesod.Auth.Auspex
import Yesod.Core
import Yesod.Form (FormMessage, defaultFormMessage)

data App = App

mkYesodData
  "App"
  [parseRoutes|
/              HomeR GET
/auth          AuthR Auth getAuth
|]

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

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

{-# NOINLINE provider #-}
provider :: Text
provider =
  toText $
    fromMaybe "http://localhost:8080" $
      viaNonEmpty head $
        unsafePerformIO getArgs

instance YesodAuth App where
  type AuthId App = Text
  authenticate = return . Authenticated . credsIdent
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins _ = [authAuspex provider]
  maybeAuthId = lookupSession "_ID"

instance YesodAuthAuspex App
