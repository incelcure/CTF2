{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Foundation where

import Data.Casino.User
import Database.Persist.Postgresql
import Yesod.Auth
import Yesod.Auth.JWT
import Yesod.Auth.Message (AuthMessage (InvalidKeyTitle))
import Yesod.Core
import Yesod.Form (FormMessage, defaultFormMessage)
import Yesod.Persist
import Prelude hiding (get)

data App = App {pool :: ConnectionPool, secret :: Text}

mkYesodData
  "App"
  [parseRoutes|
/          HomeR GET
/api/spins SpinsR GET
/api/spin  SpinR POST
/api/set   SetR POST
/api/rewards RewardsR GET
/auth      AuthR Auth getAuth
|]

instance Yesod App where
  isAuthorized HomeR _ = isLoggedIn
  isAuthorized SpinR _ = isLoggedIn
  isAuthorized SpinsR _ = isLoggedIn
  isAuthorized _ _ = return Authorized

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    p <- getsYesod pool
    runSqlPool action p

isLoggedIn :: HandlerFor App AuthResult
isLoggedIn = do
  mu <- maybeAuthId
  return $ case mu of
    Nothing -> AuthenticationRequired
    Just _ -> Authorized

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
  type AuthId App = CasinoUserId
  authenticate creds = liftHandler $ runDB $ do
    x <- getBy $ UniqueUserName $ credsIdent creds
    return $ case x of
      Just (Entity userid _) -> Authenticated userid
      Nothing -> UserError InvalidKeyTitle

  loginDest _ = HomeR
  logoutDest _ = HomeR

  authPlugins _ = [authJWT]

instance YesodAuthPersist App

instance YesodAuthJWT App where
  doesUserNameExist n = liftHandler $ do
    a <- runDB $ getBy $ UniqueUserName n
    return $ isJust a

  getJWTSecret = getsYesod secret
