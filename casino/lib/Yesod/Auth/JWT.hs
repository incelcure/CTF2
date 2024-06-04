module Yesod.Auth.JWT where

import Web.JWT qualified as JWT
import Yesod.Auth
import Yesod.Core
import Prelude hiding (get)

class YesodAuthJWT app where
  doesUserNameExist :: Text -> AuthHandler app Bool
  getJWTSecret :: AuthHandler app Text

authJWT :: (YesodAuthJWT m) => AuthPlugin m
authJWT =
  AuthPlugin "jwt" dispatch loginWidget
  where
    dispatch :: (YesodAuthJWT m) => Text -> [Text] -> AuthHandler m TypedContent
    dispatch "GET" [] = postLoginR >>= sendResponse
    dispatch _ _ = notFound

    loginWidget _ = pass

    postLoginR :: (YesodAuthJWT site) => AuthHandler site TypedContent
    postLoginR = do
      v <- JWT.toVerify . JWT.hmacSecret <$> getJWTSecret
      t <- lookupGetParam "token"
      t' <-
        runMaybeT $
          hoistMaybe $
            JWT.sub . JWT.claims
              =<< JWT.decodeAndVerifySignature v
              =<< t
      n <- maybe (permissionDenied "") (return . show) t'
      setCredsRedirect $ Creds "jwt" n []
