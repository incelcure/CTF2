module API where

import Data.Casino.User
import Data.Casino.SetRequest qualified as SR
import Database.Persist
import Foundation
import Yesod.Auth (maybeAuth)
import Yesod.Core
import Yesod.Persist.Core

postSpinR :: Handler Html
postSpinR = do
  Entity k u <- maybe (permissionDenied "") return =<< maybeAuth
  when (casinoUserSpent u >= casinoUserPoints u) $ permissionDenied ""
  runDB $ update k [CasinoUserSpent +=. 1]
  defaultLayout $ do
    [whamlet|
        <p> you spun the wheel!
      |]

postSetR :: Handler ()
postSetR = do
  req <- requireCheckJsonBody
  s' <- getsYesod secret
  when (s' /= SR.secret req) $ permissionDenied ""
  runDB $ do
    k' <- getBy (UniqueUserName (SR.subj req))
    case k' of
      Just (Entity k _) -> update k [CasinoUserPoints =. SR.value req]
      Nothing -> insert_ $ CasinoUser (SR.subj req) (SR.value req) 0

getSpinsR :: Handler Value
getSpinsR = do
  u <- maybe (permissionDenied "") (return . entityVal) =<< maybeAuth
  let c = max 0 (casinoUserPoints u - casinoUserSpent u)
  return $ object ["count" .= c]
