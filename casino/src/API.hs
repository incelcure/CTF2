module API where

import Data.Casino.SetRequest qualified as SR
import Data.Casino.SpinResult
import Data.Casino.User
import Database.Persist
import Foundation
import Web.JWT qualified as JWT
import Yesod.Auth
import Yesod.Core
import Yesod.Persist.Core
import Prelude hiding (get)

maybeM :: (Monad m) => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j x = maybe n j =<< x

postSpinR :: Handler Value
postSpinR = runDB $ do
  k <- maybe (lift $ permissionDenied "") return =<< maybeAuthId
  u <- maybe (lift $ permissionDenied "") return =<< get k
  when (casinoUserSpent u >= casinoUserPoints u) $ lift (permissionDenied "")
  SpinResult rew <- liftIO (mkSpinResult (const True))
  update k [CasinoUserSpent +=. 1]
  case rew of
    Nothing -> pass
    Just r -> do
      rk <-
        maybeM
          (insert r)
          (return . entityKey)
          $ selectFirst [RewardType ==. rewardType r, RewardValue ==. rewardValue r] []
      update k [CasinoUserRewards =. (rk : casinoUserRewards u)]
  returnJson rew

postSetR :: Handler ()
postSetR = do
  req <- requireCheckJsonBody
  s' <- getsYesod secret
  when (s' /= SR.secret req) $ permissionDenied ""
  runDB $ do
    k' <- getBy (UniqueUserName (SR.subj req))
    case k' of
      Just (Entity k _) -> update k [CasinoUserPoints =. SR.value req]
      Nothing -> insert_ $ CasinoUser (SR.subj req) (SR.value req) 0 []

getSpinsR :: Handler Value
getSpinsR = do
  u <- maybe (permissionDenied "") (return . entityVal) =<< maybeAuth
  let c = max 0 (casinoUserPoints u - casinoUserSpent u)
  return $ object ["count" .= c]

getRewardsR :: Handler Value
getRewardsR = do
  v <- JWT.toVerify . JWT.hmacSecret <$> getsYesod secret
  t' <- runMaybeT $ do
    t <- hoistMaybe =<< lift (lookupGetParam "token")
    n <- hoistMaybe $ JWT.sub . JWT.claims =<< JWT.decodeAndVerifySignature v t
    print n
    hoistMaybe =<< lift (runDB $ getBy $ UniqueUserName $ show n)
  rewardIds <-
    maybe (permissionDenied "") (return . casinoUserRewards . entityVal) t'
  rs <- runDB $ selectList [RewardId <-. rewardIds] []
  returnJson $ fmap entityVal rs
