{-# LANGUAGE NamedFieldPuns #-}
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
  when (spinsLeft u <= 0) $ lift (permissionDenied "Out of spins")
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
  runDB $
    void $
      upsertBy
        (UniqueUserName (SR.subj req))
        (mkUser (SR.subj req) (SR.value req))
        [CasinoUserPoints =. SR.value req]

getSpinsR :: Handler Value
getSpinsR = do
  u <- maybe (permissionDenied "") (return . entityVal) =<< maybeAuth
  let c = max 0 (spinsLeft u)
  return $ object ["count" .= c]

postBonusR :: Handler Value
postBonusR = do
  v <- JWT.toVerify . JWT.hmacSecret <$> getsYesod secret
  res <- runMaybeT $ do
    t <- hoistMaybe =<< lift (lookupGetParam "token")
    n <- hoistMaybe $ JWT.sub . JWT.claims =<< JWT.decodeAndVerifySignature v t
    t' <- hoistMaybe =<< lift (runDB $ getBy $ UniqueUserName $ show n)
    bonus <- hoistMaybe . (readMaybe . toString =<<) =<< lift (lookupGetParam "bonus")
    return (t', bonus)
  (u, bonus) <- maybe (permissionDenied "") return res
  CasinoUser {casinoUserBonus} <- runDB $ updateGet (entityKey u) [CasinoUserBonus +=. bonus]
  return $ object ["total" .= casinoUserBonus]

getRewardsR :: Handler Value
getRewardsR = do
  v <- JWT.toVerify . JWT.hmacSecret <$> getsYesod secret
  t' <- runMaybeT $ do
    t <- hoistMaybe =<< lift (lookupGetParam "token")
    n <- hoistMaybe $ JWT.sub . JWT.claims =<< JWT.decodeAndVerifySignature v t
    hoistMaybe =<< lift (runDB $ getBy $ UniqueUserName $ show n)
  rewardIds <-
    maybe (permissionDenied "") (return . casinoUserRewards . entityVal) t'
  rs <- runDB $ selectList [RewardId <-. rewardIds] []
  returnJson $ fmap entityVal rs
