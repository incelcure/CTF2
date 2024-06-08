{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Casino.User where

import Data.Aeson
import Database.Persist.Postgresql
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
CasinoUser
  name Text
  points Int
  spent Int
  bonus Int
  rewards [RewardId]
  UniqueUserName name
  deriving Show
Reward
  type Text
  value Text
  deriving Show Eq Generic ToJSON
|]

mkUser :: Text -> Int -> CasinoUser
mkUser n p = CasinoUser n p 0 0 []

spinsLeft :: CasinoUser -> Int
spinsLeft CasinoUser{..} = casinoUserPoints + casinoUserBonus - casinoUserSpent
