{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Casino.User where

import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Aeson

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
CasinoUser
  name Text
  points Int
  spent Int
  rewards [RewardId]
  UniqueUserName name
  deriving Show
Reward
  type Text
  value Text
  deriving Show Eq Generic ToJSON
|]
