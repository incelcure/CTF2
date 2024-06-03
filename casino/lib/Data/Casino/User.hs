{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Casino.User where

import Database.Persist.Postgresql
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
CasinoUser
    name Text
    points Int
    spent Int
    UniqueUserName name
    deriving Show
|]
