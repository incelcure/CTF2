{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Foundation where

import Yesod.Core

data App = App

mkYesodData
  "App"
  [parseRoutes|
/              HomeR GET
|]

instance Yesod App
