{-# LANGUAGE DeriveAnyClass #-}

module Data.Casino.SetRequest where

import Data.Aeson

data SetRequest = SetRequest
  { secret :: Text
  , subj :: Text
  , value :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
