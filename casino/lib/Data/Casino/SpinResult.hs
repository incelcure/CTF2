{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Casino.SpinResult where

import Data.Aeson
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Text.Printf (printf)
import Data.Casino.User

newtype SpinResult = SpinResult (Maybe Reward)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

mkSpinResult :: (SpinResult -> Bool) -> IO SpinResult
mkSpinResult = generate . suchThat arbitrary

instance Arbitrary SpinResult where
  arbitrary =
    frequency
      [ (1, return $ SpinResult Nothing)
      , (2, SpinResult . Just <$> arbitrary)
      ]

instance Arbitrary Reward where
  arbitrary = do
    frequency [(1, mkColor)]
    where
      mkColor = do
        r <- replicateM 3 (printf "%02X" <$> chooseInt (0, 255))
        return $ Reward "Color" $ "#" <> fromString (concat r)
