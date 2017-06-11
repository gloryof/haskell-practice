
{-|
Module       : Domain.Age
Description  : Age domain modle.

This module representing the age.
-}
module Domain.Age (
  Age,
  validate
) where

import           Data.Validation

-- | This type represents age.
type Age = Int

-- | Message of range error.
messageRange :: String
messageRange = "年齢は" ++ (show minAge) ++ "-" ++ (show maxAge) ++ "の間で入力してください。"


-- | The lowest value that user can enter for age.
minAge :: Int
minAge = 0

-- | The highest value that user can enter for age.
maxAge :: Int
maxAge = 200

-- | This function validate parameter value.
validate :: Int -> Validation [String] Age
validate x = if x < minAge || maxAge < x
             then Failure [messageRange]
             else Success x



