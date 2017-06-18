
{-|
module       : Domain.Age
Description  : Name domain modle.

This module representing the user name.
-}
module Domain.Name (
  Name,
  validate
) where

import           Data.Validation
import qualified Data.Text as T

import           Domain.Validate

-- | This type represents user name.
type Name = String

-- | This is name maximum length.
maxLen :: Int
maxLen = 20

-- | This is name label.
itemName :: String
itemName = "名前"

-- | This function validate parameter value.
validate :: String -> Validation [SpecError] Name
validate x = required x >>= valueLength

-- | Validate inputted value. Whether that is not empty.
required :: String -> Validation [SpecError] String
required x = if x == ""
             then Failure [(RequiredError itemName)]
             else Success x

-- | Validate inputted value. Whether that is not over max length.
valueLength :: String -> Validation [SpecError] String
valueLength x = if maxLen < (T.length $ T.pack x)
           then Failure [(LengthError itemName maxLen)]
           else Success x
