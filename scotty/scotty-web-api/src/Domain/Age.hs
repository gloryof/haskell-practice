
{-|
module       : Domain.Age
Description  : Age domain modle.

This module representing the age.
-}
module Domain.Age (
  Age(value),
  parse
) where

import           Data.Validation

import           Domain.Validate

-- | This type represents age.
data Age = Age { value :: Int }

-- | The minimum value that user can enter for age.
minAge :: MinValue
minAge = 0

-- | The maximum value that user can enter for age.
maxAge :: MaxValue
maxAge = 200

-- | This function parse to Age.
--  Return validation when parsing is failed.
parse :: Int -> Validation [SpecError] Age
parse x = if x < minAge || maxAge < x
          then Failure [(RangeError "年齢" minAge maxAge)]
          else Success (Age x)



