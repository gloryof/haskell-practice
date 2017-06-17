
{-|
module       : Domain.Age
Description  : Age domain modle.

This module representing the age.
-}
module Domain.Age (
  Age,
  validate
) where

import           Data.Validation

import           Domain.Validate

-- | This type represents age.
type Age = Int

-- | The minimum value that user can enter for age.
minAge :: MinValue
minAge = 0

-- | The maximum value that user can enter for age.
maxAge :: MaxValue
maxAge = 200

-- | This function validate parameter value.
validate :: Int -> Validation [SpecError] Age
validate x = if x < minAge || maxAge < x
             then Failure [(RangeError "年齢" minAge maxAge)]
             else Success x



