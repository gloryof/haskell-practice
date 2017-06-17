{-|
Module       : Domain.Validate
Description  : Validate domain modle.

This module provides domain validations.
-}

module Domain.Validate (
  SpecError(..),
  MinValue,
  MaxValue
) where

-- | This type represents minimum value of the range.
type MinValue = Int

-- | This type represents maximum value of the range.
type MaxValue = Int

-- | This is an domain specification error.
data SpecError = RangeError String MinValue MaxValue

instance Eq SpecError where
  (RangeError xl xmn xmx) == (RangeError yl ymn ymx) =
    (xl == yl) && (xmn == ymn) && (xmx == ymx)

instance Show SpecError where
  show (RangeError label minVal maxVal) =
    label ++ "は" ++ (show minVal) ++ "-" ++ (show maxVal) ++ "の間で入力してください。"
