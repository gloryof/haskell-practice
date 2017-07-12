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

-- | This type represents item name.
type ItemName = String

-- | This type represents minimum value of the range.
type MinValue = Int

-- | This type represents maximum value of the range.
type MaxValue = Int

-- | This is an domain specification error.
data SpecError =
  RequiredError       ItemName |
  LengthError         ItemName Int |
  RangeError          ItemName MinValue MaxValue |
  RangeMagnitudeError ItemName ItemName
  deriving Eq



instance Show SpecError where
  show (RequiredError name) =
    name ++ "は" ++ "は必須です。"
  show (LengthError name len) =
    name ++ "は" ++ (show len) ++ "文字以内で入力してください。"
  show (RangeError name minVal maxVal) =
    name ++ "は" ++ (show minVal) ++ "-" ++ (show maxVal) ++ "の間で入力してください。"
  show (RangeMagnitudeError item1 item2) =
    item1 ++ "と" ++ item2 ++ "の大小関係が不正です。"
