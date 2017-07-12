{-|
module       : Domain.UserSearch
Description  : UserSearch domain modle.

This module representing the searching user.
-}
module Domain.UserSearch (
  InputCondition(..),
  Condition,
  parse,
  getUserIds,
  getName,
  getStartAge,
  getEndAge
) where

import           Data.Validation

import qualified Domain.Age  as DA
import qualified Domain.Name as DN
import qualified Domain.User as DU
import           Domain.Validate

-- | Condition that user has be inputted.
data InputCondition =
  InputCondition {
          userIds  :: [Int]
        , name     :: Maybe String
        , ageRange :: (Maybe Int, Maybe Int)
        }

-- | User searching contidion.
data Condition =
  Condition {
              userIds'  :: [DU.UserId]
            , name'     :: Maybe DN.Name
            , ageRange' :: (Maybe DA.Age, Maybe DA.Age)
            }

-- | Parse InputCondition.
-- If parsing is succuess return Condition.
parse :: InputCondition -> Validation [SpecError] Condition
parse i = create (parseIds i) <$> parseName i <*>  parseRange i


-- | Create condition.
create :: [DU.UserId] -> Maybe DN.Name -> (Maybe DA.Age, Maybe DA.Age) -> Condition
create ui n ar = Condition ui n ar

parseIds :: InputCondition -> [DU.UserId]
parseIds i = [DU.UserId x | x <- userIds i]

-- | Parse name from InputCondition.
parseName :: InputCondition -> Validation [SpecError] (Maybe DN.Name)
parseName i = parseValue name DN.parse i

-- | Parse age range from InputCondition
parseRange :: InputCondition -> Validation [SpecError] (Maybe DA.Age, Maybe DA.Age)
parseRange i = do
  sr <- parseStartRange i
  er <- parseEndRange i
  validateRange (sr, er)

validateRange :: (Maybe DA.Age, Maybe DA.Age)
              -> Validation [SpecError] (Maybe DA.Age, Maybe DA.Age)
validateRange (sr, er) = case sr of
                           Nothing -> Success (sr, er)
                           Just x  -> case er of
                                        Nothing -> Success (sr ,er)
                                        Just y  -> if DA.value y < DA.value x then
                                                     Failure $ [RangeMagnitudeError "年齢の開始" "年齢の終了"]
                                                   else
                                                     Success (sr ,er)

-- | Parse start of range from InputCondition
parseStartRange :: InputCondition -> Validation [SpecError] (Maybe DA.Age)
parseStartRange i = parseValue (fst . ageRange) DA.parse i

-- | Parse end of range from InputCondition
parseEndRange :: InputCondition -> Validation [SpecError] (Maybe DA.Age)
parseEndRange i = parseValue (snd . ageRange) DA.parse i

-- | Parse value.
-- gf = getting value function from input condition
-- pf = parse value fuction from getting value
-- ic = input condition
parseValue :: (InputCondition -> Maybe a)
           -> (a -> Validation [SpecError] b)
           -> InputCondition
           -> Validation [SpecError] (Maybe b)
parseValue gf pf ic =
  case gf ic of
    Nothing -> Success Nothing
    Just x -> case pf x of
                Success s -> Success $ Just s
                Failure e -> Failure e

-- | Get userIds from condition.
getUserIds :: Condition -> [DU.UserId]
getUserIds c = userIds' c

-- | Get name from condition.
getName :: Condition -> Maybe DN.Name
getName c = name' c

-- | Get start of age range from condition.
getStartAge :: Condition -> Maybe DA.Age
getStartAge c = fst $ ageRange' c

-- | Get end of age range from condition.
getEndAge :: Condition -> Maybe DA.Age
getEndAge c = snd $ ageRange' c
