{-|
module       : Domain.User
Description  : User domain modle.

This module representing the user.
-}
module Domain.User(
  User,
  UserId(UserId,value),
  parse,
  create,
  getUserId,
  getName,
  getAge,
  isRegistered
) where

import           Data.Maybe
import           Data.Validation

import qualified Domain.Age as DA
import qualified Domain.Name as DN
import           Domain.Validate

-- | This type represents user id.
data UserId = UserId { value :: Int } deriving (Eq,Show)

-- | This type represents user.
data User = User {
                   userId :: Maybe UserId
                 , name   :: DN.Name
                 , age    :: DA.Age
                 } deriving (Show)

-- | Parse to user.
-- If validation failed then return SpecError.
parse :: Maybe UserId -> String -> Int -> Validation [SpecError] User
parse i n a = User i <$> DN.parse n <*> DA.parse a


-- | Create user.
create :: UserId -> DN.Name -> DA.Age -> User
create i n a = User (Just i) n a

-- | Get user id.
-- If id was not numbered then return Nothing.
getUserId :: User -> Maybe UserId
getUserId = userId

-- | Get user name.
getName :: User -> DN.Name
getName = name

-- | Get user age.
getAge :: User -> DA.Age
getAge = age

-- | If user was registered then return true.Otherwise return False.
isRegistered :: User -> Bool
isRegistered = isJust . getUserId
