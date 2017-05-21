module Domain.User(User,parseUser,createUser,userId, name,age) where

import Domain.Name as DN
import Domain.Age  as DA

import Data.Validation

data User = User {
              userId :: Maybe Int,
              name :: DN.Name,
              age  :: DA.Age
            }

parseUser :: String -> String -> Validation [String] User
parseUser nm ag = f <$> DN.parseName nm <*> DA.parseAge ag
                where
                  f = cnv Nothing

createUser :: Int -> String -> Int -> User
createUser uid nm ag = cnv (Just uid) nm ag

cnv :: Maybe Int -> DN.Name -> DA.Age -> User
cnv uid nm ag = User { userId = uid, name = nm, age = ag }
