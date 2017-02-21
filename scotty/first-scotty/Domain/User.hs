module Domain.User(User,parseUser,name,age) where

import Domain.Name as DN
import Domain.Age  as DA

import Data.Validation

data User = User {
              name :: DN.Name,
              age  :: DA.Age
            }

parseUser :: String -> String -> Validation [String] User
parseUser nm ag = cnv <$> DN.parseName nm <*> DA.parseAge ag

cnv :: DN.Name -> DA.Age -> User
cnv nm ag = User { name = nm, age = ag }
