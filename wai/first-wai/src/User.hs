module User(User(..), UserForm(..), Invalid, convert) where

import qualified Data.ByteString.Char8 as B8
import Network.Wai
import Data.Either
import Network.HTTP.Types
import Data.Char

type Value = String
type ParamName = String
type Message = String
type Invalid = (Value, Message)

data User = User {
                   name :: Either Invalid String,
                   age :: Either Invalid Int
                 }

data UserForm = UserForm {
                           user :: User,
                           valid :: Bool
                         }

convert :: Request -> UserForm
convert request = let q = queryString request
                      u = User {
                            name = cn $ ep "name" q,
                            age = ca $ ep "age" q
                          }
                  in UserForm { user = u, valid = isValid u }

isValid :: User -> Bool
isValid u = let namev = case name u of
                               Left _ -> False
                               Right _ -> True
                agev  = case age u of
                               Left _ -> False
                               Right _ -> True
            in namev && agev

cn :: String -> Either Invalid String
cn v = if length v < 20 then
                  Right v
                else
                  Left (v, "Please enter up to 20 characters.")

ca :: String -> Either Invalid Int
ca v = if any (\c -> isNumber c /= True) v then
                 Left (v, "Please enter numeric.")
               else
                 Right (read v :: Int)

ep :: ParamName -> Query -> String
ep nm q = case lookup (B8.pack nm) q of
                     Just mv -> case mv of
                                Just v -> B8.unpack v
                                Nothing -> ""
                     Nothing -> ""
