module View.UserInfo (
   UserInfo,
   convert
) where

import           Text.Mustache ((~>))
import qualified Text.Mustache as MS
import qualified Domain.User as DU
import qualified Domain.Age as DA


data UserInfo = UserInfo {
                  key  :: Int,
                  age  :: Int,
                  name :: String
                }


instance MS.ToMustache UserInfo where
  toMustache ui = MS.object
    [ "key"  ~> key ui
    , "age"  ~> age ui
    , "name" ~> name ui
    ]

convert :: DU.User -> UserInfo
convert u = UserInfo
  {
    key = case DU.userId u of
            Just x -> x
            Nothing -> 0
    ,
    age = DU.age u,
    name = DU.name u
  }
