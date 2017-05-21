module View.List (
  lstPath,
  view
) where

import qualified Data.Text.Lazy as TL
import           Text.Mustache ((~>))
import qualified Text.Mustache as MS
import qualified Domain.User as DU
import qualified Domain.Age as DA
import qualified Infra.Repository.User as IRU

import GHC.Base


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

data ListView = ListView
  {
    users :: [UserInfo]
  }

instance MS.ToMustache ListView where
  toMustache lv = MS.object
    [
      "users" ~> users lv
    ]

lstPath :: String
lstPath = "list.html"

view ::[DU.User] -> MS.Template -> TL.Text
view us tmp = TL.fromStrict $ MS.substitute tmp $ list us

list :: [DU.User] -> ListView
list us = ListView
  {
    users = map cnv us
  }


cnv :: DU.User -> UserInfo
cnv u = UserInfo
  {
    key = case DU.userId u of
            Just x -> x
            Nothing -> 0
    ,
    age = DU.age u,
    name = DU.name u
  }
