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
import qualified View.UserInfo as VUI

import GHC.Base

data ListView = ListView
  {
    users :: [VUI.UserInfo]
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
    users = map VUI.convert us
  }

