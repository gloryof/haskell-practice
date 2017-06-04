{-# LANGUAGE OverloadedStrings #-}

module View.Confirm(
  cnfPath,
  render
) where

import           Data.Text
import qualified Data.Text.Lazy as TL
import           Text.Mustache ((~>))
import qualified Text.Mustache as MS
import           Text.Parsec.Error
import qualified Domain.User as DU
import           Data.Validation as VL
import qualified View.Render as VR

import           Web.Scotty


data ConfirmView = ConfirmView {
                  name   :: String,
                  age    :: String
                }

instance MS.ToMustache ConfirmView where
  toMustache cv = MS.object
    [ "name" ~> name cv
    , "age" ~> age cv
    ]

render :: DU.User
          -> MS.Template
          -> TL.Text
render u tmp = view tmp $ cnvCn u

cnfPath :: String
cnfPath = "form-confirm.html"

view :: MS.Template -> ConfirmView -> TL.Text
view tpl iv = TL.fromStrict $ MS.substitute tpl iv

cnvCn :: DU.User -> ConfirmView
cnvCn u = ConfirmView (DU.name u) (show $ DU.age u)
