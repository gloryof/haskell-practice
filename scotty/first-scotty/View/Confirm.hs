{-# LANGUAGE OverloadedStrings #-}

module View.Confirm(
  ConfirmView(ConfirmView),
  cnfPath,
  view
) where

import           Data.Text
import qualified Data.Text.Lazy as TL
import           Text.Mustache ((~>))
import qualified Text.Mustache as MS

data ConfirmView = ConfirmView {
                  name   :: String,
                  age    :: String
                }

instance MS.ToMustache ConfirmView where
  toMustache cv = MS.object
    [ "name" ~> name cv
    , "age" ~> age cv
    ]

cnfPath :: String
cnfPath = "form-confirm.html"

view :: ConfirmView -> MS.Template -> TL.Text
view iv tpl = TL.fromStrict $ MS.substitute tpl iv
