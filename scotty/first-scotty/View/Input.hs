{-# LANGUAGE OverloadedStrings #-}

module View.Input(
  InputView (InputView),
  inpPath,
  viewEmp,
  view
) where

import           Data.Text
import qualified Data.Text.Lazy as TL
import           Text.Mustache ((~>))
import qualified Text.Mustache as MS

data InputView = InputView {
                  errors :: [String],
                  name   :: String,
                  age    :: String
                }

instance MS.ToMustache InputView where
  toMustache iv = MS.object
    [ "errors" ~> errors iv
    , "name"   ~> name iv
    , "age"    ~> age iv
    ]

inpPath :: String
inpPath = "form-input.html"

viewEmp :: MS.Template -> TL.Text
viewEmp tmp = view tmp $ InputView { errors = [], name = "", age = "" }

view ::  MS.Template -> InputView -> TL.Text
view tpl iv = TL.fromStrict $ MS.substitute tpl iv
