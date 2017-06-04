{-# LANGUAGE OverloadedStrings #-}

module View.Input(
  InputView,
  create,
  inpPath,
  render,
) where

import qualified Data.Text.Lazy as TL

import           Text.Mustache ((~>))
import qualified Text.Mustache as MS

import qualified View.Render as VR

data InputView = InputView {
                  errors :: [String],
                  edit   :: Bool,
                  key    :: (Maybe Int),
                  name   :: String,
                  age    :: String
                }

instance MS.ToMustache InputView where
  toMustache iv = MS.object
    [ "errors" ~> errors iv
    , "edit"   ~> edit iv
    , "key"    ~> key iv
    , "name"   ~> name iv
    , "age"    ~> age iv
    ]

create :: [String] -> Bool -> Maybe Int -> String -> String -> InputView
create er ed ky nm ag = InputView {
                          errors = er,
                          edit   = ed,
                          key    = ky,
                          name   = nm,
                          age    = ag }

inpPath :: String
inpPath = "form-input.html"

render :: InputView -> MS.Template -> TL.Text
render iv tpl = TL.fromStrict $ MS.substitute tpl iv
