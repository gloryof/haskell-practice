{-# LANGUAGE OverloadedStrings #-}

module View.Input(
  InputView (InputView),
  view
) where

import           Data.Text
import qualified Data.Text.IO as DT
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

view :: [String] -> InputView -> IO ()
view sp iv = do
               compiled <- MS.automaticCompile sp "form-input.html"
               case compiled of
                 Left  err -> print err
                 Right tpl -> DT.putStr $ MS.substitute tpl iv
