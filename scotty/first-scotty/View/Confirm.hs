{-# LANGUAGE OverloadedStrings #-}

module View.Confirm(
  ConfirmView(ConfirmView),
  view
) where

import           Data.Text
import qualified Data.Text.IO as DT
import           Text.Mustache ((~>))
import qualified Text.Mustache as MS

data ConfirmView = ConfirmView {
                  name   :: String,
                  age    :: String
                }

instance MS.ToMustache ConfirmView where
  toMustache cv = MS.object
    [ "name" ~> name cv
    , "age " ~> age cv
    ]

view :: [String] -> ConfirmView -> IO ()
view sp cv = do
               compiled <- MS.automaticCompile sp "form-confirm.html"
               case compiled of
                 Left  err -> print err
                 Right tpl -> DT.putStr $ MS.substitute tpl cv
