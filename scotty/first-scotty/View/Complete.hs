{-# LANGUAGE OverloadedStrings #-}

module View.Complete (
  comPath,
  view
) where

import qualified Text.Mustache as MS
import qualified Data.Text.Lazy as TL

comPath :: String
comPath =  "finish.html"

view :: MS.Template -> TL.Text
view tpl = TL.fromStrict $ MS.substitute tpl $ MS.object []
