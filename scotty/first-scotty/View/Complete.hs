{-# LANGUAGE OverloadedStrings #-}

module View.Complete (
  comPath,
  render
) where

import qualified Text.Mustache as MS
import qualified Data.Text.Lazy as TL

comPath :: String
comPath =  "finish.html"


render :: MS.Template -> TL.Text
render tpl = TL.fromStrict $ MS.substitute tpl $ MS.object []
