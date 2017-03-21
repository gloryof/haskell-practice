{-# LANGUAGE OverloadedStrings #-}

module View.Index(
  view,
  idxPath
  ) where

import           Data.Text.Lazy
import qualified Data.Text.Lazy as TL
import qualified Text.Mustache as MS

idxPath :: String
idxPath = "index.html"

view :: MS.Template -> TL.Text
view tpl = TL.fromStrict $ MS.substitute tpl $ MS.object []

