{-# LANGUAGE OverloadedStrings #-}

module View.Index(
  idxPath,
  render
) where

import           Data.Text.Lazy
import qualified Data.Text.Lazy as TL

import qualified Text.Mustache as MS

import qualified View.Render as VR

idxPath :: String
idxPath = "index.html"

render :: MS.Template -> TL.Text
render tpl = TL.fromStrict $ MS.substitute tpl $ MS.object []

