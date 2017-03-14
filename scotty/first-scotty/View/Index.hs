{-# LANGUAGE OverloadedStrings #-}

module View.Index(
  IndexRenderer,
  view,
  idxPath
  ) where

import           Data.Text.Lazy
import qualified Data.Text.IO as DT
import qualified Data.Text.Internal as TI
import qualified Data.Text.Lazy as TL
import qualified Text.Mustache as MS
import           Text.Mustache ((~>))
import           Text.Parsec.Error
import           Web.Scotty
import           View.Render


data IndexRenderer = IndexRenderer String

idxPath :: String
idxPath = "index.html"

view :: MS.Template -> TL.Text
view tpl = TL.fromStrict $ MS.substitute tpl $ MS.object []
