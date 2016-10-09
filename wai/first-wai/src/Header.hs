{-# LANGUAGE OverloadedStrings #-}
module Header where

import Network.Wai
import Network.HTTP.Types

commonHeader :: ResponseHeaders
commonHeader = [("Content-Type", "text/html")]
