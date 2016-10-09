{-# LANGUAGE OverloadedStrings #-}

module Index where

import qualified Header as H
import Network.Wai
import Network.HTTP.Types

index :: Response
index = responseFile
  status200
  H.commonHeader
  "index.html"
  Nothing
