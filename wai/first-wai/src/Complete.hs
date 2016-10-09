{-# LANGUAGE OverloadedStrings #-}

module Complete where

import qualified Header as H
import Network.Wai
import Network.HTTP.Types

complete :: Response
complete = responseFile
  status200
  H.commonHeader
  "complete.html"
  Nothing


forwardComplete :: Response
forwardComplete = responseLBS
                  status303
                  [("Location", "/complete")]
                  ""
