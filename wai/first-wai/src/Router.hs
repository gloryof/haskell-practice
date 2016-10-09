{-# LANGUAGE OverloadedStrings #-}
module Router where

import qualified Index as I
import qualified ViewForm as VF
import qualified Confirm as CF
import qualified Complete as CM
import qualified Header as H
import Network.Wai
import Network.HTTP.Types

route :: Application
route request respond = respond $ case rawPathInfo request of
  "/"         -> I.index
  "/form"     -> VF.view Nothing
  "/confirm"  -> CF.confirm request
  "/complete" -> CM.complete
  _           -> notFound

notFound :: Response
notFound = responseFile
  status404
  H.commonHeader
  "not-found.html"
  Nothing
