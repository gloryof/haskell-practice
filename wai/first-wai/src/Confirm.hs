{-# LANGUAGE OverloadedStrings #-}

module Confirm where

import qualified User as U
import qualified ViewForm as VF
import qualified Complete as C
import Network.Wai

confirm :: Request -> Response
confirm request = let usr = U.convert request
                  in decide usr

decide :: U.UserForm -> Response
decide u = if U.valid u then C.forwardComplete else VF.view (Just u)

