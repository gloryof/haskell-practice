{-# LANGUAGE OverloadedStrings #-}

import qualified Controller.Index as CID
import qualified Controller.Input as CIP
import qualified Controller.Confirm as CCF
import qualified Controller.Complete as CCP
import qualified Controller.Users as CCU

import qualified Infra.Repository.User as IRU

import qualified Text.Mustache as MS

import qualified View.Index as ID
import qualified View.Input as IN
import qualified View.Confirm as CN
import qualified View.Complete as CM
import qualified View.List as LT
import qualified View.Detail as DT


import Web.Scotty

main :: IO ()
main = do
  IRU.setup
  let tmpBase = MS.automaticCompile [ "./template" ]
  compiledIdx <- tmpBase ID.idxPath
  compiledInp <- tmpBase IN.inpPath
  compiledCnf <- tmpBase CN.cnfPath
  compiledCom <- tmpBase CM.comPath
  compiledLst <- tmpBase LT.lstPath
  compiledDtl <- tmpBase DT.dtlPath
  scotty 3000 $ do
    get  "/index"     $ CID.view compiledIdx
    get  "/users"     $ CCU.viewList compiledLst
    get "/users/:key" $ CCU.viewDetail compiledDtl
    get  "/user-form" $ CIP.viewNew compiledInp
    post "/user-form" $ CIP.viewEdit compiledInp
    post "/user-confirm" $ CCF.confirm compiledInp compiledCnf
    post "/user-complete" $ CCP.complete compiledInp compiledCom
