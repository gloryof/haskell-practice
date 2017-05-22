{-# LANGUAGE OverloadedStrings #-}


import Web.Scotty

import           Domain.User as DU

import           Control.Monad.IO.Class (liftIO)

import qualified View.Index as ID
import qualified View.Input as IN
import qualified View.Confirm as CN
import qualified View.Complete as CM
import qualified View.List as LT
import qualified View.Detail as DT
import qualified Infra.Repository.User as IRU
import qualified Data.Text.Lazy as TL
import           Data.Validation as VL
import qualified Network.HTTP.Types.Status as Status

import qualified Text.Mustache as MS
import           Text.Parsec.Error

import qualified Form.UserForm as UF()

data UserInput = UserInput {
                   iName :: String,
                   iAge  :: String
                 }

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
    get  "/index"     $ render compiledIdx ID.view
    get  "/users"     $ do
      l <- liftIO IRU.selectAll
      render compiledLst $ LT.view l
    get "/users/:key" $ do
      key <- param "key"
      u   <- liftIO $ IRU.selectById key
      case u of
        Just x  -> render compiledDtl $ DT.view x
        Nothing -> status $ Status.status404
    get  "/user-form" $ render compiledInp IN.viewEmp
    post "/user-form" $ do
      ui  <- ext
      cnv <- return $ cnvIn ui []
      render compiledInp $ IN.view cnv
    post "/user-confirm" $ do
      ui  <- ext
      rst <- return $ prs ui
      case rst of
        Failure ve -> render compiledInp vf
                      where
                        cnv = cnvIn ui ve
                        vf  = IN.view cnv
        Success  u -> render compiledCnf vf
                      where
                        cnv = cnvCn u
                        vf  = CN.view cnv
    post "/user-complete" $ do
      ui <- ext
      rst <- return $ prs ui
      case rst of
        Failure ve -> render compiledInp vf
                      where
                        cnv = cnvIn ui ve
                        vf  = IN.view cnv
        Success u  -> do
                        id <- liftIO $ IRU.save u
                        render compiledCom CM.view

render :: Either ParseError MS.Template -> (MS.Template -> TL.Text) -> ActionM ()
render e f =
  case e of
    Left  err -> raise $ TL.pack $ show err
    Right tmp -> html  $ f tmp

prs :: UserInput -> Validation [String] User
prs ui = DU.parseUser (iName ui) (iAge ui)

cnvIn :: UserInput -> [String] -> IN.InputView
cnvIn ui errs = IN.InputView errs (iName ui) (iAge ui)

cnvCn :: User -> CN.ConfirmView
cnvCn u = CN.ConfirmView (name u) (show $ age u)

ext :: ActionM UserInput
ext = do
        nm <- param "name"
        ag <- param "age"
        return UserInput { iName = nm, iAge = ag }
