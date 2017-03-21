{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import           Domain.User as DU

import qualified View.Index as ID
import qualified View.Input as IN
import qualified View.Confirm as CN
import qualified View.Complete as CM
import qualified Data.Text.Lazy as TL
import           Data.Validation as VL

import qualified Text.Mustache as MS

import qualified Form.UserForm as UF


data UserInput = UserInput {
                   iName :: String,
                   iAge  :: String
                 }

main :: IO ()
main = do
  let tmpBase = MS.automaticCompile [ "./template" ]
  compiledIdx <- tmpBase ID.idxPath
  compiledInp <- tmpBase IN.inpPath
  compiledCnf <- tmpBase CN.cnfPath
  compiledCom <- tmpBase CM.comPath
  scotty 3000 $ do
    get  "/index" $
      case compiledIdx of
        Left  err -> raise $ TL.pack $ show err
        Right tmp -> html $ ID.view tmp
    get "/user-form" $
      case compiledInp of
        Left  err -> raise $ TL.pack $ show err
        Right tmp -> html  $ IN.viewEmp tmp
    post "/user-form" $ do
      ui <- ext
      case compiledInp of
        Left  err -> raise $ TL.pack $ show err
        Right tmp -> html  $ IN.view tmp $ cnvIn ui []
    post "/user-confirm" $ do
      ui  <- ext
      rst <- return $ prs ui
      case rst of
        Failure ve ->
          case compiledInp of
            Left  err -> raise $ TL.pack $ show err
            Right tmp -> html  $ IN.view tmp $ cnvIn ui ve
        Success u ->
          case compiledCnf of
            Left  err -> raise $ TL.pack $ show err
            Right tmp -> html  $ CN.view tmp $ cnvCn u
    post "/user-complete" $ do
      case compiledCom of
        Left  err -> raise $ TL.pack $ show err
        Right tmp -> html  $ CM.view tmp

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
