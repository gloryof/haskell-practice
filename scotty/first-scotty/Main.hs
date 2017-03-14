{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import           Domain.User as DU

import qualified View.Render as RN
import qualified View.Index as ID
import qualified View.Input as IN
import qualified View.Confirm as CN
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
  scotty 3000 $ do
    get  "/index" $
      case compiledIdx of
          Left  err -> raise $ TL.pack $ show err
          Right tmp -> html $ ID.view tmp
{-  get  "/user-form" $ html $ IN.view UF.empty
  post "/user-form" $ html $
         do
           ui <- ext
           rst <- prs ui
           case rst of
             VL.Failure es -> html $ ID.view $ cnvIn ui es
             VL.Success u  -> html $ CN.view $ cnvCn u

  post "/user-confirm" $ CN.view


prs :: UserInput -> Validation [String] User
prs ui = DU.parseUser (iName ui) (iAge ui)

cnvIn :: UserInput -> [String] -> IN.InputView
cnvIn ui errs = IN.InputView errs (iName ui) (iAge ui)

cnvCn :: User -> CN.ConfirmView
cnvCn u = CN.ConfirmView (name u) (age u)

ext :: ActionM UserInput
ext = do
        nm <- param "name"
        ag <- param "age"
        return UserInput { iName = nm, iAge = ag }
-}
