module Controller.Params (
  UserInput,
  name,
  age,
  extractUserInput,
  convertUser,
  convertInputView
) where

import           Web.Scotty

import qualified Domain.User as DU
import qualified View.Input as VI
import           Data.Validation as VL

data UserInput = UserInput {
                   name :: String,
                   age  :: String
                 }

extractUserInput :: ActionM UserInput
extractUserInput = do
                     nm <- param "name"
                     ag <- param "age"
                     return UserInput { name = nm, age = ag }

convertUser :: UserInput -> Validation [String] DU.User
convertUser ui = DU.parseUser (name ui) (age ui)


convertInputView :: UserInput -> [String] -> VI.InputView
convertInputView ui errs = VI.create errs False Nothing (name ui) (age ui)
