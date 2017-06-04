module Controller.Confirm (
  confirm
) where

import qualified Controller.Response as CR
import qualified Controller.Params as CP

import           Data.Validation as VL

import           Text.Parsec.Error
import qualified Text.Mustache as MS

import qualified View.Input as VI
import qualified View.Confirm as VCF

import           Web.Scotty

confirm :: Either ParseError MS.Template
           -> Either ParseError MS.Template
           -> ActionM ()
confirm inputTmp confirmTemp =
  do
    ui  <- CP.extractUserInput
    rst <- return $ CP.convertUser ui
    case rst of
      Failure ve -> CR.asHtml inputTmp $ VI.render $  CP.convertInputView ui ve
      Success  u -> CR.asHtml confirmTemp $ VCF.render u
