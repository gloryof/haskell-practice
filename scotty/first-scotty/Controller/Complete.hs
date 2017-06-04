module Controller.Complete (
  complete
) where

import           Control.Monad.IO.Class

import qualified Controller.Response as CR
import qualified Controller.Params as CP

import           Data.Validation as VL

import           Infra.Repository.User as IRU

import           Text.Parsec.Error
import qualified Text.Mustache as MS

import qualified View.Input as VI
import qualified View.Complete as VCP

import           Web.Scotty

complete :: Either ParseError MS.Template
           -> Either ParseError MS.Template
           -> ActionM ()
complete inputTmp completeTemp =
  do
    ui  <- CP.extractUserInput
    rst <- return $ CP.convertUser ui
    case rst of
      Failure ve -> CR.asHtml inputTmp $ VI.render $  CP.convertInputView ui ve
      Success  u -> do
                      key <- liftIO $ IRU.save u
                      CR.asHtml completeTemp VCP.render
