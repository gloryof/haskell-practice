module Controller.Input (
  viewNew,
  viewEdit,
  viewRetry
) where

import qualified Controller.Response as CR
import qualified Controller.Params as CP

import           Data.Text()
import qualified Data.Text.Lazy as TL
import           Data.Validation as VL

import           Text.Parsec.Error
import qualified Text.Mustache as MS

import qualified View.Input as VI

import           Web.Scotty

viewNew :: Either ParseError MS.Template -> ActionM ()
viewNew tmp = view tmp empty

viewEdit :: Either ParseError MS.Template -> ActionM ()
viewEdit tmp = do
                 ui  <- CP.extractUserInput
                 cnv <- return $ CP.convertInputView ui []
                 view tmp cnv

viewRetry :: Either ParseError MS.Template
             -> VI.InputView
             -> ActionM ()
viewRetry tmp ui = view tmp ui

view :: Either ParseError MS.Template
        -> VI.InputView
        -> ActionM ()
view tmp ui = CR.asHtml tmp $ VI.render ui

empty :: VI.InputView
empty  = VI.create [] False Nothing  ""  ""
