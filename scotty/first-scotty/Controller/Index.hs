module Controller.Index(
  view
) where

import qualified Controller.Response as CR

import           Text.Parsec.Error
import qualified Text.Mustache as MS

import qualified View.Index as VI

import           Web.Scotty


view :: Either ParseError MS.Template -> ActionM ()
view tmp = CR.asHtml tmp VI.render
