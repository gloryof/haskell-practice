module Controller.Response (
  asHtml
) where

import qualified Data.Text.Lazy as TL

import qualified Text.Mustache as MS
import           Text.Parsec.Error

import           Web.Scotty

asHtml :: Either ParseError MS.Template
          -> (MS.Template -> TL.Text)
          -> ActionM ()
asHtml e f =
  case e of
    Left  err -> raise $ TL.pack $ show err
    Right tmp -> html $ f tmp
