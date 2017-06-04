module View.Render (
  render
) where

import            Web.Scotty
import qualified Text.Mustache as MS
import qualified Data.Text.Lazy as TL
import           Text.Parsec.Error

render :: Either ParseError MS.Template -> (MS.Template -> TL.Text) -> ActionM ()
render e f =
  case e of
    Left  err -> raise $ TL.pack $ show err
    Right tmp -> html  $ f tmp
