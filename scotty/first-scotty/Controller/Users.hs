module Controller.Users (
  viewList,
  viewDetail
) where

import           Control.Monad.IO.Class

import qualified Controller.Response as CR
import qualified Controller.Params as CP

import qualified Infra.Repository.User as IRU

import qualified Network.HTTP.Types.Status as Status

import           Text.Parsec.Error
import qualified Text.Mustache as MS

import qualified View.Detail as VD
import qualified View.List as VL
import qualified View.UserInfo as VU


import           Web.Scotty

viewList :: Either ParseError MS.Template -> ActionM ()
viewList temp = do
                  l <- liftIO IRU.selectAll
                  CR.asHtml temp $ VL.render l

viewDetail :: Either ParseError MS.Template -> ActionM ()
viewDetail temp = do
                    key <- param "key"
                    u   <- liftIO $ IRU.selectById key
                    case u of
                      Just x  -> CR.asHtml temp $ VD.render x
                      Nothing -> status $ Status.status404
