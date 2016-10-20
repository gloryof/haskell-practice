module Handler.Complete where

import Import

getCompleteR :: Handler Html
getCompleteR = defaultLayout $(widgetFile "complete")
