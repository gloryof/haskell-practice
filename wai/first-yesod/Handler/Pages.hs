module Handler.Pages where

import Import

getPagesR :: Handler Html
getPagesR = defaultLayout $(widgetFile "pages")
