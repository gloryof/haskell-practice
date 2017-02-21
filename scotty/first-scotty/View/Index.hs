{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module View.Index(view) where

import Web.Scotty

import Text.Blaze.Html.Renderer.Text (renderHtml)

view :: ActionM ()
view = html $ eitherParseFile "template/index.html"
