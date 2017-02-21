{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

import Web.Scotty

import View.Index
import View.UserForm


main :: IO ()
main = scotty 3000 $ do
  get "/index" $ do View.Index.view
  get "/user-form" $ do View.UserForm.view
  post "/user-form" $ do View.UserForm.rview
  post "/user-confirm" $ do View.UserForm.confirm
