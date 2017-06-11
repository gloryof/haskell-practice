{-# LANGUAGE OverloadedStrings #-}

module WebApi (runApp) where

import Web.Scotty

import Domain.Age

app' :: ScottyM ()
app' = do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

runApp :: IO ()
runApp = scotty 3000 app'
