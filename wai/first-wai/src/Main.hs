{-# LANGUAGE OverloadedStrings #-}

import qualified Router as R
import Network.Wai.Handler.Warp(run)

main :: IO ()
main = do
  run 3000 R.route
