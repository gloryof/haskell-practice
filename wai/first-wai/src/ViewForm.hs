{-# LANGUAGE OverloadedStrings #-}

module ViewForm(view) where

import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Header as H
import qualified User as U
import Network.Wai
import Network.HTTP.Types

view :: Maybe U.UserForm -> Response
view m = responseLBS
  status200
  H.commonHeader
  $ dom (
          case m of
            Just u  -> u
            Nothing -> emp
        )

emp :: U.UserForm
emp = let eu = U.User { U.name = Right "", U.age = Right 0  }
      in  U.UserForm { U.user = eu, U.valid = False }

dom :: U.UserForm -> BS.ByteString
dom u = LB8.pack $
        "<!doctype html>" ++
        "<html>" ++
        "<head>" ++
        "<title>input-form</title>" ++
        "</head>" ++
        "<body>" ++
        "<form action=\"confirm\">" ++
        "<dl>" ++
        "<dt>Name</dt>" ++
        "<dd>" ++
        (
          let fnm = "name"
          in case (U.name $ U.user u) of
               Right n -> txt fnm n
               Left  inv -> err fnm inv
        )  ++
        "</dd>" ++
        "<dt>Age</dt>" ++
        "<dd>" ++
        (
          let fnm = "age"
          in case (U.age $ U.user u) of
               Right a -> txt fnm $ show a
               Left inv -> err fnm inv
        )  ++
        "</dd>" ++
        "</ul>" ++
        "<button>Submit</button>" ++
        "</form>" ++
        "</body>" ++
        "</html>"

txt :: String -> String -> String
txt n v = "<input type=\"text\" name=\""++ n ++ "\" value=\"" ++ v ++  "\" />"

err :: String -> U.Invalid -> String
err n (v, m) = "<p style=\"color: red;\">" ++ m ++ "</p>" ++ (txt n v)
