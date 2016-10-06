
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp(run)
import Data.Either
import Data.Char

type Value = String
type ParamName = String
type Message = String
type Invalid = (Value, Message)

data User = User {
                   name :: Either Invalid String,
                   age :: Either Invalid Int
                 }

data UserForm = UserForm {
                           user :: User,
                           valid :: Bool
                         }

main :: IO ()
main = do
  run 3000 apps

apps :: Application
apps request respond = respond $ case rawPathInfo request of
  "/"         -> index
  "/form"     -> viewInitForm
  "/confirm"  -> confirm request
  "/complete" -> complete
  _           -> notFound

index :: Response
index = responseFile
  status200
  commonHeader
  "index.html"
  Nothing

viewInitForm :: Response
viewInitForm = let eu = User { name = Right "", age = Right 0  }
                   uf = UserForm { user = eu, valid = False }
               in viewForm uf

confirm :: Request -> Response
confirm request = let usr = convert request
                  in decide usr

complete :: Response
complete = responseFile
  status200
  commonHeader
  "complete.html"
  Nothing

notFound :: Response
notFound = responseFile
  status404
  commonHeader
  "not-found.html"
  Nothing

viewForm :: UserForm -> Response
viewForm u = responseLBS
  status200
  commonHeader
  $ viewFormDom u

viewFormDom :: UserForm -> BS.ByteString
viewFormDom u = LB8.pack $
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
                    in case (name $ user u) of
                         Right n -> createTextField fnm n
                         Left  inv -> createErrorFields fnm inv
                  )  ++
                  "</dd>" ++
                  "<dt>Age</dt>" ++
                  "<dd>" ++
                  (
                    let fnm = "age"
                    in case (age $ user u) of
                         Right a -> createTextField fnm $ show a
                         Left inv -> createErrorFields fnm inv
                  )  ++
                  "</dd>" ++
                  "</ul>" ++
                  "<button>Submit</button>" ++
                  "</form>" ++
                  "</body>" ++
                  "</html>"

createTextField :: String -> String -> String
createTextField n v = "<input type=\"text\" name=\""++ n ++ "\" value=\"" ++ v ++  "\" />"

createErrorFields :: String -> Invalid -> String
createErrorFields n (v, m) = "<p style=\"color: red;\">" ++ m ++ "</p>" ++
                                    (createTextField n v)

convert :: Request -> UserForm
convert request = let q = queryString request
                      u = User {
                            name = convertName $ extractParam "name" q,
                            age = convertAge $ extractParam "age" q
                          }
                  in UserForm { user = u, valid = isValid u }

extractParam :: ParamName -> Query -> String
extractParam nm q = case lookup (B8.pack nm) q of
                     Just mv -> case mv of
                                Just v -> B8.unpack v
                                Nothing -> ""
                     Nothing -> ""

isValid :: User -> Bool
isValid u = let namev = case name u of
                               Left _ -> False
                               Right _ -> True
                agev  = case age u of
                               Left _ -> False
                               Right _ -> True
            in namev && agev

convertName :: String -> Either Invalid String
convertName v = if length v < 20 then
                  Right v
                else
                  Left (v, "Please enter up to 20 characters.")

convertAge :: String -> Either Invalid Int
convertAge v = if any (\c -> isNumber c /= True) v then
                 Left (v, "Please enter numeric.")
               else
                 Right (read v :: Int)

decide :: UserForm -> Response
decide u = if valid u then forwardComplete else viewForm u

forwardComplete :: Response
forwardComplete = responseLBS
                  status303
                  [("Location", "/complete")]
                  ""

commonHeader :: ResponseHeaders
commonHeader = [("Content-Type", "text/html")]

