{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe (fromMaybe)
import Data.Map as Map (Map,insert,empty,lookup,fromList)

escape :: String -> String
escape x = x

{- data Html = Html Head Body Lib -}

type TagName = String

data Element = Element TagName (Map.Map String [String]) [Element]
             | EmpElement String deriving Show
class ElemFunc x where
  aid :: x -> String -> x
  cls :: x -> String -> x

instance ElemFunc Element where
  aid (Element tn as es) v = (Element tn (insert "id" [v] as) es)
  cls (Element tn as es) v = let bas = fromMaybe [] $ Map.lookup "class" as
                                 new = insert "class" (bas ++ [v]) as
                             in (Element tn new es)


{- dv (\n -> [(spn "test"), (spn "test2") , (spn "test3")]) -}
dv :: (() -> [Element]) -> Element
dv fn = Element "div" Map.empty (fn())

{- aid (cls (cls (spn "test") "test2") "test3" ) "test4" -}
spn :: String -> Element
spn xs = Element "span" Map.empty [(EmpElement $ escape xs)]

lib :: [String] -> [Element]
lib xs = foldl (\acc x -> acc ++ [(scp x)]) [] xs

css :: [String] -> [Element]
css xs = foldl (\acc x -> acc ++ [(sty x)]) [] xs

scp :: String -> Element
scp src = (Element "script" (Map.fromList [("src", [src])]) [])

sty :: String -> Element
sty ref = (Element "link" (Map.fromList [("rel", ["stylesheet"]), ("href", [ref])]) [])



{-
html(
  head (
    title "Test title"
  )
  lib (
    css ["hoge.css", "fuga.css", "../bar.css"]
    script ["hoge.js", "fuga.js", "../bar.js"]
  )
  body (
    content(
      div (
        id "root-div"
        class ["hoge-div", "fuga-div", "bar-div"]
        attr ( [("data-hoge", "1"), ("dage-fuga", "2")] )
        content(
          p (
            id "hoge-p"
          )
        )
      )
    )
  )
)
-}
