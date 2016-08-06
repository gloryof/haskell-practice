{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Html where

import Data.Map as Map (Map,empty,fromList)
import Data.Char

class Tags x where
  tag :: x -> String

{- data Html = Html Head Body Lib -}

type Sentence = String
type Attribute = (String, String)

sCls :: [String] -> Attribute
sCls cs = ("class", unwords cs)

method :: Method -> Attribute
method x = ("method", show x)

sid :: String -> Attribute
sid x = ("id", x)

data Html = Html { hd :: [HeadElement]
                 , body :: [BodyElement]
                 } deriving Show
data Method = GET | POST deriving Show
data InputType = TEXT | PASSWORD deriving Show

data HeadElement = HeadElement { hTagName:: String, hAttribute:: Map.Map String String, hContent:: String }
                   deriving Show

instance Tags HeadElement where
  tag x = hTagName x

title :: String -> HeadElement
title x = HeadElement { hTagName = "title", hAttribute = Map.empty, hContent = x }

meta :: String -> String -> HeadElement
meta k v = HeadElement { hTagName = "meta", hAttribute = fromList [(k, v)], hContent = "" }

data BodyElement = BlockElement { bTagName:: String, bAttribute:: Map.Map String String, children::  [BodyElement] }
             | InlineElement { bTagName:: String, bAttribute:: Map.Map String String,  content:: String }
             | EmptyElement { bTagName:: String }
             | PseudoElement { content:: String }
             deriving Show

createBlockElement :: String -> [Attribute] -> [BodyElement] -> BodyElement
createBlockElement nm as es = BlockElement { bTagName = nm, bAttribute = fromList as, children = es }


createInlineElement :: String -> [Attribute] -> String -> BodyElement
createInlineElement nm as cn = InlineElement { bTagName = nm, bAttribute = fromList as, content = cn }

{- dv [(sCls ["hoge","fuga"])] [(text "test")] -}
dv :: [Attribute] -> [BodyElement] -> BodyElement
dv as es = createBlockElement "div" as es

form :: [Attribute] -> [BodyElement] -> BodyElement
form as es = createBlockElement "form" as es

ul :: [Attribute] -> [BodyElement] -> BodyElement
ul as es = createBlockElement "ul" as es

li :: [Attribute] -> [BodyElement] -> BodyElement
li as es = createBlockElement "li" as es

dl :: [Attribute] -> [BodyElement] -> BodyElement
dl as es = createBlockElement "dl" as es

dt :: [Attribute] -> [BodyElement] -> BodyElement
dt as es = createBlockElement "dt" as es

dd :: [Attribute] -> [BodyElement] -> BodyElement
dd as es = createBlockElement "dd" as es

{- p [(sCls ["hoge", "fuga"])] ["test1", "test2"] -}
p :: [Attribute] -> [Sentence] -> BodyElement
p as cnts = let adBr ls = if length ls > 0 then [br()] else []
                cld = foldl (\acc v -> acc ++ (adBr acc) ++ [PseudoElement { content = v }]) [] cnts
            in createBlockElement "p" as cld

text :: String -> BodyElement
text v = PseudoElement { content = v }

{- spn  [sCls ["hoge", "fuga", "bar"])] "content" -}
spn :: [Attribute] -> String -> BodyElement
spn as cnt  = InlineElement { bTagName = "span", bAttribute = fromList as, content = cnt }

button :: [Attribute] -> String -> BodyElement
button as cnt  = InlineElement { bTagName = "button", bAttribute = fromList as, content = cnt }

a :: [Attribute] -> String -> BodyElement
a as cnt  = InlineElement { bTagName = "a", bAttribute = fromList as, content = cnt }

input :: [Attribute] -> InputType -> String ->  BodyElement
input as tp nm = let ta = ("type", map toLower $ show tp)
                     na = ("name", nm)
                 in createInlineElement "input" (as ++ [na, ta]) ""

br _ = EmptyElement { bTagName = "br" }
