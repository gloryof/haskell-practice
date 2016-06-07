{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Html where

import Data.Map as Map (Map,empty,fromList)

{- data Html = Html Head Body Lib -}

type Sentence = String
type Attributes = [(String, [String])]

data Html = Html { hd :: Element
                 , body :: Element
                 } deriving Show

data Element = BlockElement { tagName:: String, attribute:: Map.Map String [String], children::  [Element] }
             | InlineElement { tagName:: String, attribute:: Map.Map String [String],  content:: String }
             | EmptyElement { tagName:: String }
             | PseudoElement { content:: String }
             deriving Show

{- dv [("class", ["hoge","fuga"])] [(text "test")] -}
dv :: Attributes -> [Element] -> Element
dv as es = BlockElement { tagName = "div", attribute = fromList as, children = es }

{- p [("class", ["hoge", "fuga"])] ["test1", "test2"] -}
p :: Attributes -> [Sentence] -> Element
p as cnts = let adBr ls = if length ls > 0 then [br()] else []
                cld = foldl (\acc v -> acc ++ (adBr acc) ++ [PseudoElement { content = v }]) [] cnts
            in BlockElement { tagName = "p", attribute = fromList as, children = cld }

text :: String -> Element
text v = PseudoElement { content = v }

{- spn  [("class", ["hoge", "fuga", "bar"])] "content" -}
spn :: Attributes -> String -> Element
spn as cnt  = InlineElement { tagName = "span", attribute = fromList as, content = cnt }

br _ = EmptyElement { tagName = "br" }
