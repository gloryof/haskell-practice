module HtmlPres where

import Html

import Data.Map as Map (Map,empty)
import Data.Map.Lazy as ML (foldlWithKey)

escape :: String -> String
escape x = x

outputAttr :: Map.Map String String -> String
outputAttr as = let expand val = "\"" ++ val ++ "\""
                    cat k v = k ++ "=" ++ (expand v)
                in ML.foldlWithKey (\acc k v -> acc ++ " "  ++ (cat k v)) "" as

tagStart :: String -> Map.Map String String -> String
tagStart tn as = "<" ++ tn ++ (outputAttr as)  ++ ">"

tagSingle :: String -> Map.Map String String -> String
tagSingle tn as = "<" ++ tn ++ (outputAttr as)  ++ "/>"

tagEnd :: String -> String
tagEnd tn = "</" ++ tn ++ ">"

class HtmlPres x where
  toHtml :: x -> String

instance HtmlPres Html where
  toHtml (Html hd bd) = let hcn = foldl (\acc x -> acc ++ (toHtml x)) "" hd
                            hdom = "<head>" ++ hcn ++ "</head>"
                            bcn = foldl (\acc x -> acc ++ (toHtml x)) "" bd
                            bdom = "<body>" ++ bcn ++ "</body>"
                            doct = "<!DOCTYPE html>"
                        in doct ++ "<html>" ++ hdom ++ bdom ++ "</html>"

instance HtmlPres HeadElement where
  toHtml (HeadElement nm as cnt) = if length cnt < 1 then tagSingle nm as
                                   else let ts = tagStart nm as
                                            tx = escape cnt
                                            te = tagEnd nm
                                        in ts ++ tx ++ te

instance HtmlPres BodyElement where
  toHtml x = case x of
    BlockElement  tn as cld -> let ts = tagStart tn as
                                   tx = foldl (\acc v -> acc ++ (toHtml v)) "" cld
                                   te = tagEnd tn
                               in ts ++ tx ++ te
    InlineElement tn as cnt -> let ts = tagStart tn as
                                   tx = escape cnt
                                   te = tagEnd tn
                               in ts ++ tx ++ te
    EmptyElement  tn        -> tagEnd tn
    PseudoElement cnt       -> cnt

{- toHtml $ dv [("class", ["hoge","fuga"])] [(text "test")] -}
{- toHtml $ p [("class", ["hoge", "fuga"])] ["test1", "test2"] -}
{- toHtml $ spn  [("class", ["hoge", "fuga", "bar"])] "content" -}

