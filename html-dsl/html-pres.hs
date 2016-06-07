import Html

import Data.Map as Map (Map)
import Data.Map.Lazy as ML (foldlWithKey)

escape :: String -> String
escape x = x

outputAttr :: Map.Map String [String] -> String
outputAttr as = let expand vals = "\"" ++ (unwords vals) ++ "\""
                    cat k v = k ++ "=" ++ (expand v)
                in ML.foldlWithKey (\acc k v -> acc ++ " "  ++ (cat k v)) "" as

tagStart :: String -> Map.Map String [String] -> String
tagStart tn as = "<" ++ tn ++ (outputAttr as)  ++ ">"

tagEnd :: String -> String
tagEnd tn = "</" ++ tn ++ ">"

class HtmlPres x where
  toHtml :: x -> String

instance HtmlPres Element where
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
