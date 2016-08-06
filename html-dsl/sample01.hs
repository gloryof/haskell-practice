import Html
import HtmlPres


import Data.Map as Map (Map,insert,empty,lookup,fromList)

test _ = let attr = [(sCls ["hoge", "fuga"]), ("id", "test-dom")]
             elm = [(text "test"), (spn []"test2")]
         in dv attr elm

test2 _ = Html {
    hd = [title "test-title"],
    body = [(test())]
  }
