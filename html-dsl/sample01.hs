import Html

import Data.Map as Map (Map,insert,empty,lookup,fromList)

test _ = let attr = [("class", ["hoge", "fuga"]), ("id", ["test-dom"])]
             elm = [(text "test"), (spn []"test2")]
         in dv attr elm
