module Tool.NumTool(isNum) where

import Data.Char as DC

isNum :: String -> Bool
isNum []     = False
isNum (v:[]) = isDigit v
isNum (v:vs) = if isDigit v then isNum vs else False
