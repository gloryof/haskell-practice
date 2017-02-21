{-# LANGUAGE OverloadedStrings #-}

module Domain.Age(Age, parseAge) where

import Tool.NumTool
import Data.Validation

type Age = Int

minAge :: Int
minAge = 0

maxAge :: Int
maxAge = 200

typMsg :: String
typMsg = "年齢には数字を入力してください。"

ranMsg :: String
ranMsg = "年齢は" ++ (show minAge) ++ "-" ++ (show maxAge) ++ "の間で入力してください。"

parseAge :: String -> Validation [String] Age
parseAge v = case vnu v of
            Failure msg -> Failure msg
            Success v2  -> vrn $ (read v2 :: Int)

vnu :: String -> Validation [String] String
vnu v = if isNum v == False
          then Failure [typMsg]
          else Success v

vrn :: Int -> Validation [String] Age
vrn v = if v < minAge || maxAge < v
          then Failure [ranMsg]
          else Success v
