{-# LANGUAGE OverloadedStrings #-}

module Domain.Name(Name,parseName) where

import Data.Validation
import qualified Data.Text as T

type Name = String

maxLen :: Int
maxLen = 20

reqMsg :: String
reqMsg = "名前は必須です。"

lenMsg :: String
lenMsg = "名前は" ++ "文字以内で入力してださい。"

parseName :: String -> Validation [String] Name
parseName v = vne v >>= vlen

vne :: String -> Validation [String] String
vne v = if v == ""
          then Failure [reqMsg]
          else Success v

vlen :: String -> Validation [String] Name
vlen v = if maxLen < (T.length $ T.pack v)
           then Failure [lenMsg]
           else Success v

