{-# LANGUAGE FlexibleInstances #-}

module Relation where

import Data.List

data Heading = Heading String deriving Show
{- data Body = Body [Tulple] deriving Show -}

data Relation = Relation String [Heading] [(String, String)] {- [Body] -} deriving Show
data Relations = Relations [Relation] deriving Show

class RelFunc x where
  str :: String -> x
  relation :: (() -> x) -> (() -> [Heading]) -> (() -> [(String, String)]) -> x
  projection :: [Heading] -> x -> x
  selection :: [(String, String)] -> x -> x

  relation rfn pfn sfn = selection (sfn()) $ projection (pfn()) (rfn())

{-
instance RelFunc Relations where
  str x = Relations [(Relation x (toHeading ["*"]) [])]
  projection y (Relation x _ z) = Relation x y z
  selection z (Relation x y _) = Relation x y z
-}

instance RelFunc Relation where
  str x = Relation x (toHeading ["*"]) []
  projection y (Relation x _ z) = Relation x y z
  selection z (Relation x y _) = Relation x y z

class Sql x where
  toSql :: x -> String

instance Sql Relation where
  toSql (Relation nm hd slc) = (toSql hd) ++ " FROM " ++ nm ++ (toSql slc)

instance Sql [(String, String)] where
  toSql [] = ""
  toSql (x:xs) = " WHERE " ++ (toSql x) ++ foldl (\acc y -> acc ++ " AND " ++ (toSql y)) "" xs

instance Sql (String, String) where
  toSql (x, y) = x ++ " = " ++ y

instance Sql [Heading] where
  toSql [] = ""
  toSql (x:xs) = "SELECT " ++  (toSql x) ++  foldl (\acc y -> acc ++ "," ++ (toSql y)) "" xs

instance Sql Heading where
  toSql (Heading x) = x
 
toHeading :: [String] -> [Heading]
toHeading xs = map (Heading) xs

toCondition :: [String] -> [(String, String)]
toCondition xs = map split xs

split :: String -> (String, String)
split x = case (elemIndex '=' x) of
  Just n -> (fst $ splitAt n x, snd $ splitAt (n + 1) x)
  Nothing -> ("", "")
