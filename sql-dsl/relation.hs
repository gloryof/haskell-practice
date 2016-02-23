{-# LANGUAGE FlexibleInstances #-}

type Name = String

data Projection = Projection [String] deriving Show
data Selection = Selection [String] deriving Show
data Single = Single Name Projection Selection deriving Show

class Relation x where
  relation :: String -> x
  projection :: Projection -> x ->  x
  selection :: Selection -> x -> x

instance Relation Single where
  relation x = Single x (Projection []) (Selection [])
  projection y (Single x _ z) = Single x y z
  selection  z (Single x y _) = Single x y z

class Sql x where
  toSql :: x -> String

instance Sql Single where
  toSql (Single nm prj slc) = "SELECT " ++ (toSql slc) ++ (toSql nm) ++ (toSql prj)

instance Sql Name where
  toSql x = " FROM " ++ x

instance Sql Projection where
  toSql (Projection []) = ""
  toSql (Projection (x:xs)) = " WHERE " ++ x ++ foldl (\acc y -> acc ++ " AND " ++ y) "" xs

instance Sql Selection where
  toSql (Selection []) = "*"
  toSql (Selection (x:[])) = x
  toSql (Selection (x:xs)) = x ++ foldl  (\acc y -> acc ++ ", " ++ y) "" xs

{-
(relation "test")::Single

(projection (Projection ["hoge = 1"]) $ relation "test")::Single

(selection (Selection ["fuga"]) $ projection (Projection ["hoge = 1"]) $ relation "test")::Single

-}
