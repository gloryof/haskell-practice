{-# LANGUAGE FlexibleInstances #-}

type Name = String

data Projection = Projection [String] deriving Show
data Selection = Selection [String] deriving Show
data Single = Single Name Projection Selection deriving Show

class Relation x where
  projection :: x -> Projection -> x
  selection :: x -> Selection -> x

instance Relation Single where
  projection (Single x _ z) y = Single x y z
  selection  (Single x y _) z = Single x y z

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

single :: String -> Single
single x = Single x (Projection []) (Selection [])

{-
toSql $ selection  (projection  (single "test") (Projection ["hoge = 1", "fuga = 1"])) (Selection ["fuga", "hoge"])
これは辛い・・・
-}
