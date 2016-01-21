data Selection = Selection [String] deriving Show
data Projection = Projection [String] deriving Show
data Relation = Relation String Selection Projection  deriving Show

class Semantics exp where
  toSql:: exp -> String

instance Semantics Relation where
  toSql(Relation nm rel prj) = "SELECT " ++ toSql rel ++ " FROM " ++ nm ++ toSql prj

instance Semantics Selection where
  toSql(Selection []) = "*"
  toSql(Selection (x:[])) = x
  toSql(Selection (x:xs)) = x ++ ", " ++ toSql (Selection xs)

instance Semantics Projection where
  toSql(Projection []) = ""
  toSql(Projection (x:xs)) = " WHERE " ++  x ++ foldl (\acc x -> acc ++ " AND " ++ x) "" xs

rel :: String -> Selection -> Projection -> Relation
rel x slc prj = Relation x slc prj

extract :: [String] -> Selection
extract xs = (Selection xs)

project :: [String] -> Projection
project xs = (Projection xs)

{-
toSql $ rel "hoge" (extract ["fuga = 1", "bar = 2"]) (project ["fuga, bar"])
なら動くけど、
rel "hoge" . extract ["fuga = 1", "bar = 2"] . project ["fuga, bar"]
みたいにしたい。

rel "hoge" >>= extract ["fuga = 1", "bar = 2"] >= project ["fuga, bar"]じゃないとダメ？
-}
