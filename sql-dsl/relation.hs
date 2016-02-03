data Name = Name (Maybe String) deriving Show
data Selection = Selection [String] deriving Show
data Projection = Projection [String] deriving Show
data Relation = Relation Name Selection Projection  deriving Show

relation :: String ->  Relation -> Maybe Relation
relation nm (Relation _ slc prj) = Just (Relation (Name (Just nm)) slc prj)

extract :: [String] -> Relation -> Maybe Relation
extract slc (Relation nm _ prj) = Just (Relation nm (Selection slc) prj)

projection :: [String] ->  Relation -> Maybe Relation
projection prj (Relation nm slc _) = Just (Relation nm slc (Projection prj))

class Sql a where
  toSql :: a -> String

instance Sql Relation where
  toSql(Relation nm rel prj) = "SELECT " ++ toSql rel ++ toSql nm ++ toSql prj

instance Sql Name where
  toSql (Name Nothing) = ""
  toSql (Name (Just x)) = " FROM " ++ x

instance Sql Selection where
  toSql(Selection []) = "*"
  toSql(Selection (x:[])) = x
  toSql(Selection (x:xs)) = x ++ ", " ++ toSql (Selection xs)

instance Sql Projection where
  toSql(Projection []) = ""
  toSql(Projection (x:xs)) = " WHERE " ++  x ++ foldl (\acc x -> acc ++ " AND " ++ x) "" xs

rel :: String -> Maybe Relation
rel x  = Just (Relation (Name (Just x)) (Selection []) (Projection []))

{-
rel "hoge" >>= extract ["fuga = 1", "bar = 2"] >= projection ["fuga, bar"]
今度は toSqlできない・・・
-}
