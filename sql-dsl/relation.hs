data Relation = Relation String deriving Show
data Selection = Selection [String] deriving Show
data Projection = Projection [String] deriving Show

rel :: String -> Relation
rel = Relation

toSql :: Relation -> String
toSql (Relation x) = "SELECT * FROM " ++ x

{-
extract :: Relation -> Selection -> Relation
proj :: Relation -> Projection -> Relation
-}
