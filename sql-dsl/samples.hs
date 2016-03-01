import Relation

pattern1 _ = 
  (relation
   (\n -> str "TEST")
   (\n -> Projection ["fuga = 0"])
   (\n -> Selection ["hoge"])
  )

pattern2 _ = 
  (relation
   (\n -> pattern1 ())
   (\n -> Projection ["fuga = 2"])
   (\n -> Selection ["gefu"])
  )::Single
