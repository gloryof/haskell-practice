import Relation

pattern1 _ =
  (relation
   (\n -> str "test")
   (\n -> toHeading ["*"])
   (\n -> toCondition [])
  )

pattern2 _ =
  (relation
   (\n -> pattern1())
   (\n -> toHeading ["fuga"])
   (\n -> toCondition ["gefu=2"])
  )::Relation

pattern3 _ =
  (relation
   (\n -> str "t1")
   (\n -> toHeading ["c1", "c2", "c3"])
   (\n -> toCondition ["c1=2","c2='test'"])
  )::Relation

pattern4 _ =
  prod (pattern1()) (pattern3())
