import Relation

pattern1 _ = 
  (relation
   (\n -> str "TEST")
   (\n -> toHeading ["*"])
   (\n -> toCondition [])
  )

pattern2 _ = 
  (relation
   (\n -> pattern1 ())
   (\n -> toHeading ["fuga"])
   (\n -> toCondition ["gefu=2"])
  )::Relation
