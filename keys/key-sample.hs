{-# LANGUAGE FlexibleInstances #-}

data Hoge = Hoge String String

class Key x where
  lock :: x -> String
  val :: x -> String

  lock x = "Key " ++ (val x) ++ " is locked."

instance Key Int where
  val x = show x

instance Key Char where
  val x = [x]

instance Key String where
  val x = x

instance Key Hoge where
  val (Hoge x y) = "[val1 : " ++ x ++ "][val2 : " ++ y ++ "]"
