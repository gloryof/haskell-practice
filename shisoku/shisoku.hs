data Square = Square Integer Integer deriving Show

class Exp x where
  exec :: x ->  (x -> x) -> x

class Shisoku x where
  tasu :: x -> x -> x
  hiku :: x -> x -> x
  kake :: x -> x -> x
  waru :: x -> x -> x

instance Exp Integer where
  exec x fn = fn x

instance Shisoku Integer where
  tasu x y = x + y
  hiku x y = x - y
  kake x y = x * y
  waru x y = x `div` y

instance Shisoku Square where
  tasu (Square x1 y1) (Square x2 y2) = Square (tasu x1 x2) (tasu y1 y2)
  hiku (Square x1 y1) (Square x2 y2) = Square (hiku x1 x2) (hiku y1 y2)
  kake (Square x1 y1) (Square x2 y2) = Square (kake x1 x2) (kake y1 y2)
  waru (Square x1 y1) (Square x2 y2) = Square (waru x1 x2) (waru y1 y2)
