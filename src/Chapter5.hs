module Chapter5 where

--data Shape
--  = Circle Float Float Float
--  | Rectangle Float Float Float Float
--  deriving (Show)
--
--area :: Shape -> Float
--area (Circle _ _ r) = pi * r ^ 2
--area (Rectangle x1 x2 y1 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
--
--

data Point = Point Float Float deriving (Show)

data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String
  }
  deriving (Show)

personNew = show simple ++ "\n" ++ show normal
  where
    simple = Person "hoge" "fuga" 17 180 "xxxxxxxxxxxxx"
    normal = Person {firstName = "hoge", lastName = "fuga", age = 17, height = 180.0, phoneNumber = "xxxxxxxxxxxxx"}
