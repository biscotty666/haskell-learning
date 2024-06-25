-- data Shape = Circle Float Float Float
           -- | Rectangle Float Float Float Float
             -- deriving (Show)

-- First try
-- surface :: Shape -> Float
-- surface (Circle _ _ r)          = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float
           | Rectangle Point Point
             deriving (Show)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
    Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

baseCircle :: Float -> Shape
baseCircle radius = Circle (Point 0 0) radius

data Person = Person {  firstName   :: String
                      , lastName    :: String
                      , age         :: Int
                      , height      :: Float
                      , phoneNumber :: String
                      , flavor      :: String
                      } deriving (Show)

data Car = Car { company :: String
               , model   :: String
               , year    :: Int } deriving (Show)
