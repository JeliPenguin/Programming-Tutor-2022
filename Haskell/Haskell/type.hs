

data Point = Point Float Float deriving (Show)
-- Circle and Rectangle are value constructors which are functions
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)

area :: Shape -> Float

area (Circle _  r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Value constructors can be mapped: map (Circle 10 20) [4,5,6,7] partially apply and so on

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point(x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = 
    Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

--record syntax

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String} deriving (Show)