import Data.Char
square :: Int -> Int
pyth :: Int -> Int -> Int
isTriple :: Int -> Int -> Int -> Bool
isTripleAny :: Int -> Int -> Int -> Bool
halfEvens :: [Int] -> [Int]
inRange ::  Int -> Int ->[Int] -> [Int]
countPositives :: [Int] -> Int
capitalised ::  String -> String
title ::  [String] -> [String]
square x = x * x
pyth x y = square x + square y
isTriple x y z = (pyth x y) == square z
isTripleAny x y z = (isTriple x y z) || (isTriple x z y) || (isTriple y x z) || (isTriple y z x) || (isTriple z x y) || (isTriple z y x)
halfEvens xs = [if even x then x `div` 2 else x|x <- xs]
inRange x y zs = [z |z<-zs,z>=x,z<=y]
countPositives xs = sum [1|x<-xs,x>0]
capitalised xs = [if x == head xs then toUpper x else toLower x|x<-xs]
title xss = [if (xs == head xss) || length xs >=4 then capitalised xs else map toLower xs | xs <- xss]















