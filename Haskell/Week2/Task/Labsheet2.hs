import Data.Char
inRange :: Int -> Int -> [Int] -> [Int]
countPositive :: [Int] -> Int
capitalised ::  String -> String
title ::  [String] -> [String]
isort :: Ord a => [a] -> [a]
merge :: Ord a => [a] -> [a] -> [a]
msort :: Ord a => [a] -> [a]
rotor :: Int -> [a] -> [a]
makeKey :: Int -> [(Char,Char)]
lookUp :: Char -> [(Char,Char)] -> Char
encipher :: Int -> Char -> Char
normalise :: String -> String
encipherStr :: Int -> String -> String
decrpyt :: String -> String

inRange x y [] = []
inRange x y (z:zs) 
    | z >= x && z <= y = z : inRange x y zs
    | otherwise = []++inRange x y zs

countPositive [] = 0
countPositive xs
    | head xs > 0 = head xs + countPositive (tail xs)
    | otherwise = 0 + countPositive (tail xs)


capitalised xs = toUpper (head xs) : lower (tail xs)
    where
        lower [] = []
        lower (head:tail) = toLower head : lower tail

title [] = []
title (head:tail)
    | null head = title tail
    | otherwise = capitalised head : lower tail
    where
        lower [] = []
        lower (head:tail)
            | length head >= 4 = capitalised head : lower tail
            | otherwise = map toLower head : lower tail

isort [] = []
isort (x:xs) = insert x (isort xs)
    where
        insert x [] = [x]
        insert x (y:ys)
            | x < y = x:y:ys
            | otherwise = y:(insert x ys)

merge xs [] = xs
merge [] ys = ys
merge xs ys
    | head xs > head ys = head ys : merge xs (tail ys)
    | otherwise = head xs : merge (tail xs) ys

msort [x] = [x]
msort [] = []
msort xs = merge (isort (msort (lHalf xs))) (isort (msort (rHalf xs)))
    where 
        lHalf ys = take ((length ys)`div`2) ys
        rHalf ys = drop ((length ys) `div` 2) ys

rotor x ys
    | x < 0 || x >= length ys = error "Invalid offset"
    | x == 0 = ys
    | otherwise = rotor (x-1) (drop 1 ys ++ [head ys])

makeKey offset = zip ['A'..'Z'] (rotor offset ['A'..'Z'])

lookUp x xs = snd(last[(z,y) | (z,y)<-xs,z == x])

encipher offset x = lookUp x (makeKey offset)

normalise xs = [toUpper x|x<- xs,x`elem`['A'..'Z'] || x`elem`['a'..'z'] || x`elem`['0'..'9']]

encipherStr offset string = [if not (x`elem`['0'..'9']) then encipher offset x else x| x<-normalise string]

decrypt cipher = 