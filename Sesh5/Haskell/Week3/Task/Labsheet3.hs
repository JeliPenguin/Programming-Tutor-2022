mult :: (Num a)=>[a]->a
posList :: [Int] -> [Int]
trueList :: [Bool] -> Bool
evenList :: [Int]->Bool
--maxList :: (Ord a)=>[a]->a
inRange ::  Int -> Int -> [Int] -> [Int]
countPositives :: [Int] -> Int
myLength :: [a] -> Int

mult xs = foldr (*) 1 xs

posList xs = filter (>0) xs

trueList xs = foldr (&&) True xs

evenList xs = trueList (map even xs)

--maxList

inRange x y zs
    | x > y = filter (\compare-> compare >=y && compare <= x) zs
    | otherwise = filter (\compare-> compare >=x && compare <= y) zs

countPositives xs = foldr (+) 0 (posList xs)

myLength xs = foldr (+) 0 (map (\element->1) xs)

map f xs = 