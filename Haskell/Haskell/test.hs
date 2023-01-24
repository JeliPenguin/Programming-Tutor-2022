lucky :: Int -> String
factorial :: Int -> Int
myCompare :: (Ord a)=> a -> a -> Ordering
lucky 7 = "Lucky"
lucky x = "Not"

factorial 0 = 1
factorial n = n * factorial (n-1)
a `myCompare` b
    --Using type class and guards (Similar to if more readable)
    |a == b = EQ
    |a <= b = LT
    |a >= b = GT