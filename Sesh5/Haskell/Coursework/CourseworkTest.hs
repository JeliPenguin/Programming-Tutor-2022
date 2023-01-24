{- DO NOT CHANGE MODULE NAME, if you do, the file will not load properly -}
module CourseworkRev where

import qualified Data.Set as HS (fromList, toList,powerSet,null,member,difference,singleton,intersection,union,size,delete,insert,map,foldr)
import Test.QuickCheck
import Test.HUnit

{-
  Your task is to design a datatype that represents the mathematical concept of
  a (finite) set of elements (of the same type). We have provided you with an
  interface (do not change this!) but you will need to design the datatype and
  also support the required functions over sets. Any functions you write should
  maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a
  list. Alternatively, one could use an algebraic data type, wrap a binary
  search tree, or even use a self-balancing binary search tree. Extra marks will
  be awarded for efficient implementations (a self-balancing tree will be more
  efficient than a linked list for example).

  You are **NOT** allowed to import anything from the standard library or other
  libraries. Your edit of this file should be completely self-contained.

  **DO NOT** change the type signatures of the functions below: if you do, we
  will not be able to test them and you will get 0% for that part. While sets
  are unordered collections, we have included the Ord constraint on some
  signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Everything must be in
  this file.

  See the note **ON MARKING** at the end of the file.
-}

{-
   PART 1.
   You need to define a Set datatype.
-}

-- you **MUST** change this to your own data type. The declaration of Set a =
-- Int is just to allow you to load the file into ghci without an error, it
-- cannot be used to represent a set.
data Set a =  Leaf a (Set a) (Set a) | Empty
  deriving(Show)

{-
   PART 2.
   If you do nothing else, you must get the toList, fromList and equality working. If they
   do not work properly, it is impossible to test your other functions, and you
   will fail the coursework!
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Ord a => Set a -> [a]
toList Empty = []
toList (Leaf i l r) = toList l ++ [i] ++ toList r

-- fromList: do not forget to remove duplicates!

fromList :: Ord a => [a] -> Set a
fromList [] = Empty
fromList xs = Leaf (ys !! mid) (fromList (take mid ys)) (fromList (drop (mid+1) ys))
  where
    sort [] = []
    sort(z:zs) = sort [a | a <- zs,a<z] ++ [z] ++ sort [b |b<-zs , b > z]
    ys = sort xs
    mid = length ys `div` 2

-- Make sure you satisfy this property. If it fails, then all of the functions
-- on Part 3 will also fail their tests
toFromListProp :: IO ()
toFromListProp =
  quickCheck
    ((\xs -> (HS.toList . HS.fromList $ xs) == (toList . fromList $ xs)) :: [Int] -> Bool)

-- test if two sets have the same elements (pointwise equivalent).
instance (Ord a) => Eq (Set a) where
  s1 == s2 = (toList s1 == toList s2)

-- you should be able to satisfy this property quite easily
eqProp :: IO ()
eqProp =
  quickCheck ((\xs -> (fromList . HS.toList . HS.fromList $ xs) == fromList xs) :: [Char] -> Bool)

{-
   PART 3. Your Set should contain the following functions. DO NOT CHANGE THE
   TYPE SIGNATURES.
-}

-- the empty set
empty :: Set a
empty = Empty

-- is it the empty set?
isNull :: Set a -> Bool
isNull s = cardinality s== 0

-- build a one element Set
singleton :: a -> Set a
singleton x = Leaf x Empty Empty

-- insert an element *x* of type *a* into Set *s*. Make sure there are no
-- duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = singleton x
insert x (Leaf i l r)
  | x == i = Leaf i l r
  | x < i = Leaf i (insert x l) r
  | otherwise = Leaf i l (insert x r)

-- join two Sets together be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = setfoldr insert s1 s2

-- return, as a Set, the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = setfoldr (removeSet) (setfoldr (removeSet) (union s1 s2) (difference s2 s1)) (difference s1 s2)

-- all the elements in *s1* not in *s2*
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = setfoldr (removeSet) (union s1 s2) s2

-- is element *x* in the Set s1?
member :: (Ord a) => a -> Set a -> Bool
member x Empty = False
member x (Leaf i l r)
  | x == i = True
  | x < i = member x l
  | otherwise = member x r

-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality Empty = 0
cardinality (Leaf i l r) = 1 + cardinality l + cardinality r

-- apply a function to every element in the Set
setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f Empty = Empty
setmap f (Leaf i l r) = union (insert (f i) (setmap f l)) (insert (f i) (setmap f r))

-- right fold a Set using a function *f*
setfoldr :: (a -> b -> b) -> b -> Set a -> b
setfoldr f acc Empty = acc
setfoldr f acc (Leaf i l r) = f i (setfoldr f (setfoldr f acc l) r)

-- remove an element *x* from the set
-- return the set unaltered if *x* is not present
removeSet :: (Ord a) => a -> Set a -> Set a
removeSet x Empty = Empty
removeSet x (Leaf i l r)
  | member x (Leaf i l r) = if (x == i) then union l r else if (x < i) then Leaf i (removeSet x l) r else Leaf i l (removeSet x r)
  | otherwise = (Leaf i l r)

-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)
powerSet Empty = singleton Empty
powerSet s = uni (setm (insl lm) (powerSet remainSet)) (powerSet remainSet)
  where
    setm f Empty = Empty
    setm f (Leaf i l r) = uni (insl (f i) (setm f l)) (insl (f i) (setm f r))
    uni s1 s2 = setfoldr insl s1 s2
    insl x Empty = singleton x
    insl x (Leaf i l r) = Leaf i (insl x l) r
    removel Empty = Empty
    removel (Leaf i Empty r) = r
    removel (Leaf i l r) = Leaf i (removel l) r
    leftmost (Leaf i Empty _) = i
    leftmost (Leaf i l _) = leftmost l
    lm = leftmost s
    remainSet = removel s


li = [1,2,3,4,5]
a = toList (setmap toList (powerSet (fromList li)))


powerSetProp xs = (map (HS.toList) (HS.toList (HS.powerSet (HS.fromList $ xs)))) == (toList (setmap (toList) (powerSet (fromList xs))))

insertProp x xs = (HS.toList (HS.insert x (HS.fromList xs)) == toList (insert x (fromList xs)))

toFromListProp' xs = (HS.toList (HS.fromList xs) == (toList (fromList xs)))

removeSetProp x xs = (HS.toList (HS.delete x (HS.fromList xs)) == toList (removeSet x (fromList xs)))

cardinalityProp xs = (HS.size (HS.fromList xs)) == (cardinality (fromList xs))

unionProp xs ys = (HS.toList (HS.union (HS.fromList xs) (HS.fromList ys))) == (toList (union (fromList xs) (fromList ys)))

intersectionProp xs ys = (HS.toList (HS.intersection (HS.fromList xs) (HS.fromList ys))) == (toList (intersection (fromList xs) (fromList ys)))

singletonProp x = (HS.toList (HS.singleton x)) == (toList (singleton x))

differenceProp xs ys = (HS.toList (HS.difference (HS.fromList xs) (HS.fromList ys))) == (toList (difference (fromList xs) (fromList ys)))

memberProp x xs = (HS.member x (HS.fromList xs)) == (member x (fromList xs))

isNullProp xs = HS.null (HS.fromList xs) == isNull (fromList xs)

setmapProp1 xs = (HS.toList (HS.map (+1) (HS.fromList xs)) == toList (setmap (+1) (fromList xs)))

setmapProp2 xs = (HS.toList (HS.map (*(-3)) (HS.fromList xs)) == toList (setmap (*(-3)) (fromList xs)))

setmapProp3 xs = (HS.toList (HS.map (>0) (HS.fromList xs)) == toList (setmap (>0) (fromList xs)))

setfoldrProp1 xs = HS.foldr (+) 0 (HS.fromList xs) == setfoldr (+) 0 (fromList xs)

setfoldrProp2 xs = HS.foldr (&&) True (HS.fromList xs) == setfoldr (&&) True (fromList xs)

setfoldrProp3 xs = HS.foldr max 18 (HS.fromList xs) == setfoldr max 18 (fromList xs)

powerProp =
  quickCheck
    ((\xs ->(map  HS.toList (HS.toList(HS.powerSet (HS.fromList $ xs)  ))) == (toList (setmap toList(powerSet (fromList $ xs) )))) :: [Int] -> Bool)
{-
   ON MARKING:

   Be careful! This coursework will be marked using QuickCheck, against
   Haskell's own Data.Set implementation. This testing will be conducted
   automatically via a marking script that tests for equivalence between your
   output and Data.Set's output. There is no room for discussion, a failing test
   means that your function does not work properly: you do not know better than
   QuickCheck and Data.Set! Even one failing test means 0 marks for that
   function. Changing the interface by renaming functions, deleting functions,
   or changing the type of a function will cause the script to fail to load in
   the test harness. This requires manual adjustment by a TA: each manual
   adjustment will lose 10% from your score. If you do not want to/cannot
   implement a function, leave it as it is in the file (with undefined).

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough
   for a passing mark of 40%, as long as both toList and fromList satisfy the
   toFromListProp function.

   The maximum mark for those who use Haskell lists to represent a Set is 70%.
   To achieve a higher grade than is, one must write a more efficient
   implementation. 100% is reserved for those brave few who write their own
   self-balancing binary tree.
-}


a1 = (insert 21 (insert 20 (insert 2 (singleton 3))))
a2 = (insert 11 (insert 12345 (insert 1 (insert 5 (insert 2 (singleton 7))))))
a3 = (insert 7 (insert 11 (insert 1 (insert 11 (insert 12345 (insert 1 (insert 5 (insert 2 (singleton 7)))))))))
a4 = (insert 1 (insert 2 (singleton 3)))

test1 = TestCase (assertEqual "empty test 1" False (member 3 empty))
test2 = TestCase (assertEqual "singleton test 1" True (member 3 (singleton 3)))
test3 = TestCase (assertEqual "singleton test 2" [7] (toList (singleton 7)))
test4 = TestCase (assertEqual "singleton test 3" False (member 3 (singleton 7)))
test5 = TestCase (assertEqual "member test 1" True (member 3 a1))
test6 = TestCase (assertEqual "member test 2" False (member 7 a1))
test7 = TestCase (assertEqual "member test 3" True (member 7 a3))
test8 = TestCase (assertEqual "toList test 1" [2,3,20,21] (toList a1))
test9 = TestCase (assertEqual "toList test 2" [1,2,5,7,11,12345] (toList a2))
test10 = TestCase (assertEqual "toList test 3" [1,2,5,7,11,12345] (toList a3))
test11 = TestCase (assertEqual "union test 1" [1,2,3,5,7,11,20,21,12345] (toList (union a1 a3)))
test12 = TestCase (assertEqual "union test 2" [1,2,5,7,11,12345] (toList (union a2 a2)))
test13 = TestCase (assertEqual "union test 3" [1,2,5,7,11,321,12345] (toList (union a2 (singleton 321))))
test14 = TestCase (assertEqual "intersection test 1" [1,2,5,7,11,12345] (toList (intersection a2 a3)))
test15 = TestCase (assertEqual "intersection test 2" [2] (toList (intersection a1 a2)))
test16 = TestCase (assertEqual "intersection test 3" [2,3,20,21] (toList (intersection a1 a1)))
test17 = TestCase (assertEqual "difference test 1" [] (toList (difference a2 a3)))
test18 = TestCase (assertEqual "difference test 2" [3,20,21] (toList (difference a1 a2)))
test19 = TestCase (assertEqual "difference test 3" [] (toList (difference a1 a1)))
test20 = TestCase (assertEqual "fromList test 1" True (member 3 (fromList [1,2,3,4,5])))
test21 = TestCase (assertEqual "fromList test 2" [1,2,3,4,7] (toList (union (fromList [1,2,3]) (fromList [3,4,7]))))
test22 = TestCase (assertEqual "fromList test 3" [7] (toList (intersection (fromList [1,2,7]) (fromList [7,8,9]))))
test23 = TestCase (assertEqual "isNull test 1" True (isNull (empty)))
test24 = TestCase (assertEqual "isNull test 2" True (isNull (difference a2 a3)))
test25 = TestCase (assertEqual "isNull test 3" False (isNull (intersection (fromList [1,2,7]) (fromList [7,8,9]))))
test26 = TestCase (assertEqual "insert test 1" True (member 20 (insert 20 (fromList [1,2,7]))))
test27 = TestCase (assertEqual "insert test 2" [7,8,9,42] (toList (insert 42 (fromList [7,8,9]))))
test28 = TestCase (assertEqual "insert test 3" False (isNull (insert 42 empty)))
test29 = TestCase (assertEqual "cardinality test 1" 0 (cardinality empty))
test30 = TestCase (assertEqual "cardinality test 2" 6 (cardinality a3))
test31 = TestCase (assertEqual "cardinality test 3" 7 (cardinality (fromList [1,2,3,17,18,19,42])))
test32 = TestCase (assertEqual "setmap test 1" [1] (toList (setmap (\_ -> 1) a1)))
test33 = TestCase (assertEqual "setmap test 2" [3,4,21,22] (toList (setmap (+1) a1)))
test34 = TestCase (assertEqual "setmap test 3" [True] (toList (setmap (\x -> member x a1) a1)))
test35 = TestCase (assertEqual "setfoldr test 1" 12371 (setfoldr (+) 0 a3))
test36 = TestCase (assertEqual "setfoldr test 2" 460 (setfoldr (\x y -> y + 10*x) 0 a1))
test37 = TestCase (assertEqual "setfoldr test 3" True (setfoldr (\x y -> y && member x a1) True a1))
test38 = TestCase (assertEqual "equality operator test 1" True (a2 == a3))
test39 = TestCase (assertEqual "equality operator test 2" False (a1 == a3))
test40 = TestCase (assertEqual "equality operator test 3" True (a2 == (fromList [7, 5, 12345, 2, 1, 11])))
test41 = TestCase (assertEqual "removeSet test 1" False (member 3 (removeSet 3 a1)))

tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test3" test3,
                  TestLabel "test4" test4,
                  TestLabel "test5" test5,
                  TestLabel "test6" test6,
                  TestLabel "test7" test7,
                  TestLabel "test8" test8,
                  TestLabel "test9" test9,
                  TestLabel "test10" test10,
                  TestLabel "test10" test11,
                  TestLabel "test12" test12,
                  TestLabel "test13" test13,
                  TestLabel "test14" test14,
                  TestLabel "test15" test15,
                  TestLabel "test16" test16,
                  TestLabel "test17" test17,
                  TestLabel "test18" test18,
                  TestLabel "test19" test19,
                  TestLabel "test20" test20,
                  TestLabel "test21" test21,
                  TestLabel "test22" test22,
                  TestLabel "test23" test23,
                  TestLabel "test24" test24,
                  TestLabel "test25" test25,
                  TestLabel "test26" test26,
                  TestLabel "test27" test27,
                  TestLabel "test28" test28,
                  TestLabel "test29" test29,
                  TestLabel "test30" test30,
                  TestLabel "test31" test31,
                  TestLabel "test32" test32,
                  TestLabel "test33" test33,
                  TestLabel "test34" test34,
                  TestLabel "test35" test35,
                  TestLabel "test36" test36,
                  TestLabel "test37" test37,
                  TestLabel "test38" test38,
                  TestLabel "test39" test39,
                  TestLabel "test40" test40,
                  TestLabel "test41" test41]

run = runTestTT tests