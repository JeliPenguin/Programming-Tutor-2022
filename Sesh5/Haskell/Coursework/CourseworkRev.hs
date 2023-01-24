{- DO NOT CHANGE MODULE NAME, if you do, the file will not load properly -}
module CourseworkRev where

import qualified Data.Set as HS (fromList, toList)
import Test.QuickCheck

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
