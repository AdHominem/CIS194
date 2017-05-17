module Lecture where

import Data.List

-- Enumeration Type:
data Thing = Shoe
    | Ship
    | SealingWax
    | Cabbage
    | King
    deriving Show

-- Algebraic Data Type:
data FailableDouble = Failure
    | OK Double
    deriving Show

-- Type Constructor and Value Constructor can have the same name:
data Person = Person String Int Thing
    deriving Show

-- Accessors without boilerplate code
data Customer = Customer {
      customerID      :: Int
    , customerName    :: String
    , customerAddress :: String
    } deriving (Show)

-- Exercise: Proof that Haskell lists are isomorphic to the following implementation
data List a = Cons a (List a)
    | Nil
    deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList Nil = []
toList (Cons x xs) = x : toList xs

{- Exercise: Binary Tree using Maybe instead of Empty
    data Tree a = (Maybe (Tree a)) Node a (Maybe (Tree a))
    first solution does not allow empty trees!
    Solution is a Node which maybe contains a value and two Trees, each of which could be either empty or full
 -}
data MyTree a = MyNode (Maybe (a, MyTree a, MyTree a)) deriving Show

-- Safe total function without throwing errors:
tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing

-- Local variables with let...in:
lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance

-- ... and with where:
lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
    where reserve    = 100
          newBalance = balance - amount

-- Local functions:
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"

-- Global Variable:
itemName = "Weighted Companion Cube"

-- Exercises:
myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength []    = 0

mean :: (Fractional a) => [a] -> a
mean xs = sum xs / realToFrac (length xs)

palindromize :: [a] -> [a]
palindromize xs = xs ++ reverse xs

palindromizeNative :: [a] -> [a]
palindromizeNative [] = []
palindromizeNative (x:xs) = x : palindromizeNative xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Without compare
compareSublists :: (Ord a, Ord b) => [a] -> [b] -> Ordering
compareSublists xs ys
        | lenxs < lenys = LT
        | lenys < lenxs = GT
        | otherwise = EQ
        where lenxs = length(xs)
              lenys = length(ys)

sortBySublistLength :: (Ord n) => [[n]] -> [[n]]
sortBySublistLength xs = sortBy compareSublists xs

-- Superior solution
sortBySublistLengthSup :: (Ord n) => [[n]] -> [[n]]
sortBySublistLengthSup n = sortBy listLength n
    where listLength [] [] = EQ
          listLength _ [] = GT
          listLength [] _ = LT
          listLength (_:xs) (_:ys) = listLength xs ys

intersperse2 :: a -> [[a]] -> [a]
intersperse2 _ [] = []
intersperse2 _ [ys] = ys
intersperse2 x (y:ys) = y ++ [x] ++ intersperse2 x ys

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

left = Node 10 Empty Empty
rightRight = Node 60 Empty Empty
right = Node 50 Empty rightRight
root = Node 42 left right

height :: Tree a -> Int
height Empty                = 0
height (Node _ Empty Empty) = 0
height (Node _ ltree rtree) = 1 + max (height ltree) (height rtree)

'