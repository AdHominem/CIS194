{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches (_:_) [] = 0
exactMatches (x:xs) (y:ys)
 | x == y = 1 + exactMatches xs ys
 | otherwise = exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\c -> length (filter (==c) code)) colors


-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y = sum $ zipWith min (countColors x) (countColors y)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exMatches $ (matches secret guess) - exMatches
 where exMatches = (exactMatches secret guess)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move mCode mExMatches mMatches) code =
 exMatches == mExMatches && matches mCode code - exMatches == mMatches
  where exMatches = (exactMatches mCode code)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (\code -> isConsistent move code) codes

-- Exercise 6 -----------------------------------------

-- Extends a given Code by prepending a Peg: extendCode [Red, Green] Blue -> [Blue, Red, Green]
extendCode :: [a] -> a -> [a]
extendCode code peg = peg : code

-- Extends a given Code by all possible Pegs, turning the Code into a list of Codes
fullyExtendCode :: [a] -> [a] -> [[a]]
fullyExtendCode colors code = map (extendCode code) colors

-- Yields the product of a single list and a two dimensional list
-- combine [1,2] [[3,4],[5,6]] = [[1,3,4],[2,3,4],[1,5,6],[2,5,6]]
combine :: [a] -> [[a]] -> [[a]]
combine choices [] = map (:[]) choices
combine choices ys = concatMap (\y -> map (:y) choices) ys

-- Applies a given function n times to a given input
nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x
  | n <= 0   = x
  |otherwise = nTimes (n - 1) f (f x)

allCodes :: Int -> [Code]
allCodes n = nTimes n (combine colors) []

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
