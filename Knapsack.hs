module Knapsack where

import Data.List
import System.Random
import Text.Read
import Data.Ord

{-
    This module implements a Knapsack Problem solver using an approximate
    solution which is more effective on large input sizes
-}

-- | Each item consists of an integer value and weight
data Item = Item {
      value        :: Int
    , weight       :: Int
    } deriving (Eq)

instance Ord Item where
    compare = flip (comparing getIndex)

instance Show Item where
   show (Item value weight) = "Item of " ++ show value ++ "€ weighting " ++ show weight ++ "kg."

getIndex :: Item -> Double
getIndex item = fromIntegral (value item) / fromIntegral (weight item)

-- | Generates a list of random numbers which can later be used to create random Items
getRandomNumbers :: (Random a, RandomGen g, Num a) => g -> Int -> [a]
getRandomNumbers gen n = take n (randomRs (1, 10) gen)

-- | Turns a list of integers into a list of Items which has half the size, since every Item consumes two integers
generateItems :: [Int] -> [Item]
generateItems [] = []
generateItems [x] = []
generateItems (x:y:xs) = (Item x y) : generateItems xs

-- | Reads a line from stdout and ensures it is a positive integer
getLineInt :: IO Int
getLineInt = do
  line <- getLine
  case readMaybe line of
    Just x | x >= 0 -> return x
           | otherwise -> putStrLn "The integer must be positive!" >> getLineInt
    Nothing -> putStrLn "Please enter a positive integer!" >> getLineInt

-- | Returns the added weight of all Items in a list
getWeight :: [Item] -> Int
getWeight items = sum (map weight items)

{-|
    Attempts to fill the knapsack with the supplied items, which are expected to be sorted by profitability
    The list of items is processed descending and items which are too heavy are removed, until the list is empty.
-}
fillKnapsack :: [Item] -> [Item] -> Int -> [Item]
fillKnapsack [] sack _ = sack
fillKnapsack (i:is) sack limit = if weight i <= limit - getWeight sack
    then fillKnapsack is (i : sack) limit
    else fillKnapsack is sack limit

-- | Creates a message indicating the success of the program
generateSuccessMessage :: [Item] -> Int -> String
generateSuccessMessage [] limit = "Arr! Our theft is forfeit! Next time we better take a bigger sack with us!"
generateSuccessMessage sack limit = if limit == getWeight sack
    then "Awesome, we filled the whole bag! " ++ base
    else "Nice! " ++ base
    where
        base = "We got " ++ show (length sack) ++ " items totaling " ++ show (sum (map value sack)) ++ "€!"

main :: IO ()
main = do
    putStrLn "Welcome to the Knapsack Problem solver!"

    putStrLn "Please enter the amount of items to generate: "
    itemCount <- getLineInt

    putStrLn $ "\nGenerating " ++ (show itemCount) ++ " items:"
    gen <- getStdGen
    let items = sort (generateItems (getRandomNumbers gen (itemCount * 2)))
    mapM_ print items

    putStrLn "\nPlease enter the maximum weight: "
    weightLimit <- getLineInt

    putStrLn $ "\nThe limit is set to " ++ (show weightLimit) ++ "kg."

    let knapsack = [] :: [Item]
    putStrLn "\nThis is the sack: "
    let filledSack = fillKnapsack items knapsack weightLimit
    mapM_ print filledSack
    putStrLn $ generateSuccessMessage filledSack weightLimit



