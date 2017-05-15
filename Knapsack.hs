module Knapsack where

import Data.List
import System.Random
import Text.Read
import Data.Function (on)
import Data.Ord

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

getRandomNumbers :: (Random a, RandomGen g, Num a) => g -> Int -> [a]
getRandomNumbers gen n = take n (randomRs (1, 10) gen)

generateItems :: [Int] -> [Item]
generateItems [] = []
generateItems [x] = []
generateItems (x:y:xs) = (Item x y) : generateItems xs

getLineInt :: IO Int
getLineInt = do
  line <- getLine
  case readMaybe line of
    Just x | x >= 0 -> return x
           | otherwise -> putStrLn "That number is too low!" >> getLineInt
    Nothing -> putStrLn "Please enter a valid number!" >> getLineInt

getWeight :: [Item] -> Int
getWeight items = sum (map weight items)

fillKnapsack :: [Item] -> [Item] -> Int -> [Item]
fillKnapsack [] sack _ = sack
fillKnapsack (i:is) sack limit = if weight i <= limit - getWeight sack
    then fillKnapsack is (i : sack) limit
    else fillKnapsack is sack limit

generateSuccessMessage :: [Item] -> Int -> String
generateSuccessMessage [] limit = "Arr! Our theft is forfeit! Next time we better take a bigger sack with us!"
generateSuccessMessage sack limit = if limit == getWeight sack
    then "Awesome, we filled the whole bag! " ++ base
    else "Nice! " ++ base
    where
        base = "We got " ++ show (length sack) ++ " items totaling " ++ show (sum (map value sack)) ++ "€!"

main :: IO ()
main = do
    putStrLn "Please enter the amount of items to generate: "
    itemCount <- getLineInt

    putStrLn $ "\nGenerating " ++ (show itemCount) ++ " items:"
    gen <- getStdGen
    let nums = getRandomNumbers gen (itemCount * 2)
    let items = sort (generateItems nums)
    mapM_ print items

    putStrLn "\nPlease enter the maximum weight: "
    weightLimit <- getLineInt

    putStrLn $ "\nThe limit is set to " ++ (show weightLimit) ++ "kg."

    let knapsack = [] :: [Item]
    putStrLn "\nThis is the sack: "
    let filledSack = fillKnapsack items knapsack weightLimit
    mapM_ print filledSack
    putStrLn $ generateSuccessMessage filledSack weightLimit



