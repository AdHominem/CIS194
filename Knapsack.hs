module Knapsack where

import Data.List
import System.Random
import Text.Read
import Data.Ord

{-
    This module implements a Knapsack Problem solver using an approximate
    solution which is more effective on large input sizes, using the
    profitability index (value / weight)
-}

-- | Each item consists of an integer value and weight
data Item = Item {
      attribute :: String
    , name      :: String
    , value     :: Int
    , weight    :: Int
    } deriving (Eq)

-- | Items are sorted by profitability index by default
instance Ord Item where
    compare = flip (comparing getIndex)

instance Show Item where
   show item = attribute item ++ " " ++ name item ++ " worth " ++ show (value item) ++ "€, weighting " ++ show (weight item) ++ "kg."

-- | The profitability index is the quotient of value and weight
getIndex :: Item -> Double
getIndex item = fromIntegral (value item) / fromIntegral (weight item)

attributes = ["Red", "Blue", "Green", "Yellow", "Orange", "Violet", "Purple", "Black", "White", "Magic", "Heavy",
    "Wizardly", "Occult", "Magical", "Functional", "Pure", "Enchanted", "Glowing", "Nuclear", "Atomic", "Holy"]
things = ["Shirt", "Socks", "Pants", "Skirt", "Trousers", "Umbrella", "Blanket", "Toothbrush", "First Aid Kit",
    "Camera", "Dog", "Cat", "Laptop", "Opium Pipe", "Lizard", "Copy of SICP", "Towel", "Whiskey", "Compiler",
    "Keyboard", "Two-handed Hammer", "Fuel Cell", "Hand Grenade", "Cannabis", "Mage Cloak", "Cigar",
    "Bottle of Alcohol"]

{-|
    Generates a list of random Items in a given range
    Note that randomR returns a tuple containing both the generated value as well as the new generator,
    thus to access the elements separately fst and snd is used. Inside the where block, the generator is
    passed from one call to another and eventually used in the recursive call of generateItems
-}
generateItems :: (RandomGen g) => g -> Int -> (Int, Int) -> [Item]
generateItems generator n range = if n == 0
    then []
    else (Item (attributes !! fst colorNum) (things !! fst thingNum) (fst value) (fst weight)) : generateItems (snd thingNum) (n - 1) range
    where
        value = randomR range generator
        weight = randomR range (snd value)
        colorNum = randomR (1, length attributes - 1) (snd weight)
        thingNum = randomR (1, length things - 1) (snd colorNum)

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
generateSuccessMessage sack limit = if limit == weight
    then "Awesome, we filled the whole bag! " ++ base
    else "Nice! We used " ++ show weight ++ "/" ++ show limit ++ "kg!" ++ base
    where
        weight = getWeight sack
        base = "We got " ++ show (length sack) ++ " items totaling " ++ show (sum (map value sack)) ++ "€!"

main :: IO ()
main = do
    putStrLn "Welcome to the Knapsack Problem solver!"

    putStrLn "Please enter the amount of items to generate: "
    itemCount <- getLineInt

    putStrLn "Please enter the minimum number for weights and values: "
    minNum <- getLineInt

    putStrLn "Please enter the maximum number for weights and values: "
    maxNum <- getLineInt

    putStrLn $ "\nGenerating " ++ show itemCount ++ " items:"
    gen <- getStdGen
    let items = sort (generateItems gen itemCount (minNum, maxNum))

    if itemCount >= 100000
    then putStrLn "Not listing the items because it's just too much! ;-)"
    else mapM_ print items

    putStrLn "\nPlease enter the maximum weight: "
    weightLimit <- getLineInt

    putStrLn $ "\nThe limit is set to " ++ show weightLimit ++ "kg."

    let knapsack = [] :: [Item]
    putStrLn "\nFilling the sack... this might take some time:"
    let filledSack = fillKnapsack items knapsack weightLimit

    if length filledSack >= 100000
    then putStrLn "Not listing the collected items because it's just too much! ;-)"
    else mapM_ print filledSack

    putStrLn $ generateSuccessMessage filledSack weightLimit