module Knapsack where

import Data.List
import System.Random
import Text.Read
import Data.Ord
import Control.Concurrent


{-
    This module implements a Knapsack Problem solver using an approximate
    solution which is more effective on large input sizes, using the
    profitability index (value / weight)
-}


-- | Each item has a dedicated name, an integer value and a weight.
data Item = Item {
      attribute :: String
    , name      :: String
    , value     :: Int
    , weight    :: Int
    } deriving (Eq)


-- | They can be displayed...
instance Show Item where
   show item = attribute item ++ " " ++ name item ++ " worth " ++ show (value item) ++ "€, weighting "
    ++ show (weight item) ++ "kg."


-- | ...and compared. Items are sorted by profitability index by default
instance Ord Item where
    compare = flip (comparing getIndex)


-- | The profitability index is the quotient of value and weight
getIndex :: Item -> Double
getIndex (Item _ _ value weight) = fromIntegral value / fromIntegral weight


{-|
    Generates a list of random Items in a given range.
    Note that randomR returns a tuple containing both the generated value as well as the new generator which is
    passed from one call to another and eventually used in the recursive call of generateItems
-}
generateItems :: (RandomGen g) => g -> Int -> (Int, Int) -> [Item]
generateItems gen n range
    | n == 0    = []
    | otherwise = (Item (attributes !! attributeNum) (things !! thingNum) value weight)
        : generateItems gen'''' (n - 1) range
    where
        (value, gen') = randomR range gen
        (weight, gen'') = randomR range gen'
        (attributeNum, gen''') = randomR (1, length attributes - 1) gen''
        (thingNum, gen'''') = randomR (1, length things - 1) gen'''


-- | Reads a line from stdout and ensures it is an integer larger or equal than a given number
readIntLargerThan :: Int -> IO Int
readIntLargerThan n = do
    line <- getLine
    case readMaybe line of
        Just x | x >= n     -> return x
               | otherwise  -> putStrLn ("The integer must be bigger or equal " ++ show n ++ "!") >> readIntLargerThan n
        Nothing             -> putStrLn "Please enter an integer!" >> readIntLargerThan n


-- | Returns the added weight of all Items in a list
getWeight :: [Item] -> Int
getWeight items = sum (map weight items)


{-|
    Attempts to fill the knapsack with the supplied items, which are expected to be sorted by profitability
    The list of items is processed descending and items which are too heavy are removed, until the list is empty.
-}
fillKnapsack :: [Item] -> [Item] -> Int -> [Item]
fillKnapsack [] sack _ = sack
fillKnapsack (i:is) sack limit
    | weight i <= limit - getWeight sack = fillKnapsack is (i : sack) limit
    | otherwise                          = fillKnapsack is sack limit


-- | Creates a message indicating the success of the program
generateSuccessMessage :: [Item] -> Int -> String
generateSuccessMessage [] limit = "Arr! The stuff is too heavy! Next time we better take a bigger knapsack with us!"
generateSuccessMessage sack limit
    | limit == weight = "\nAwesome, we filled the whole bag! " ++ base
    | otherwise       = "\nNice! We used " ++ show weight ++ "/" ++ show limit ++ "kg! " ++ base
    where
        weight = getWeight sack
        base = "We got " ++ show (length sack) ++ " items totaling " ++ show (sum (map value sack)) ++ "€!"


main :: IO ()
main = do
    putStrLn ".:! Welcome to the Knapsack Problem solver !:."
    threadDelay 1000000

    putStrLn "Please enter the amount of items to generate: "
    itemCount <- readIntLargerThan 1

    putStrLn "Please enter the minimum number for weights and values: "
    minNum <- readIntLargerThan 1

    putStrLn "Please enter the maximum number for weights and values: "
    maxNum <- readIntLargerThan minNum

    putStrLn $ "\nGenerating " ++ show itemCount ++ " items..."
    gen <- getStdGen
    let items = sort (generateItems gen itemCount (minNum, maxNum))

    mapM_ print (take 100 items)
    if itemCount > 100
    then putStrLn $ "Magical things would have happened to your terminal if I printed " ++ show itemCount
        ++ " items, so I rather showed you the first 100."
    else putStrLn "These are your items!"
    threadDelay 1000000

    putStrLn "\nPlease enter the maximum weight your knapsack can carry: "
    weightLimit <- readIntLargerThan 1

    let knapsack = [] :: [Item]
    putStrLn "\nFilling the sack... this might take some time:"
    let filledSack = fillKnapsack items knapsack weightLimit

    mapM_ print (take 100 filledSack)
    if length filledSack > 100
    then putStrLn $ "Your terminal would have entered a world of wonders if I printed " ++ show (length filledSack)
        ++ " items, so I rather showed you the first 100."
    else putStrLn "These are your items collected!"
    threadDelay 1000000

    putStrLn $ generateSuccessMessage filledSack weightLimit


attributes = ["Red", "Blue", "Green", "Yellow", "Orange", "Violet", "Purple", "Black", "White", "Magic", "Heavy",
    "Wizardly", "Occult", "Magical", "Functional", "Pure", "Enchanted", "Glowing", "Nuclear", "Atomic", "Holy",
    "Space", "Underwater", "Golden", "Shiny", "Artificial", "Enhanched", "Autistic", "Wonderful", "Sparkling",
    "Shimmering"]

things = ["Shirt", "Socks", "Pants", "Skirt", "Trousers", "Umbrella", "Blanket", "Toothbrush", "First Aid Kit",
    "Camera", "Dog", "Cat", "Laptop", "Opium Pipe", "Lizard", "Copy of SICP", "Towel", "Whiskey", "Compiler",
    "Keyboard", "Two-handed Hammer", "Fuel Cell", "Hand Grenade", "Cannabis", "Mage Cloak", "Cigar",
    "Bottle of Alcohol", "Bottle of Mystery Juice", "Bottle of Goat Milk", "Fernando's Fragrance", "Opium Cigar",
    "Ice Cream", "Episode of Sweet Boy Beach", "Manatee", "Walrus", "Baby Goat", "Vertical Seafood Taco",
    "Gauss Rifle"]