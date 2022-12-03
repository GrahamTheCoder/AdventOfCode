import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.Read

sumElf :: [Int] -> Maybe Int -> [Int]
sumElf elves Nothing = 0 : elves
sumElf [] (Just calories) = [calories]
sumElf (current : rest) (Just calories) = (current + calories) : rest

getCaloriesPerElf :: [String] -> [Int]
getCaloriesPerElf = foldl sumElf [] . map readMaybe

getElfWithMostCalories :: [String] -> Int
getElfWithMostCalories = foldl max 0 . getCaloriesPerElf

sumElvesWithMostCalories :: Int -> [String] -> Int
sumElvesWithMostCalories nElves inputLines = sum (take nElves (sortBy (comparing Down) (getCaloriesPerElf inputLines)))

main :: IO Int
main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    let mostCalories = getElfWithMostCalories inputLines
    putStrLn $ show mostCalories
    return mostCalories