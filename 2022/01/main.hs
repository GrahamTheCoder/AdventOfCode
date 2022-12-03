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
sumElvesWithMostCalories nElves inputLines = 
    let sortDesc = sortBy $ comparing Down
        caloriesPerElfDesc = sortDesc $ getCaloriesPerElf inputLines
    in sum $ take nElves caloriesPerElfDesc

main :: IO Int
main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    let mostCalories = getElfWithMostCalories inputLines
    let top3MostCalories = sumElvesWithMostCalories 3 inputLines
    putStrLn $ show mostCalories
    putStrLn $ show top3MostCalories
    return top3MostCalories