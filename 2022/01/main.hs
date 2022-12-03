import Control.Monad
import Data.Char
import Text.Read

sumElf :: [Int] -> Maybe Int -> [Int]
sumElf elves Nothing = 0:elves
sumElf [] (Just calories) = [calories]
sumElf (current:rest) (Just calories) = (current+calories):rest

getElfWithMostCalories :: [String] -> Int
getElfWithMostCalories inputLines = 
    let caloriesPerElf = foldl sumElf [] $ map readMaybe inputLines
    in foldl max 0 caloriesPerElf

main :: IO Int
main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    let mostCalories = getElfWithMostCalories inputLines
    putStrLn $ show mostCalories
    return mostCalories