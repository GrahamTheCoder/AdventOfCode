import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.Read

parseCompartments :: String -> (String, String)
parseCompartments input = splitAt ((length input) `div` 2) input

getPriority :: Char -> Int
getPriority c =
    let basePriority = if 'a' <= c && c <= 'z' then ord 'a' - 1 else ord 'A' - 27
    in (ord c) - basePriority

getDuplicate :: (String, String) -> Char
getDuplicate (a, b) = head (intersect a b)

getSumOfPrioritiesForDuplicates :: [String] -> Int
getSumOfPrioritiesForDuplicates backpacks =
    let duplicates = fmap getDuplicate (fmap parseCompartments backpacks)
    in sum $ fmap getPriority duplicates

getSumOfPrioritiesForBadges :: [String] -> Int
getSumOfPrioritiesForBadges (b1:b2:b3:rest) =
    let badge = head (intersect (intersect b1 b2) b3)
    in getPriority badge + getSumOfPrioritiesForBadges rest
getSumOfPrioritiesForBadges [] = 0

main :: IO Int
main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    let part1Result = getSumOfPrioritiesForDuplicates inputLines
    let part2Result = getSumOfPrioritiesForBadges inputLines
    putStrLn $ show (part1Result, part2Result)
    return part1Result