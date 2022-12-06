import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.Read

decrypt :: [Char] -> (Int, Int)
decrypt [o, ' ', m] = (1 + (ord o) - (ord 'A'), 1 + (ord m) - (ord 'X'))

getScoreFromOpponentAndMine :: (Int, Int) -> Int
getScoreFromOpponentAndMine (o, m)
    | o == m = 3 + m
    | m - 1 == o || m + 2 == o = 6 + m
    | otherwise = 0 + m

getScoreFromOpponentAndStrat :: (Int, Int) -> Int
getScoreFromOpponentAndStrat (o, 1) = 1 + mod (o - 2) 3
getScoreFromOpponentAndStrat (o, 2) = 3 + o
getScoreFromOpponentAndStrat (o, 3) = 6 + 1 + mod o 3

main :: IO Int
main = do
    contents <- readFile "input.txt"
    let inputLines = fmap decrypt $ lines contents
    let part1Score = sum $ fmap getScoreFromOpponentAndMine inputLines
    let part2Score = sum $ fmap getScoreFromOpponentAndStrat inputLines
    putStrLn $ show (part1Score, part2Score)
    return part2Score