import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.Read

-- 0 for Rock/Loss, 1 for Paper/Draw, 2 for Scissors/Win
decrypt :: [Char] -> (Int, Int)
decrypt [o, ' ', m] = ((ord o) - (ord 'A'), (ord m) - (ord 'X'))

getScoreFromOpponentAndMine :: (Int, Int) -> Int
getScoreFromOpponentAndMine (o, m)
    | o == m = 4 + m
    | mod (m - 1) 3 == o = 7 + m
    | otherwise = 1 + m

getScoreFromOpponentAndStrat :: (Int, Int) -> Int
getScoreFromOpponentAndStrat (o, 0) = 1 + mod (o - 1) 3
getScoreFromOpponentAndStrat (o, 1) = 4 + o
getScoreFromOpponentAndStrat (o, 2) = 7 + mod (o + 1) 3

main :: IO Int
main = do
    contents <- readFile "input.txt"
    let inputLines = fmap decrypt $ lines contents
    let part1Score = sum $ fmap getScoreFromOpponentAndMine inputLines
    let part2Score = sum $ fmap getScoreFromOpponentAndStrat inputLines
    putStrLn $ show (part1Score, part2Score)
    return part2Score