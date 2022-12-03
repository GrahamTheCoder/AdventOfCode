import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.Read

decrypt :: [Char] -> (Int, Int)
decrypt [o, ' ', m] = (1 + (ord o) - (ord 'A'), 1 + (ord m) - (ord 'X'))

getScore :: (Int, Int) -> Int
getScore (o, m)
    | o == m = 3 + m
    | m - 1 == o || m + 2 == o = 6 + m
    | otherwise = 0 + m

main :: IO Int
main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    let score = fmap (getScore. decrypt) inputLines
    return $ sum score