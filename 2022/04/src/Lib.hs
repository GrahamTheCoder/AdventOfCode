module Lib
    ( parseLine, fullOverlap
    ) where


import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Ord
import Data.Maybe
import Text.Read

tupleFromTwoList :: [a] -> (a, a)
tupleFromTwoList (a:b:[]) = (a, b)

parseRange :: String -> [Int]
parseRange rangeStr = fmap read (splitOn "-" rangeStr)

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line = 
    let elves = splitOn "," line
    in tupleFromTwoList $ fmap (tupleFromTwoList . parseRange) elves

fullOverlap :: Ord a => Eq a => (a, a) -> (a, a) -> Bool
fullOverlap (a, b) (c, d) = a >= c && b <= d || c >= a && d <= b