module Lib
    ( parseLine, fullyOverlap, countFullyOverlappingFromText, partlyOverlap, countPartlyOverlapping, countPartlyOverlappingFromText
    ) where


import Data.List
import Data.List.Split
import Data.Ord
import Data.Maybe
import GHC.Utils.Misc
import Text.Read

tupleFromTwoList :: [a] -> (a, a)
tupleFromTwoList (a:b:[]) = (a, b)

parseRange :: String -> [Int]
parseRange rangeStr = fmap read (splitOn "-" rangeStr)

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line = 
    let elves = splitOn "," line
    in tupleFromTwoList $ fmap (tupleFromTwoList . parseRange) elves

fullyOverlap :: Ord a => Eq a => (a, a) -> (a, a) -> Bool
fullyOverlap (a, b) (c, d) = a >= c && b <= d || c >= a && d <= b


partlyOverlap :: Ord a => Eq a => (a, a) -> (a, a) -> Bool
partlyOverlap (a, b) (c, d) = a >= c && a <= d || b >= c && b <= d || a <= c && b >= c || a <= d && b >= d

tuplFunc :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
tuplFunc f (a,b) = f a b

countFullyOverlapping :: [String] -> Int
countFullyOverlapping s = 
    let pairs = fmap parseLine s
        tuplOverlap = tuplFunc fullyOverlap
    in count tuplOverlap pairs

countPartlyOverlapping :: [String] -> Int
countPartlyOverlapping s = 
    let pairs = fmap parseLine s
        tuplOverlap = tuplFunc partlyOverlap
    in count tuplOverlap pairs

countFullyOverlappingFromText :: String -> Int
countFullyOverlappingFromText s = countFullyOverlapping (lines s)

countPartlyOverlappingFromText :: String -> Int
countPartlyOverlappingFromText s = countPartlyOverlapping (lines s)