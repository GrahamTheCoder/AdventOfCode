module Lib
    ( parseLine, range
    ) where


import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Ord
import Data.Maybe
import Data.Range
import Text.Read

range :: a -> a -> Range a
range a b = SpanRange (Bound a Inclusive) (Bound b Inclusive)

rangeFromTwoList :: [a] -> Range a
rangeFromTwoList (a:b:[]) = range a b

tupleFromTwoList :: [a] -> (a, a)
tupleFromTwoList (a:b:[]) = (a, b)

parseRange :: String -> [Int]
parseRange rangeStr = fmap read (splitOn "-" rangeStr)

parseLine :: String -> (Range Int, Range Int)
parseLine line = 
    let elves = splitOn "," line
    in tupleFromTwoList $ fmap (rangeFromTwoList . parseRange) elves
