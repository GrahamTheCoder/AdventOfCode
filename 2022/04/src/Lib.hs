module Lib
    ( parseLine
    ) where


import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Ord
import Data.Maybe
import Text.Read

tupl (a:b:[]) = (a,b)

parseRange :: String -> [Int]
parseRange range = fmap read (splitOn "-" range)

parseLine :: String -> [(Int, Int)]
parseLine line = 
    let elves = splitOn "," line
    in fmap (tupl . parseRange) elves
