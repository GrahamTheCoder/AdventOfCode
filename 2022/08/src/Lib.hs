module Lib
    ( part1, part2
    ) where

import Control.Applicative
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import GHC.Utils.Misc (count)
import Debug.Trace (traceShow, traceShowId)

parseFile :: String -> [[Int]]
parseFile fileContents = map (map (\x -> ord x - ord '0')) $ lines fileContents

combineMapsFromEachDirection scan combineTwo str =
    let rowsOfCols = parseFile str
        resultMap1 = map scan rowsOfCols
        resultMap2 = map (reverse . scan . reverse) rowsOfCols
        colsOfRows = transpose rowsOfCols
        resultMap3 = transpose $ map scan colsOfRows
        resultMap4 = transpose $ map (reverse . scan . reverse) colsOfRows
        combine4Rows w x y z = combineTwo (combineTwo w x) (combineTwo y z)
        combine4Maps a b c d = getZipList $ combine4Rows <$> ZipList a <*> ZipList b <*> ZipList c <*> ZipList d
    in getZipList $ combine4Maps <$> ZipList resultMap1 <*> ZipList resultMap2 <*> ZipList resultMap3 <*> ZipList resultMap4

part1 :: String -> Int
part1 str =
    let scan _ [] = []
        scan tallestSeen (e:es) = (e > tallestSeen) : scan (max tallestSeen e) es
        resultMapCombined = combineMapsFromEachDirection (scan (negate 1)) (||) str
    in sum $ map (count id) resultMapCombined

part2 :: String -> Int
part2 str =
    let scan _ _ [] = []
        -- Returns: a list of the viewing distance to the edge starting from the left
        -- Impl: Scans left to right keeping track of the last index of a given height seen.
        -- Style: Would it be any better/worse to use foldl/foldr etc. here?
        scan lastIndexOf currentIndex (h:hs) = 
            let thisDistance = currentIndex - fromMaybe 0 (Map.lookup h lastIndexOf)
                adjustedMap = foldl (\m (k,v) -> Map.insert k v m) lastIndexOf [(x, currentIndex) | x <- [0..h]]
            in thisDistance : scan adjustedMap (currentIndex+1) hs
        resultMapCombined = combineMapsFromEachDirection (scan Map.empty 0) (*) str
    in maximum $ map maximum resultMapCombined