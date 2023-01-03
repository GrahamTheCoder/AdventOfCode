module Lib
    ( part1, part2
    ) where

import Control.Applicative
import Data.Char
import Data.List
import GHC.Utils.Misc
import Debug.Trace (traceShow, traceShowId)



parseFile :: String -> [[Int]]
parseFile fileContents = map (map (\x -> ord x - ord '0')) $ lines fileContents

part1 :: String -> Int
part1 str =
    let scan _ [] = []
        scan tallestSeen (e:es) = (e > tallestSeen) : scan (max tallestSeen e) es
        mapScan = map (scan (negate 1))
        rowsOfCols = parseFile str
        seenMap1 = mapScan rowsOfCols
        seenMap2 = map reverse $ mapScan (map reverse rowsOfCols)
        colsOfRows = transpose rowsOfCols
        seenMap3 = transpose $ mapScan colsOfRows
        seenMap4 = transpose $ map reverse $ mapScan (map reverse colsOfRows)
        combine4Rows w x y z = w || x || y || z
        combine4Maps a b c d = getZipList $ combine4Rows <$> ZipList a <*> ZipList b <*> ZipList c <*> ZipList d
        seenMapCombined = traceShowId $ getZipList $ combine4Maps <$> ZipList seenMap1 <*> ZipList seenMap2 <*> ZipList seenMap3 <*> ZipList seenMap4
    in sum $ map (count id) seenMapCombined

part2 :: String -> Int
part2 _ = 0