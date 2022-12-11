module Lib
    ( endOfFirstUniqueRun
    ) where

import Data.List

allUnique :: Ord a => [a] -> Bool
allUnique l =
    let sorted = sort l
        allUniqueSorted (c1:c2:rest) = c1 /= c2 && allUniqueSorted (c2:rest)
        allUniqueSorted _ = True
    in allUniqueSorted sorted

endOfFirstUniqueRun :: Ord a => Int -> [a] -> Int
endOfFirstUniqueRun n (first:rest) = if allUnique (take n (first:rest)) then n else 1 + endOfFirstUniqueRun n rest
