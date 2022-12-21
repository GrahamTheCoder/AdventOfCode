module Main (main) where

import Lib

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn $ "part 1: " ++ (show $ sumOfQualityLevels 24 (lines contents))
    putStrLn $ "part 2: " ++ (show $ productOfGeodesProduced 32 (take 3 (lines contents)))

