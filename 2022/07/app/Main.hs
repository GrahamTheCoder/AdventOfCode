module Main (main) where

import Lib

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn $ show $ totalSizeOfDirectoriesBelow 100000 contents
    putStrLn $ show $ smallestDirectoryToDelete 70000000 30000000 contents
