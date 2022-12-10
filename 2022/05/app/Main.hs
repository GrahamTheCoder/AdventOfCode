module Main (main) where

import Lib

main :: IO ()
main = do
    contents <- readFile "input.txt"
    --putStrLn $ show $ parseFile contents
    putStrLn $ show $ solvePartOne contents
    putStrLn $ show $ solvePartTwo contents
    return ()
