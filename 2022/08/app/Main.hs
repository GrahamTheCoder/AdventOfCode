module Main (main) where

import Lib

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn $ show $ part1 contents
    putStrLn $ show $ part2 contents
