module Main (main) where

import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ show $ getPassword input
    putStrLn $ show $ part2 input
