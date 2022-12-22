module Main (main) where

import Lib

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn $ show $ getValue (lines contents) "root"
    putStrLn $ show $ partTwo (lines contents) "root"
