module Main (main) where

import Lib

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn $ show $ endOfFirstUniqueRun 4 contents
    putStrLn $ show $ endOfFirstUniqueRun 14 contents
    return ()

