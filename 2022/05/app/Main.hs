module Main (main) where

import Lib

main :: IO ()
main = do
    contents <- readFile "example.txt"
    return ()
