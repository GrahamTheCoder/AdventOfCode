module Main (main) where

import Lib

main :: IO ()
main = do
    answer <- fmap getPassword $ readFile "input.txt"
    putStrLn $ show $ answer
