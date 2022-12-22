module Main (main) where

import Lib

main :: IO ()
main = do
    answer <- fmap getPassword $ readFile "example.txt"
    putStrLn $ show $ answer
