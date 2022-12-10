module Lib
    ( parseStackLine, parseStacks, parseFile
    ) where


import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Ord
import Text.Read

tuplFunc :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
tuplFunc f (a,b) = f a b

parseRange :: String -> [Int]
parseRange rangeStr = fmap read (splitOn "-" rangeStr)

parseInstructionWords :: [String] -> (Int, Int, Int)
parseInstructionWords (_:n:_:source:_:destination:[]) = (read n, read source, read destination)

parseInstructions :: [String] -> [(Int, Int, Int)]
parseInstructions = fmap (parseInstructionWords . (splitOn " "))

parseStackLine :: [Char] -> [Char]
parseStackLine (_:s:_:' ':rest) = s : (parseStackLine rest)
parseStackLine (_:s:_:[]) = [s]
parseStackLine [] = []

parseStacks :: [String] -> [[Char]]
parseStacks allLines =
    let filteredLines = fmap parseStackLine allLines
        justStackLines = take ((length filteredLines) - 1) filteredLines
        stacks = transpose justStackLines
    in fmap (filter (\x -> x /= ' ')) stacks

parseFile :: String -> ([[Char]], [(Int, Int, Int)])
parseFile s = 
    let (stackText:instructionText:[]) = splitOn "\n\n" s
    in (parseStacks (lines stackText), parseInstructions (lines instructionText))
