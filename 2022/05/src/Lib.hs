module Lib
    ( parseStackLine, parseStacks, parseFile, applyInstruction, applyInstructions, solvePartOne, solvePartTwo
    ) where


import Data.List
import Data.List.Split

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

popN :: Int -> Int -> [[a]] -> ([a], [[a]])
popN 1 n (stack:remainingStacks) = 
    let (top, rest) = splitAt n stack
    in (top, rest:remainingStacks)
popN index n (stack:remainingStacks) =
    let (crates, stacks) = popN (index-1) n remainingStacks
    in (crates, stack:stacks)

pushN :: Int -> [a] -> [[a]] -> [[a]]
pushN 1 crates (stack:remainingStacks) = (crates ++ stack):remainingStacks
pushN index crates (stack:remainingStacks) = stack : (pushN (index-1) crates remainingStacks)

applyInstruction :: Int -> ([[Char]], [(Int, Int, Int)]) -> ([[Char]], [(Int, Int, Int)])
applyInstruction maxCrates (stacks, (n, source, dest):rest) =
    let (toMove, tempStacks) = popN source (min n maxCrates) stacks
        newStacks = pushN dest toMove tempStacks
        newInstructions = if n > maxCrates then (n-maxCrates, source, dest):rest else rest
    in (newStacks, newInstructions)

applyInstructions :: Int -> ([[Char]], [(Int, Int, Int)]) -> [[Char]]
applyInstructions maxCrates (stacks, []) = stacks
applyInstructions maxCrates stacksAndInstructions = applyInstructions maxCrates $ applyInstruction maxCrates stacksAndInstructions


solvePartOne contents = fmap head $ applyInstructions 1 $ parseFile contents
solvePartTwo contents = fmap head $ applyInstructions 1000 $ parseFile contents