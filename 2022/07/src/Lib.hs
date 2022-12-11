module Lib
    ( totalSizeOfDirectoriesBelow, getDirectorySizes, splitCommands, parseCommand
    ) where

import qualified Data.Map as Map
import Data.List.Split
import Data.Maybe
import Data.String.Utils

data Instruction a b = Root | Up | SubDir a | AddEntries b

data Node a b = Directory (Map.Map a (Node a b))
    | File a b

parseNode ["dir", dirName] = (dirName, Directory (Map.fromList []))
parseNode [size, fileName] = (fileName, File fileName (read size :: Int))

parseCommand "cd /" = Root
parseCommand "cd .." = Up
parseCommand ('c':'d':' ':dir) = SubDir dir
parseCommand ('l':'s':'\n':listing) = AddEntries $ fmap (parseNode . (splitOn " ")) $ splitOn "\n" listing

splitCommands str = fmap strip $ drop 1 $ splitOn "$ " str

parseCommands str = fmap parseCommand $ splitCommands str

interpretCommand (root, currentDir) Root = (root, [])
interpretCommand (root, (currentDir:parentPath)) Up = (root, parentPath)
interpretCommand (root, currentPath) (SubDir dir) = (root, dir:currentPath)
interpretCommand (root, fullPath) (AddEntries nodesToAdd) = (addEntries root (reverse fullPath) nodesToAdd, fullPath)

addEntries updated [] [] = updated
addEntries (Directory subDirs) [] ((key,val):otherNodes) = addEntries (Directory $ Map.insert key val subDirs) [] otherNodes
addEntries (Directory subDirs) (subDir:rest) nodesToAdd = Directory $ Map.insert subDir (addEntries (fromJust (Map.lookup subDir subDirs)) rest nodesToAdd) subDirs

getSize (File a b) = b
getSize (Directory map) = sum $ fmap getSize $ Map.elems map

getDirectories (Directory m) = (Directory m):(concat (fmap getDirectories (Map.elems m)))
getDirectories _ = []

getDirectorySizes str =
    let (root, _) = foldl interpretCommand ((Directory Map.empty), []) $ parseCommands str
    in fmap getSize $ getDirectories root

totalSizeOfDirectoriesBelow sizeLimit str =
    sum $ filter (\d -> d <= sizeLimit) $ getDirectorySizes str
