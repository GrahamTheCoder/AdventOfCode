module Lib
    ( areCorrectOrder, part1
    ) where


import Control.Applicative
import Data.Char
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Sequence (mapWithIndex, fromList)
import Data.Void
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace (traceShow, traceShowId)

type MParser = Parsec Void String
data Node a = Leaf a | Branches [Node a] deriving (Show, Eq)

parseNode :: Num a => MParser (Node a)
parseNode = Leaf <$> L.decimal <|> 
    Branches <$> M.between (M.char '[') (M.char ']') (parseNode `M.sepBy` M.char ',')

parseFile fileContents = case M.runParser (parseNode `M.sepBy1` some M.newline) "" fileContents of
    Left s -> error (show s)
    Right m -> return m

isLessThan :: (Ord a, Eq a, Show a) => (Node a, Node a) -> Bool
isLessThan (Leaf l, Leaf r) = l < r
isLessThan (Branches [], Branches (r:_)) = True
isLessThan (Branches _, Branches []) = False
isLessThan (Branches (l:ls), Branches (r:rs)) = isLessThan (l, r) || not (isLessThan (r, l)) && isLessThan (Branches ls, Branches rs)
isLessThan (leftLeaf@(Leaf _), r) = isLessThan (Branches [leftLeaf], r)
isLessThan (l, rightLeaf@(Leaf _)) = isLessThan (l, Branches [rightLeaf])

areCorrectOrder str = map (\(a:b:_) -> isLessThan (a,b)) $ chunksOf 2 $ head (parseFile str)

part1 :: String -> Int
part1 str =
    let allNodes = head (parseFile str)
        areCorrectOrder = map (\(a:b:_) -> isLessThan (a,b)) $ chunksOf 2 allNodes
    in sum $ toList $ mapWithIndex (\i x -> if x then i + 1 else 0) $ fromList areCorrectOrder
