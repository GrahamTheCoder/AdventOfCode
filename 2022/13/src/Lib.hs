module Lib
    ( areCorrectOrder, part1, part2
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
import Debug.Trace.FunctionCall

type MParser = Parsec Void String
data Node a = Leaf a | Branches [Node a] deriving (Show)

parseNode :: Num a => MParser (Node a)
parseNode = Leaf <$> L.decimal <|> 
    Branches <$> M.between (M.char '[') (M.char ']') (parseNode `M.sepBy` M.char ',')

parseFile fileContents = case M.runParser (parseNode `M.sepBy1` some M.newline) "" fileContents of
    Left s -> error (show s)
    Right m -> return m

isLessThan :: (Ord a) => (Node a, Node a) -> Bool
isLessThan (Leaf l, Leaf r) = l < r
isLessThan (Branches [], Branches (r:_)) = True
isLessThan (Branches _, Branches []) = False
isLessThan (Branches (l:ls), Branches (r:rs)) = isLessThan (l, r) || not (isLessThan (r, l)) && isLessThan (Branches ls, Branches rs)
isLessThan (leftLeaf@(Leaf _), r) = isLessThan (Branches [leftLeaf], r)
isLessThan (l, rightLeaf@(Leaf _)) = isLessThan (l, Branches [rightLeaf])

areCorrectOrder str = map (\(a:b:_) -> isLessThan (a,b)) $ chunksOf 2 $ head (parseFile str)

part1 :: String -> Int
part1 str = sum $ toList $ mapWithIndex (\i x -> if x then i + 1 else 0) $ fromList $ areCorrectOrder str

instance (Ord a, Show a) => Eq (Node a) where
    (==) x y | isLessThan (x, y) = False
             | isLessThan (y, x) = False
             | otherwise = True

instance (Ord a, Show a) => Ord (Node a) where
    compare x y | isLessThan (x, y) = LT
                | isLessThan (y, x) = GT
                | otherwise = EQ

part2 :: String -> Int
part2 str =
    let extraNode1 = Branches [Branches [Leaf 2]]
        extraNode2 = Branches [Branches [Leaf 6]]
        allNodes = extraNode1:extraNode2:head (parseFile str)
        orderedNodes = sort allNodes
        getIndex n = fromJust (elemIndex n orderedNodes) + 1
    in getIndex extraNode1 * getIndex extraNode2
