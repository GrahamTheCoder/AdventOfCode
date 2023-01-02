module Lib
    ( areCorrectOrder, part1
    ) where


import Control.Applicative
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Sequence
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

parsePacketPair :: MParser (Node Int, Node Int)
parsePacketPair = do
    n1 <- parseNode
    M.newline
    n2 <- parseNode
    return (n1, n2)

parseFile fileContents = case M.runParser (parsePacketPair `M.sepBy1` some M.newline) "" fileContents of
    Left s -> error (show s)
    Right m -> return m

isLessThan :: (Ord a, Eq a, Show a) => (Node a, Node a) -> Bool
isLessThan (Leaf l, Leaf r) = traceShow (l, r) (l < r)
isLessThan (Branches [], Branches (r:_)) = True
isLessThan (Branches _, Branches []) = False
isLessThan (Branches (l:ls), Branches (r:rs)) = isLessThan (l, r) || not (isLessThan (r, l)) && isLessThan (Branches ls, Branches rs)
isLessThan (leftLeaf@(Leaf _), r) = isLessThan (Branches [leftLeaf], r)
isLessThan (l, rightLeaf@(Leaf _)) = isLessThan (l, Branches [rightLeaf])

areCorrectOrder :: String -> [Bool]
areCorrectOrder str = map isLessThan (head (parseFile str))

part1 :: String -> Int
part1 str = sum $ toList $ mapWithIndex (\i x -> if x then i + 1 else 0) (fromList (areCorrectOrder str))
