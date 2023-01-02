module Lib
    ( part1, part2
    ) where

import Control.Applicative
import Data.Char
import Data.Foldable
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Data.Void
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace (traceShow, traceShowId)

type MParser = Parsec Void String

-- right, down
newtype Point a = Point (a, a) deriving (Show, Eq, Ord)

parseLine :: Num a => MParser [Point a]
parseLine = (curry Point <$> (L.decimal <* M.char ',') <*> L.decimal) `M.sepBy1` M.string " -> "


parseFile fileContents = case M.runParser (parseLine `M.sepBy1` M.newline) "" fileContents of
    Left s -> error (show s)
    Right m -> return m

data Content = Rock | Sand deriving (Show, Eq)

createBoard :: Ord a => [[Point a]] -> Map.Map (Point a) Content
createBoard lines =
    let maxX = map (max . map fst) lines
        maxY = map (max . map snd) lines
    in Map.fromList 

part1 :: String -> Int
part1 _ = 0

part2 :: String -> Int
part2 _ = 0