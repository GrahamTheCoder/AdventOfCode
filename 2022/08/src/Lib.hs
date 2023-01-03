module Lib
    ( part1, part2
    ) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Void
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace (traceShow)


type MParser = Parsec Void String

-- right, down
newtype Point a = Point (a, a) deriving (Show, Eq, Ord)

parseLine :: Num a => MParser [Point a]
parseLine = (curry Point <$> (L.decimal <* M.char ',') <*> L.decimal) `M.sepBy1` M.string " -> "

parseFile :: (Num a, Monad m) => String -> m [[Point a]]
parseFile fileContents = case M.runParser (parseLine `M.sepBy1` M.newline) "" fileContents of
    Left s -> error (show s)
    Right m -> return m

part1 :: String -> Int
part1 str = 0

part2 :: String -> Int
part2 str = 0