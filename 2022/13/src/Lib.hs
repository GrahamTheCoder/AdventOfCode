module Lib
    ( areCorrectOrder
    ) where


import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Void
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace (traceShow, traceShowId)

type MParser = Parsec Void String
data Node a = Leaf a | Branches [Node a] deriving (Show)

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

areCorrectOrder :: String -> [Bool]
areCorrectOrder str = map (const True) (traceShowId $ head (parseFile str))
