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

data Content = Rock | Sand deriving (Show, Eq)

pointsBetween :: Integral a => Point a -> Point a -> [Point a]
pointsBetween firstPoint@(Point (x1, y1)) secondPoint@(Point (x2, y2)) =
    let getStep a b = (b-a) `div` abs (b-a)
        newPoint
            | x1 /= x2 = Point (x1 + getStep x1 x2, y1)
            | y1 /= y2 = Point (x1, y1 + getStep y1 y2)
            | otherwise = error "Unreachable"
    in if firstPoint == secondPoint then [secondPoint] else firstPoint : pointsBetween newPoint secondPoint

pointsForLine :: Integral a => [Point a] -> [Point a]
pointsForLine (p1:p2:rest) = pointsBetween p1 p2 ++ drop 1 (pointsForLine (p2:rest))
pointsForLine x = x

type Board = Map.Map (Point Int) Content

createBoard :: [[Point Int]] -> Board
createBoard lineDefs =
    let linePoints = map (\x -> (x, Rock)) $ concatMap pointsForLine lineDefs
    in Map.fromList linePoints

getSandUnitsAtRest :: Maybe Int -> Board -> Int
getSandUnitsAtRest floorGap board = 
    let pointList = map fst $ Map.toList board
        xs = map (\(Point (x, _)) -> x) pointList
        ys = map (\(Point (_, y)) -> y) pointList
        minX = minimum xs
        maxX = maximum xs
        maxY = maximum ys
        inAbyss (Point (x,y)) = x < minX || x > maxX || y > maxY
        getForPoint p@(Point (x,y)) b0
            | isNothing floorGap && inAbyss p = (0, b0, False)
            | isJust floorGap && y == maxY + fromJust floorGap || Map.member p b0 = (0, b0, True)
            | otherwise = let 
                (p1, p2, p3) = (Point (x, y + 1), Point (x - 1, y + 1), Point (x + 1, y + 1))
                (n1, b1, r1) = getForPoint p1 b0
                (n2, b2, r2) = if r1 then getForPoint p2 b1 else (0, b1, False)
                (n3, b3, r3) = if r2 then getForPoint p3 b2 else (0, b2, False)
                (n4, b4, r4) = if r3 then traceShow p (1, Map.insert p Sand b3, True) else (0, b3, False)
            in (n1 + n2 + n3 + n4, b4, r4)
        (restingSand, _, _) = getForPoint (Point (500, 0)) board --TODO Check for issues with rock at (500,0)
    in restingSand 
        


part1 :: String -> Int
part1 str = getSandUnitsAtRest Nothing $ createBoard $ head $ parseFile str

part2 :: String -> Int
part2 str = getSandUnitsAtRest (Just 2) $ createBoard $ head $ parseFile str