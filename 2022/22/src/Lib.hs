module Lib
    ( getPassword
    ) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (strip, unpack, Text, pack)
import Data.Void
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace (traceShow, traceShowId)

type MParser = Parsec Void String

newtype Col = Col Int
newtype Row = Row Int
newtype Facing = Facing Int
newtype Position = Position (Facing, Col, Row)
data MapSquare = NonExistent | Passable | Impassable deriving (Show, Eq)
data Movement = Forward Int | TurnLeft | TurnRight
    deriving (Show, Eq)

mapSquare :: MParser MapSquare
mapSquare =
  (M.char ' ' >> return NonExistent) <|> 
  (M.char '.' >> return Passable) <|> 
  (M.char '#' >> return Impassable)

forward :: MParser Movement
forward = do
    dist <- some M.numberChar
    return $ Forward (read dist)

movement :: MParser Movement
movement = 
  forward <|> 
  (M.char 'L' >> return TurnLeft) <|>
  (M.char 'R' >> return TurnRight)
  

onlyOneNewline :: MParser ()
onlyOneNewline = M.try $ do
    M.newline
    M.notFollowedBy M.newline

mapLine :: MParser [MapSquare]
mapLine = do
  mapSquares <- some mapSquare
  return mapSquares

type Instructions = ([[MapSquare]], [Movement])

boardMap :: MParser Instructions
boardMap = do
  mapLines <- mapLine `M.sepBy1` onlyOneNewline
  _ <- some M.newline
  movements <- some movement
  return (mapLines, movements)

-- I'm not really sure why this returns a list rather than just one
parseInstructions :: String -> [Instructions]
parseInstructions fileContents = case M.runParser boardMap "" fileContents of
    Left s -> error (show s)
    Right m -> return m



-- You begin the path in the leftmost open tile facing right
-- The final password is the sum of 1000 times the row, 4 times the column, and the facing.
--    Rows start from 1 at the top and count downward; columns start from 1 at the left and count rightward.
--    Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).
-- If a movement instruction would take you off of the map, you wrap around to the other side of the board (unless blocked)
getPassword :: FilePath -> Int
getPassword str = 
    let ((rowsFirstBoard, movements):_) = parseInstructions str
        initialFacing = 0
        initialPosition = (fromJust (elemIndex Passable (head rowsFirstBoard)), 0)
        numCols = length $ head rowsFirstBoard
        numRows = length rowsFirstBoard
        getElement (column, row) = case drop column $ head $ drop row rowsFirstBoard of
          [] -> NonExistent
          (x:_) -> x
        moveSingleSquare 0 (col, row) = ((col+1) `mod` numCols, row)
        moveSingleSquare 1 (col, row) = (col, (row+1) `mod` numRows)
        moveSingleSquare 2 (col, row) = ((col-1) `mod` numCols, row)
        moveSingleSquare 3 (col, row) = (col, (row-1) `mod` numRows)
        move [] facing (r,c) = (facing, c, r)
        move (TurnLeft:ms) facing pos = move ms ((facing - 1) `mod` 4) pos
        move (TurnRight:ms) facing pos = move ms ((facing + 1) `mod` 4) pos
        -- WIP/TODO: Implement movement that skips non-existent squares and stops at blockages here:
        move ((Forward n):ms) facing pos = 
          let potentialNextPosition = until (\s -> (getElement s) /= NonExistent) (moveSingleSquare facing) ((moveSingleSquare facing) pos)
          in case getElement potentialNextPosition of
            Impassable -> move ms facing pos
            Passable -> move (if n > 1 then ((Forward (n-1)):ms) else ms) facing potentialNextPosition
        (finalFacing, finalRow, finalCol) = move movements initialFacing initialPosition
    in 1000 * (finalRow + 1) + 4 * (finalCol + 1) + finalFacing


