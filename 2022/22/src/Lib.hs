module Lib
    ( getPassword, part2
    ) where

import Control.Applicative
import Data.List
import qualified Data.Map as Map
import qualified Data.Matrix as Mx
import Data.Maybe
import Data.Void
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace (traceShow, traceShowId)

type MParser = Parsec Void String

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
    dist <- M.some M.numberChar
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
mapLine = do M.some mapSquare

type Instructions = ([[MapSquare]], [Movement])

boardMap :: MParser Instructions
boardMap = do
  mapLines <- mapLine `M.sepBy1` onlyOneNewline
  _ <- M.some M.newline
  movements <- M.some movement
  return (mapLines, movements)

-- I'm not really sure why this returns a list rather than just one
parseInstructions :: String -> [Instructions]
parseInstructions fileContents = case M.runParser boardMap "" fileContents of
    Left s -> error (show s)
    Right m -> return m

moveSingleSquare :: Int -> (Int, Int) -> (Int, Int)
moveSingleSquare 0 (col, row) = (col+1, row)
moveSingleSquare 1 (col, row) = (col, row+1)
moveSingleSquare 2 (col, row) = (col-1, row)
moveSingleSquare 3 (col, row) = (col, row-1)

getBasics str =
    let ((rowsFirstBoard, movements):_) = parseInstructions str
        initialFacing = 0
        initialPosition = (fromJust (elemIndex Passable (head rowsFirstBoard)), 0)
        endOfBoard = max (maximum (map length rowsFirstBoard)) (length rowsFirstBoard)
        getElement (c, r)
          | 0 <= c && c < endOfBoard && 0 <= r && r < endOfBoard = case map (drop c) $ drop r rowsFirstBoard of 
            ((a:_):_) -> a
            b -> NonExistent
          | otherwise = NonExistent
    in (movements, initialFacing, initialPosition, endOfBoard, getElement)

-- You begin the path in the leftmost open tile facing right
-- The final password is the sum of 1000 times the row, 4 times the column, and the facing.
--    Rows start from 1 at the top and count downward; columns start from 1 at the left and count rightward.
--    Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).
-- If a movement instruction would take you off of the map, you wrap around to the other side of the board (unless blocked)
getPassword :: String -> Int
getPassword str = 
    let (movements, initialFacing, initialPosition, endOfBoard, getElement) = getBasics str
        moveSingleSquareWrapped f p = let (c, r) = moveSingleSquare f p in (c `mod` endOfBoard, r `mod` endOfBoard)
        move [] facing (c,r) = (facing, c, r)
        move (TurnLeft:ms) facing pos = move ms ((facing - 1) `mod` 4) pos
        move (TurnRight:ms) facing pos = move ms ((facing + 1) `mod` 4) pos
        move ((Forward n):ms) facing pos = 
          let potentialNextPosition = until (\s -> getElement s /= NonExistent) (moveSingleSquareWrapped facing) (moveSingleSquareWrapped facing pos)
          in case getElement potentialNextPosition of
            Impassable -> move ms facing pos
            Passable -> move (if n > 1 then Forward (n-1):ms else ms) facing potentialNextPosition
        (finalFacing, finalCol, finalRow) = move movements initialFacing initialPosition
    in 1000 * (finalRow + 1) + 4 * (finalCol + 1) + finalFacing

type IntMx = Mx.Matrix Int

instance Ord a => Ord (Mx.Matrix a) where
  compare x y = compare (Mx.toList x) (Mx.toList y)

-- CubePosition: 3dpos moveforward rotCube      (NB: turnRight = turnLeft^3)
newtype CubePosition = CubePosition (IntMx, IntMx, IntMx) deriving (Show, Eq)

-- These are just created from defining my axes to match to the 2d co-ords: x goes right, y goes down, z goes inwards
-- Then plugging in a 90 degree turn into rotation matrix: https://www.cuemath.com/algebra/rotation-matrix/

initialAxisMoveForward, initialAxisPassEdge, initialAxisTurnLeft :: Mx.Matrix Int
initialAxisMoveForward = Mx.fromList 3 1 [1, 0, 0]
initialAxisPassEdge = Mx.fromLists [ [0,0,negate 1], [0,1,0], [1,0,0] ]
initialAxisTurnLeft = Mx.fromLists [ [0,1,0], [negate 1,0,0], [0,0,1] ]

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

part2 :: String -> Int
part2 str = 
    let (movements, initialFacing, initialPos, endOfBoard, getElement) = getBasics str
        m = endOfBoard `div` 4
        t = m + 1
        positionExists (x,y) = getElement (x,y) /= NonExistent
        initialCubePos = CubePosition (Mx.fromList 3 1 [1, 1, 0], initialAxisMoveForward, Mx.identity 3)
        rotateCube 0 _ c = c
        rotateCube n rotation (CubePosition (pos3, _, rotCube)) =
          let newRotCube = rotCube * rotation ^ n
          in CubePosition (pos3, newRotCube * initialAxisMoveForward, newRotCube)
        cubeTurnLeft n = rotateCube n initialAxisTurnLeft
        cubeMoveForward n (CubePosition (pos3, moveForward, rotCube)) =
          let trivialMove = pos3 + moveForward
              coOrdInDirectionOfTravel = abs $ Mx.unsafeGet 1 1 $ Mx.transpose trivialMove * moveForward
              isOnCorner = coOrdInDirectionOfTravel `elem` [0, t]
              trivialCubePosition = CubePosition (trivialMove, moveForward, rotCube)
              intermediatePos = if isOnCorner then cubeMoveForward 1 (rotateCube 1 initialAxisPassEdge trivialCubePosition) else trivialCubePosition
          in if n > 1 then cubeMoveForward (n-1) intermediatePos else intermediatePos

        squareFromCube =
          let getForTurns pos facing leftTurns = (moveSingleSquare newFacing pos, newFacing, leftTurns)
                  where newFacing = (facing-leftTurns) `mod` 4
              perimiterMove pos facing = fromJust $ find (positionExists.fst3) $ map (getForTurns pos facing) [1, 0, 3]
              walkPerimiter squarePos facing cubePos cubeToSquare =
                let (newSquarePos, newFacing, leftTurns) = perimiterMove squarePos facing
                    newCubePos@(CubePosition (pos3, _, _)) = cubeMoveForward 1 (cubeTurnLeft leftTurns cubePos)
                in if Map.member pos3 cubeToSquare then cubeToSquare
                else walkPerimiter newSquarePos newFacing newCubePos (Map.insert pos3 newSquarePos cubeToSquare)
          in walkPerimiter initialPos initialFacing initialCubePos Map.empty
        getFacing edgeSquarePos cubePosition =
            let getFacingFrom2Pos (x, y) (nx, ny) | nx > x = 0 | ny > y = 1 | nx < x = 2 | ny < y = 3
                  | otherwise = error $ "Cannot discern facing from two identical points: " ++ show (x,y)
                farEdgeCubePos@(CubePosition (farEdgeCubeCoOrds, _, _)) = cubeMoveForward (m - 1) cubePosition
             in case Map.lookup farEdgeCubeCoOrds squareFromCube of 
                Just farEdge2Pos -> getFacingFrom2Pos edgeSquarePos farEdge2Pos
                _ -> getFacing edgeSquarePos farEdgeCubePos -- Only walked perimiter, so may have to look further than one edge
        move [] facing (c,r) _ = (facing, c, r)
        move (TurnLeft:ms) facing pos cubePos = move ms ((facing - 1) `mod` 4) pos (cubeTurnLeft 1 cubePos)
        move (TurnRight:ms) facing pos cubePos = move ms ((facing + 1) `mod` 4) pos (cubeTurnLeft 3 cubePos)
        move ((Forward n):ms) facing pos cubePos@(CubePosition (_, _, oldRot)) = 
          let nextCubePos@(CubePosition (nextCubeCoOrds, _, newRot)) = cubeMoveForward 1 cubePos
              (nextSquarePos, nextFacing) = case Map.lookup nextCubeCoOrds squareFromCube of
                  Just p -> (p, if oldRot == newRot then facing else getFacing p nextCubePos)
                  Nothing -> (moveSingleSquare facing pos, facing)
          in case getElement nextSquarePos of
              Impassable -> move ms facing pos cubePos
              Passable -> move (if n > 1 then Forward (n-1):ms else ms) nextFacing nextSquarePos nextCubePos
              NonExistent -> error $ "Reached non-existent square" ++ show nextSquarePos
        (finalFacing, finalCol, finalRow) = move movements initialFacing initialPos initialCubePos
    in 1000 * (finalRow + 1) + 4 * (finalCol + 1) + finalFacing