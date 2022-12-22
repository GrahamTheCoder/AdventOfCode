module Lib
    ( getValue, partTwo
    ) where

import qualified Data.HashMap.Strict as HashMap
import Data.List.Split
import Data.String.Utils
import Data.Maybe
import Debug.Trace

data Expr
  = Int Int
  | Var String
  -- Bool is for commutativity
  | BinaryInvertible Expr Expr (Int -> Int -> Int) (Int -> Int -> Int) String Bool
  | Equals Expr Expr

instance Show Expr where
    show (Int x) = (show x)
    show (Var x) = (show x)
    show (BinaryInvertible x y _ _ infixOpString c) = "(" ++ (show x) ++ " " ++ infixOpString ++ " " ++ (show y) ++ ")"
    show (Equals x y) = (show x) ++ " == " ++ (show y)

parseExpr :: [String] -> Expr
parseExpr (l:[]) = case (maybeRead l :: Maybe Int) of
    Just num -> Int num
    _ -> Var l
parseExpr (l:"+":r) = BinaryInvertible (parseExpr [l]) (parseExpr r) (+) (-) "+" True
parseExpr (l:"-":r) = BinaryInvertible (parseExpr [l]) (parseExpr r) (-) (+) "-" False
parseExpr (l:"*":r) = BinaryInvertible (parseExpr [l]) (parseExpr r) (*) (div) "*" True
parseExpr (l:"/":r) = BinaryInvertible (parseExpr [l]) (parseExpr r) (div) (*) "/" False

parseLine :: String -> (String, Expr)
parseLine line = 
    let (name:expr:_) = splitOn ": " line
    in (name, parseExpr (splitOn " " expr))

parse :: [String] -> HashMap.HashMap [Char] Expr
parse unparsedLines = HashMap.fromList $ fmap parseLine unparsedLines

evaluateWherePossible :: HashMap.HashMap String Expr -> String -> Expr
evaluateWherePossible namedExpressions name =
  let evaluate (Var name) = case HashMap.lookup name namedExpressions of
          Just x -> evaluate x
          Nothing -> Var name
      evaluate (BinaryInvertible x y op inverseOp str isCommutative) = case (evaluate x, evaluate y) of
        (Int lhs, Int rhs) -> Int (lhs `op` rhs)
        (Int lhs, rhs) -> BinaryInvertible (Int lhs) rhs op inverseOp str isCommutative
        (lhs, Int rhs) -> BinaryInvertible lhs (Int rhs) op inverseOp str isCommutative
      evaluate (Equals lhs rhs) = Equals (evaluate lhs) (evaluate rhs)
      evaluate x = x
  in evaluate (Var name)

getValue :: [String] -> String -> Int
getValue unparsedLines name = 
  let (Int answer) = evaluateWherePossible (parse unparsedLines) name
  in answer

rearrangeVariableToLhs :: Expr -> Expr
rearrangeVariableToLhs (Equals (Int lhs) rhs) = rearrangeVariableToLhs (Equals rhs (Int lhs))
rearrangeVariableToLhs (Equals (Var lhs) (Int rhs)) = Equals (Var lhs) (Int rhs)
rearrangeVariableToLhs (Equals (BinaryInvertible (Int x) y op invertOp  _ isCommutative) (Int rhs)) = 
  let intValue = if isCommutative then rhs `invertOp` x  else x `op` rhs
  in Equals y (Int intValue)
rearrangeVariableToLhs (Equals (BinaryInvertible x (Int y) _ invertOp _ _) (Int rhs)) =
  Equals x (Int (rhs `invertOp` y))

partTwo :: [String] -> String -> Int
partTwo unparsedLines name =
  let namedExpressions = parse unparsedLines
      (BinaryInvertible rootLhs rootRhs _ _ _ _) = fromJust $ HashMap.lookup name namedExpressions
      updatedExpressions = HashMap.insert name (Equals rootLhs rootRhs) $ HashMap.delete "humn" namedExpressions
      initialTree = evaluateWherePossible updatedExpressions name
      rearrangeVariableToLhs' tree =
        let newTree = rearrangeVariableToLhs tree
        in trace (show newTree) newTree
      getAnswer (Equals (Var _) (Int y)) = Just y
      getAnswer _ = Nothing
      answer = fromJust $ getAnswer $ until (isJust.getAnswer) rearrangeVariableToLhs' initialTree
  in answer
