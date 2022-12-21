module Lib
    ( getRootValue
    ) where

import qualified Data.HashMap.Strict as HashMap
import Data.List.Split
import Data.String.Utils
import Data.Maybe

data Expr
  = Int Int
  | Var String
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)

parseExpr (l:[]) = case (maybeRead l :: Maybe Int) of
    Just num -> Int num
    _ -> Var l
parseExpr (l:"+":r) = Sum (parseExpr [l]) (parseExpr r)
parseExpr (l:"-":r) = Subtr (parseExpr [l]) (parseExpr r)
parseExpr (l:"*":r) = Product (parseExpr [l]) (parseExpr r)
parseExpr (l:"/":r) = Division (parseExpr [l]) (parseExpr r)

parseLine line = 
    let (name:expr:_) = splitOn ": " line
    in (name, parseExpr (splitOn " " expr))

recursivelyEvaluate :: (HashMap.HashMap String Expr -> String -> Int)
recursivelyEvaluate namedExpressions name =
  let evaluateName name = evaluate $ fromJust $ HashMap.lookup name namedExpressions
      evaluate (Int x) = x
      evaluate (Var x) = evaluateName x
      evaluate (Sum x y) = (evaluate x) + (evaluate y)
      evaluate (Subtr x y) = (evaluate x) - (evaluate y)
      evaluate (Product x y) = (evaluate x) * (evaluate y)
      evaluate (Division x y) = (evaluate x) `div` (evaluate y)
  in evaluateName name

getRootValue unparsedLines = recursivelyEvaluate (HashMap.fromList $ fmap parseLine unparsedLines) "root"
