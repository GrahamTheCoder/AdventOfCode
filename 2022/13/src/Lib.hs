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


areCorrectOrder :: String -> [Bool]
areCorrectOrder _ = []
