module Prob10 where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (signed, decimal)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE

type Parser = Parsec Void String
data Op = Noop | Add Int deriving (Show, Eq)

inputParser :: Parser [Op]
inputParser = parseLine `sepBy` newline

parseLine :: Parser Op
parseLine =
  Add <$ string "addx " <*> signed space decimal
  <|>
  Noop <$ string "noop"

solution :: [Op] -> Int
solution ops= let
  valuesPerCycle = NE.reverse $ foldl' toValueDuringCycle (NE.singleton 1) ops
  cycles = [20,60..220]
  valsAtCycles = map (last . flip NE.take valuesPerCycle) cycles
  in sum $ zipWith (*) cycles valsAtCycles

toValueDuringCycle :: NonEmpty Int -> Op -> NonEmpty Int
toValueDuringCycle values@(v :| _) Noop = v <| values
toValueDuringCycle values@(v :| _) (Add addV) = v + addV <| v <| values

solution' :: [Op] -> Int
solution' ops= let
  valuesPerCycle = NE.reverse $ foldl' toValueDuringCycle (NE.singleton 1) ops
  cycles = [20,60..220]
  valsAtCycles = map (last . flip NE.take valuesPerCycle) cycles
  in sum $ zipWith (*) cycles valsAtCycles
