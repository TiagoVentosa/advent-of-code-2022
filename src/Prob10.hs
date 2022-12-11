module Prob10 where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (signed, decimal)
import Data.Foldable (foldl', toList)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.List.Split as Split
import Data.List (intercalate)

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

solution' :: [Op] -> String
solution' ops= let
  valuesPerCycle = NE.reverse $ foldl' toValueDuringCycle (NE.singleton 1) ops
  linesValues = Split.chunksOf 40 $ toList valuesPerCycle
  linesPixels = fmap valuesToPixel linesValues
  in intercalate "\n" linesPixels

valuesToPixel :: [Int] -> String
valuesToPixel = go 0
  where go _ []   = []
        go n (v:vs)
          | n `elem` [(v-1)..(v+1)] = '#':go (n+1) vs
          | otherwise               = '.':go (n+1) vs

