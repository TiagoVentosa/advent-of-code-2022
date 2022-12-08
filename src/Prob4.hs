module Prob4 (inputParser, solution, solution') where

import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec (eof, Parsec, sepBy)
import Data.Void (Void)

type Parser = Parsec Void String

inputParser :: Parser [(Int, Int, Int, Int)]
inputParser = sepBy lineParser newline <* eof

lineParser :: Parser (Int, Int, Int, Int)
lineParser = do
  firstLow <- decimal
  _ <- char '-'
  firstHigh <- decimal
  _ <- char ','
  secondLow <- decimal
  _ <- char '-'
  secondHigh <- decimal
  pure (firstLow, firstHigh, secondLow, secondHigh)

solution :: [(Int, Int, Int, Int)] -> Int
solution input = let
  overlapLines = filter isFullOverlap input
  in length overlapLines
  where isFullOverlap (a, b, c, d) = (a <=c && b >= d) || ( a>=c && b<=d)


solution' :: [(Int, Int, Int, Int)] -> Int
solution' input = let
  overlapLines = filter isPartialOverlap input
  in length overlapLines
  where isPartialOverlap (a, b, c, d) = (a >= c && a <= d) || (c >= a && c <= b)