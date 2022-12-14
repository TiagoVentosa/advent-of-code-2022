module Prob13 where

import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char ( newline, char)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String
data ValOrList = Val Int | VList [ValOrList] deriving (Eq)

-- so it is easier to see
instance Show ValOrList where
  show (Val i) = show i
  show (VList l) = show l

inputParser :: Parser [([ValOrList], [ValOrList])]
inputParser = ((,) <$> parseList <*> parseList) `sepBy` newline

parseList :: Parser [ValOrList]
parseList = between (char '[') (char ']') (parseElement `sepBy` char ',') <* optional newline

parseElement :: Parser ValOrList
parseElement = (Val <$> decimal) <|> (VList <$> parseList)

solution :: a -> a
solution = id