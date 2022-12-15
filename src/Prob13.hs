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
inputParser = 
  ((,) <$> 
   parseList <* newline <*> 
   parseList <* optional newline
  ) `sepBy` newline

parseList :: Parser [ValOrList]
parseList = between (char '[') (char ']') (parseElement `sepBy` char ',')

parseElement :: Parser ValOrList
parseElement = (Val <$> decimal) <|> (VList <$> parseList)

-- list comparison in haskell already works as the problem wants 
-- (check first element, if equal check next, if one list ends its considered smaller)
instance Ord ValOrList where
  compare (Val a) (Val b) = compare a b
  compare (VList a) (VList b) = compare a b
  compare a@(Val _) (VList b) = compare [a] b
  compare (VList a) b@(Val _) = compare a [b]

solution :: [([ValOrList], [ValOrList])] -> Int
solution = sum . fmap fst . filter snd . zip [1..] . fmap (uncurry (<))  