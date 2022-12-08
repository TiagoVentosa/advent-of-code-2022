module Prob6 (inputParser, solution, solution') where

import Text.Megaparsec
import Data.Void (Void)

type Parser = Parsec Void String

inputParser :: Parser String
inputParser = many anySingle

solution :: String -> Int
solution = findSignal 0
  where findSignal n s 
          | distinct (take 4 s) = n + 4
          | otherwise           = findSignal (n+1) (tail s)

solution' :: String -> Int
solution' = findSignal 0
  where findSignal n s 
          | distinct (take 14 s) = n + 14
          | otherwise           = findSignal (n+1) (tail s)
      
distinct :: Eq a => [a] -> Bool
distinct []     = True
distinct (x:xs) = x `notElem` xs && distinct xs