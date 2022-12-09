module Prob8 where

import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char (newline)
import Data.List (singleton, transpose)

type Parser = Parsec Void String


inputParser :: Parsec Void String [[Int]]
inputParser = many (read . singleton <$> anySingleBut '\n') `sepBy` newline

solution :: [[Int]] -> Int
solution treesMatrix =
  let linesVisible = map toVisibleTrees treesMatrix
      colsVisible = transpose $ map toVisibleTrees $ transpose treesMatrix
      visibleMatrix = zipWith (zipWith (||)) linesVisible colsVisible
  in sum $ map (length . filter id) visibleMatrix

toVisibleTrees :: [Int] -> [Bool]
toVisibleTrees trees =
  let rightVisible = zipWith (>) trees $ tail $ scanr max (-1) trees
      leftVisible = zipWith (>) trees $ init $ scanl max (-1) trees
  in zipWith (||) rightVisible leftVisible