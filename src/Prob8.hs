module Prob8 where

import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char (newline)
import Data.List (singleton, transpose)
import Data.Bifunctor (bimap)

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
  
solution' :: [[Int]] -> Int
solution' treesMatrix =
  let linesVisible = map toScenicScore treesMatrix
      colsVisible = transpose $ map toScenicScore $ transpose treesMatrix
      visibleMatrix = zipWith (zipWith (*)) linesVisible colsVisible
  in maximum $ map maximum visibleMatrix

toScenicScore :: [Int] -> [Int]
toScenicScore trees =
  let rightScore = go trees
      leftScore = reverse . go . reverse $ trees
  in zipWith (*) rightScore leftScore
  where go []     = []
        go (t:ts) = scoreTree t ts : go ts
        scoreTree t ts = 
          uncurry (+) $ 
          bimap length (fromEnum . (/=[])) $ 
          span (<t) ts