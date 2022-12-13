module Prob12 where

import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Data.Matrix as M
import Data.Char (ord)
import Text.Megaparsec.Char (newline)
import Data.List (elemIndex, sortBy, sortOn)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Foldable (foldlM, foldrM)

type Parser = Parsec Void String

restOfLine :: Parser String
restOfLine = many (anySingleBut '\n')

inputParser :: Parser [String]
inputParser = restOfLine `sepBy` newline

type Position = (Int, Int)

solution :: [String] -> Int
solution matrixList = let
  startPos = findStart matrixList
  endPos = findEnd matrixList
  heightMap = M.fromLists $ fmap (fmap toHeight) matrixList
  in minimum $ length <$> findPaths startPos endPos heightMap

findStart :: [String] -> Position
findStart = find 'S'

findEnd :: [String] -> Position
findEnd = find 'E'

find :: Char -> [String] -> Position
find char matrixList = let
  first = fmap (elemIndex char) matrixList
  column = (!! 0) $ filter isJust first
  line = elemIndex column first
  in (fromJust line + 1, fromJust column + 1)

toHeight :: Char -> Int
toHeight 'S' = 0
toHeight 'E' = 25
toHeight l = ord l - ord 'a'

findPaths :: Position -> Position -> M.Matrix Int -> [[Position]]
findPaths start end heighMap =
  go [start]
  where go route
          | head route == end = pure route
          | otherwise =  calculateNextRoutes heighMap route >>= go


calculateNextRoutes :: M.Matrix Int -> [Position] -> [[Position]]
calculateNextRoutes _ [] = error "shouldn't be called"
calculateNextRoutes heighMap currPath@((row, col):_) =
  let up = (row - 1, col)
      down = (row + 1, col)
      left = (row, col - 1)
      right = (row, col + 1)
      unexploredRoutes = filter (`notElem` currPath) [up, down, left, right]
      validPaths = mapMaybe eliminateInvalidPath $ zip unexploredRoutes $
        fmap (flip (uncurry M.safeGet) heighMap) unexploredRoutes
  in fmap (:currPath) validPaths
    where eliminateInvalidPath (_, Nothing) = Nothing
          eliminateInvalidPath (pos, Just height)
            | (height - heighMap M.! (row, col)) > 1 = Nothing
            | otherwise = Just pos