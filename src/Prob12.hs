{-# LANGUAGE TupleSections #-}
module Prob12 where

import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Data.Matrix as M
import Data.Char (ord)
import Text.Megaparsec.Char (newline)
import Data.List (elemIndex, elemIndices, singleton)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Control.Monad.State (get, modify, State, evalState)
import qualified Data.Set as S
import Data.Foldable (traverse_)

type Parser = Parsec Void String

restOfLine :: Parser String
restOfLine = many (anySingleBut '\n')

inputParser :: Parser [String]
inputParser = restOfLine `sepBy` newline

type Position = (Int, Int)
type Path = [Position]
type PathsState = State (S.Set Position) -- already visited positions

solution :: [String] -> (Path, Int)
solution matrixList = let
  startPos = findStart matrixList
  endPos = findEnd matrixList
  heightMap = M.fromLists $ fmap (fmap toHeight) matrixList
  in findShortestPath startPos endPos heightMap

solution' :: [String] -> (Path, Int)
solution' matrixList = let
  startPoss = findStarts' matrixList
  endPos = findEnd matrixList
  heightMap = M.fromLists $ fmap (fmap toHeight) matrixList
  in findShortestPath' startPoss endPos heightMap

findStart :: [String] -> Position
findStart = find 'S'

findStarts' :: [String] -> [Position]
findStarts' heightMap = find 'S' heightMap : find' 'a' heightMap

findEnd :: [String] -> Position
findEnd = find 'E'

find :: Char -> [String] -> Position
find char matrixList = let
  firstStep = fmap (elemIndex char) matrixList
  column = (!! 0) $ filter isJust firstStep
  line = elemIndex column firstStep
  in (fromJust line + 1, fromJust column + 1)

find' :: Char -> [String] -> [Position]
find' char matrixList = let
  columns = fmap (elemIndices char) matrixList :: [[Int]]
  zipRows = zip [0..] columns  :: [(Int, [Int])]
  in (\(row,col) -> (row + 1, col +1)) <$> concatMap (\(row, cols) -> fmap (row,) cols) zipRows

toHeight :: Char -> Int
toHeight 'S' = 0
toHeight 'E' = 25
toHeight l = ord l - ord 'a'

findShortestPath :: Position -> Position -> M.Matrix Int -> (Path, Int)
findShortestPath start end heightMap =
  evalState (findPathsAndLength start end heightMap) $ S.singleton start

findShortestPath' :: [Position] -> Position -> M.Matrix Int -> (Path, Int)
findShortestPath' starts end heightMap =
  evalState (findPathsAndLength' starts end heightMap) $ S.fromList starts


findPathsAndLength :: Position -> Position -> M.Matrix Int -> PathsState (Path, Int)
findPathsAndLength start end heightMap =
  go 0 [[start]]
    where go iteration routes
            | end `elem` fmap head routes = let
              route = head $ filter ((== end) . head) routes
              in pure (route, iteration)
            | otherwise = do
               res <- traverse (calculateNextRoutes heightMap) routes :: PathsState [[Path]]
               go (iteration + 1) $ concat res

findPathsAndLength' :: [Position] -> Position -> M.Matrix Int -> PathsState (Path, Int)
findPathsAndLength' starts end heightMap =
  go 0 $ fmap singleton starts
    where go iteration routes
            | end `elem` fmap head routes = let
              route = head $ filter ((== end) . head) routes
              in pure (route, iteration)
            | otherwise = do
               res <- traverse (calculateNextRoutes heightMap) routes :: PathsState [[Path]]
               go (iteration + 1) $ concat res


calculateNextRoutes :: M.Matrix Int -> Path -> PathsState [Path]
calculateNextRoutes _ [] = error "shouldn't be called"
calculateNextRoutes heighMap currPath@((row, col):_) = do
  exploredPositions <- get
  let nextPos = [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
      validPaths = filter (`S.notMember` exploredPositions) $
        mapMaybe eliminateInvalidPath $
        zip nextPos $
        fmap getHeight nextPos
  traverse_ (modify .  S.insert) validPaths
  pure $ fmap (:currPath) validPaths
  where
    getHeight (x, y)= M.safeGet x y heighMap
    eliminateInvalidPath (_, Nothing) = Nothing
    eliminateInvalidPath (pos, Just height)
      | (height - heighMap M.! (row, col)) > 1 = Nothing
      | otherwise = Just pos