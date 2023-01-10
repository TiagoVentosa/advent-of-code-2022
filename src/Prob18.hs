{-# LANGUAGE PartialTypeSignatures #-}

module Prob18 where

import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, newline)
import qualified Data.Set as S
import Control.Monad.State (State, get, modify, execState)
import Data.Foldable (traverse_)
import qualified ListT as L
import ListT (ListT)

type Parser = Parsec Void String
data Jet = JLeft | JRight deriving (Show)

type Cube = (Int, Int, Int)

inputParser :: Parser [Cube]
inputParser = parseCube `sepBy` newline

parseCube :: Parser Cube
parseCube =
  (,,) <$>
  decimal <* char ',' <*>
  decimal <* char ',' <*>
  decimal

solution :: [Cube] -> Int
solution cubes = sum $ fmap freeSurfaces cubes
  where freeSurfaces cube = 6 - length (filter (isNeighbour cube) cubes)
        isNeighbour (x1, y1, z1) (x2, y2, z2) = (abs (x1-x2) + abs (y1-y2) + abs (z1-z2)) == 1


solution' :: [Cube] -> Int
solution' cubes = sum $ fmap freeSurfaces cubes
  where freeSurfaces cube = length (filter (isNeighbour cube) airCubes)
        isNeighbour (x1, y1, z1) (x2, y2, z2) = (abs (x1-x2) + abs (y1-y2) + abs (z1-z2)) == 1
        airCubes = let
          xs = fmap (\(x,_,_) -> x) cubes
          ys = fmap (\(_,y,_) -> y) cubes
          zs = fmap (\(_,_,z) -> z) cubes
          maxX = maximum xs
          maxY = maximum ys
          maxZ = maximum zs
          minX = minimum xs
          minY = minimum ys
          minZ = minimum zs
          getNeightbours (x, y, z) = filter
            (\pos@(xx, yy, zz) ->
              xx <= maxX+1 && xx >= minX-1 &&
              yy <= maxY+1 && yy >= minY-1 &&
              zz <= maxZ+1 && zz >= minZ-1 &&
              pos `notElem` cubes)
            [(x+1, y, z), (x-1, y, z), (x, y+1, z), (x, y-1, z), (x, y, z+1), (x, y, z-1)]
          in findReachablePos (minX-1, minY-1, minZ-1) getNeightbours


type PathsState a = ListT (State (S.Set a)) -- already visited positions


findReachablePos :: (Ord pos) => pos -> (pos -> [pos]) -> [pos]
findReachablePos start getNeighbours =
  S.toList $ execState (L.toReverseList $ findPathsAndLength start getNeighbours) $ S.singleton start

findPathsAndLength :: (Ord pos) => pos -> (pos -> [pos]) -> PathsState pos [pos]
findPathsAndLength start getNeighbours =
  go $ pure start
    where go path = calculateNextPaths getNeighbours path >>= go

calculateNextPaths :: (Ord pos) => (pos -> [pos]) -> [pos] -> PathsState pos [pos]
calculateNextPaths _ [] = error "should never be called for empty list"
calculateNextPaths getNeighbours currPath@(currPos : _) = do
  exploredPositions <- get
  let nextPoss = filter (`S.notMember` exploredPositions) $ getNeighbours currPos
  traverse_ (modify . S.insert) nextPoss
  L.fromFoldable $ fmap (: currPath) nextPoss

