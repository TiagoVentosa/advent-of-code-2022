{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
module Prob14 where

import Data.Void (Void)
import Text.Megaparsec
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Text.Megaparsec.Char (newline, char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Maybe (fromMaybe)

type Parser = Parsec Void String
type Position = (Int,Int)

inputParser :: Parser (Set Position)
inputParser = S.unions <$> parseRockStruct `sepBy` newline

parseRockStruct :: Parser (S.Set Position)
parseRockStruct = do
   (fstEdge :| edges) <- parseEdges
   pure $ snd $ foldl insertRocks (fstEdge, S.empty) edges
   where insertRocks ((prevX, prevY), set) curr@(x, y)
           | prevY == y =
             (curr, foldr S.insert set $ fmap (,y) if prevX < x then [prevX..x] else [x..prevX])
           | otherwise =
             (curr, foldr S.insert set $ fmap (x,)  if prevY < y then [prevY..y] else [y..prevY] )

parseEdges :: Parser (NonEmpty Position)
parseEdges = NE.fromList <$> ((,) <$> decimal <* char ',' <*> decimal) `sepBy1` string " -> "

solution :: Set Position -> Int
solution = go 0
  where 
    go nOfSand obstacles =
      case dropSandUnit obstacles of
        Just newObs -> go (nOfSand + 1) newObs
        Nothing -> nOfSand
            
solution' :: Set Position -> Int
solution' rocks = go 0 rocks
  where 
    floorY = S.findMax (S.map snd rocks) + 2
    go nOfSand obstacles
      | newObs <- dropSandUnit' obstacles floorY, 
        (500, 0) `S.notMember` newObs             = go (nOfSand + 1) newObs
      | otherwise                                 = nOfSand + 1

dropSandUnit :: Set Position -> Maybe (Set Position)
dropSandUnit obstacles = newObstacles (500, 0)
  where 
    newObstacles (x, y) = do
      (_, obsY) <- S.lookupMin $ S.filter (\(sx, sy) -> sx == x && sy > y) obstacles
      newObstacles' obsY
      where 
        newObstacles' obsY -- new func just because of nicer syntax for if then else
          | (obsY - y) /= 1                    = newObstacles (x, obsY - 1)
          | (x-1, y+1) `S.notMember` obstacles = newObstacles (x-1, y+1)
          | (x+1, y+1) `S.notMember` obstacles = newObstacles (x+1, y+1)
          | otherwise                          = Just $ S.insert (x, y) obstacles

dropSandUnit' :: Set Position -> Int -> Set Position
dropSandUnit' obstacles floorY = newObstacles (500, 0)
  where 
    newObstacles (x, y) = fromMaybe (S.insert (x, floorY - 1) obstacles) $ checkBeforeFloor (x, y)
    checkBeforeFloor (x, y) = do
      (_, obsY) <- S.lookupMin $ S.filter (\(sx, sy) -> sx == x && sy > y) obstacles
      Just $ checkBeforeFloor' obsY
      where 
        checkBeforeFloor' obsY -- new func just because of nicer syntax for if then else
          | (obsY - y) /= 1                    = newObstacles (x, obsY - 1)
          | (x-1, y+1) `S.notMember` obstacles = newObstacles (x-1, y+1)
          | (x+1, y+1) `S.notMember` obstacles = newObstacles (x+1, y+1)
          | otherwise                          = S.insert (x, y) obstacles
