module Prob16.Minpath (findShortestPath) where
  
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ((<|), NonEmpty(..))
import Control.Monad.State (State, evalState, get, modify)
import Data.Foldable (traverse_)

type PathsState a = State (S.Set a) -- already visited positions

type Path a = NonEmpty a

findShortestPath :: (Ord pos) => pos -> pos -> (pos -> [pos]) -> Int
findShortestPath start end getNeighbours =
  snd $ evalState (findPathsAndLength start end getNeighbours) $ S.singleton start

findPathsAndLength :: (Ord pos) => pos -> pos -> (pos -> [pos]) -> PathsState pos (Path pos, Int)
findPathsAndLength start end getNeighbours =
  go 0 [NE.singleton start]
    where go iteration paths
            | end `elem` fmap NE.head paths = let
              path = head $ filter ((== end) . NE.head) paths
              in pure (path, iteration)
            | otherwise = do
               res <- traverse (calculateNextPaths getNeighbours) paths
               go (iteration + 1) $ concat res
               
calculateNextPaths :: (Ord pos) => (pos -> [pos]) -> Path pos -> PathsState pos [Path pos]
calculateNextPaths getNeighbours currPath@(currPos :| _) = do
  exploredPositions <- get
  let nextPoss = filter (`S.notMember` exploredPositions) $ getNeighbours currPos
  traverse_ (modify . S.insert) nextPoss
  pure $ fmap (<| currPath) nextPoss