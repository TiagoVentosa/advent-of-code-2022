{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
module Prob16 where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, string, space, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Prob16.Minpath
import Data.List (singleton, intersect)

type Parser = Parsec Void String
type ValveName = (Char, Char)

inputParser :: Parser [(ValveName, Int, NonEmpty ValveName)]
inputParser = parseValve `sepBy` newline

parseValve :: Parser (ValveName, Int, NonEmpty ValveName)
parseValve = do
  name <- string "Valve " *> parseValveName
  rate <- string " has flow rate=" *> decimal
  tunnels <- string "; tunnel" *> optional (char 's') *>
    string " lead" *> optional (char 's') *>
    string " to valve" *> optional (char 's') *>
    space *> parseValveName `sepBy1` string ", "
  pure (name, rate, NE.fromList tunnels)

parseValveName :: Parser ValveName
parseValveName = (,) <$> anySingle <*> anySingle

data Valve = Valve {
  valveRate :: Int,
  valveTunnels :: NonEmpty (ValveName, Int)
} deriving (Eq, Show)

solution :: [(ValveName, Int, NonEmpty ValveName)] -> Int
solution input = let
  valvesMap = M.fromList $ fmap toValvePair input
  usefulValves = map first $ filter (\(_, rate, _) -> rate /= 0) input
  completeValveMap = fillPaths valvesMap usefulValves
  in maximum $ calculatePressureForRoutes usefulValves completeValveMap
  where toValvePair (name, rate, others) = (name, Valve rate (fmap (,1) others) )
        first (name, _, _) = name

solution' :: [(ValveName, Int, NonEmpty ValveName)] -> Int
solution' input = let
  valvesMap = M.fromList $ fmap toValvePair input
  usefulValves = map first $ filter (\(_, rate, _) -> rate /= 0) input
  completeValveMap = fillPaths valvesMap usefulValves
  routes = calculatePressureForRoutes' usefulValves completeValveMap
  in getBestDoubleRoute routes
  where toValvePair (name, rate, others) = (name, Valve rate (fmap (,1) others) )
        first (name, _, _) = name


startingValve :: ValveName
startingValve = ('A', 'A')

fillPaths :: Map ValveName Valve -> [ValveName] -> Map ValveName Valve
fillPaths valvesMap usefulValves = foldr addPathsToMap valvesMap (startingValve : usefulValves)
  where addPathsToMap name mapToUpdate= let
          knownPaths = fmap fst $ valveTunnels $ mapToUpdate M.! name
          unknownPaths = filter (/= name) $ filter (`notElem` knownPaths) usefulValves
          pathsWithDistance = zip unknownPaths $ fmap (\end -> findShortestPath name end getNeighbourValves) unknownPaths
          updatedMap = M.update (Just . appendTunnels pathsWithDistance) name mapToUpdate
          updatedMap' = foldr (updateOtherTunnel name) updatedMap pathsWithDistance
          in updatedMap'
        getNeighbourValves name = fmap fst $ NE.toList $ valveTunnels $ valvesMap M.! name
        updateOtherTunnel name (name', dist) mapToUpdate' = M.update (Just . appendTunnels (singleton (name, dist))) name' mapToUpdate'

appendTunnels :: [(ValveName, Int)] -> Valve -> Valve
appendTunnels newTunnels (Valve rate tunnels) = Valve rate (NE.appendList tunnels newTunnels)

calculatePressureForRoutes :: [ValveName] -> Map ValveName Valve -> [Int]
calculatePressureForRoutes usefulValves valvesMap =
  go $ Right (NE.singleton startingValve, 30, 0)
  where go (Left finalValue) = pure finalValue
        go currState =
          calculateNextValve usefulValves valvesMap currState >>= go

calculatePressureForRoutes' :: [ValveName] -> Map ValveName Valve -> [([ValveName], Int)]
calculatePressureForRoutes' usefulValves valvesMap =
  go $ Right (NE.singleton startingValve, 26, 0)
  where go (Left finalValue) = pure finalValue
        go currState =
          calculateNextValve' usefulValves valvesMap currState >>= go


calculateNextValve :: [ValveName] -> Map ValveName Valve ->
                      Either Int (NonEmpty ValveName, Int, Int) ->
                      [Either Int (NonEmpty ValveName, Int, Int)]
calculateNextValve _ _ (Left finalValue) = pure $ Left finalValue
calculateNextValve usefulValves valvesMap (Right (visitedValves@(currValve :| _), timeLeft, pressureReleased)) = let
  nextValves = filter (`notElem` visitedValves) usefulValves
  nextValvesWithDistance = filter ((>= 0) . newTimeLeft . snd) $
    NE.filter ((`elem` nextValves) . fst) $ valveTunnels $ valvesMap M.! currValve
  in case nextValvesWithDistance of
    [] -> pure $ Left pressureReleased
    _ -> fmap nextState nextValvesWithDistance
  where newTimeLeft dist = timeLeft - dist - 1
        nextState (valve, dist) =
          Right (valve <| visitedValves,
          newTimeLeft dist,
          (+ pressureReleased) $ (* newTimeLeft dist) $ valveRate $ valvesMap M.! valve)

calculateNextValve' :: [ValveName] -> Map ValveName Valve ->
                      Either ([ValveName], Int) (NonEmpty ValveName, Int, Int) ->
                      [Either ([ValveName], Int) (NonEmpty ValveName, Int, Int)]
calculateNextValve' _ _ (Left finalValue) = pure $ Left finalValue
calculateNextValve' usefulValves valvesMap (Right (visitedValves@(currValve :| _), timeLeft, pressureReleased)) = let
  nextValves = filter (`notElem` visitedValves) usefulValves
  nextValvesWithDistance = filter ((>= 0) . newTimeLeft . snd) $
    NE.filter ((`elem` nextValves) . fst) $ valveTunnels $ valvesMap M.! currValve
  in case nextValvesWithDistance of
    [] -> pure $ Left (NE.init visitedValves, pressureReleased)
    _ -> Left (NE.init visitedValves, pressureReleased) : fmap nextState nextValvesWithDistance
  where newTimeLeft dist = timeLeft - dist - 1
        nextState (valve, dist) =
          Right (valve <| visitedValves,
          newTimeLeft dist,
          (+ pressureReleased) $ (* newTimeLeft dist) $ valveRate $ valvesMap M.! valve)

getBestDoubleRoute :: [([ValveName], Int)] -> Int
getBestDoubleRoute routes =
  foldr go 0 routes
  where go (route, press) prevMax = let
          routeMax = maximum $ (0:) $ map snd $ filter (null . (`intersect` route) . fst) $ filter ((> (prevMax - press)) . snd) routes
          in max prevMax (press + routeMax)
