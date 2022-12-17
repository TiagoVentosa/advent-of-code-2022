module Prob15 where

import Data.Void (Void)
import Text.Megaparsec
import qualified Data.Set as S
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.List (sort, foldl')
import Data.Maybe (mapMaybe, fromJust)
import Control.Applicative (asum)

type Parser = Parsec Void String
type Position = (Int,Int)
type SensorBeacon = (Position, Position)

inputParser :: Parser [SensorBeacon]
inputParser = parseSensorBeaconPair `sepBy` newline

parseSensorBeaconPair :: Parser SensorBeacon
parseSensorBeaconPair = do
  sensorX <- string "Sensor at x=" *> signed space decimal
  sensorY <- string ", y=" *> signed space decimal
  beaconX <- string ": closest beacon is at x=" *> signed space decimal
  beaconY <- string ", y=" *> signed space decimal
  pure ((sensorX, sensorY), (beaconX,beaconY))



solution :: [SensorBeacon] -> Int
solution input = let
  line = 2000000
  beaconsInLine = fmap fst $ filter ((==line) . snd) $ fmap snd input
  in
    S.size $ S.filter (`notElem` beaconsInLine) $ S.fromList $
    concatMap (\(x, reach) -> [x - reach .. x + reach]) $
    fmap (\((x, y), dist) -> (x, dist - abs (y - line))) $
    filter (\((_, y), dist) -> abs (y - line) <=dist) $
    fmap (\((x, y), (bx, by)) -> ((x, y), abs (bx - x) + abs (by - y)))
    input

type Range = (Int, Int)

solution' :: [SensorBeacon] -> Int
solution' input = let
  limit = 4000000
  sensorsWithRange = fmap toSensorWithRange input
  in
    (\(y, x) -> x*4000000 + y) $
    fromJust $
    asum $ -- basically find the first Just
    fmap liftMaybe $ zip [0..limit] $
    fmap (findPointNotCovered sensorsWithRange limit) [0..limit]
  where liftMaybe (_ , Nothing) = Nothing
        liftMaybe (y, Just x) = Just (y, x)

toSensorWithRange :: SensorBeacon -> (Position, Int)
toSensorWithRange ((x, y), (bx, by)) = ((x, y), abs (bx - x) + abs (by - y))

--findPointNotCovered :: [(Position, Int)] Int -> Maybe Int
findPointNotCovered :: [(Position, Int)] -> Int -> Int -> Maybe Int
findPointNotCovered sensorsWithRange limit line = let
  firstRange : sortedLineRanges = sort $ toRangesInLine sensorsWithRange limit line
  in either Just (const Nothing) $ foldl' (mergeRangeOrFindPoint limit) (Right firstRange) sortedLineRanges

toRangesInLine :: [(Position, Int)] -> Int -> Int -> [Range]
toRangesInLine sensorsWithRange limit l = mapMaybe toRangeInLine sensorsWithRange
  where toRangeInLine ((x, y), dist)
          | distInLine <- dist - abs (y - l),
            distInLine >= 0 = Just (max 0 (x - distInLine), min limit (x + distInLine))
          | otherwise = Nothing

mergeRangeOrFindPoint :: Int -> Either Int Range -> Range -> Either Int Range
mergeRangeOrFindPoint limit (Right (ls, le)) (hs, he)
  | le + 1 >= hs = Right (max 0 (min ls hs), min limit (max le he))
  | otherwise = Left (le + 1)
mergeRangeOrFindPoint _ point _ = point