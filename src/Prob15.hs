module Prob15 where

import Data.Void (Void)
import Text.Megaparsec
import qualified Data.Set as S
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

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