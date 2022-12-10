module Prob9 where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (spaceChar, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List (foldl')
import qualified Data.Set as S


type Parser = Parsec Void String
data Direction = MUp | MDown | MLeft | MRight deriving (Show, Eq)

inputParser :: Parser [Direction]
inputParser = concat <$> parseLine `sepBy` newline

parseLine :: Parser [Direction]
parseLine = do
  direction <- toDirection <$> satisfy (`elem` "UDLR")
  _ <- spaceChar
  times <- decimal
  pure $ replicate times direction
  where toDirection 'U' = MUp
        toDirection 'D' = MDown
        toDirection 'L' = MLeft
        toDirection 'R' = MRight
        toDirection _ = error "impossible"

type Position = (Int, Int)
data ProbState = PS {
  headPos :: Position,
  tailPos :: Position,
  tailHist:: S.Set Position
}

solution :: [Direction] -> Int
solution = let
  hPos = (0, 0)
  tPos = (0, 0)
  tHist = S.singleton tPos
  in S.size . tailHist . foldl' applyDirection (PS hPos tPos tHist)

applyDirection :: ProbState -> Direction -> ProbState
applyDirection (PS (hx, hy) tPos tHist) dir =
  let newHeadPos = case dir of
        MUp    -> (hx, hy+1)
        MDown  -> (hx, hy-1)
        MLeft  -> (hx+1, hy)
        MRight -> (hx-1, hy)
      newTailPos = updateCoordinate newHeadPos tPos
      newTailHist = S.insert newTailPos tHist
  in PS newHeadPos newTailPos newTailHist

updateCoordinate :: Position -> Position -> Position
updateCoordinate (hx, hy) (tx, ty)
 | hx-tx `elem` [-1..1] && hy-ty `elem` [-1..1] = (tx, ty)
 | otherwise = (tx + normalize (hx-tx), ty + normalize (hy-ty))
 where normalize 0 = 0
       normalize n = n `div` abs n

data ProbState' = PS' {
  headPos' :: Position,
  tailsPos' :: [Position],
  tailHist':: S.Set Position
}

solution' :: [Direction] -> Int
solution' = let
  hPos = (0, 0)
  tsPos = replicate 9 (0, 0)
  tHist = S.singleton (0,0)
  in S.size . tailHist' . foldl' applyDirection' (PS' hPos tsPos tHist)

applyDirection' :: ProbState' -> Direction -> ProbState'
applyDirection' (PS' (hx, hy) tsPos tHist) dir =
  let newHeadPos = case dir of
        MUp    -> (hx, hy+1)
        MDown  -> (hx, hy-1)
        MLeft  -> (hx+1, hy)
        MRight -> (hx-1, hy)
      newTailsPos = init $ foldr (\tPos (hPos:rest) -> updateCoordinate hPos tPos:hPos:rest) [newHeadPos] tsPos
      newTailHist = S.insert (head newTailsPos) tHist
  in PS' newHeadPos newTailsPos newTailHist
