module Prob17 where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Data.List (sortOn)
import Data.Ord (Down(..))

type Parser = Parsec Void String
data Jet = JLeft | JRight deriving (Show)

data Rows = Rows {
  rFirst :: [Int],
  rSecond :: [Int],
  rThird :: [Int],
  rFourth :: [Int],
  rFifth :: [Int],
  rSixth :: [Int],
  rSeventh :: [Int]
  } deriving (Show)

inputParser :: Parser [Jet]
inputParser = many $ (JLeft <$ char '<') <|> (JRight <$ char '>')

solution :: [Jet] -> Int
solution jetPattern = getMaxHeight $ snd $
  foldl' settleRock (cycle jetPattern, Rows [0] [0] [0] [0] [0] [0] [0]) $ take 2022 rocks

rocks :: [Int -> Rows]
rocks = cycle [
  \y -> Rows [] [] [y] [y] [y] [y] [],
  \y -> Rows [] [] [y+1] [y+2, y+1, y] [y+1] [] [],
  \y -> Rows [] [] [y] [y] [y+2, y+1, y] [] [],
  \y -> Rows [] [] [y+3, y+2, y+1, y] [] [] [] [],
  \y -> Rows [] [] [y+1, y] [y+1, y] [] [] []
  ]

settleRock :: ([Jet], Rows) -> (Int -> Rows) -> ([Jet], Rows)
settleRock (jets, droppedRocks) generateRock = applyJet jets (generateRock $ 4 + getMaxHeight droppedRocks)
  where applyJet [] _ = error "impossible. jets can't reach the end"
        applyJet (jet:restJets) rock = dropRock restJets $ fromMaybe rock $ do
           shiftedRock <- shift jet rock
           guard $ not $ isOverlapping shiftedRock droppedRocks
           pure shiftedRock
        dropRock [] _ = error "impossible. jets can't reach the end"
        dropRock jets' rock = let
          shifted = shiftDown rock
          in if isOverlapping shifted droppedRocks
             then (jets', rock `addTo` droppedRocks)
             else applyJet jets' shifted

shift :: Jet -> Rows -> Maybe Rows
shift JLeft = shiftLeft
shift JRight = shiftRight

shiftLeft :: Rows -> Maybe Rows
shiftLeft (Rows [] sec thi fou fif six sev) = Just $ Rows sec thi fou fif six sev []
shiftLeft _ = Nothing

shiftRight :: Rows -> Maybe Rows
shiftRight (Rows fis sec thi fou fif six []) = Just $ Rows [] fis sec thi fou fif six
shiftRight _ = Nothing

shiftDown :: Rows -> Rows
shiftDown (Rows fis sec thi fou fif six sev) =
  Rows (fmap (subtract 1) fis) (fmap (subtract 1) sec) (fmap (subtract 1) thi) (fmap (subtract 1) fou)
       (fmap (subtract 1) fif) (fmap (subtract 1) six) (fmap (subtract 1) sev)

isOverlapping :: Rows -> Rows -> Bool
isOverlapping rock droppedRocks = let
  getters = [rFirst, rSecond, rThird, rFourth, rFifth, rSixth, rSeventh]
  rockCols = fmap ($ rock) getters
  droppedRocksCols = fmap ($ droppedRocks) getters
  in or $ uncurry isColumnsOverlapping <$> zip rockCols droppedRocksCols

addTo :: Rows -> Rows -> Rows
addTo (Rows rFis rSec rThi rFor rFif rSix rSev)
      (Rows drFis drSec drThi drFor drFif drSix drSev) =
      Rows (sortOn Down $ rFis ++ drFis) (sortOn Down $ rSec ++ drSec) (sortOn Down $ rThi ++ drThi) (sortOn Down $ rFor ++ drFor)
           (sortOn Down $ rFif ++ drFif) (sortOn Down $ rSix ++ drSix) (sortOn Down $ rSev ++ drSev)

isColumnsOverlapping :: [Int] -> [Int] -> Bool
isColumnsOverlapping [] _ = False
isColumnsOverlapping _ [] = False
isColumnsOverlapping rock droppedRocks = let
  rockMin = last rock
  significantDroppedRocks = takeWhile (>= rockMin) droppedRocks
--  significantDroppedRocks = droppedRocks
  in any (`elem` rock) significantDroppedRocks


getMaxHeight :: Rows -> Int
getMaxHeight rows = maximum $
  fmap (($ rows) . (head .)) [rFirst, rSecond, rThird, rFourth, rFifth, rSixth, rSeventh]