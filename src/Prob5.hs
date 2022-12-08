module Prob5 where

import Text.Megaparsec (Parsec, anySingleBut, many, eof, (<|>), sepBy1, sepBy)
import Data.Void (Void)
import Text.Megaparsec.Char (char, letterChar, newline, string)
import Data.List (transpose, foldl')
import Data.Maybe (catMaybes)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data MoveOrder = MoveOrder {fromStack :: Int, toStack :: Int} deriving (Show, Eq)
data MoveOrder' = MoveOrder' {numberCrates :: Int, fromStack' :: Int, toStack' :: Int} deriving (Show, Eq)

inputParser :: Parser ([String], [MoveOrder])
inputParser = do
    initState <- parseInitialCratesState
    _ <- newline
    orders <- parseOrders
    eof
    pure (initState,orders)
    
inputParser' :: Parser ([String], [MoveOrder'])
inputParser' = do
    initState <- parseInitialCratesState
    _ <- newline
    orders <- parseOrders'
    eof
    pure (initState,orders)

parseInitialCratesState :: Parser [String]
parseInitialCratesState = do
    crateLines <- many $ parseCrateLine <* newline
    _ <- many $ anySingleBut '\n'
    _ <- newline
    pure $ catMaybes <$> transpose crateLines

parseCrateLine :: Parser [Maybe Char]
parseCrateLine = parseCrate `sepBy1` char ' '

parseCrate :: Parser (Maybe Char)
parseCrate =
  Just <$ char '[' <*> letterChar <* char ']'
  <|>
  Nothing <$ string "   "

parseOrders :: Parser [MoveOrder]
parseOrders = concat <$> parseOrder `sepBy` newline

parseOrders' :: Parser [MoveOrder']
parseOrders' = parseOrder' `sepBy` newline

parseOrder :: Parser [MoveOrder]
parseOrder = do
   _ <- string "move "
   times <- decimal
   _ <- string " from "
   fromNumber <- decimal
   _ <- string " to "
   toNumber <- decimal
   pure $ replicate times $ MoveOrder (fromNumber - 1) (toNumber - 1)

parseOrder' :: Parser MoveOrder'
parseOrder' = do
   _ <- string "move "
   times <- decimal
   _ <- string " from "
   fromNumber <- decimal
   _ <- string " to "
   toNumber <- decimal
   pure $ MoveOrder' times (fromNumber - 1) (toNumber - 1)


solution :: ([String], [MoveOrder]) -> String
solution (initState, orders) = head <$> foldl' applyOrder initState orders
  where applyOrder state (MoveOrder fromIndex toIndex) =
          let stackFrom  = state !! fromIndex
              stackTo    = state !! toIndex
              crate     = head stackFrom
              newFrom    = tail stackFrom
              newTo      = crate:stackTo
          in setAt fromIndex newFrom $ setAt toIndex newTo state

solution' :: ([String], [MoveOrder']) -> String
solution' (initState, orders) = head <$> foldl' applyOrder initState orders
  where applyOrder state (MoveOrder' n fromIndex toIndex) =
          let stackFrom  = state !! fromIndex
              stackTo    = state !! toIndex
              crates     = take n stackFrom
              newFrom    = drop n stackFrom
              newTo      = crates ++ stackTo
          in setAt fromIndex newFrom $ setAt toIndex newTo state
    
setAt :: Int -> a -> [a] -> [a]
setAt 0  new (_:xs) = new:xs
setAt n  new (x:xs) = x:setAt (n-1) new xs
setAt _  _   []     = error "should never reach here"
