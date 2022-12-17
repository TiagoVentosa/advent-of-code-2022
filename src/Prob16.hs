module Prob16 where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, string, space, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

type Parser = Parsec Void String
type ValveName = (Char, Char)
data Valve = Valve {
  valveName :: ValveName,
  valveRate :: Int,
  valveTunnels :: NonEmpty ValveName
} deriving (Eq, Show)

inputParser :: Parser [Valve]
inputParser = parseValve `sepBy` newline

parseValve :: Parser Valve
parseValve = do
  name <- string "Valve " *> parseValveName
  rate <- string " has flow rate=" *> decimal
  tunnels <- string "; tunnel" *> optional (char 's') *>
    string " lead" *> optional (char 's') *>
    string " to valve" *> optional (char 's') *>
    space *> parseValveName `sepBy` string ", "
  pure Valve {valveName = name, valveRate = rate, valveTunnels = NE.fromList tunnels}

parseValveName :: Parser ValveName
parseValveName = (,) <$> anySingle <*> anySingle



solution :: a -> a
solution = id