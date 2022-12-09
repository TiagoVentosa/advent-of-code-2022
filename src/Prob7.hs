{-# LANGUAGE NumericUnderscores #-}
module Prob7 where

import Text.Megaparsec hiding (State)
import Data.Void (Void)
import Text.Megaparsec.Char (newline, string)
import Data.Functor (($>), (<&>))
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad.State.Lazy
import Data.List (isSuffixOf)

type Parser = StateT
 [String] -- use state monad for current path while parsing
 (Parsec Void String)

data Folder  = Folder {path :: [String], size :: Int} deriving (Show)

restOfLine :: Parser String
restOfLine = many (anySingleBut '\n')

inputParser :: Parsec Void String [Folder]
inputParser = fst <$> runStateT inputParserState []

inputParserState :: Parser [Folder]
inputParserState = parseCommand `sepBy` newline <* eof

parseCommand :: Parser Folder
parseCommand =
  many (try parseUp *> newline) *> parseFolder

parseUp :: Parser ()
parseUp = do
  _ <- string "$ cd .."
  currentPath <- get
  put $ tail currentPath

parseFolder :: Parser Folder
parseFolder = do
  _ <- string "$ cd "
  cdTo <- restOfLine
  _ <- newline *> string "$ ls" *> newline
  sizes <- parseSizeOutput `sepBy` (notFollowedBy (string "\n$") *> newline)
  currentPath <- get
  let newPath = cdTo:currentPath
  put newPath
  pure $ Folder newPath (sum sizes)

parseListSize :: Parser Int
parseListSize =
  string "ls" *>
  newline *>
  parseSizeOutput `sepBy` (notFollowedBy (string "\n$") *> newline) <&>
  sum

parseSizeOutput :: Parser Int
parseSizeOutput =
  (string "dir " *> restOfLine $> 0)
  <|>
  (decimal <* restOfLine)

solution :: [Folder] -> Int
solution folders =
  sum  $
  filter (<=100000) $
  map (size . addSubFoldersSize folders) folders

addSubFoldersSize :: [Folder] -> Folder -> Folder
addSubFoldersSize folders (Folder fpath _) =
  let newSize = sum $ map size $ filter (isSuffixOf fpath . path) folders
  in Folder fpath newSize

solution' :: [Folder] -> Int
solution' folders =
  let foldersSizes = map (size . addSubFoldersSize folders) folders
      usedSpace = head foldersSizes
      availableSpace = 70_000_000 - usedSpace
      requiredSpace = 30_000_000 - availableSpace
      in minimum $ filter (>= requiredSpace) foldersSizes
