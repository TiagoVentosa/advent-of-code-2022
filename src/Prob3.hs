module Prob3 (solution, solution') where

import Data.List (intersect)
import Data.Char (ord)


solution :: [String] -> Int
solution content = let
  tuplesList = fmap parseLine content
  priorityList = fmap toPriority tuplesList
  in sum priorityList

parseLine :: String -> (String, String)
parseLine line = splitAt (length line `div` 2) line

toPriority :: (String, String) -> Int
toPriority (fstComp, scdComp) = case fstComp `intersect` scdComp of
  letter:_ -> calculatePriority letter
  _ -> undefined

calculatePriority :: Char -> Int
calculatePriority l
          | ascii <- ord l, ascii < 97 = ascii - 38
          | ascii <- ord l, otherwise = ascii - 96

solution' :: [String] -> Int
solution' content = let
  groupsList = parseGroups content
  priorityList = fmap toPriority' groupsList
  in sum priorityList


parseGroups :: [String] -> [(String, String, String)]
parseGroups = snd . foldr buildResult ([], [])
  where buildResult bag (temp, result)
            | length temp < 2 = (bag:temp, result)
            | [first, second] <- temp = ([], (first, second, bag):result)
            | otherwise = undefined
          
toPriority' :: (String, String, String) -> Int
toPriority' (first, second, third) = case first `intersect` second `intersect` third of
  letter:_ -> calculatePriority letter
  _ -> undefined


