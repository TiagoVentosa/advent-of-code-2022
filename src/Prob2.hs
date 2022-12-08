module Prob2 (
solution,
solution'
) where

rock :: Int
rock = 1
paper :: Int
paper = 2
scissors :: Int
scissors = 3
  
win :: Int
win = 6
tie :: Int
tie = 3
lose :: Int
lose = 0

solution :: [String] -> Int
solution content = let
  tuplesList = parseTuples content
  scores = fmap toScore tuplesList
  in sum scores

parseTuples :: [String] -> [(Int, Int)]
parseTuples = fmap $ toTuple . words
  where toTuple [x, y] = (fromString x, fromString y)
        toTuple _ = undefined
        fromString "A" = rock
        fromString "X" = rock
        fromString "B" = paper
        fromString "Y" = paper
        fromString "C" = scissors
        fromString "Z" = scissors
        fromString _ = undefined
toScore :: (Int, Int) -> Int
toScore (theirs, ours) = ours + game theirs ours

game :: Int -> Int -> Int 
game theirs ours = case (ours - theirs) `mod` 3 of 
  1 -> win
  2 -> lose
  _ -> tie
  
solution' :: [String] -> Int
solution' content = let
  tuplesList = parseTuples' content
  scores = fmap toScore' tuplesList
  in sum scores
  
parseTuples' :: [String] -> [(Int, Int)]
parseTuples' = fmap $ toTuple . words
  where toTuple [x, y] = (fromString x, fromString y)
        toTuple _ = undefined
        fromString "A" = rock
        fromString "X" = lose
        fromString "B" = paper
        fromString "Y" = tie
        fromString "C" = scissors
        fromString "Z" = win
        fromString _ = undefined

toScore' :: (Int, Int) -> Int
toScore' (theirs, result) = result + computePlay theirs result

computePlay :: Int -> Int -> Int 
computePlay theirs result = case result of 
  6 -> 1 + theirs `mod` 3
  0 -> 1 + (theirs - 2) `mod` 3 
  _ -> theirs
