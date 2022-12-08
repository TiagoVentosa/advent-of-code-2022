module Prob1 (solution, solution') where

import Data.List (sort)


--someFunc :: IO ()
--someFunc = putStrLn "someFunc"
split :: String -> [String] -> [[String]]
split separator = foldr grouper []
  where 
    grouper str []        = [[str]]
    grouper str ([]:rest) = [str]:rest
    grouper str (curr:rest)
      | str /= separator  = (str:curr):rest
      | otherwise         = []:curr:rest
      
    

solution :: [String] -> Int
solution content = let
  groupedContents = split "" content
  convertedToInt = fmap (fmap read) groupedContents
  summedList = fmap sum convertedToInt 
  in maximum summedList


solution' :: [String] -> Int
solution' content = let
  groupedContents = split "" content
  convertedToInt = fmap (fmap read) groupedContents
  summedList = fmap sum convertedToInt 
  in  sum $ take 3 $ reverse $ sort summedList