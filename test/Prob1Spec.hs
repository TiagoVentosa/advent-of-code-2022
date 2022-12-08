module Prob1Spec where

import Test.Hspec
import Test.QuickCheck
import Prob1 (solution)
import Data.List (intersperse,)

intToString :: Int -> String
intToString = show

spec :: Spec
spec = do
  describe "Solution" $ do
    it "sums a single elf calories" $ property $
      \x y z -> solution (fmap intToString [x,y,z]) == x + y + z

    it "returns largest value when separated by empty strings" $ property $
      \sums -> solution (intersperse "" (fmap intToString (0:sums))) == maximum (0:sums)

    it "should solve the solution example" $
      solution ["1000", 
       "2000",
       "3000",
       "",
       "4000",
       "",
       "5000",
       "6000",
       "",
       "7000",
       "8000",
       "9000",
       "",
       "10000"] `shouldBe` 24000