module Prob4Spec where

import Test.Hspec
import Prob4
import Text.Megaparsec (parseMaybe)

spec :: Spec
spec = do
  describe "inputParser" $ do
    it "should return single line" $
      parseMaybe inputParser "2-4,6-8" `shouldBe` Just [(2, 4, 6, 8)]
    it "should return multiple lines" $
      parseMaybe inputParser "2-4,6-8\n2-3,4-5" 
      `shouldBe`
      Just [(2, 4, 6, 8), (2,3,4,5)]