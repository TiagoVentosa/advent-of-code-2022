module Prob5Spec where

import Test.Hspec
import Prob5
import Text.Megaparsec (parse)
import Test.Hspec.Megaparsec
import Test.QuickCheck.Property (property)



spec :: Spec
spec = do
  describe "inputParser" $ do
    context "when parsing the initial state" $ do
      it "should parse single line" $
        parse inputParser ""
        "[Z] [M] [P]\n\
        \ 1   2   3 \n\
        \\n"
        `shouldParse`
        (["Z", "M", "P"], [])
      it "should parse multiple lines" $
        parse inputParser ""
        "[A] [N] [Q] [Y]\n\
        \[Z] [M] [P] [A]\n\
        \ 1   2   3   4 \n\
        \\n"
        `shouldParse`
        (["AZ", "NM", "QP", "YA"], [])
      it "should parse lines with holes" $
        parse inputParser ""
        "[A]                \n\
        \[B]     [Q]        \n\
        \[Z] [M] [P] [A] [C]\n\
        \ 1   2   3   4   5 \n\
        \\n"
        `shouldParse`
        (["ABZ", "M", "QP", "A", "C"], [])
    context "when parsing orders" $ do
      it "should parse single order" $
        parse inputParser ""
        "\n\
        \\n\
        \move 1 from 2 to 3"
        `shouldParse`
        ([], [MoveOrder 1 2])
      it "should parse multiple orders" $
        parse inputParser ""
        "\n\
        \\n\
        \move 2 from 2 to 3\n\
        \move 1 from 3 to 4"
        `shouldParse`
        ([], [MoveOrder 1 2, MoveOrder 1 2, MoveOrder 2 3])
  describe "solution" $ do
    it "should give the top crates" $ property $ \top1 crates1 top2 crates2 ->
      solution ([top1:crates1, top2:crates2], [])
      ==
      [top1, top2]
    it "should apply single move" $ property $ \top1 subtop1 crates1 crates2 ->
      solution ([top1:subtop1:crates1, crates2], [MoveOrder 0 1])
      ==
      [subtop1, top1]
    it "should apply multiple moves" $ property $ \top1 subtop1 top2 crates1 crates2 crates3 ->
      solution ([top1:subtop1:crates1, top2:crates2, crates3], [MoveOrder 0 1, MoveOrder 1 2])
      ==
      [subtop1, top2, top1]