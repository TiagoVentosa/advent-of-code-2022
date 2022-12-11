module Prob11 where

import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Functor ((<&>), ($>))
import Control.Monad.State (State, evalState, gets, modify, replicateM)
import qualified Data.Sequence as S
import Data.List (partition, sortOn)
import Data.Ord (Down(..))

type Parser = Parsec Void String
data Monkey = Monkey Operation ThrowTest deriving (Eq, Show)
data Operation = Add Int | Mult Int | Sqr deriving (Eq, Show)
data ThrowTest = ThrowTest Int Int Int deriving (Eq, Show)

restOfLine :: Parser String
restOfLine = many (anySingleBut '\n')

inputParser :: Parser [([Int],Monkey)]
inputParser = parseMonkey `sepBy` (newline <* newline)

parseMonkey :: Parser ([Int], Monkey)
parseMonkey =
  (,) <$ restOfLine <* newline
  <*> parseStartingItems <* newline
  <*> (
    Monkey <$> parseOperation <* newline
    <*> parseThrowTest
  )

parseStartingItems :: Parser [Int]
parseStartingItems =
  space *> string "Starting items: " *> decimal `sepBy` string ", "

parseOperation :: Parser Operation
parseOperation =
  space *> string "Operation: new = old " *>
  ((string "+ " *> decimal <&> Add)
  <|>
  (string "* " *> (
      (decimal <&> Mult)
      <|>
      (string "old" $> Sqr)
    )
  ))

parseThrowTest :: Parser ThrowTest
parseThrowTest =
  ThrowTest
  <$ space <* string "Test: divisible by " <*> decimal <* newline
  <* space <* string "If true: throw to monkey " <*> decimal <* newline
  <* space <* string "If false: throw to monkey " <*> decimal


solution :: [([Int], Monkey)] -> Int
solution monkeysWithStartingItems = let
  initialConditions = S.fromList $ fmap fst monkeysWithStartingItems
  monkeys = fmap snd monkeysWithStartingItems
  in product $ take 2 $ sortOn Down $ evalState (playRounds monkeys) initialConditions

playRounds :: [Monkey] -> State (S.Seq [Int]) [Int]
playRounds monkeys = foldr1 (zipWith (+)) <$> replicateM 20 (playRound monkeys)
  
playRound :: [Monkey] -> State (S.Seq [Int]) [Int]
playRound monkeys = do
  let monkeysWithNm = zip ([0..]::[Int]) monkeys
      plays = fmap playTurn monkeysWithNm
  sequence plays

playTurn :: (Int, Monkey) -> State (S.Seq [Int]) Int
playTurn (nm, Monkey operation test) = do
  items <- gets (`S.index` nm)
  let operatedItems = fmap ((`div` 3) . doOperation operation) items
  sendToOtherMonkeys test operatedItems
  modify (S.update nm [])
  pure $ length items
  
  
solution' :: [([Int], Monkey)] -> Int
solution' monkeysWithStartingItems = let
  initialConditions = S.fromList $ fmap fst monkeysWithStartingItems
  monkeys = fmap snd monkeysWithStartingItems
  in product $ take 2 $ sortOn Down $ evalState (playRounds' monkeys) initialConditions

playRounds' :: [Monkey] -> State (S.Seq [Int]) [Int]
playRounds' monkeys = foldr1 (zipWith (+)) <$> replicateM 10000 (playRound' monkeys)
  
playRound' :: [Monkey] -> State (S.Seq [Int]) [Int]
playRound' monkeys = do
  let maxWorry = product $ fmap (\(Monkey _ (ThrowTest d _ _)) -> d) monkeys
      monkeysWithNm = zip ([0..]::[Int]) monkeys
      plays = fmap (playTurn' maxWorry) monkeysWithNm
  sequence plays

playTurn' :: Int -> (Int, Monkey) -> State (S.Seq [Int]) Int
playTurn' maxWorry (nm, Monkey operation test) = do
  items <- gets (`S.index` nm)
  let operatedItems = fmap ((`mod` maxWorry ). doOperation operation) items
  sendToOtherMonkeys test operatedItems
  modify (S.update nm [])
  pure $ length items

doOperation :: Operation -> Int -> Int
doOperation (Add a) v = v + a
doOperation (Mult m) v = v * m
doOperation Sqr v = v * v

sendToOtherMonkeys :: ThrowTest -> [Int] -> State (S.Seq [Int]) ()
sendToOtherMonkeys 
  (ThrowTest divisable trueTarget falseTarget)
  items = do 
    let (passed, failed) = partition ((==0) . (`mod` divisable)) items
    modify (S.adjust (++ passed) trueTarget)
    modify (S.adjust (++ failed) falseTarget)