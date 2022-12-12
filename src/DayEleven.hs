{-# LANGUAGE OverloadedRecordDot #-}

module DayEleven where

import Control.Arrow (second)
import Data.List (foldl', sortOn)
import Data.List.Split (dropDelims, onSublist, split)
import Data.Ord (Down (Down))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment ( getArgs )

data Monkey = Monkey
  { items :: [Int],
    op :: Int -> Int,
    nDiv :: Int,
    test :: Int -> Int,
    insepectionCount :: Int
  }

instance Show Monkey where
  show m =
    "Monkey "
      ++ show m.items
      ++ " "
      ++ show m.insepectionCount

processInputDay11 :: IO (String, String)
processInputDay11 = do
  args <- getArgs
  content <- T.lines <$> T.readFile (head args)
  let monkeyTexts = split (dropDelims $ onSublist [""]) content
  let monkeys = map readMonkey monkeyTexts
  let modulo = processModulo monkeys
  let p1 = take 2 $ sortOn (Down . insepectionCount) $ foldl' (\m _ -> monkeyRound m 3 modulo 0) monkeys ([1 .. 20] :: [Int])
  let p2 = take 2 $ sortOn (Down . insepectionCount) $ foldl' (\m _ -> monkeyRound m 1 modulo 0) monkeys ([1 .. 10000] :: [Int])
  let p1Business = business (head p1) (p1 !! 1)
  let p2Business = business (head p2) (p2 !! 1)
  pure (show p1Business, show p2Business)

business :: Monkey -> Monkey -> Int
business m1 m2 = m1.insepectionCount * m2.insepectionCount

-- processedMonkeys :: IO [Monkey]
-- processedMonkeys = do
--   content <- T.lines <$> T.readFile "/home/phi/Documents/codes/haskell/advent-of-code/inputs/DayEleven.txt"
--   let monkeyTexts = split (dropDelims $ onSublist [""]) content
--   pure $ map readMonkey monkeyTexts

readMonkey :: [T.Text] -> Monkey
readMonkey [_, startingItems, operation, testDef, ifTrue, ifFalse] = Monkey i op n (\x -> if t x then tTrue else tFalse) 0
  where
    i = read @[Int] . T.unpack $ "[" <> T.replace "Starting items: " "" startingItems <> "]"
    op = readOp $ T.replace "Operation: " "" operation
    n = read @Int . T.unpack $ T.replace "Test: divisible by" "" testDef
    t = flip divisible n
    tTrue = read @Int . T.unpack $ T.replace "If true: throw to monkey " "" ifTrue
    tFalse = read @Int . T.unpack $ T.replace "If false: throw to monkey " "" ifFalse
readMonkey _ = error "Couldn't parse Monkey"

processModulo :: [Monkey] -> Int
processModulo = foldl' (\md monkey -> monkey.nDiv `lcm` md) 1

readOp :: T.Text -> (Int -> Int)
readOp t = case T.words t of
  ["new", "=", "old", "*", "old"] -> (\x -> x * x)
  ["new", "=", "old", "*", T.unpack -> i] -> (* read i)
  ["new", "=", "old", "+", "old"] -> (\x -> x + x)
  ["new", "=", "old", "+", T.unpack -> i] -> (+ read i)
  ["new", "=", "old", "-", T.unpack -> i] -> subtract (read i)
  _ -> error $ "Couldn't parse operation: " ++ T.unpack t

monkeyRound :: [Monkey] -> Int -> Int -> Int -> [Monkey]
monkeyRound m d modulo i = if i == length m then m else monkeyRound (insertItems newMonkeys changes) d modulo (i + 1)
  where
    changes = inspect (m !! i) modulo d
    newMonkeys = adjust (\mm -> mm {items = [], insepectionCount = mm.insepectionCount + length mm.items}) i m

insertItems :: [Monkey] -> [(Int, Int)] -> [Monkey]
insertItems = foldl' (\mList (val, mPos) -> adjust (\m -> m {items = m.items ++ [val]}) mPos mList)

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f i = uncurry (++) . second (\xs -> f (head xs) : tail xs) . splitAt i

inspect :: Monkey -> Int -> Int  -> [(Int, Int)]
inspect m modulo d = map (\item -> let val = m.op item `mod` modulo `div` d in (val, m.test val)) m.items

divisible :: Int -> Int -> Bool
divisible x y = (== 0) $ x `mod` y
