module DayFour
  ( processInputDay4,
    buildRanges,
    checkOverlap,
    checkFullOverlap,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.IntSet (disjoint, fromDistinctAscList, isSubsetOf)
import System.Environment (getArgs)

type Range = (Int, Int)

processInputDay4 :: IO (String, String)
processInputDay4 = do
  args <- getArgs
  content <- readFile (head args)
  pure (show . count True $ checkFullOverlap <$> buildRanges content, show . count True $ checkOverlap <$> buildRanges content)

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count c (x : xs) = if c == x then 1 + count c xs else count c xs

buildRanges :: String -> [(Range, Range)]
buildRanges s = dBimap (dBimap read . split '-') . split ',' <$> lines s
  where
    dBimap g = bimap g g

checkOverlap :: (Range, Range) -> Bool
checkOverlap ((i1, f1), (i2, f2)) = not $ l1 `disjoint` l2 || l2 `disjoint` l1
  where
    l1 = fromDistinctAscList [i1 .. f1]
    l2 = fromDistinctAscList [i2 .. f2]

checkFullOverlap :: (Range, Range) -> Bool
checkFullOverlap ((i1, f1), (i2, f2)) = l1 `isSubsetOf` l2 || l2 `isSubsetOf` l1
  where
    l1 = fromDistinctAscList [i1 .. f1]
    l2 = fromDistinctAscList [i2 .. f2]

split :: Eq a => a -> [a] -> ([a], [a])
split c xs = inner xs c []
  where
    inner (x : xxs) cc acc = if x == cc then (acc, xxs) else inner xxs cc (acc ++ [x])
    inner _ _ _ = ([], [])
