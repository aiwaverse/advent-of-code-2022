module DayEight
  ( processInputDay8,
    mkTreeMap,
    visibles,
    scenicScore,
    TreeMap (),
  )
where

import Data.Char (digitToInt)
import Data.Matrix (Matrix (..), fromLists, getCol, getElem, getRow)
import qualified Data.Vector as V
import System.Environment (getArgs)

type TreeMap = Matrix Int

processInputDay8 :: IO (String, String)
processInputDay8 = do
  args <- getArgs
  content <- lines <$> readFile (head args)
  let m = mkTreeMap content
  pure (show $ visibles m, show $ bestScenicScore m)

mkTreeMap :: [String] -> TreeMap
mkTreeMap = fromLists . map (map digitToInt)

visibles :: Matrix Int -> Int
visibles m = inner m (allPairs (nrows m) (ncols m)) 0
  where
    inner _ [] acc = acc
    inner mm ((row, col) : xs) acc = if isVisible mm row col then inner mm xs (acc + 1) else inner mm xs acc

isVisible :: TreeMap -> Int -> Int -> Bool
isVisible m r c = left || right || up || down
  where
    e = getElem r c m
    left = null $ V.filter (>= e) $ V.take (c - 1) $ getRow r m
    right = null $ V.filter (>= e) $ V.drop c $ getRow r m
    up = null $ V.filter (>= e) $ V.take (r - 1) $ getCol c m
    down = null $ V.filter (>= e) $ V.drop r $ getCol c m

bestScenicScore :: TreeMap -> Int
bestScenicScore m = maximum $ map (uncurry (scenicScore m)) (allPairs (nrows m) (ncols m))

scenicScore :: TreeMap -> Int -> Int -> Int
scenicScore m r c = left * right * up * down
  where
    e = getElem r c m
    left = length . takeWhilePlusOne (< e) . V.toList . V.reverse . V.take (c - 1) $ getRow r m
    right = length . takeWhilePlusOne (< e) . V.toList . V.drop c $ getRow r m
    up = length . takeWhilePlusOne (< e) . V.toList . V.reverse . V.take (r - 1) $ getCol c m
    down = length . takeWhilePlusOne (< e) . V.toList . V.drop r $ getCol c m

takeWhilePlusOne :: (a -> Bool) -> [a] -> [a]
takeWhilePlusOne _ [] = []
takeWhilePlusOne f (x : xs) = if f x then x : takeWhilePlusOne f xs else [x]

allPairs :: Int -> Int -> [(Int, Int)]
allPairs n m = [(i, j) | i <- [1 .. n], j <- [1 .. m]]
