module DayEight where


import qualified Data.Text as T
import Data.Matrix
    ( fromLists, getCol, getElem, getRow, prettyMatrix, Matrix(..) )
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Data.Char (digitToInt)
import qualified Data.Vector as V
import Debug.Trace (trace)

type TreeMap = Matrix Int

mkTreeMap :: [String] -> Matrix Int
mkTreeMap = fromLists . map (map digitToInt)

visibles :: Matrix Int -> Int
visibles m = inner m (allPairs (nrows m) (ncols m)) 0
  where
    inner _ [] acc = acc
    inner mm ((row, col) : xs) acc = if isVisible mm row col then inner mm xs (acc + 1) else inner mm xs acc

isVisible :: Matrix Int -> Int -> Int -> Bool
isVisible m r c = trace ("(" ++ show r ++ ", " ++ show c ++ "): " ++ show e ++ " - " ++ show (left || right || up || down) )$ left || right || up || down
  where
    e = getElem r c m
    left = null $ V.filter (> e) $ V.take (r - 1) $ getRow r m
    right = null $ V.filter (> e) $ V.drop r $ getRow r m
    up = null $ V.filter (> e) $ V.take (c - 1) $ getCol c m
    down = null $ V.filter (> e) $ V.drop c $ getCol c m

allPairs :: Int -> Int -> [(Int, Int)]
allPairs n m = [(i, j) | i <- [1..n], j <- [1..m]]

testIO :: IO ()
testIO = do
  content <- lines <$> readFile "/home/phi/Documents/codes/haskell/advent-of-code/inputs/DayEight.txt"
  putStrLn $ prettyMatrix (mkTreeMap content)