{-#LANGUAGE TupleSections #-}
module DayFourteen where

import Data.List.Split ( splitOn )
import Data.Matrix hiding ( trace )
import Data.List (foldl', find)
import qualified Data.Vector as V
import System.Environment ( getArgs )

-- I want to apologize for this solution, this is so horrible and almost luck that I want to rewrite this completely
-- But until then, behold, this code only works with these numbers (or larger), don't change them
processInputDay14 :: IO ()
processInputDay14 = do
  args <- getArgs
  content <- lines <$> readFile (head args)
  let linesToDraw = concatMap (concatMap (uncurry calculateCoordinates) . generatePairs . splitOn " -> ") content
  let (cols, rows) = foldl' (\(currX, currY) (x, y) -> (max currX x, max currY y)) (0, 0) linesToDraw
  let cave = fromList (rows + 3) (cols + 150) (cycle ".") -- Don't ask why + 3, or + 150 (part1 works with +3 or larger on the columns too)
  let linesToDrawPart2 = linesToDraw ++ map (,rows + 2) [1 .. cols + 149]
  let caveWithRocks = foldl' (\c (x, y) -> setElem '#' (y + 1, x + 1) c) cave linesToDraw
  let caveWithRocksAndFloor = foldl' (\c (x, y) -> setElem '#' (y + 1, x + 1) c) cave linesToDrawPart2
  print (countSands caveWithRocks 0, countSands caveWithRocksAndFloor 1)

countSands :: Matrix Char -> Int -> Int
countSands m c = case dropSand m (1, 501) of
  Nothing -> c
  Just mm -> countSands mm (c + 1)

-- Matrix are 1 indexed, AOC asks for 0 index
dropSand :: Matrix Char -> (Int, Int) -> Maybe (Matrix Char)
dropSand m pos = case nextPos of
  Nothing -> Nothing
  Just p -> if p == pos then Nothing else Just $ setElem 'o' p m 
  where nextPos = nextSandPos m pos

nextSandPos :: Matrix Char -> (Int, Int) -> Maybe (Int, Int)
nextSandPos m (x, y) = case offset of
  Nothing -> Nothing
  Just n  -> case newPos n of 
    Nothing -> Just (x, y)
    Just np -> nextSandPos m np
  where
    offset = fmap (subtract 1 . snd) $ find (\(c, i) -> (c == '#' || c == 'o') && i > x) $ zip (V.toList $ getCol y m) ([1..] :: [Int])
    newPos ofst | ofst /= x = Just (ofst, y)
                | (y - 1) > 0 && m ! (ofst + 1, y - 1) == '.' = Just (ofst + 1, y - 1)
                | (y + 1) < ncols m && m ! (ofst + 1, y + 1) == '.' = Just (ofst + 1, y + 1)
                | otherwise = Nothing

calculateCoordinates :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
calculateCoordinates s@(startX, startY) e@(endX, endY)
  | startX == endX = verticalLine s e [] (signum (endY - startY))
  | otherwise = horizontalLine s e [] (signum (endX - startX))
 where
   verticalLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
   verticalLine (sx, sy) (ex, ey) acc c = if sy == ey then (ex, ey) : acc else verticalLine (sx, sy + c) (ex, ey) ((sx, sy) : acc) c
   horizontalLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
   horizontalLine (sx, sy) (ex, ey) acc c = if sx == ex then (ex, ey) : acc else horizontalLine (sx + c, sy) (ex, ey) ((sx, sy) : acc) c

generatePairs :: [String] -> [((Int, Int), (Int, Int))]
generatePairs (s : sx : ssx) = read ("((" <> s <> "),(" <> sx <> "))") : generatePairs (sx : ssx)
generatePairs _ = []
