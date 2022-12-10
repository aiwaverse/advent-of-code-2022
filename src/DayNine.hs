module DayNine
  ( processInputDay9,
    getPositionsVisited,
    move,
  )
where

import Data.List (uncons)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

data Motion = L | R | U | D deriving (Show)

processInputDay9 :: IO (String, String)
processInputDay9 = do
  args <- getArgs
  content <- T.lines <$> T.readFile (head args)
  let motions = processFile content
  pure (show $ length $ getPositionsVisited (replicate 2 (0, 0)) motions Set.empty, show $ length $ getPositionsVisited (replicate 10 (0, 0)) motions Set.empty)

getPositionsVisited :: [(Int, Int)] -> [Motion] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
getPositionsVisited (last -> t) [] s = Set.insert t s
getPositionsVisited knots (m : ms) s = let newKnots = move knots m in getPositionsVisited newKnots ms (Set.insert (last knots) s)

move :: [(Int, Int)] -> Motion -> [(Int, Int)]
move (fromJust . uncons -> (h, t)) mt =
  let newHead = moveHead h mt
   in newHead : moveTails newHead t

moveTails :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
moveTails _ [] = []
moveTails knot (x : xs) = let newTail = moveTail knot x in newTail : moveTails newTail xs

processFile :: [T.Text] -> [Motion]
processFile [] = []
processFile (t : ts) = case T.words t of
  ["R", T.unpack -> i] -> replicate (read i) R ++ processFile ts
  ["L", T.unpack -> i] -> replicate (read i) L ++ processFile ts
  ["U", T.unpack -> i] -> replicate (read i) U ++ processFile ts
  ["D", T.unpack -> i] -> replicate (read i) D ++ processFile ts
  _ -> error ("Line wasn't correct: " <> T.unpack t)

moveHead :: (Int, Int) -> Motion -> (Int, Int)
moveHead (headX, headY) mt = case mt of
  R -> (headX + 1, headY)
  L -> (headX - 1, headY)
  U -> (headX, headY + 1)
  D -> (headX, headY - 1)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail h@(headX, headY) t@(tailX, tailY) = if headX == tailX || headY == tailY then simpleMove h t else diagonalMove h t
  where
    simpleMove (hx, hy) (tx, ty)
      | hx - tx > 1 = (tx + 1, ty)
      | tx - hx > 1 = (tx - 1, ty)
      | hy - ty > 1 = (tx, ty + 1)
      | ty - hy > 1 = (tx, ty - 1)
      | otherwise = (tx, ty)
    diagonalMove (hx, hy) (tx, ty)
      | hx - tx + hy - ty > 2 = (tx + 1, ty + 1)
      | hx - tx + ty - hy > 2 = (tx + 1, ty - 1)
      | tx - hx + hy - ty > 2 = (tx - 1, ty + 1)
      | tx - hx + ty - hy > 2 = (tx - 1, ty - 1)
      | otherwise = (tx, ty)
