module DayFourteen where

import Data.List (find, foldl')
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import qualified Data.Map as M
import qualified Data.Set as S

type Cave = M.Map Int (S.Set Int)

processInputDay14 :: IO ()
processInputDay14 = do
  --args <- getArgs
  content <- T.lines <$> T.readFile "/home/phi/Documents/codes/haskell/advent-of-code/inputs/DayFourteen.txt"
  let coords = concatMap (fillGapsOfLines . pairUp . getCoordinatesFromText) content
  print (createCaveWithRocks M.empty coords)

positionToDrop :: Cave -> (Int, Int) -> Maybe (Int, Int)
positionToDrop c (x, y) = undefined

createCaveWithRocks :: Cave -> [(Int, Int)] -> Cave
createCaveWithRocks = foldl' insertTile

fillGapsOfLines ::  [((Int, Int), (Int, Int))] -> [(Int, Int)]
fillGapsOfLines = concatMap stepBetween

stepBetween :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
stepBetween ((x1, y1), (x2, y2))
  | x1 == x2  = [(x1, y) | y <- if y1 < y2 then [y1..y2] else [y1,y1-1..y2]]
  | y1 == y2  = [(x, y1) | x <- if x1 < x2 then [x1..x2] else [x1,x1-1..x2]]
  | otherwise = []

pairUp :: [(Int, Int)] -> [((Int, Int), (Int, Int))]
pairUp [] = []
pairUp (x : xs : xxs) = (x, xs) : pairUp (xs : xxs)
pairUp _ = []

getCoordinatesFromText :: T.Text -> [(Int, Int)]
getCoordinatesFromText t = map (\tt -> read $ T.unpack $ "(" <> tt <> ")") $ T.splitOn " -> " t

insertLineOfRocks :: Cave -> [(Int, Int)] -> Cave
insertLineOfRocks = foldl insertTile

insertTile :: Cave -> (Int, Int) -> Cave
insertTile cave (col, row) = M.insertWith S.union col (S.singleton row) cave