module DayTen
  ( processInputDay10,
    readInst,
    execution,
    getRelevantSginalStrengths,
    drawLine,
    display,
  )
where

import Data.List (foldl', intercalate, mapAccumL)
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

processInputDay10 :: IO ()
processInputDay10 = do
  args <- getArgs
  content <- T.lines <$> T.readFile (head args)
  let exec = execution content 1
  let cycles = [20, 60, 100, 140, 180, 220]
  let strength = getRelevantSginalStrengths cycles exec
  let instLines = dropLast $ chunksOf 40 exec
  let crt = display $ map (drawLine 0) instLines
  print strength
  putStrLn crt

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

readInst :: T.Text -> [Int -> Int]
readInst i = case T.words i of
  ["noop"] -> [id]
  ["addx", read . T.unpack -> n] -> [id, (+ n)]
  _ -> error ("Can't read instruction " <> T.unpack i)

execution :: [T.Text] -> Int -> [Int]
execution ts regX = reverse $ foldl' (\regs inst -> inst (head regs) : regs) [regX] insts
  where
    insts = concatMap readInst ts

getRelevantSginalStrengths :: [Int] -> [Int] -> Int
getRelevantSginalStrengths cycles exec = foldl' (\total c -> total + ((exec !! (c - 1)) * c)) 0 cycles

drawLine :: Int -> [Int] -> String
drawLine _ [] = []
drawLine point (x : xs)
  | (point - x) `between` (-1, 1) = '#' : drawLine (point + 1) xs
  | otherwise = ' ' : drawLine (point + 1) xs

between :: Ord a => a -> (a, a) -> Bool
between a (x, y) = a >= x && a <= y

display :: [String] -> String
display = intercalate "\n"
