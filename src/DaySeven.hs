module DaySeven where

import Data.Foldable (Foldable (foldl'))
import Data.Map (Map, insert, insertWith, empty)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (intersperse)
import Debug.Trace
import System.Environment (getArgs)

type Directories = Map T.Text Integer

type Path = [T.Text]

processSystemLine :: [T.Text] -> Path -> Directories -> Directories
processSystemLine [] _ folders = folders
processSystemLine (x : xs) path folders = case T.words x of
  ["$", "cd", ".."] -> processSystemLine xs (dropLast path) folders
  ["$", "cd", dirName] -> processSystemLine xs (path ++ ["/", dirName]) folders
  ["dir", dirName] -> processSystemLine xs path (insert (mconcat $ path ++ ["/", dirName]) 0 folders)
  ["$", "ls", _] -> processSystemLine xs path folders
  [T.unpack -> size, _] -> trace size $
    processSystemLine xs path (insertWith (+) (mconcat path) (read size) folders)
  _ -> processSystemLine xs path folders

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

testIO :: IO ()
testIO = do
  content <- drop 1 . T.lines <$> T.readFile "/home/phi/Documents/codes/haskell/advent-of-code/inputs/test.txt"
  print $ T.words <$> content
