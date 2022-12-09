module DaySeven
  ( processInputDay7,
    processSystemLine,
    sumFolders,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

type Directories = M.Map T.Text Integer

type Path = [T.Text]

processInputDay7 :: IO (String, String)
processInputDay7 = do
  args <- getArgs
  content <- T.lines <$> T.readFile (head args)
  let folders = sumFolders $ processSystemLine content [] M.empty
  let sumOfSizes = sum . M.elems . M.filter (<= 100000) $ folders
  let freeSpace = 70000000 - M.findWithDefault 0 "/" folders
  let neededSpace = 30000000 - freeSpace
  print neededSpace
  let bestFolderToDelete = minimum . M.elems . M.filter (>= neededSpace) $ folders
  pure (show sumOfSizes, show bestFolderToDelete)

processSystemLine :: [T.Text] -> Path -> Directories -> Directories
processSystemLine [] _ folders = folders
processSystemLine (x : xs) path folders = case T.words x of
  ["$", "cd", ".."] -> processSystemLine xs (dropTwoLast path) folders
  ["$", "cd", dirName] ->
    processSystemLine
      xs
      (path ++ (if dirName == "/" then [dirName] else [dirName, "/"]))
      folders
  ["dir", dirName] -> processSystemLine xs path (M.insert (mconcat $ path ++ [dirName, "/"]) 0 folders)
  ["$", "ls"] -> processSystemLine xs path folders
  [size, _] ->
    processSystemLine xs path (M.insertWith (+) (mconcat path) (read $ T.unpack size) folders)
  _ -> processSystemLine xs path folders

sumFolders :: Directories -> Directories
sumFolders m = M.mapWithKey (\key val -> (+ val) . sum . M.elems . M.filterWithKey (\k _ -> key `T.isPrefixOf` k && key /= k) $ m) m

dropTwoLast :: [a] -> [a]
dropTwoLast [] = []
dropTwoLast [_, _] = []
dropTwoLast (x : xs) = x : dropTwoLast xs
