module DaySeven where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Text as T
import System.Environment (getArgs)

data SystemData = File Int T.Text | Folder Int T.Text [SystemData] | Empty deriving (Show)

data Command = Ls | CdIn T.Text | CdOut | Nop deriving (Show)

processInputDay7 :: IO (String, String)
processInputDay7 = do
  args <- getArgs
  content <- readFile (head args)
  pure ("", "")

lineToCommand :: T.Text -> Command
lineToCommand (T.words -> ["$", "cd", ".."]) = CdOut
lineToCommand (T.words -> ["$", "cd", dir]) = CdIn dir
lineToCommand (T.words -> ["$", "ls"]) = Ls
lineToCommand _ = Nop

processCommandLine :: ([T.Text], SystemData) -> SystemData
processCommandLine ([], currData) = currData
processCommandLine (currCommand : xs, Folder size dir contents) = case command of
  Ls -> processCommandLine (processLs xs (Folder 0 dir []))
  CdIn dirName -> Folder size dir  (processCommandLine (processCdIn xs dirName) : contents)
  CdOut -> undefined
  Nop -> undefined
  where
    command = lineToCommand currCommand
processCommandLine _ = Empty

processLs :: [T.Text] -> SystemData -> ([T.Text], SystemData)
processLs inputs (Folder s n _) = (rest, Folder s n (foldl' (\acc item -> if head (T.words item) == "dir" then acc else lineToFile item : acc) [] lsInfo))
  where
    (lsInfo, rest) = span (\input -> head (T.words input) /= "$") inputs
    lineToFile line =
      let [size, name] = T.words line
       in File (read . T.unpack $ size) name
processLs inputs _ = (inputs, Empty) 

processCdIn :: [T.Text] -> T.Text -> ([T.Text], SystemData)
processCdIn inputs dirName = (drop (length relevantCommands + 1) inputs, processCommandLine (relevantCommands, Folder 0 dirName []))
  where
    relevantCommands = getRelevantCommands inputs 0
    getRelevantCommands :: [T.Text] -> Int -> [T.Text]
    getRelevantCommands [] _ = []
    getRelevantCommands (x : xs) cdCount = case T.words x of
      ["$", "cd", ".."] -> if cdCount == 0 then [] else x : getRelevantCommands xs (cdCount - 1)
      ["$", "cd", _] -> x : getRelevantCommands xs (cdCount + 1)
      _ -> x : getRelevantCommands xs cdCount
