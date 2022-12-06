module DaySix
  ( processInputDay6,
    findMarker,
  )
where

import System.Environment (getArgs)

processInputDay6 :: IO (String, String)
processInputDay6 = do
  args <- getArgs
  content <- readFile (head args)
  pure (show $ findMarker content 4, show $ findMarker content 14)

findMarker :: String -> Int -> Int
findMarker s point = inner s point
  where
    inner ss@(_ : _) acc = if diff $ take point ss then acc else inner (drop 1 ss) (acc + 1)
    inner _ acc = acc
    diff [] = True
    diff (c : cs) = c `notElem` cs && diff cs
