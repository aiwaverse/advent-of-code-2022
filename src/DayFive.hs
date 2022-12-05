module DayFive
  ( processInputDay5,
    moveA,
    moveB,
    processCrateLine,
    processMovementLine,
    extractInfo,
  )
where

import Data.Bifunctor (Bifunctor (second))
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

processInputDay5 :: IO (String, String)
processInputDay5 = do
  args <- getArgs
  content <- T.readFile (head args)
  let (crates, movements) = extractInfo content
  let processedCrates = processCrateLine <$> crates
  let processedMovements = processMovementLine <$> movements
  let joinedCrates = [mconcat $ (!! i) <$> processedCrates | i <- [0 .. head (length <$> processedCrates) - 1]]
  let processedInputA = foldl' (flip moveA) joinedCrates processedMovements
  let processedInputB = foldl' (flip moveB) joinedCrates processedMovements
  let topCratesA = map T.head processedInputA
  let topCratesB = map T.head processedInputB
  pure (show topCratesA, show topCratesB)

moveA :: (Int, Int, Int) -> [T.Text] -> [T.Text]
moveA (q, s, f) crates = added
  where
    removed = [if i == s then T.drop q crate else crate | (crate, i) <- zip crates [1 ..]]
    toAdd = T.take q $ crates !! (s - 1)
    added = [if i == f then T.foldl' (flip T.cons) crate toAdd else crate | (crate, i) <- zip removed [1 ..]]

moveB :: (Int, Int, Int) -> [T.Text] -> [T.Text]
moveB (q, s, f) crates = added
  where
    removed = [if i == s then T.drop q crate else crate | (crate, i) <- zip crates [1 ..]]
    toAdd = T.take q $ crates !! (s - 1)
    added = [if i == f then toAdd <> crate else crate | (crate, i) <- zip removed [1 ..]]

processCrateLine :: T.Text -> [T.Text]
processCrateLine t = T.strip . T.filter (`notElem` ['[', ']']) <$> T.chunksOf 4 t

processMovementLine :: T.Text -> (Int, Int, Int)
processMovementLine t = (read $ w !! 1, read $ w !! 3, read $ w !! 5)
  where
    w = words . T.unpack $ t

extractInfo :: T.Text -> ([T.Text], [T.Text])
extractInfo = second (drop 2) . span ((/= ' ') . T.head) . T.lines
