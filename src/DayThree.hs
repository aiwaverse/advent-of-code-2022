module DayThree
  ( processInputDay3,
    findBadges,
    rucksackCompartments,
    findCommonItem,
    priority,
  )
where

import Data.Char (isLower, ord)
import Data.List (foldl1')
import Data.Set (elemAt, fromList, intersection)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

processInputDay3 :: IO (String, String)
processInputDay3 = do
  args <- getArgs
  content <- T.readFile (head args)
  let operationOne = priority . findCommonItem . rucksackCompartments
  let fileLines = T.lines content
  let fileLinesString = map T.unpack fileLines
  pure (show . sum . map operationOne $ fileLines, show . sum . findBadges $ fileLinesString)

findBadges :: [String] -> [Int]
findBadges (r1 : r2 : r3 : rr) = priority badge : findBadges rr
  where
    setOfThree = foldl1' intersection (map fromList [r1, r2, r3])
    badge = elemAt 0 setOfThree
findBadges [] = []
findBadges _ = [0]

rucksackCompartments :: T.Text -> (T.Text, T.Text)
rucksackCompartments r = T.splitAt (div (T.length r) 2) r

findCommonItem :: (T.Text, T.Text) -> Char
findCommonItem (T.unpack -> r1, T.unpack -> r2) = elemAt 0 $ intersection (fromList r1) (fromList r2)

priority :: Char -> Int
priority 'a' = 1
priority 'A' = 27
priority c@(isLower -> True) = 1 + ord c - ord 'a'
priority c@(isLower -> False) = 27 + ord c - ord 'A'
priority c = priority c -- apparently HLS can't see this is unecessary
