module DayThree where

import Data.Char (isLower, ord)
import Data.Set (elemAt, fromList, intersection)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

processInputDay3 :: IO (String, String)
processInputDay3 = do
  args <- getArgs
  content <- T.readFile (args !! 0)
  let operation = priority . findCommonItem . rucksackCompartments
  let fileLines = T.lines content
  pure (show . sum . (map operation) $ fileLines, "")

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
