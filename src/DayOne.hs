module DayOne
  ( processInput,
    generateElfCaloriesList,
    findCaloriesOfElfWithMostCalories,
    findCaloriesOfThreeElvesWithMostCalories,
  )
where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down (Down))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

processInput :: IO (String, String)
processInput = do
  args <- getArgs
  content <- T.readFile (args !! 0)
  let elfLines = T.lines content
  let calories = map sum $ generateElfCaloriesList elfLines
  pure (show $ findCaloriesOfElfWithMostCalories calories, show $ findCaloriesOfThreeElvesWithMostCalories calories)

generateElfCaloriesList :: [T.Text] -> [[Integer]]
generateElfCaloriesList xs = map (map (read . T.unpack)) $ splitOn [""] xs

findCaloriesOfElfWithMostCalories :: [Integer] -> Integer
findCaloriesOfElfWithMostCalories = head . sortOn Down

findCaloriesOfThreeElvesWithMostCalories :: [Integer] -> Integer
findCaloriesOfThreeElvesWithMostCalories = sum . take 3 . sortOn Down
