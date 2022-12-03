module DayTwo where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

data Play = Rock | Paper | Scissors deriving (Show)

processInputDay2 :: IO (String, String)
processInputDay2 = do
  args <- getArgs
  content <- T.readFile (args !! 0)
  pure (show . totalScore $ transformInputPartOne content, show . totalScore $ transformInputPartTwo content)

totalScore :: [(Play, Play)] -> Integer
totalScore = sum . map (uncurry score)

score :: Play -> Play -> Integer
score opponent me = value me + result opponent me

value :: Play -> Integer
value Rock = 1
value Paper = 2
value Scissors = 3

result :: Play -> Play -> Integer
result Rock Paper = 6
result Rock Scissors = 0
result Paper Scissors = 6
result Paper Rock = 0
result Scissors Rock = 6
result Scissors Paper = 0
result _ _ = 3

transformInputPartTwo :: T.Text -> [(Play, Play)]
transformInputPartTwo text = map determinePlayOpponent $ map (T.splitOn " ") (T.lines text)
  where
    determinePlayOpponent ["A", me] = (Rock, calculatePlay me Rock)
    determinePlayOpponent ["B", me] = (Paper, calculatePlay me Paper)
    determinePlayOpponent ["C", me] = (Scissors, calculatePlay me Scissors)
    determinePlayOpponent _ = error "Invalid file"

calculatePlay :: T.Text -> Play -> Play
calculatePlay "X" = losePlay
calculatePlay "Y" = id
calculatePlay "Z" = winPlay
calculatePlay _ = error "Invalid file"

winPlay :: Play -> Play
winPlay Rock = Paper
winPlay Paper = Scissors
winPlay Scissors = Rock

losePlay :: Play -> Play
losePlay Paper = Rock
losePlay Scissors = Paper
losePlay Rock = Scissors

transformInputPartOne :: T.Text -> [(Play, Play)]
transformInputPartOne text = map determinePlayOpponent $ map (T.splitOn " ") (T.lines text)
  where
    determinePlayOpponent ["A", me] = (Rock, determinePlayMe me)
    determinePlayOpponent ["B", me] = (Paper, determinePlayMe me)
    determinePlayOpponent ["C", me] = (Scissors, determinePlayMe me)
    determinePlayOpponent _ = error "Invalid file"
    determinePlayMe "X" = Rock
    determinePlayMe "Y" = Paper
    determinePlayMe "Z" = Scissors
    determinePlayMe _ = error "Invalid file"
