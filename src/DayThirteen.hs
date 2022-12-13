module DayThirteen(
  PacketData(..),
  processInputDay13,
  readPacketData,
) where

import Data.Char (isDigit)
import Data.List (elemIndex, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

data PacketData = Number Int | List [PacketData] deriving (Read, Eq)

instance Show PacketData where
  show (Number x) = show x
  show (List l) = show l

instance Ord PacketData where
  compare (Number n1) (Number n2) = compare n1 n2
  compare (List l1) (List l2) = compare l1 l2
  compare n@(Number _) l@(List _) = compare (List [n]) l
  compare l@(List _) n@(Number _) = compare l (List [n])

processInputDay13 :: IO (String, String)
processInputDay13 = do
  args <- getArgs
  content <- filter (not . null) . lines <$> readFile (head args)
  let unsortedData = map readPacketData content
  let toCompare = map lstToPair (chunksOf 2 unsortedData)
  let part1 = sum $ map snd $ filter fst $ zip (map (uncurry (<=)) toCompare) ([1 ..] :: [Int])
  let sortedPackets = sort $ unsortedData ++ [[List [Number 2]], [List [Number 6]]]
  let posDivider2 = (+ 1) $ fromJust $ elemIndex [List [Number 2]] sortedPackets
  let posDivider6 = (+ 1) $ fromJust $ elemIndex [List [Number 6]] sortedPackets
  let part2 = posDivider2 * posDivider6
  pure (show part1, show part2)

lstToPair :: Show a => [a] -> (a, a)
lstToPair [a, b] = (a, b)
lstToPair a = error ("List had more than two elements: " ++ show a)

readPacketData :: String -> [PacketData]
readPacketData = getList . read . makeInputReadable

getList :: PacketData -> [PacketData]
getList (List x) = x
getList _ = error "Not a List"

makeInputReadable :: String -> String
makeInputReadable [] = []
makeInputReadable ('[' : xs) = ("List " ++ "[") ++ makeInputReadable xs
makeInputReadable ('1' : '0' : xs) = "Number 10 " ++ makeInputReadable xs
makeInputReadable (x : xs) = if isDigit x then ("Number " ++ [x] ++ " ") ++ makeInputReadable xs else x : makeInputReadable xs
