module DayFifteen
  ( processInputDay15,
    Sensor (..),
  )
where

-- Does this works? Yes
-- Is it fast? No it takes too much time
-- Don't run this

import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

data Sensor = Sensor
  { pos :: (Int, Int),
    beacon :: (Int, Int),
    dist :: Int
  }
  deriving (Show)

processInputDay15 :: IO ()
processInputDay15 = do
  sensors <- map readSensorLine . T.lines <$> T.readFile "/home/phi/Documents/codes/haskell/advent-of-code/inputs/DayFifteen.txt"
  let (minx, maxx) = minmax $ fst . pos <$> foldl' (\acc s -> s : s {pos = (fst s.pos + s.dist, snd s.pos)} : s {pos = (fst s.pos - s.dist, snd s.pos)} : acc) [] sensors
  let coordinates = [(x, 2000000) | x <- [minx .. maxx]] :: [(Int, Int)]
  -- let positionsOccupiedBySensorRanges = foldl' (\set sen -> set `Set.union` generatePointsOfSensor sen) Set.empty sensors
  let possiblePositions = [(x, y) :: (Int, Int) | x <- [0 .. 4000000], y <- [0 .. 4000000]]
  let position = findBeaconPos sensors possiblePositions
  let c = length . filter id $ map (cannotContainABeacon sensors) coordinates
  print (c, tuningFrequency position)

generatePointsOfSensor :: Sensor -> Set.Set (Int, Int)
generatePointsOfSensor s = Set.fromList [(x, y) | x <- [fst s.pos - s.dist .. fst s.pos + s.dist], y <- [snd s.pos - s.dist .. snd s.pos + s.dist], distance (x, y) s.pos <= s.dist]

minmax :: Ord a => [a] -> (a, a)
minmax [] = error "minmax on empty list"
minmax (x : xs) = findMinMax xs (x, x)
  where
    findMinMax [] acc = acc
    findMinMax (l : ls) (y, z) = findMinMax ls (min y l, max z l)

distance :: (Int, Int) -> (Int, Int) -> Int
distance (p1, p2) (q1, q2) = abs (p1 - q1) + abs (p2 - q2)

tuningFrequency :: (Int, Int) -> Integer
tuningFrequency (fromIntegral -> x, fromIntegral -> y) = x * 4000000 + y

findBeaconPos :: [Sensor] -> [(Int, Int)] -> (Int, Int)
findBeaconPos s (p : ps) = if canContainABeacon s p then p else findBeaconPos s ps
findBeaconPos _ [] = error "Can't find a beacon position"

canContainABeacon :: [Sensor] -> (Int, Int) -> Bool
canContainABeacon [] _ = True
canContainABeacon (s : sx) p = ((distance p s.pos > s.dist) && p /= s.beacon) && canContainABeacon sx p

cannotContainABeacon :: [Sensor] -> (Int, Int) -> Bool
cannotContainABeacon [] _ = False
cannotContainABeacon (s : sx) p = ((distance p s.pos <= s.dist) && p /= s.beacon) || cannotContainABeacon sx p

readSensorLine :: T.Text -> Sensor
readSensorLine t = case map (read . T.unpack) $ getAllTextMatches (t =~ ("[-]?[0-9]+" :: T.Text) :: AllTextMatches [] T.Text) of
  [posx, posy, beaconx, beacony] -> Sensor (posx, posy) (beaconx, beacony) (distance (posx, posy) (beaconx, beacony))
  _ -> error $ "Couldn't read sensor line: " <> T.unpack t
