module DayTwelve
  ( processInputDay12,
    part1,
    part2,
    runAStar,
    adjacencyFunction,
    createGraph,
    getAdj,
  )
where

import Control.Monad (liftM2)
import Data.Char (ord)
import Data.Graph (Graph, Vertex, graphFromEdges, vertices)
import Data.Graph.AStar (aStar)
import Data.HashSet (HashSet, fromList)
import Data.List (foldl', minimumBy, sortOn)
import Data.Maybe (fromJust, isJust, mapMaybe)
import System.Environment (getArgs)

processInputDay12 :: IO (String, String)
processInputDay12 = do
  args <- getArgs
  content <- lines <$> readFile (head args)
  let g = createGraph content
  pure (show $ part1 g, show $ part2 g)

part1 ::
  ( Graph,
    Vertex -> (Char, (Int, Int), [(Int, Int)]),
    (Int, Int) -> Maybe Vertex
  ) ->
  Int
part1 g@(graph, nodeFromVertex, _) = maybe (-1) length $ runAStar g s e
  where
    ((_, e, _), (_, s, _)) =
      lstToPair $ sortOn (\(node, _, _) -> node) $ filter (\(node, _, _) -> node == 'S' || node == 'E') $ map nodeFromVertex (vertices graph)

part2 ::
  ( Graph,
    Vertex -> (Char, (Int, Int), [(Int, Int)]),
    (Int, Int) -> Maybe Vertex
  ) ->
  Int
part2 g@(graph, nodeFromVertex, _) = length $ minimumBy (\a b -> compare (length a) (length b)) $ mapMaybe (\s -> runAStar g s e) starts
  where
    (_, e, _) = head $ filter (\(node, _, _) -> node == 'E') $ map nodeFromVertex (vertices graph)
    starts = map (\(_, ss, _) -> ss) $ filter (\(node, _, _) -> node == 'a' || node == 'S') $ map nodeFromVertex (vertices graph)

runAStar ::
  ( Graph,
    Vertex -> (Char, (Int, Int), [(Int, Int)]),
    (Int, Int) -> Maybe Vertex
  ) ->
  (Int, Int) ->
  (Int, Int) ->
  Maybe [(Int, Int)]
runAStar g s e = aStar (adjacencyFunction g) (\_ _ -> 1 :: Int) (const 0) (== e) s

lstToPair :: Show a => [a] -> (a, a)
lstToPair [a, b] = (a, b)
lstToPair a = error ("List with S and E wasn't correct: " ++ show a)

adjacencyFunction :: (Graph, Vertex -> (Char, (Int, Int), [(Int, Int)]), (Int, Int) -> Maybe Vertex) -> (Int, Int) -> HashSet (Int, Int)
adjacencyFunction g a = fromList adj
  where
    (_, nodeFromVertex, vertexFromKey) = g
    (_, _, adj) = nodeFromVertex . fromJust . vertexFromKey $ a

createGraph :: [String] -> (Graph, Vertex -> (Char, (Int, Int), [(Int, Int)]), (Int, Int) -> Maybe Vertex)
createGraph stringList = graphFromEdges $ runLines stringList 0 0 []
  where
    runLines sl row col material
      | row == length sl = material
      | col == length (sl !! row) = runLines sl (row + 1) 0 material
      | otherwise = let c = (sl !! row) !! col in runLines sl row (col + 1) (material ++ [(c, (row, col), getAdj sl row col c)])

getAdj :: [String] -> Int -> Int -> Char -> [(Int, Int)]
getAdj stringList row col c =
  foldl'
    ( \acc e ->
        if isJust e && (ord . trsfm . fst . fromJust $ e) <= ord (trsfm c) + 1
          then snd (fromJust e) : acc
          else acc
    )
    []
    [l, r, u, d]
  where
    trsfm cc
      | cc == 'S' = 'a'
      | cc == 'E' = 'z'
      | otherwise = cc
    l = liftM2 (,) ((stringList !!! row) >>= (!!! (col - 1))) (Just (row, col - 1))
    r = liftM2 (,) ((stringList !!! row) >>= (!!! (col + 1))) (Just (row, col + 1))
    u = liftM2 (,) ((stringList !!! (row - 1)) >>= (!!! col)) (Just (row - 1, col))
    d = liftM2 (,) ((stringList !!! (row + 1)) >>= (!!! col)) (Just (row + 1, col))

(!!!) :: [a] -> Int -> Maybe a
_ !!! (signum -> (-1)) = Nothing
[] !!! _ = Nothing
(x : xs) !!! i = if i == 0 then Just x else xs !!! (i - 1)
