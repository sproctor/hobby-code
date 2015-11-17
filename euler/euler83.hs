import Debug.Trace
import Data.String.Utils
import Data.Matrix
import Data.Maybe

import Helpers

data Vertex = Vertex
    { value :: Int
    , path :: Maybe Int
    , visited :: Bool
    } deriving Show

input =
  [
    [131, 673, 234, 103, 18],
    [201, 96, 342, 965, 150],
    [630, 803, 746, 422, 111],
    [537, 699, 497, 121, 956],
    [805, 732, 524, 37, 331]
  ]

findShortest :: Matrix Vertex -> (Int, Int)
findShortest m =
  aux 1 1 (1, 1, Nothing)
  where
    aux row col short@(r, c, p)
      | row > nrows m = (r, c)
      | col > ncols m = aux (row+1) 1 short
      | otherwise =
          let
            e = getElem row col m
            newShort =
              if visited e
                then short
                else
                  case p of
                    Nothing -> (row, col, path e)
                    Just x ->
                      case path e of
                        Nothing -> short
                        Just y -> if y < x then (row, col, path e) else short
          in aux row (col+1) newShort

findNeighbors :: Int -> Int -> Matrix Vertex -> [(Int, Int)]
findNeighbors row col m =
  (get (row-1) col) ++ (get (row+1) col) ++ (get row (col-1)) ++ (get row (col+1))
  where
    maxr = nrows m
    maxc = ncols m
    get :: Int -> Int -> [(Int, Int)]
    get r c =
      if r > 0 && r <= maxr && c > 0 && c <= maxc && not vis then [(r, c)] else []
      where
        vis = visited $ getElem r c m

updatePath :: Int -> Matrix Vertex -> (Int, Int) -> Matrix Vertex
updatePath p m pos@(r, c) =
  case path (getElem r c m) of
    Nothing -> setElem newElem pos m
    Just x -> if v + p < x then setElem newElem pos m else m
  where
    elem = getElem r c m
    v = value elem
    newElem = Vertex v (Just (v + p)) False

visit :: Int -> Int -> Matrix Vertex -> Matrix Vertex
visit row col m =
  foldl (updatePath p) (setElem newElem (row, col) m) neighbors
  where
    elem = getElem row col m
    neighbors = findNeighbors row col m
    p = case path elem of
        Just x -> x
        Nothing -> error "visiting element with no path"
    newElem = Vertex (value elem) (Just p) True

solution :: Matrix Vertex -> Int
solution m
  | visited dest = fromJust $ path dest
  | otherwise =
    let (r, c) = findShortest m in
    solution $ visit r c m
  where
    dest = getElem (nrows m) (ncols m) m

initialGraph m =
  setElem (Vertex v (Just v) False) (1, 1) graph
  where
    vertexLists = map (map (\x -> Vertex x Nothing False)) m
    graph = fromLists vertexLists
    start = getElem 1 1 graph
    v = value start

main = do
  contents <- readFile "p082_matrix.txt"
  let lines = split "\n" $ trim contents
  let matrix2 = map (map read . split ",") lines :: [[Int]]
  print matrix2
  print $ solution $ initialGraph matrix2
