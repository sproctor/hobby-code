import Debug.Trace
import Data.String.Utils

matrix =
  [
    [131, 673, 234, 103, 18],
    [201, 96, 342, 965, 150],
    [630, 803, 746, 422, 111],
    [537, 699, 497, 121, 956],
    [805, 732, 524, 37, 331]
  ]

headMatrix :: [[a]] -> ([a], [[a]])
headMatrix m = unzip $ headMatrix' m

headMatrix' :: [[a]] -> [(a, [a])]
headMatrix' [] = []
headMatrix' ([]:_) = []
headMatrix' ((r:rs):rows) = (r, rs) : headMatrix' rows

distanceRow :: [Int] -> [Int] -> [Int]
distanceRow [] _ = error "empty row"
distanceRow (start:row) prev =
  case prev of
    [] -> start : distanceRow' row [] start
    (p:ps) -> p + start : distanceRow' row ps (p + start)
  where
    distanceRow' [] _ path = []
    distanceRow' (r:rs) [] _ = r : distanceRow' rs [] r
    distanceRow' (r:rs) (p:ps) upPath
      | upPath < p = upPath + r : distanceRow' rs ps (upPath + r)
      | otherwise = p + r : distanceRow' rs ps (p + r)

distanceRowReverse [] [] = []
distanceRowReverse [] _ = error "bad row"
distanceRowReverse _ [] = error "bad row"
distanceRowReverse (start:row) (x:xs) =
  let
    rest = distanceRowReverse row xs
    d =
      case rest of
        [] -> x
        (r:_) -> if r + start < x then r + start else x
  in
    d : rest

solution :: [[Int]] -> [Int] -> Int
solution [] s = trace (show s) $ minimum s
solution ([]:_) s = solution [] s
solution m prev =
  let (row, rows) = headMatrix m in
  let rowDown = distanceRow row prev in
  let rowUp = distanceRowReverse row rowDown in
  trace (show rowDown) $
  trace (show rowUp) $
  solution rows rowUp

main = do
  contents <- readFile "p082_matrix.txt"
  let lines = split "\n" contents
  let matrix2 = map (map read . split ",") lines :: [[Int]]
  print matrix2
  print $ solution matrix2 []
