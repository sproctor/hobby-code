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

distanceRow :: [Int] -> [Int] -> [Int]
distanceRow [] _ = error "empty row"
distanceRow (start:row) prev =
  case prev of
    [] -> start : distanceRow' row prev start
    (p:ps) -> p + start : distanceRow' row ps (p + start)
  where
    distanceRow' [] _ path = []
    distanceRow' (r:rs) [] leftTotal = leftTotal + r : distanceRow' rs [] (leftTotal + r)
    distanceRow' (r:rs) (p:ps) leftTotal
      | leftTotal < p = leftTotal + r : distanceRow' rs ps (leftTotal + r)
      | otherwise = p + r : distanceRow' rs ps (p + r)

solution :: [[Int]] -> [Int] -> Int
solution [] prev = trace (show prev) $ last prev
solution ([]:rows) prev = solution rows prev -- skip empty rows
solution (row:rows) prev = trace (show prev) $ solution rows (distanceRow row prev)

main = do
  contents <- readFile "p081_matrix.txt"
  let lines = split "\n" contents
  let matrix2 = map (map read . split ",") lines :: [[Int]]
  print matrix2
  print $ solution matrix2 []
