import Debug.Trace

getDecimalPlaces c p y
  | newy == 0 = []
  | c <= 0 = []
  | otherwise =
    -- trace ("getDecimalPlaces " ++ (show p) ++ " " ++ (show y) ++ (show c)) $
      x : getDecimalPlaces (c-1) newp newy
  where
    x = findx 1 p y
    newy = 100 * (y - (20 * p + x) * x)
    newp = p * 10 + x

findx x p y
  | v > y = x - 1
  | otherwise = findx (x+1) p y
  where
    v = (20 * p + x) * x

main =
  print $ sum $ map (sum . getDecimalPlaces 100 0) [0..99]
