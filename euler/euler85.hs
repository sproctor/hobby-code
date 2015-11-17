countRectangles x y =
  x' * y'
  where
    x' = (x * (x+1)) `div` 2
    y' = (y * (y+1)) `div` 2

target = 2000000

solution x y s@(_, _, best)
  | x > y = solution 1 (y+1) newS
  | r > target && x == 1 = newS
  | r > target = solution 1 (y+1) newS
  | otherwise = solution (x+1) y newS
  where
    r = countRectangles x y
    d = abs(target - r)
    newS
      | d < best = (x, y, d)
      | otherwise = s

main =
  print $ solution 1 1 (1, 1, target)
