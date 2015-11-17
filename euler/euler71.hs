import Helpers

hcf = gcd

findN n d
  | hcf n d == 1 = n
  | otherwise = findN (n - 1) d

solution d cur@(curN, curD, curR)
  | d > 1000000 = cur
  | r > curR = solution (d+1) (n, d, r)
  | otherwise = solution (d+1) cur
  where
    n = findN ((3 * d) `div` 7) d
    r = (fromIntegral n) / (fromIntegral d)

main = do
  print $ hcf 8 2
  print $ hcf 11 3
  print $ hcf 20 15
  print $ hcf 11 121
  print $ hcf 11 11
  print $ hcf 39 91
  print $ solution 8 (2, 5, 0.4)
