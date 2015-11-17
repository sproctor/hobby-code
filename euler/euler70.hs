import Helpers

findSolution :: Integer -> (Integer, Double) -> (Integer, Double)
findSolution n solution@(_,ratio)
  | n == 10000000 = solution
  | r < ratio && isPermutation n t = findSolution (n+1) (n,r)
  | otherwise = findSolution (n+1) solution
  where
    t = totient n
    r = (fromIntegral n) / (fromIntegral t)

main =
  print $ findSolution 3 (2, 1.5)
