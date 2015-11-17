import Helpers

findSolution :: Integer -> (Integer, Double) -> (Integer, Double)
findSolution n solution@(_,max)
  | n > 1000000 = solution
  | s > max = findSolution (n+1) (n,s)
  | otherwise = findSolution (n+1) solution
  where
    s = (fromIntegral n) / (fromIntegral (totient n))

main =
  print $ findSolution 2 (1, 1.0)
