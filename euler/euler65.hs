sumDigits n
  | n == 0 = 0
  | otherwise = (n `mod` 10) + sumDigits (n `quot` 10)

findTerm prev prev2 k
  = f * prev + prev2
  where
    f = if k `mod` 3 == 0 then (k `div` 3) * 2
        else 1

findSolution prev prev2 k
  | k == 100 = sumDigits term
  | otherwise = findSolution term prev (k + 1)
  where
    term = findTerm prev prev2 k

main =
  print (findSolution 1 1 1)
