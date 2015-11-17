isqrt = truncate . sqrt . fromInteger

getPeriod n
  | n == s * s = 0
  | otherwise = findNext 1 s 0
  where
    s = isqrt n
    findNext a b p
      | p > 0 && a == 1 && b == s = p
      | otherwise = findNext newA newB (p + 1)
      where
        newA = (n - b * b) `quot` a
        newB = findB (-b)
        findB x
          | possibleB > s = x
          | otherwise = findB possibleB
          where
            possibleB = x + newA

listPeriods n max
  | n > max = []
  | otherwise = (getPeriod n):(listPeriods (n + 1) max)

main =
  print (length (filter odd (listPeriods 2 10000)))
