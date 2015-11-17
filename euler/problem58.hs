import Helpers

-- example used for https://github.com/sebfisch/primes/issues/4

{-
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime x
  | x < 2 || x `mod` 2 == 0 = False
  | True = aux 3
  where
    aux n
      | n * n > x      = True
      | x `mod` n == 0 = False
      | True           = aux (n+2)
-}

solution :: Integer -> Int -> (Integer, Int, Integer)
solution l c
  | (fromIntegral newC) / (fromIntegral total) < 0.1 = (l, newC, total)
  | l > 100000 = (l, newC, total)
  | otherwise = solution (l+2) newC
  where
    maxD = l^2
    total = 2 * l - 1
    newC = c + (if isPrime (maxD              ) then 1 else 0)
             + (if isPrime (maxD -     (l - 1)) then 1 else 0)
             + (if isPrime (maxD - 2 * (l - 1)) then 1 else 0)
             + (if isPrime (maxD - 3 * (l - 1)) then 1 else 0)

main = print (solution 3 0)
