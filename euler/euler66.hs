isqrt :: Integer -> Integer
isqrt = truncate . sqrt . fromInteger

isSquare :: Integer -> Bool
isSquare n =
  root * root == n
  where
    root = isqrt n

fact n = aux 2 n []
  where
    aux k n factors
      | k ^ 2 > n = n:factors
      | n `mod` k == 0 = aux 2 (n `div` k) (k:factors)
      | otherwise = aux (k + 1) n factors

checkX :: Integer -> Integer -> Bool
checkX x d
  | x == 1 = False
  | otherwise = isSquare ((x * x - 1) `quot` d)

continuedFract s =
  aux 0 1 a0
  where
    a0 = isqrt s
    aux mprev dprev aprev
      | a == 2 * a0 = [a]
      | otherwise = a:(aux m d a)
      where
        m = dprev * aprev - mprev
        d = s - m^2 `div` dprev
        a = (a0 + m) `div` d
    
findX :: Integer -> Integer -> Integer
findX n d
  | checkX x1 d = x1
  | checkX x2 d = x2
  | checkX x3 d = x3
  | checkX x4 d = x4
  | otherwise = findX (n + 1) d
  where
    frac = continuedFraction d
    r = length frac - 1
    t = d * n * n
    x1 =     t - 1
    x2 =     t + 1
    x3 = t + t - 1
    x4 = t + t + 1

--findSolution :: Integer -> Integer -> Integer
findSolution :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
findSolution d rs
  | d > 160 = rs
  | isSquare d = findSolution (d + 1) rs
  | otherwise = findSolution (d + 1) ((d, x):rs)
  where
    x = findX 1 d
    --new_max = if x > max then x else max

printSolution :: [(Integer, Integer)] -> IO ()
printSolution [] = return ()
printSolution ((d, x):rest) =
  do
    print (d, (fact d), x, (fact (x + 1)), (fact (x - 1)))
    printSolution rest

main =
    printSolution (findSolution 2 [])
