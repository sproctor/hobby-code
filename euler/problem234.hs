isPrime :: Integer -> Bool
isPrime 2 = True
isPrime x
  | x < 2 || x `mod` 2 == 0 = False
  | True = aux 3
  where
    aux n
      | n * n > x      = True
      | x `mod` n == 0 = False
      | otherwise      = aux (n+2)

nextPrime x =
  if isPrime x
    then x
    else nextPrime (x+1)

isDivisible d x = x `mod` d == 0

solution p1 p2 max acc =
  if p2 * p2 > max
    then acc
         + sum (map (*p1) (filter (not . isDivisible p2) [(p1+1)..(max `div` p1)]))
         + sum (map (*p2) (filter (not . isDivisible p1) [(((p1*p1) `div` p2) + 1)..(max `div` p2)]))
    else solution p2 pn max (acc + sum (map (*p1) (filter (not . isDivisible p2) l1))
                                 + sum (map (*p2) (filter (not . isDivisible p1) l2)))
  where
    pn = nextPrime (p2+1)
    l1 = [(p1+1)..((p2 * p2) `div` p1)]
    l2 = [(((p1*p1) `div` p2) + 1)..(p2 - 1)]

main = print (solution 2 3 999966663333 0)
