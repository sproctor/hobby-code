pow :: Integer -> Integer -> Integer
pow x y = aux y 1
  where
    aux n r
      | n == 0         = r
      | n `mod` 2 == 0 = r * pow (x*x) (n `div` 2)
      | True           = aux (n-1) (r*x)

countDigits :: Integer -> Integer
countDigits x =
  let
    aux n =
      if x `div` (pow 10 n) == 0
        then n
        else aux (n+1)
  in aux 1

count num n
  | countDigits (pow num n) == n = count num (n+1)
  | otherwise                    = (n-1)

loop n acc
  | n == 0 = acc
  | otherwise = loop (n-1) (acc + count n 1)

main = print (loop 9 0)
