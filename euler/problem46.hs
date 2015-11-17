isPrime :: Integer -> Bool
isPrime 2 = True
isPrime x
  | x `mod` 2 == 0 || x < 2 = False
  | True                  = aux 3
  where
    max_factor = truncate (sqrt (fromInteger x))
    aux n
      | n > max_factor = True
      | x `mod` n == 0   = False
      | True           = aux (n + 2)
  
checkNum :: Integer -> Bool
checkNum n =
  let
    aux s
      | squareTimesTwo >= n         = False
      | isPrime(n - squareTimesTwo) = True
      | True                        = aux (s + 1)
      where
        squareTimesTwo = 2 * s * s
  in aux 1

checkNums n =
  let
    aux p
      | p == n           = 0
      | isPrime p        = aux (p + 2)
      | not (checkNum p) = p
      | True             = aux (p + 2)
  in aux 9

main = print (checkNums 10000)
