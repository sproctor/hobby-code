getPrimes x =
  let
    aux n primes =
      if n > x
        then primes
        else aux p (primes++[p])
      where p = getNextPrime n primes
  in aux 3 [2, 3]

isPrime x primes
  | x == 2 = True
  | x < 2  = False
  | True   =
    let
      aux n [] = True {- only works up to 1,000,000 (or the max prime squared -}
      aux _ (n:ns)
        | n * n > x      = True
        | x `mod` n == 0 = False
        | True           = aux (n+2) ns
    in aux 3 primes

getNextPrime x primes
  | isPrime next primes = next
  | True                = getNextPrime (x+1) primes
  where
    next = x + 1

primeSequence x primes = aux 0 x 0
  where
    aux lastSum n lastCount =
      if sum > 1000000
        then (0, 0)
        else
          if nextCount > count
            then (nextCount, nextSum)
            else
              if isPrime sum primes
                then (count, sum)
                else (0, 0)
      where sum = lastSum + n
            count = lastCount + 1
            (nextCount, nextSum) = aux sum (getNextPrime n primes) (count)
        
solution x primes cur@(curSeq, _)
  | x > 1000000 = cur
  | seq > curSeq = solution next primes (seq, sum)
  | otherwise = solution next primes cur
  where
    next = getNextPrime x primes
    (seq, sum) = primeSequence x primes

isqrt = truncate . sqrt . fromInteger

main = print $ solution 2 (getPrimes (isqrt 1000000)) (0,0)
