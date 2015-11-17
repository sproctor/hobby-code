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

getPositions :: Integer -> Integer -> [Integer]
getPositions x digit = aux 1 x []
  where
    aux pos rest acc
      | rest == 0  = acc
      | m == digit = aux (pos+1) d (pos:acc)
      | otherwise  = aux (pos+1) d acc
      where (d,m) = divMod rest 10

replace :: Integer -> [Integer] -> Integer -> Integer
replace x [] _ = x
replace x (n:rest) digit = replace num rest digit
  where
    rem = 10^(n-1)
    num = ((x `div` (10^n)) * 10 + digit) * rem + (x `mod` rem)

ilength = fromIntegral . length

firstDigit :: Integer -> Integer
firstDigit x =
  if x > 10 then firstDigit (quot x 10)
            else x

checkDigit :: Integer -> Integer -> Integer -> [Integer]
checkDigit x digit primes = if positions == [] then [] else checkNumber digit []
  where
    positions = getPositions x digit
    checkNumber :: Integer -> [Integer] -> [Integer]
    checkNumber n acc
      | primes - ilength acc > 10 - n = acc
      | ilength acc == primes         = acc
      | n > 9                         = acc
      | isPrime number                = checkNumber (n+1) (number:acc)
      | otherwise                     = checkNumber (n+1) acc
      where number = (replace x positions n)

checkNumber :: Integer -> Integer -> Maybe [Integer]
checkNumber x primes
  | x > 1000000                = Nothing
  | ilength zresults >= primes = Just zresults
  | ilength fresults >= primes = Just fresults
  | otherwise                  = checkNumber (x+1) primes
  where zresults = checkDigit x 0 primes
        fresults = checkDigit x (firstDigit x) primes

main = print (checkNumber 10000 8)
