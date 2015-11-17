import Data.List

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

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [y : ps | (y,ys) <- selections xs, ps <- perms ys]
selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]

getPermutations :: Integer -> [Integer]
getPermutations x = sort (map read (perms (show x)))

getPrimePermutations :: Integer -> [Integer]
getPrimePermutations number = nub (filter (\x-> x >= 1000 && isPrime x) (getPermutations number))

isSequence []         = False
isSequence [_]        = False
isSequence (x:y:rest) | diff <= 0 = False
                      | True      = aux (y:rest) 2 || isSequence (x:rest) || isSequence (y:rest)
  where diff = y - x
        aux [_] terms = terms >= 3
        aux (a:b:r) terms
          | b-a == diff = aux (b:r) (terms+1)
          | True        = aux (a:r) terms

getPrimes :: Integer -> Maybe [Integer]
getPrimes x
  | not (isPrime x) = Nothing
  | True            = if length nums >= 3 && isSequence nums then Just nums else Nothing
  where nums = getPrimePermutations x

solution start end =
  let loop p result | p > end = result
                    | True    = loop (p+1) (maybe result (\x->x:result) (getPrimes p))
  in loop start []

main = print (solution 1000 9999)
