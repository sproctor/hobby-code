import Data.List
import Helpers

checkDigits :: Integer -> Integer -> Integer -> Integer
checkDigits start digits cubes
  | n >= maxN = 0
  | n < 10 ^ (digits - 1) = checkDigits (start + 1) digits cubes
  | countPermutations n start maxN == cubes = start
  | otherwise = checkDigits (start + 1) digits cubes
  where
    maxN = 10 ^ digits
    n = start ^ 3

countPermutations ref x maxN
  | n >= maxN = 0
  | isPermutation ref n = 1 + countPermutations ref (x + 1) maxN
  | otherwise = countPermutations ref (x + 1) maxN
  where
    n = x ^ 3

findNumber digits cubes
  | n > 0 = n
  | otherwise = findNumber (digits + 1) cubes
  where
    n = checkDigits 1 digits cubes

main =
  do
    print (isPermutation 1234 4321)
    print (isPermutation 1234 3321)
    print (countPermutations (345 ^ 3) 345 (10 ^ 8))
    print (findNumber 1 3)
    print (findNumber 1 5)
