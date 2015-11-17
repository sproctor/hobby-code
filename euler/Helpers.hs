module Helpers where

import Data.Char (isSpace)
import Data.List
import qualified Data.PSQueue as PSQ

primes :: Integral int => [int]
--primes = 2 : [x | x <- [3..], isPrime x]
--primes = 2 : 3 : 5 : 7 : [x | x <- (spin wheel4 11), isPrime x]
primes = 2 : 3 : 5 : 7 : (sieve $ spin wheel4 11)

{-# SPECIALIZE primes :: [Integer] #-}
{-# SPECIALIZE primes :: [Int] #-}

isPrime :: Integral int => int -> Bool
isPrime x = all (\p -> x `mod` p > 0) (factorsToTry x)
  where
    factorsToTry x = takeWhile (\p -> p*p <= x) primes

{-# SPECIALIZE isPrime :: Integer -> Bool #-}
{-# SPECIALIZE isPrime :: Int -> Bool #-}

spin :: Integral int => [int] -> int -> [int]
spin (x:xs) n = n : spin xs (n + x)

{-# SPECIALIZE spin :: [Integer] -> Integer -> [Integer] #-}
{-# SPECIALIZE spin :: [Int] -> Int -> [Int] #-}

sieve :: Integral int => [int] -> [int]
sieve [] = []
sieve (x:xs) = x : sieve' xs (insertprime x xs PSQ.empty)
  where
    insertprime p xs table = PSQ.insert (map (* p) xs) (p*p) table
    sieve' [] table = []
    sieve' (x:xs) table
      | nextComposite <= x = sieve' xs (adjust table)
      | otherwise = x : sieve' xs (insertprime x xs table)
      where
        nextComposite = case PSQ.findMin table of
          Just b -> PSQ.prio b
          Nothing -> error "empty queue"
        adjust table
          | n <= x = adjust $ PSQ.insert ns n' $ PSQ.deleteMin table
          | otherwise = table
          where
            (n':ns, n) = case PSQ.findMin table of
              Just b -> (PSQ.key b, PSQ.prio b)
              Nothing -> error "empty queue"

{-# SPECIALIZE sieve :: [Integer] -> [Integer] #-}
{-# SPECIALIZE sieve :: [Int] -> [Int] #-}

-- return a list of (factor, count) where the count is the number
-- of times number is evenly divisible by factor
primeFactorsCombined :: Integral int => int -> [(int, Int)]
primeFactorsCombined number =
  coprimeFactorsCombined primes number

{-# SPECIALIZE primeFactorsCombined :: Integer -> [(Integer, Int)] #-}
{-# SPECIALIZE primeFactorsCombined :: Int -> [(Int, Int)] #-}

coprimeFactorsCombined :: Integral int => [int] -> int -> [(int, Int)]
coprimeFactorsCombined possibleFactors number =
  reverse $ coprimeFactorsCombined' possibleFactors number []

{-# SPECIALIZE coprimeFactorsCombined :: [Integer] -> Integer -> [(Integer, Int)] #-}
{-# SPECIALIZE coprimeFactorsCombined :: [Int] -> Int -> [(Int, Int)] #-}

-- p is not the same as the last one added to list
coprimeFactorsCombined' :: Integral int => [int] -> int -> [(int, Int)] -> [(int, Int)]
coprimeFactorsCombined' possibleFactors@(p:ps) num acc
  | p * p > num = (num, 1) : acc
  | num `rem` p == 0 =
     aux_same possibleFactors (num `quot` p) (p,1) acc
  | otherwise = coprimeFactorsCombined' ps num acc
  where
    -- p is the same as the last one added to the list
    aux_same possibleFactors@(p:ps) num last@(f,n) acc
      | p * p > num =
        if num == f
          then (f,n+1) : acc
          else (num,1) : last : acc
      | num `rem` p == 0 =
        aux_same possibleFactors (num `quot` p) (f,n+1) acc
      | otherwise = coprimeFactorsCombined' ps num (last : acc)

{-# SPECIALIZE coprimeFactorsCombined' :: [Integer] -> Integer -> [(Integer, Int)] -> [(Integer, Int)] #-}
{-# SPECIALIZE coprimeFactorsCombined' :: [Int] -> Int -> [(Int, Int)] -> [(Int, Int)] #-}

primeFactors :: Integral int => int -> [int]
primeFactors 1 = []
primeFactors number =
  reverse $ primeFactors' primes number []

{-# SPECIALIZE primeFactors :: Integer -> [Integer] #-}
{-# SPECIALIZE primeFactors :: Int -> [Int] #-}

primeFactors' :: Integral int => [int] -> int -> [int] -> [int]
primeFactors' possibleFactors@(p:ps) num acc
  | p * p > num = num : acc
  | num `rem` p == 0 =
    primeFactors' possibleFactors (num `quot` p) (p : acc)
  | otherwise = primeFactors' ps num acc

{-# SPECIALIZE primeFactors' :: [Integer] -> Integer -> [Integer] -> [Integer] #-}
{-# SPECIALIZE primeFactors' :: [Int] -> Int -> [Int] -> [Int] #-}

{-
wheel2 :: [Integer]
wheel2 = 2 : 4 : wheel2
wheel3 :: [Integer]
wheel3 = 4:2:4:2:4:6:2:6:wheel3
-}
wheel4 :: Integral int => [int]
wheel4 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel4

{-# SPECIALIZE wheel4 :: [Integer] #-}
{-# SPECIALIZE wheel4 :: [Int] #-}

totient :: Integral int => int -> int
totient n = totient_helper $ coprimeFactorsCombined' primes n []
  where
    totient_helper [] = 1
    totient_helper ((p, k):rest) = (p - 1) * p ^ (k - 1) * (totient_helper rest)

{-# SPECIALIZE totient :: Integer -> Integer #-}
{-# SPECIALIZE totient :: Int -> Int #-}

isPermutation :: (Ord a, Show a) => a -> a -> Bool
isPermutation x y =
  x1 == y1
  where
    x1 = sort $ show x
    y1 = sort $ show y

{-# SPECIALIZE isPermutation :: Integer -> Integer -> Bool #-}
{-# SPECIALIZE isPermutation :: Int -> Int -> Bool #-}

coprime :: Integral int => int -> int -> Bool
coprime a b = gcd a b == 1

{-# SPECIALIZE coprime :: Integer -> Integer -> Bool #-}
{-# SPECIALIZE coprime :: Int -> Int -> Bool #-}

getDigits :: Integral int => int -> [int]
getDigits x
  | x < 10 = [x]
  | otherwise =
      let (d, m) = divMod x 10 in
      m : getDigits d

{-# SPECIALIZE getDigits :: Integer -> [Integer] #-}
{-# SPECIALIZE getDigits :: Int -> [Int] #-}

merge :: Ord a => [a] -> [a] -> [a]
merge xs ys =
  reverse $ merge' xs ys []

{-# SPECIALIZE merge :: [Integer] -> [Integer] -> [Integer] #-}
{-# SPECIALIZE merge :: [Int] -> [Int] -> [Int] #-}

merge' :: Ord a => [a] -> [a] -> [a] -> [a]
merge' [] ys acc = ys `rappend` acc
merge' xs [] acc = xs `rappend` acc
merge' allx@(x:xs) ally@(y:ys) acc
  | x < y = merge' xs ally (x:acc)
  | otherwise = merge' allx ys (y:acc)

{-# SPECIALIZE merge' :: [Integer] -> [Integer] -> [Integer] -> [Integer] #-}
{-# SPECIALIZE merge' :: [Int] -> [Int] -> [Int] -> [Int] #-}

rappend :: [a] -> [a] -> [a]
rappend [] ys = ys
rappend (x:xs) ys = rappend xs (x:ys)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
