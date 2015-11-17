module Main where

import System.Environment ( getArgs )
import Control.Parallel
import Data.List.Utils -- From MissingH

import Helpers

{-
prime_factors :: Integer -> [Integer]
prime_factors n = aux 2 n (\x->x)
  where aux i remaining k
          | i * i > remaining = k [remaining]
          | remaining `mod` i == 0 = aux i (quot remaining i) (\x->k(i:x))
          | True = aux (i + 1) remaining k

merge :: [Integer] -> [Integer] -> [Integer]
merge l1 l2 = aux l1 l2 (\x->x)
  where aux [] l k = k l
        aux l [] k = k l
        aux (x1:l1) (x2:l2) k
          | x1 < x2 = aux l1 (x2:l2) (\v->k(x1:v))
          | True = aux (x1:l1) l2 (\v->k(x2:v))
-}

multiplyList m l a =
  let
    aux [] k = k []
    aux (y:rest) k
      | n > a = k []
      | True = aux rest (\x->k(n:x))
      where
        n = m * y
  in aux l (\x->x)

combine_factors :: [Integer] -> Integer -> [Integer]
combine_factors l a =
  let
    aux :: [Integer] -> [Integer] -> Integer -> [Integer] -> [Integer]
    aux [] result _ _ = result
    aux (x:rest) result last last_set
      | x == last =
        let r = (multiplyList x last_set a)
        in aux rest (merge result r) x r
      | True =
        let r = x:(multiplyList x result a)
        in aux rest (merge result r) x r
  in aux l [] 0 []

s :: Integer -> Integer -> [Integer] -> [Integer] -> Integer
s a p f1 f2 =
  let diff = a*a - 1
      twoA = 2*a
      cPlusBmax = p - a
      countSolutions :: [Integer] -> Integer -> Integer
      countSolutions [] total = total
      countSolutions (cMinusB:ds) total =
        let cPlusB = quot diff cMinusB
            twoB = cPlusB - cMinusB
        in if (even twoB) && twoA <= twoB && cPlusB <= cPlusBmax
             then countSolutions ds (total+1)
             else countSolutions ds total
  in countSolutions (1:(combine_factors (merge f1 f2) a)) 0

solution :: Integer -> Integer
solution p = solutionAccum p 2 (quot (p-1) 2)

solutionAccum :: Integer -> Integer -> Integer -> Integer
solutionAccum p start total = part1 `par` part2 `par` part3 `par` (part4 `pseq` (part1+part2+part3+part4))
  where
    end = quot p 3
    break1 = (quot (end - start) 4) + start
    break2 = (quot (end - start) 2) + start
    break3 = 3 * (quot (end - start) 4) + start
    part1 = aux start (break1-1) (primeFactors (start-1)) (primeFactors start) total
    part2 = aux break1 (break2-1) (primeFactors (break1-1)) (primeFactors break1) 0
    part3 = aux break2 (break3-1) (primeFactors (break2-1)) (primeFactors break2) 0
    part4 = aux break3 end (primeFactors (break3-1)) (primeFactors break3) 0
    aux a max fAminus1 fA total
      | a > max = total
      | True =
        let fAplus1 = primeFactors (a+1)
        in aux (a+1) max fA fAplus1 ((s a p fAminus1 fAplus1) + total)


{-
naive p = [(a,b,c) |
           a <- [1..quot p 3],
           b <- [a..quot p 2],
           c <- [b..quot p 2],
           a + b + c <= p
           && a*a + b*b == c*c + 1]
-}

main = do
  args <- getArgs
  if length args == 3
    then print (solutionAccum (read (args!!0)) (read (args!!1)) (read (args!!2)))
    else print(solution (read (args!!0)))
