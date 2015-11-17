import List
import Char

equalstep :: Eq a => [a] -> [a] -> Bool
equalstep [] [] = True
equalstep [_] [] = False
equalstep [] [_] = False
equalstep (x:xs) (y:ys)
 | (x == y)  = equalstep xs ys
 | otherwise = False

integerToDigits x = map (\a->digitToInt a) (show x)
checkNumber :: Integer -> Bool
checkNumber x =
  let
    base = sort (integerToDigits x)
    numDigits = length base
    checkMultiple n = equalstep base (sort (integerToDigits (n * x)))
    loop 7 = True
    loop n = if checkMultiple n then loop (n+1) else False
  in loop 2

solution :: Integer -> Maybe Integer
solution 100000000 = Nothing
solution x =
  if checkNumber x then Just x
  else solution (x+1)

main = print (solution 1)
