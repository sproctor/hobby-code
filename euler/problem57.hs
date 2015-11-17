countDigits :: Integer -> Integer
countDigits x =
  let
    aux n =
      if x `div` (10^n) == 0
        then n
        else aux (n+1)
  in aux 1


solution n d i c
  | i > 1000 = c
  | countDigits n > countDigits d = solution (2 * d + n) (d + n) (i+1) (c+1)
  | otherwise                     = solution (2 * d + n) (d + n) (i+1) c

main = print (solution 3 2 1 0)
