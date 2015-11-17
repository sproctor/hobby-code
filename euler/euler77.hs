import Debug.Trace

import Helpers

primesums :: Integer -> Int
primesums d = solution' primes d
  where
    solution' :: [Integer] -> Integer -> Int
    solution' alln@(n:ns) goal
      | n > goal = -- trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = 0") $
        0
      | n == goal =
        --trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = " ++ (show s)) $
        1
      | otherwise =
        --let nexti = i++";"++(show (n,goal)) in
        let s = solution' ns goal + solution' alln (goal - n) in
        --trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = " ++ (show s)) $
        s

solution n
  | primesums n > 5000 = n
  | otherwise = solution (n+1)

main =
  print $ solution 10
