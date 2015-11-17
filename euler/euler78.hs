import Debug.Trace
import System.Environment
import Data.Vector ( Vector, singleton, snoc, (!) )

p :: Vector Integer -> Int -> Integer
p v n =
  -- | n == 1 = -- trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = 1") $ 1
  -- | n == 0 = 1
  -- | n < 0 = -- trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = 0") $ 0
  -- | otherwise =
      sum $ map t [1..n]
      -- trace ("p(" ++ (show n) ++ ") = " ++ (show s) ++ "; " ++ (show m)) $ s
    --let nexti = i++";"++(show (n,goal)) in
    --trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = " ++ (show s)) $
  where
    t k =
      (-1)^(k+1) * (p' (n - k * (3 * k - 1) `div` 2) + p' (n - k * (3 * k + 1) `div` 2))
      -- trace ("n = " ++ (show n) ++ " p'(" ++ (show k) ++ ") = " ++ (show r)) r
    p' :: Int -> Integer
    p' n
      | n < 0 = 0
      | otherwise = v ! n

solution v n
  | s `mod` 1000000 == 0 = n
  | otherwise = solution (snoc v s) (n+1)
  where
    s = p v n

main =
  print $ solution (singleton 1) 1
