import Debug.Trace

solution n = solution' (n-1) n

solution' :: Int -> Int -> Int
solution' n goal
  | n == 1 = -- trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = 1") $
    1
  | n < 1 = -- trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = 0") $
    0
  | goal < 1 = -- trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = 0") $
    0
  | n == goal =
    let s = 1 + solution' (n-1) goal in
    --trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = " ++ (show s)) $
    s
  | otherwise =
    --let nexti = i++";"++(show (n,goal)) in
    let s = solution' (n-1) goal + solution' (min n (goal - n)) (goal - n) in
    --trace (i ++ ": solution " ++ (show n) ++ " " ++ (show goal) ++ " = " ++ (show s)) $
    s

main =
  print $ solution 100
