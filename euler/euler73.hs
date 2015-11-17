countFrac d = aux 1 0
  where
    aux n c
      | n == d = c
      | gcd n d == 1 && n * 3 > d && n * 2 < d = aux (n+1) (c + 1)
      | otherwise = aux (n+1) c

solution d c
  | d < 4 = c
  | otherwise = solution (d-1) (c + countFrac d)

main =
  print $ solution 12000 0
