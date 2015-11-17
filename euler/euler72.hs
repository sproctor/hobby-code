import Helpers

solution d acc
  | d < 2 = acc
  | otherwise = solution (d-1) (acc + totient d)

main =
  print $ solution 1000000 0
