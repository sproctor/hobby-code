import Helpers

fact n = product [1..n]

decimalfact x =
  sum $ map fact $ getDigits x

countLinks x = countLinks' x 1
  where
    countLinks' :: Integer -> Integer -> Integer
    -- 169 -> 363601 -> 1454 -> 169
    countLinks' 169 c = c + 2
    countLinks' 363601 c = c + 2
    countLinks' 1454 c = c + 2
    -- 871 -> 45361 -> 871
    countLinks' 871 c = c + 1
    countLinks' 45361 c = c + 1
    -- 872 -> 45362 -> 872
    countLinks' 872 c = c + 1
    countLinks' 45362 c = c + 1
    countLinks' x c
      | c > 60 = error $ "too many links" ++ (show x)
      | df == x = c
      | otherwise = countLinks' (decimalfact x) (c + 1)
      where
        df = decimalfact x

solution x count
  | x < 70 = count
  | countLinks x == 60 = solution (x-1) (count+1)
  | otherwise = solution (x-1) count

main =
  print $ solution 1000000 0
