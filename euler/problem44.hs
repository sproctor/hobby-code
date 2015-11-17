toPentagonal :: Integer -> Integer
toPentagonal n =
  quot (n * (3 * n - 1)) 2

isPentagonal :: Integer -> Bool
isPentagonal num =
  let
    n = quot (1 + truncate (sqrt (fromInteger (1 + 24 * num)))) 6
  in num == toPentagonal n

checkPent :: Integer -> Maybe (Integer, Integer)
checkPent n =
  let
    pent = toPentagonal n
    aux pos =
      if pos == 0
        then Nothing
        else
          let testPent = toPentagonal pos in
          if isPentagonal (pent + testPent)
              && isPentagonal (pent - testPent)
            then Just (pent, testPent)
            else aux (pos - 1)
  in aux (n - 1)

checkPents :: Integer -> Maybe (Integer, Integer)
checkPents max =
  let
    aux p =
      if p > max
        then Nothing
      else
        case checkPent p of
          Nothing -> aux (p + 1)
          x       -> x
  in aux 2

main =
  print (checkPents 100000)
