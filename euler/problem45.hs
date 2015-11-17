toPentagonal :: Integer -> Integer
toPentagonal n =
  quot (n * (3 * n - 1)) 2

isPentagonal :: Integer -> Bool
isPentagonal p =
  p == toPentagonal (quot (1 + truncate (sqrt (fromInteger (1 + 24 * p)))) 6)

toTriangle :: Integer -> Integer
toTriangle n =
  quot (n * (n + 1)) 2

isTriangle :: Integer -> Bool
isTriangle t =
  t == toTriangle (quot (truncate (sqrt (fromInteger (1 + 8 * t))) - 1) 2)

toHexagonal :: Integer -> Integer
toHexagonal n =
  n * (2 * n - 1)

isHexagonal :: Integer -> Bool
isHexagonal h =
  h == toHexagonal (quot (1 + truncate (sqrt (fromInteger (1 + 8 * h)))) 4)

checkNum :: Integer -> Bool
checkNum n =
  isPentagonal n && isHexagonal n

checkNums :: Integer -> Integer
checkNums start =
  if checkNum (toTriangle start)
    then toTriangle start
    else checkNums (start + 1)

main = print (checkNums 286)
