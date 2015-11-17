isqrt = truncate . sqrt . fromIntegral

isPerfectSquare x =
  let root = isqrt x
  in root * root == x

findFirstPoint n =
  let
    aux a
      | a == n           = Nothing
      | b*b == b_squared = Just (a,b,(isqrt ((a*a + b*b) `div` 2)))
      | otherwise        = aux (a+1)
      where
        b_squared = ((n*n) `div` 2) - a*a
        b = isqrt b_squared
  in aux 0

countPoints n =
  case findFirstPoint n of
    Nothing    -> 4
    Just (a1,b1,c1) -> solutions a1 b1 c1
      where
        solutions a b c
          | c <= c1 =
            (if c == c1 then 1 else 0) +
            solutions (abs (2*c-a-2*b)) (abs (2*c-2*a-b)) (abs (3*c-2*a-2*b)) +
            solutions (a-2*b+2*c) (2*a-b+2*c) (2*a-2*b+3*c) +
            solutions (2*c-a+2*b) (2*c-2*a+b) (3*c-2*a+2*b) +
            solutions (2*c+a+2*b) (2*c+2*a+b) (3*c+2*a+2*b)
          | otherwise = 0

findPoints x
  | x == 10000           = 0
  | countPoints x == 420 = x
  | otherwise            = findPoints x

main = print (countPoints 10000) 
