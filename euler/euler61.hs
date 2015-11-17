perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [y : ps | (y,ys) <- selections xs, ps <- perms ys]
selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]

isqrt = truncate . sqrt . fromInteger

toTriangle :: Integer -> Integer
toTriangle n =
  quot (n * (n + 1)) 2

fromTriangle :: Integer -> Integer
fromTriangle t =
  quot (isqrt (1 + 8 * t) - 1) 2

toSquare :: Integer -> Integer
toSquare n =
  n * n

fromSquare :: Integer -> Integer
fromSquare s =
  isqrt s

toPentagonal :: Integer -> Integer
toPentagonal n =
  quot (n * (3 * n - 1)) 2

fromPentagonal :: Integer -> Integer
fromPentagonal p =
  quot (1 + isqrt (1 + 24 * p)) 6

toHexagonal :: Integer -> Integer
toHexagonal n =
  n * (2 * n - 1)

fromHexagonal :: Integer -> Integer
fromHexagonal n =
  quot (1 + isqrt (1 + 8 * n)) 4

toHeptagonal :: Integer -> Integer
toHeptagonal n =
  n * (5 * n - 3) `quot` 2

fromHeptagonal :: Integer -> Integer
fromHeptagonal n =
  quot (3 + isqrt (9 + 40 * n)) 10

toOctagonal :: Integer -> Integer
toOctagonal n =
  n * (3 * n - 2)

fromOctagonal :: Integer -> Integer
fromOctagonal n =
  quot (2 + isqrt (4 + 12 * n)) 6

toFigurate :: Integer -> Integer -> Integer
toFigurate sides n =
  case sides of
    3 -> toTriangle n
    4 -> toSquare n
    5 -> toPentagonal n
    6 -> toHexagonal n
    7 -> toHeptagonal n
    8 -> toOctagonal n

fromFigurate sides n =
  case sides of
    3 -> fromTriangle n
    4 -> fromSquare n
    5 -> fromPentagonal n
    6 -> fromHexagonal n
    7 -> fromHeptagonal n
    8 -> fromOctagonal n

isFigurate :: Integer -> Integer -> Bool
isFigurate sides n =
  n == toFigurate sides (fromFigurate sides n)

firstFigurate :: Integer -> Integer -> Integer
firstFigurate sides min =
  if isFigurate sides min
    then fromFigurate sides min
    else firstFigurate sides (min + 1)

findFirst :: Integer -> [Integer] -> [Integer]
findFirst start (f:figs)
  | t > 9999 = []
  | l == [] = findFirst (start + 1) (f:figs)
  | otherwise = t:l
  where
    t = toTriangle start
    last2d = t `mod` 100
    l = findNext f figs (firstFigurate f (last2d * 100)) last2d (t `quot` 100)

findNext :: Integer -> [Integer] -> Integer -> Integer -> Integer -> [Integer]
findNext sides figs start first2d tFirst2d
  | n < 1000 || n >= (first2d + 1) * 100 = []
  | figs == [] && last2d == tFirst2d = [n]
  | figs == [] || l == [] = findNext sides figs (start + 1) first2d tFirst2d
  | otherwise = n:l
  where
    f:fs = figs
    n = toFigurate sides start
    last2d = n `mod` 100
    l = findNext f fs (firstFigurate f (last2d * 100)) last2d tFirst2d

main =
  print (map sum (map (findFirst (firstFigurate 3 1000)) (perms [4, 5, 6, 7, 8])))
