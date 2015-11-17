import Data.List

import Helpers

split :: Integer -> [[Integer]]
split x = aux 1 []
  where
    aux pos acc
      | d == 0                                          = acc
      | isPrime d && isPrime m && isPrime (iconcat m d) && iconcat d m == x =
          aux (pos+1) ([d,m]:acc)
      | otherwise                                       = aux (pos+1) acc
      where (d,m) = divMod x (10^pos)

countDigits :: Integer -> Integer
countDigits x = aux 1 0
  where
    aux num n
      | num > x   = n
      | otherwise = aux (num * 10) (n+1)

iconcat :: Integer -> Integer -> Integer
iconcat a b = a * 10^(countDigits b) + b

fits :: [Integer] -> [Integer] -> Bool
fits [] _ = True
fits (x:xs) pl =
  if foldr (&&) True (map (\ p -> p == x || (isPrime (iconcat p x)
                                             && isPrime (iconcat x p))) pl)
    then fits xs pl
    else False

mergeLists :: [[Integer]] -> [[Integer]] -> [[Integer]]
mergeLists l [] = l
mergeLists l (x:xs) = x:(mergeLists (aux l []) xs)
  where
    aux [] acc = acc
    aux (pl:pls) acc = 
      if fits pl x
        then aux pls ((nub (pl++x)):pl:acc)
        else aux pls (pl:acc)

solutions :: Integer -> [[Integer]] -> [Integer]
solutions x acc
  | x > 1000000 = []
  | isPrime x =
     case find (\l->length l >= 5) newL of
       Just l  -> l
       Nothing -> solutions (x+2) newL
  | otherwise = solutions (x+2) acc
  where
    newL = nub (mergeLists acc (split x))

main = do
  print $ split 17011
  print $ isPrime 1711
  print $ isPrime 17011
  print (isPrime 193)
  print (isPrime 319)
  print l
  print (sum l)
  where
    l = solutions 3 []
