import Data.Bits
import Maybe
import Char

testValues :: Int -> Int -> Int -> [Int] -> Int -> [Int] -> Maybe [Int]
testValues _ _ _ [] pos decoded = Just decoded
testValues a b c (x:xs) pos decoded =
  if (v >= 32 && v <= 122)
    then testValues a b c xs (pos+1) (decoded++[v])
    else Nothing
  where
    v = case pos `mod` 3 of
          0 -> xor x a
          1 -> xor x b
          2 -> xor x c

findSolution :: Int -> Int -> Int -> [Int] -> [(Int,String)]
findSolution a b c str
  | a > 122 = []
  | b > 122 = findSolution (a+1) 97 c str
  | c > 122 = findSolution a (b+1) 97 str
  | isJust decoded = (sum (fromJust decoded),map chr (fromJust decoded)):(findSolution a b (c+1) str)
  | otherwise = findSolution a b (c+1) str
  where
    decoded = testValues a b c str 0 []

getNum str = aux 0 str
  where
    aux n [] = (n, [])
    aux n (c:rest)
      | c == ','               = (n, rest)
      | c == '\r' || c == '\n' = (n, [])
      | otherwise = aux (n * 10 + digitToInt(c)) rest

parseInput :: String -> [Int]
parseInput [] = []
parseInput str =
  let (num, rest) = getNum str
  in num:(parseInput rest)

main = do s <- readFile "cipher1.txt"
          print (findSolution 97 97 97 (parseInput s))
