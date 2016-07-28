import Debug.Trace
import qualified Data.PSQueue as PSQ
import Data.List

import Helpers

pyperimeters = [ p | m <- [2..], n <- [1..m], let p = 2 * m * (m + n), coprime m n && (even m || even n) ]

sortedPerims =
  sortedPerims' [] pyperimeters
  where
    sortedPerims' allx@(x:xs) ally@(y:ys)
      | x == y = sortedPerims' allx ys
      | y < x = sortedPerims' (y:allx) ys
      | y < 2 * x =
          if y `elem` xs
            then sortedPerims' allx ys
            else sortedPerims' (x:insert y xs) ys
      | otherwise = x : sortedPerims' xs ally
    sortedPerims' [] (y:ys) = sortedPerims' [y] ys

pysieve [] = []
pysieve (x:xs) = x : pysieve' xs (insertperi x PSQ.empty)
  where
    insertperi p table = PSQ.insert p (p+p) table
    pysieve' allx@(x:xs) table
      | nextMult == x = -- trace ("hit a multiple at " ++ (show x)) $
        pysieve' xs (insertperi x (adjust count table))
      | x < nextMult = -- trace ("py: " ++ (show x) ++ " mult: " ++ (show nextMult)) $
        x : pysieve' xs (insertperi x table)
      | count > 1 = -- trace ("skip " ++ (show count) ++" on " ++ (show nextMult)) $
        pysieve' allx (adjust count table)
      | otherwise = -- trace ("mult: " ++ (show nextMult) ++ " p: " ++ (show p) ++ " x: " ++ (show x)) $
        nextMult : pysieve' allx (adjust 1 table)
      where
        (nextMult, p) = case PSQ.findMin table of
          Just b -> (PSQ.prio b, PSQ.key b)
          Nothing -> error "empty queue"
        count = length $ PSQ.atMost nextMult table
    adjust 0 table = table
    adjust n table =
      case PSQ.findMin table of
        Just b -> -- trace ("adjusting " ++ (show b)) $
          adjust (n-1) $ PSQ.adjust ((+) (PSQ.key b)) (PSQ.key b) table
        Nothing -> error "empty queue"

solution :: Integer -> Integer
solution maxP = solution' 0 $ pysieve sortedPerims
  where
    solution' acc (x:xs)
      | x > maxP = acc
      | otherwise = -- trace ("p: " ++ (show x)) $
        solution' (acc+1) xs

stupidSolution :: Integer -> Integer
stupidSolution maxP =
  s 12 0
  where
    s m acc
      | p > maxP = acc
      | countTriangles p == 1 = trace ("p: " ++ (show p)) $
        s (p+2) (acc+1)
      | otherwise = s (p+2) acc

countTriangles p =
  aux 1 1 0
  where
    aux a b acc
      | 2 * a >= p = acc
      | b > c = aux (a+1) (a+2) acc
      | a^2 + b^2 == c^2 = trace ("p: " ++ (show p) ++ " acc: " ++ (show acc) ++ " a,b,c " ++ (show (a,b,c)))
        aux a (b+1) (acc+1)
      | otherwise = aux a (b+1) acc
      where
        c = p - a - b

main = do
  let p = 2000
  print $ solution p
  print $ stupidSolution p
