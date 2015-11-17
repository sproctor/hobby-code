import Debug.Trace
import Data.Bits
import Data.Array.ST
import Data.Array.Base

import Helpers

import Math.NumberTheory.Powers.Squares (isSquare)

isSquare1 :: Integer -> Bool
isSquare1 x =
  y * y == x
  where
    root = sqrt $ fromIntegral x
    y = truncate $ root + 0.5

appSqrt :: Integer -> Integer
appSqrt n =
  sqrt2 n 0 0
  where
    sqrt2 :: Integer -> Int -> Integer -> Integer
    sqrt2 n' b x
      | n' == 0 = x
      | otherwise = sqrt2 (shiftR n' 2) nextb x'
      where
        nextb = b + 1
        x1 | testBit n' 0 = setBit (shiftR x 1) b
           | otherwise = x
        x' | testBit n' 1 = (shiftR x1 1) + (shift 9 (b-3))
           | otherwise = x1

psqArray :: Int -> UArray Int Bool
psqArray n = runSTUArray $ do
  arr <- newArray (0, n-1) False
  let
    stop = (n `quot` 2) + 1
    psq' x
      | x < stop = do
        unsafeWrite arr (x * x `rem` n) True
        psq' (x+1)
      | otherwise = return arr
  unsafeWrite arr 0 True
  unsafeWrite arr 1 True
  psq' 2

psq256 = psqArray 256
psq819 = psqArray 819
psq1025 = psqArray 1025
psq2047 = psqArray 2047
psq4097 = psqArray 4097
psq341 = psqArray 341
psq693 = psqArray 693
psq325 = psqArray 325

checkArray :: UArray Int Bool -> Integer -> Integer -> Bool
checkArray arr k n =
  unsafeAt arr (fromIntegral (n `rem` k))

isPossibleSquare :: Integer -> Bool
isPossibleSquare n =
  (  unsafeAt psq256 ((fromIntegral n) .&. 255)
  && checkArray psq819 819 n
  && checkArray psq1025 1025 n
  && checkArray psq2047 2047 n
  && checkArray psq4097 4097 n
  && checkArray psq341 341 n
  )

isSquare2 :: Integer -> Bool
isSquare2 n =
  isPossibleSquare n &&
  let r = heron n 2 in
  r * r == n

isSquare4 :: Integer -> Integer -> Maybe Integer
isSquare4 n a =
  if isPossibleSquare n
    then
      let r = heron n a in
      if r * r == n
        then Just r
        else Nothing
    else Nothing

heron :: Integer -> Integer -> Integer
heron n a =
  go (step a)
  where
    step k = (k + n `quot` k) `quot` 2
    go k
      | m < k = go m
      | otherwise = k
      where
        m = step k

isSquare3 :: Integer -> Bool
isSquare3 n =
  binarySearch 1 (n `div` 2)
  where
    binarySearch low high
      | sq == n = True
      | low >= high = False
      | n < sq = binarySearch low (mid - 1)
      | otherwise = binarySearch (mid + 1) high
      where
        mid = (low + high) `div` 2
        sq = mid^2

countSquares :: Integer -> Int
countSquares max =
  aux 1 1 0
  where
    aux x a acc
      | x > max = acc
      -- | isSquare2 n = aux (x+1) (acc+1)
      | otherwise =
        case isSquare4 n a of
          Just r -> aux (x+1) r (acc+1)
          Nothing -> aux (x+1) a acc
      where
        n = (x + 100000000000)^2

main =
  print $ countSquares 1000000
