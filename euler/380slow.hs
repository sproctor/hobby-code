lapCell :: Int -> Int -> Int -> Int -> Double
lapCell n m x y
    | c1 == c2 =
        if isCorner c1 then 2
        else if isEdge c1 then 3
        else 4
    | isAdjacent c1 c2 = -1
    | otherwise = 0
    where c1 = indexToCell x
          c2 = indexToCell y
          indexToCell i = (i `quot` m, i `mod` m)
          isCorner (x,y) = (x == 0 || x == n-1) && (y == 0 || y == m-1)
          isEdge (x,y) = (x == 0 || x == n-1) || (y == 0 || y == m-1)
          isAdjacent (x1,y1) (x2,y2) =
              x1 == x2 && abs(y1-y2) == 1
              || y1 == y2 && abs(x1-x2) == 1

coDet :: Int -> Int -> Int -> [Int] -> Double
coDet n m row removedCols
    | row + 2 == size = (getCell (0, 0))
                          * (getCell (1, 1))
                          - (getCell (0, 1))
                          * (getCell (1, 0))
    | otherwise = forCol (row - m) + forCol (row - 1) + forCol row + forCol (row + 1)
                    + forCol (row + m)
    where size = n * m
          forCol c
              | c < 0 || c > size - 1 || cell == 0 || c `elem` removedCols = 0
              | otherwise = sign * cell * (coDet n m (row+1) (c:removedCols))
              where cell = lapCell n m row c
                    count = length (filter ((>) c) removedCols)
                    sign = if (c - count) `mod` 2 == 0 then 1
                           else -1
          getCell :: (Int, Int) -> Double
          getCell (x,y) = lapCell n m (row+x) (findC y 0)
              where findC :: Int -> Int -> Int
                    findC remaining col
                        | col `elem` removedCols = findC remaining (col+1)
                        | remaining == 0 = col
                        | otherwise = findC (remaining-1) (col+1)


solve n m = coDet n m 1 [0]
main = putStrLn $ show $ solve 4 5

