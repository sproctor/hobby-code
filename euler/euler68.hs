import Data.List

getGroups a =
  [(a !! 0, a !! 6, a !! 7), (a !! 1, a !! 7, a !! 8), (a !! 2, a !! 8, a !! 9),
   (a !! 3, a !! 9, a !! 5), (a !! 4, a !! 5, a !! 6)]
{-
getGroups a =
  [(a !! 0, a !! 4, a !! 5), (a !! 1, a !! 5, a !! 3), (a !! 2, a !! 3, a !! 4)]
-}

sumGroup (a1, a2, a3) =
  a1 + a2 + a3

isMagic [] = True
isMagic (_:[]) = True
isMagic (group1:group2:rest) =
  if sumGroup group1 == sumGroup group2
    then isMagic (group2:rest)
    else False

isValid [] = True
isValid ((_, a, b):rest) =
  if a == 10 || b == 10
    then False
    else isValid rest

isOrdered [] = True
isOrdered (_:[]) = True
isOrdered (a:b:rest) =
  if a < b
    then isOrdered (a:rest)
    else False
orderGroups (a:rest) =
  if isOrdered (a:rest)
    then (a:rest)
    else orderGroups (rest ++ [a])

groupsToInteger :: [(Int, Int, Int)] -> Int
groupsToInteger gs =
  read $ concat $ map show $ concat $ map groupToList $ orderGroups gs
groupToList (a1, a2, a3) = [a1, a2, a3]

main =
  print $ maximum $ map groupsToInteger $ filter isMagic $ filter isValid $ map getGroups $ permutations [1..10]
