factorial n = if n == 0 then 1
                        else n*factorial (n-1)

choose n r = (quot (quot (factorial n) (factorial r)) (factorial (n-r)))

solution n
  | n == 0    = 0
  | otherwise = loop n + solution (n-1)
    where
      loop r
        | r == 0               = 0
        | choose n r > 1000000 = 1 + loop (r-1)
        | otherwise            = loop (r-1)

main = print (solution 100)
