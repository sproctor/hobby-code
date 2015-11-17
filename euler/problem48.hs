power x y = aux y 1
  where
    aux 0 acc = acc
    aux i acc = aux (i - 1) (acc * x)

sumOfPowers x = aux x 0
  where
    aux 0 acc = acc
    aux i acc = aux (i - 1) (acc + power i i)

main = print (sumOfPowers 1000)
