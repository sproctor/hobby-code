last10pow x y = aux x y 1
  where
    aux base exponent acc
      | exponent == 0         = acc
      | exponent `mod` 2 == 0 = aux (base*base `mod` 10000000000)
                                    (quot exponent 2) acc
      | otherwise             = aux base (exponent-1)
                                    (acc*base `mod` 10000000000)

main = print ((28433 * last10pow 2 7830457 + 1) `mod` 10000000000)
