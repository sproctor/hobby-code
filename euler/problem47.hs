import Helpers

consecutiveNumbers n =
  if length (primeFactors n) == 4
    then 1 + consecutiveNumbers (n + 1)
    else 0

solution n =
  let
    aux 1000000 = 0
    aux p =
      let
        ns = consecutiveNumbers p
      in
        if ns >= 4 then p
        else aux (p + ns + 1)
  in aux 2

main = print (solution 700)

