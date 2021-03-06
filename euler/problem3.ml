let rec has_factors x n =
  match n with
    1 -> false
  | _ -> if x mod n = 0 then true else has_factors x (n - 1);;

let is_prime x =
  not (has_factors x (truncate (sqrt (float_of_int x))));;

let prime_factor x =
  let max_factor = truncate (sqrt (float_of_int x)) in
  let rec prime_factor_helper x n =
    if n > max_factor then x
    else if x mod n = 0 && is_prime n then n
    else prime_factor_helper x (n + 1)
  in
  prime_factor_helper x 2;;

let rec largest_prime_factor x =
  let factor = prime_factor x in
  if x = factor then x else largest_prime_factor (x / factor);;
  
print_int (largest_prime_factor 600851475143); print_newline ();;
