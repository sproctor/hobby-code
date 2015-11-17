let is_prime x =
  let max_factor = truncate (sqrt (float_of_int x)) in
  let rec is_prime_helper n =
    if n > max_factor then true
    else if x mod n = 0 then false
    else is_prime_helper (n + 2)
  in
  match x with
    2 -> true
  | _ when x mod 2 = 0 || x < 2 -> false
  | _ -> is_prime_helper 3;;

(*
print_string ((string_of_bool (is_prime 7)) ^ "\n");;
print_string ((string_of_bool (is_prime 20)) ^ "\n");;
print_string ((string_of_bool (is_prime 21)) ^ "\n");;
print_string ((string_of_bool (is_prime 2)) ^ "\n");;
*)

let is_prime_64 x =
  let two = Int64.of_int 2 in
  let max_factor = Int64.of_float (sqrt (Int64.to_float x)) in
  let rec is_prime_helper_64 n =
    if n > max_factor then true
    else if Int64.rem x n = Int64.zero then false
    else is_prime_helper_64 (Int64.add n two)
  in
  match x with
    _ when x = two -> true
  | _ when Int64.rem x two = Int64.zero || x < two -> false
  | _ -> is_prime_helper_64 (Int64.of_int 3);;

(*
print_string ((string_of_bool (is_prime_64 (Int64.of_int 7))) ^ "\n");;
print_string ((string_of_bool (is_prime_64 (Int64.of_int 20))) ^ "\n");;
print_string ((string_of_bool (is_prime_64 (Int64.of_int 21))) ^ "\n");;
print_string ((string_of_bool (is_prime_64 (Int64.of_int 2))) ^ "\n");;
*)

let prime_factor x =
  let max_factor = truncate (sqrt (float_of_int x)) in
  let rec prime_factor_helper x n =
    if n > max_factor then x
    else if x mod n = 0 && is_prime n then n
    else prime_factor_helper x (n + 1)
  in
  prime_factor_helper x 2;;

let rec prime_factors x =
  let factor = prime_factor x in
  print_int factor; print_string " ";
  if x <> factor then prime_factors (x / factor);;
