open Helpers;;

let sum_of_lesser_primes max =
  let rec aux n acc =
    let two = Int64.of_int 2 in
    let num = Int64.sub n Int64.one in
    if n < two then acc
    else if is_prime_64 num then aux num (Int64.add acc num) else aux num acc
  in aux max Int64.zero;;

print_string (Int64.to_string (sum_of_lesser_primes (Int64.of_string "2000000")));
print_newline ();;
