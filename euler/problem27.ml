open Helpers;;

let count_primes a b =
  let rec aux n =
    if is_prime_64 (Int64.add (Int64.add (Int64.mul n n) (Int64.mul a n)) b)
      then aux (Int64.succ n)
    else n
  in Int64.to_int (aux Int64.zero);;

let max_primes = ref 0 in
let max_a = ref 0 in
let max_b = ref 0 in
for a = -999 to 999 do
  for b = -999 to 999 do
    let primes = count_primes (Int64.of_int a) (Int64.of_int b) in
    if primes > !max_primes then (max_primes := primes; max_a := a; max_b := b)
  done
done;
print_int !max_primes; print_newline ();
print_int !max_a; print_newline ();
print_int !max_b; print_newline ();;
