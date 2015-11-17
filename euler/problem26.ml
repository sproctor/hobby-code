open Big_int;;

let x = ref 0;;
let digits = ref 0;;

let num_recurring_digits x =
  let rec aux n =
    let ten_to_n = power_int_positive_int 10 n in
    if int_of_big_int (mod_big_int ten_to_n (big_int_of_int x)) = 1 then n
    else aux (n + 1)
  in if x mod 2 = 0 || x mod 5 = 0 then 0
  else aux 1;;

print_int (num_recurring_digits 17); print_newline ();;
print_float (1. /. 17.); print_newline ();;
print_int (num_recurring_digits 7); print_newline ();;
print_float (1. /. 7.); print_newline ();;

for i = 2 to 999 do
  let n = num_recurring_digits i in
  if n > !digits then (digits := n; x := i)
done;;

print_int !x; print_newline ();;
print_int !digits; print_newline ();;
