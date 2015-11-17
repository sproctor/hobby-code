open Helpers;;

let pow x y =
  let rec aux i acc =
    if i = 0 then acc
    else aux (i - 1) (x * acc)
  in aux y 1;;

let is_trunc_prime number =
  let rec aux_l x =
    if x = 0 then true else
    if is_prime x then aux_l (x / 10)
    else false
  in let rec aux_r pos =
    if pos = 0 then true else
    let x = number mod (pow 10 pos) in
    if is_prime x then aux_r (pos - 1)
    else false
  in aux_l number && aux_r (String.length (string_of_int number));;

let sum = ref 0;;
let count = ref 0;;

for i = 10 to 1000000 do
  if is_trunc_prime i then (sum := !sum + i; count := !count + 1; print_int i; print_newline ())
done;;

print_int !sum; print_newline ();;
print_int !count; print_newline ();;
