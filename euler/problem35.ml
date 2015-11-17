open Helpers;;

let rec get_num s pos =
  if pos > 0 then get_num ((String.sub s 1 (String.length s - 1))
     ^ (String.sub s 0 1)) (pos - 1)
  else int_of_string s;;

let is_circ_prime x =
  let s = string_of_int x in
  let rec aux pos =
    if pos > String.length s then true
    else if is_prime (get_num s pos) then aux (pos + 1)
    else false
  in aux 0;;

let sum = ref 0;;

for i = 2 to 1000000 do
  if is_circ_prime i then (sum := !sum + 1; print_int i; print_newline ())
done;;

print_int !sum; print_newline ();;
