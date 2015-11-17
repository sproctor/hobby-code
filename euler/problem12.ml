let get_factors number =
  let max = truncate (sqrt (float_of_int number)) in
  let rec aux pos acc =
    if pos > max then acc
    else if number mod pos = 0 then
      if pos * pos = number then aux (pos + 1) (acc + 1)
      else aux (pos + 1) (acc + 2)
    else aux (pos + 1) acc
  in aux 1 0;;

(*
print_int (get_factors 2); print_newline ();;
print_int (get_factors 3); print_newline ();;
print_int (get_factors 6); print_newline ();;
print_int (get_factors 10); print_newline ();;
print_int (get_factors 15); print_newline ();;
print_int (get_factors 21); print_newline ();;
print_int (get_factors 28); print_newline ();;
*)

let sum = ref 0 in
let number = ref 0 in
let factors = ref 0 in
for i = 1 to 50000 do
  sum := !sum + i;
  let f = get_factors !sum in
  if f > !factors then (factors := f; number := i;
    print_string ((string_of_int i) ^ " " ^ (string_of_int !sum) ^ " " ^ (string_of_int f) ^ "\n"); flush stdout)
done;;
