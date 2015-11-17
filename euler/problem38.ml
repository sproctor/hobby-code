let contains_one str c =
  try
    let i = String.index str c in
    try
      let _ = String.index_from str (i + 1) c in false
    with Not_found -> true
  with Not_found -> false;;

(*
print_string (string_of_bool (contains_one "2113" '1')); print_newline ();;
print_string (string_of_bool (contains_one "21398" '1')); print_newline ();;
print_string (string_of_bool (contains_one "21323441" '1')); print_newline ();;
*)

let is_pandigital str =
  if String.contains str '0' then false
  else
    List.for_all (contains_one str)
        ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'];;

(*
print_string (string_of_bool (is_pandigital 123 45 6789)); print_newline ();;
print_string (string_of_bool (is_pandigital 21398 0 234)); print_newline ();;
print_string (string_of_bool (is_pandigital 2 3456789 1)); print_newline ();;
print_string (string_of_bool (is_pandigital 234 456 7891)); print_newline ();;
*)

let sum = ref 0;;
exception Break;;

let build_number x =
  let rec aux i acc =
    if String.length acc < 9 || i <= 2 then aux (i + 1) (acc ^(string_of_int (x * i)))
    else acc
  in aux 1 "";;

for i = 1 to 999999 do
  let n = build_number i in
  if String.length n = 9 && is_pandigital n then
    (print_int i; print_string (" " ^ n); print_newline ();)
done;;
