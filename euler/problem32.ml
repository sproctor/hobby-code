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

let is_pandigital a b c =
  let str = (string_of_int a) ^ (string_of_int b) ^ (string_of_int c) in
  if String.contains str '0' then false
  else
    List.for_all (contains_one str)
        ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'];;

print_string (string_of_bool (is_pandigital 123 45 6789)); print_newline ();;
print_string (string_of_bool (is_pandigital 21398 0 234)); print_newline ();;
print_string (string_of_bool (is_pandigital 2 3456789 1)); print_newline ();;
print_string (string_of_bool (is_pandigital 234 456 7891)); print_newline ();;

let sum = ref 0;;
exception Break;;

for i = 1 to 99999 do
  try
  if not (String.contains (string_of_int i) '0') then
    for p = 1 to truncate (sqrt (float_of_int i)) do
      if i mod p = 0 && is_pandigital p (i / p) i then
        (sum := !sum + i; print_int p; print_string " "; print_int (i / p); print_string " "; print_int i; print_newline (); raise Break)
    done
  with Break -> ()
done;;

print_int !sum; print_newline ();;
