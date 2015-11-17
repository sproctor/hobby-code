let is_palindrome s =
  let len = String.length s in
  let rec aux pos =
    if pos > len / 2 then true
    else if s.[pos] != s.[len - pos - 1] then false
    else aux (pos + 1)
  in aux 0;;

let is_d_palindrome x =
  is_palindrome (string_of_int x);;

let binary_of_int number =
  let rec aux x acc =
    if x = 0 then acc
    else
      let b = x mod 2 in
      if b = 0 then aux (x / 2) ("0" ^ acc)
      else aux (x / 2) ("1" ^ acc)
  in aux number "";;

let is_b_palindrome x =
  is_palindrome (binary_of_int x);;

let sum = ref 0;;

for i = 1 to 999999 do
  if is_d_palindrome i && is_b_palindrome i then sum := !sum + i (* ; print_int i; print_newline ()) *)
done;;

print_int !sum; print_newline ();;
