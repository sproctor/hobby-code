let chain_length x =
  let rec aux x acc =
    if x = 1 then acc
    else if x mod 2 = 0 then aux (x / 2) (acc + 1)
    else aux (3 * x + 1) (acc + 1)
  in aux x 1;;

(*
print_int (chain_length 12); print_newline ();;
*)

let max = ref 0 in
let start = ref 0 in
for i = 500000 to 1000000 do
  let l = chain_length i in
  if l > !max then (start := i; max := l)
done;
print_int !start; print_newline ();
print_int !max; print_newline();
