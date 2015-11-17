open Helpers;;

let get_nth_prime n =
  let rec get_prime start remaining =
    if remaining = 0 then start - 1
    else if is_prime start then get_prime (start + 1) (remaining - 1)
    else get_prime (start + 1) remaining
  in
  get_prime 2 n;;

print_int (get_nth_prime 10001); print_newline ();;
