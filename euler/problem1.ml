let ismultiple m x =
  (x mod m) = 0;;

print_string ("ismultiple 3 9 = " ^ (string_of_bool (ismultiple 3 9)));
  print_newline();;

let rec answerto n =
  match n with
    0 -> 0
  | _ -> if (ismultiple 3 n) || (ismultiple 5 n) then (n + (answerto (n - 1)))
      else (answerto (n - 1));;

let _ = print_string "Answer = "; print_int (answerto 999); print_newline ();;
