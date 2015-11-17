open Big_int;;

let acc = ref 2;;

let rec fib last1 last2 =
  if String.length (string_of_big_int last1) >= 1000 then last1
  else (acc := !acc + 1; fib (add_big_int last1 last2) last1);;

print_string (string_of_big_int (fib unit_big_int unit_big_int));
print_newline ();;
print_int !acc; print_newline ();;
