let fib_sum_even m =
  let n = ref 1 in
  let prev = ref 1 in
  let total = ref 0 in
  while !n <= m do
    if !n mod 2 = 0 then total := !total + !n;
    let tmp = !n in
    n := !n + !prev;
    prev := tmp;
  done;
  !total;;

print_int (fib_sum_even 4000000); print_newline ();;
