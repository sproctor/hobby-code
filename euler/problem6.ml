let rec sumto x =
  match x with
    1 -> 1
  | _ -> x + sumto (x - 1);;

let rec sum_of_squares x =
  match x with
    1 -> 1
  | _ -> x * x + sum_of_squares (x - 1);;

let difference x =
  let sum = sumto x in
  sum * sum - sum_of_squares x;;

print_int (difference 100); print_newline ();;
