open Big_int;;

let big_string = string_of_big_int (power_int_positive_int 2 1000);;

let sum_str str =
  let get pos =
    int_of_char str.[pos] - int_of_char '0' in
  let rec aux pos acc =
    if pos = String.length str then acc
    else aux (pos + 1) (acc + get pos)
  in aux 0 0;;

print_int (sum_str big_string); print_newline ();;
