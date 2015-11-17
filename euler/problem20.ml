open Big_int;;
open Helpers;;

let factorial n =
  let rec aux i acc =
    if gt_big_int i n then acc
    else aux (succ_big_int i) (mult_big_int acc i)
  in aux unit_big_int unit_big_int;;

let big_string = string_of_big_int (factorial (big_int_of_int 100));;

print_int (sum_str big_string); print_newline ();;
