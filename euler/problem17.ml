exception Out_of_range;;

let digit_to_num_chars x =
    match x with
      0 -> 0
    | 1 -> 3 (* one *)
    | 2 -> 3 (* two *)
    | 3 -> 5 (* three *)
    | 4 -> 4 (* four *)
    | 5 -> 4 (* five *)
    | 6 -> 3 (* six *)
    | 7 -> 5 (* seven *)
    | 8 -> 5 (* eight *)
    | 9 -> 4 (* nine *)
    | _ -> raise Out_of_range;;

let get_ones x =
  let ones = x mod 10 in
  if (x mod 100) - ones = 10 then
    match ones with
      0 -> 3 (* ten *)
    | 1 -> 6 (* eleven *)
    | 2 -> 6 (* twelve *)
    | 3 -> 8 (* thirteen *)
    | 4 -> 8 (* fourteen *)
    | 5 -> 7 (* fifteen *)
    | 6 -> 7 (* sixteen *)
    | 7 -> 9 (* seventeen *)
    | 8 -> 8 (* eighteen *)
    | 9 -> 8 (* nineteen *)
    | _ -> raise Out_of_range
  else digit_to_num_chars ones
  ;;

let get_tens x =
  match (x mod 100) - (x mod 10) with
     0 -> 0
  | 10 -> 0 (* covered by get_ones *)
  | 20 -> 6 (* twenty *)
  | 30 -> 6 (* thirty *)
  | 40 -> 5 (* forty *)
  | 50 -> 5 (* fifty *)
  | 60 -> 5 (* sixty *)
  | 70 -> 7 (* seventy *)
  | 80 -> 6 (* eighty *)
  | 90 -> 6 (* ninety *)
  |  _ -> raise Out_of_range;;

let get_hundreds x =
  (* hundreds place *) digit_to_num_chars ((x mod 1000) / 100) +
  (* "hundred" *) (if x mod 1000 >= 100 then 7 else 0) +
  (* "and" *) (if (x mod 1000) > 100 && x mod 100 != 0 then 3 else 0);;

let get_num_chars x =
  get_ones x + get_tens x + get_hundreds x;;

(*
print_int (get_num_chars 342); print_newline ();;
print_int (get_num_chars 115); print_newline ();;
print_int (get_num_chars 3); print_newline ();;
print_int (get_num_chars 1); print_newline ();;
print_int (get_num_chars 300); print_newline ();;
*)

let rec total_chars x acc =
  if x = 0 then acc
  else total_chars (x - 1) (acc + get_num_chars x);;

print_int (total_chars 999 0); print_newline ();;
