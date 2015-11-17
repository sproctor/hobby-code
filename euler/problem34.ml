let fact n =
  let rec aux pos acc =
    if pos <= 1 then acc
    else aux (pos - 1) (acc * pos)
  in aux n 1;;

print_int (fact 9); print_newline ();;

let get_digits i =
  let rec aux x l =
    if x = 0 then l
    else aux (x / 10) ((x mod 10)::l)
  in aux i [];;

let print_int_nl x = print_int x; print_newline ();;

List.map print_int_nl (get_digits 123);;

let fact_arr = Array.init 10 fact;;

let sum_of_df digits =
  let rec aux ds acc =
    match ds with
      [] -> acc
    | d::rest -> aux rest (acc + fact_arr.(d))
  in aux digits 0;;

let sum = ref 0;;

for i = 10 to 3000000 do
  let digits = get_digits i in
  if i = sum_of_df digits then
    (sum := !sum + i;
     print_int i; print_newline ())
done;;

print_int !sum; print_newline ();;
