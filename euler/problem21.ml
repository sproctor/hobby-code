let sum_divisors number =
  let max = number / 2 in
  let rec aux pos total =
    if pos > max then total
    else if number mod pos = 0 then aux (pos + 1) (total + pos)
    else aux (pos + 1) total
  in aux 1 0;;

let my_hash = Hashtbl.create 5000;;
let pair_sum = ref 0;;

for i = 2 to 10000 do
  let sum = sum_divisors i in
  if sum > i then Hashtbl.add my_hash i sum
  else
    try
      let pair = Hashtbl.find my_hash sum in
      if pair = i then (pair_sum := !pair_sum + i + sum; print_string ((string_of_int i) ^ " " ^ (string_of_int sum) ^ "\n"))
    with Not_found -> ()
done;;

print_int !pair_sum; print_newline ();;
