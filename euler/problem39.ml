let is_triple a b c =
  a * a + b * b = c * c;;

let zero x = 0;;
let p_arr = Array.init 1001 zero;;

for p = 6 to 1000 do
  for a = 1 to p / 3 do
    for b = (a + 1) to (p - a) / 2 do
      let c = p - a - b in
      if is_triple a b c then p_arr.(p) <- (p_arr.(p) + 1)
    done
  done
done;;

let max = ref 0;;
let max_pos = ref 0;;

for i = 1 to 1000 do
  if p_arr.(i) > !max then
    (max_pos := i; max := p_arr.(i))
done;;

print_int !max; print_string " "; print_int !max_pos; print_newline ();;
