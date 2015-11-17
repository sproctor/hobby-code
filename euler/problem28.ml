let sum = ref 0;;

for i = 1 to 500 do
  let side = 2 * i + 1 in
  let sq = side * side in
  let diags = sq + (sq - (side - 1)) + (sq - 2 * (side - 1))
    + (sq - 3 * (side - 1)) in
  sum := !sum + diags
done;;

print_int !sum; print_newline ();;
