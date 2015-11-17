let is_triple a b c =
  a * a + b * b = c * c;;

exception Found_number;;

for a = 1 to 500 do
  for b = a to (1000 - a) / 2 do
    let c = 1000 - a - b in
    if is_triple a b c then (print_int (a * b * c) ; print_newline (); raise Found_number)
  done
done;;
