let rec paths_from x y =
  if x = 0 || y = 0 then 1
  else if x = y then 2 * paths_from x (y - 1)
  else (x + y) * (paths_from (x - 1) y) / x;;

print_int (paths_from 20 20); print_newline ();;
