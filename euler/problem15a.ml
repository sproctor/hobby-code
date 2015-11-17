let paths_from num =
  let rec aux x acc =
    if x = num then acc
    else aux (x + 1) (2 * (2 * x + 1) * acc / (x + 1))
  in aux 0 1;;

print_int (paths_from 20); print_newline ();;
