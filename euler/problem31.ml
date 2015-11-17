let coins = [1; 2; 5; 10; 20; 50; 100; 200; ];;

let ways_to_coins target =
  let rec aux coins x =
      match coins with
        [] -> 0
      | c::rest ->
        if x > target then 0
        else if x = target then 1
        else aux coins (x + c) + aux rest x
  in aux coins 0;;

print_int (ways_to_coins 200); print_newline ();;
