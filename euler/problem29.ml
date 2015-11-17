open Big_int;;

let rec print_list l =
  match l with
    [] -> ()
  | e::rest -> print_int e; print_newline (); print_list rest;;

let rec merge cmp l1 l2 =
  match l1, l2 with
    [], [] -> []
  | [], _ -> l2
  | _, [] -> l1
  | e1::r1, e2::r2 ->
      if cmp e1 e2 = 0 then merge cmp l1 r2 (* throw out one of them *)
      else if cmp e1 e2 = -1 then e1::(merge cmp r1 l2)
      else e2::(merge cmp l1 r2);;

let min_v = 2;;
let max_v = 100;;

let build_list a =
  let rec aux b acc =
    if b < min_v then acc
    else aux (b - 1) ((power_int_positive_int a b)::acc)
  in aux max_v [];;

(* print_list (merge [111; 222;] (build_list 4));; *)

let rec work a acc =
  if a > max_v then acc
  else ( (*print_int a; print_string " "; print_int (List.length acc);
    print_newline ();*)
    work (a + 1) (merge compare_big_int acc (build_list a)));;

let l = work min_v [] in
print_int (List.length l); print_newline ();;
(* print_list (work min_v []); print_newline ();; *)

