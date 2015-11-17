let numbers = [| 1; 0; 2; 3; 4; 5; 6; 7; 8; 9; |];;

let print_array arr =
  let rec aux pos =
    if pos < Array.length arr then
      (print_int arr.(pos); aux (pos + 1))
  in aux 0; print_newline ();;

let sort numbers start =
  let len = (Array.length numbers - start) in
  let sub = Array.sub numbers start len in
  Array.sort compare sub;
  Array.blit sub 0 numbers start len;;

let switch numbers pos1 pos2 =
  let tmp = numbers.(pos2) in
  numbers.(pos2) <- numbers.(pos1);
  numbers.(pos1) <- tmp;;

let get_smallest_over min numbers start =
  let rec aux pos =
    if pos >= Array.length numbers - 1 then pos
    else (
      let next = aux (pos + 1) in
      (* print_array [| numbers.(next); min; numbers.(pos) |]; *)
      if numbers.(next) <= min || numbers.(pos) < numbers.(next) then pos
      else next
  ) in aux start;;

let get_next numbers =
  let rec get_pos start =
    if numbers.(start) < numbers.(start + 1) then start
    else get_pos (start - 1)
  in let pos = get_pos (Array.length numbers - 2) in
  (* print_int numbers.(pos); print_string " ";
  print_int pos; print_string " "; *)
  let smallest = get_smallest_over numbers.(pos) numbers (pos + 1) in
  (* print_int numbers.(smallest); print_string " "; print_int smallest; print_newline (); *)
  switch numbers pos smallest;
  if pos + 1 < Array.length numbers then sort numbers (pos + 1);;

let sum = ref 0;;

let get_num pos len =
  let rec aux p acc =
    if p = pos + len then acc
    else aux (p + 1) (10 * acc + numbers.(p))
  in aux pos 0;;

let count = ref 0 in
try
while true do
  get_next numbers;
  count := !count + 1;
  if !count = 100000
    then (print_array numbers; count := 0);
  if get_num 1 3 mod 2 = 0
  && get_num 2 3 mod 3 = 0
  && get_num 3 3 mod 5 = 0
  && get_num 4 3 mod 7 = 0
  && get_num 5 3 mod 11 = 0
  && get_num 6 3 mod 13 = 0
  && get_num 7 3 mod 17 = 0
    then sum := !sum + get_num 0 10
  (* print_int (get_smallest_over 7 numbers 8); print_newline (); *)
  (* print_array numbers *)
done
with _ -> ();;

print_int !sum; print_newline ();;
