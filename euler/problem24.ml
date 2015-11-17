let numbers = [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; |];;

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

for i = 2 to 1000000 do
  get_next numbers;
  (* print_int (get_smallest_over 7 numbers 8); print_newline (); *)
  (* print_array numbers *)
done;;

print_array numbers;;
