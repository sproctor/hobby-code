open Helpers;;

let contains_one s c =
  try
    let i = String.index s c in
    not (String.contains_from s i c)
  with Not_found -> false;;

let is_pandigital x =
  let s = string_of_int x in
  let rec aux n =
    if n > String.length s then true
    else
      if contains_one s (char_of_int n) then aux (n + 1)
      else false
  in aux 1;;

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

exception Done;;

let get_next numbers =
  let rec get_pos start =
    if start < 0 then raise Done else
    if numbers.(start) < numbers.(start + 1) then start
    else get_pos (start - 1)
  in let pos = get_pos (Array.length numbers - 2) in
  (* print_int numbers.(pos); print_string " ";
  print_int pos; print_string " "; *)
  let smallest = get_smallest_over numbers.(pos) numbers (pos + 1) in
  (* print_int numbers.(smallest); print_string " "; print_int smallest; print_newline (); *)
  switch numbers pos smallest;
  if pos + 1 < Array.length numbers then sort numbers (pos + 1);;

let number = ref Int64.zero;;

for nums = 4 to 9 do
  let numbers = Array.init nums (fun x -> (x +1)) in
  try
    while true do
      get_next numbers;
      let to_number n =
        let rec aux n acc =
          if n >= Array.length numbers then acc
          else aux (n + 1) ((Int64.add (Int64.of_int numbers.(n))) (Int64.mul (Int64.of_int 10) acc))
        in aux 0 Int64.zero in
      let i = to_number numbers in
      (* print_string (Int64.to_string i); print_newline (); *)
      if is_prime_64 i then if i > !number then number := i
    done
  with Done -> ()
done;;

print_string (Int64.to_string !number); print_newline ();;
