open Data.Mutable

let sum_divisors number =
  let max = number / 2 in
  let rec aux pos total =
    if pos > max then total
    else if number mod pos = 0 then aux (pos + 1) (total + pos)
    else aux (pos + 1) total
  in aux 1 0;;

let abundant_numbers = Dynarray.create ();;
let sum_numbers = ref 0;;

let contains needle haystack =
  let rec aux spos epos =
    if spos >= epos then false
    else
      let pos = (spos + epos) / 2 in
      let v = Dynarray.get haystack pos in
      if needle = v then true
      else if needle < v then aux spos pos
      else aux (pos + 1) epos
  in aux 0 (Dynarray.length haystack);;

let is_sum_of_abundants number =
  let rec aux pos =
    if pos >= Dynarray.length abundant_numbers then false
    else
      let a = Dynarray.get abundant_numbers pos in
      let b = number - a in
      if a >= number then false
      else if contains b abundant_numbers then true
      else aux (pos + 1)
  in aux 0;;

for i = 2 to 1000 do
  if not (is_sum_of_abundants i) then
    sum_numbers := !sum_numbers + i;
  let sum = sum_divisors i in
  if sum > i then Dynarray.add abundant_numbers i
done;;

print_int (Dynarray.length abundant_numbers); print_newline ();;
print_int !sum_numbers; print_newline ();;
(* add one because we don't include 1 *)
