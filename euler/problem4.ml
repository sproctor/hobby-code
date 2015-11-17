let is_palindrome str =
  let length = String.length str in
  let rec is_palindrome_helper x =
    if x > length / 2 then true
    else if str.[x] = str.[length - 1 - x] then is_palindrome_helper (x + 1)
    else false
  in
  is_palindrome_helper 0;;

let current_highest = ref 0 in
for x = 100 to 999 do
  for y = 100 to 999 do
    let v = x * y in
    if v > !current_highest && is_palindrome (string_of_int v) then
      current_highest := v
  done
done;
print_int !current_highest; print_newline ();;
