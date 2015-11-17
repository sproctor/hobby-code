
let buf = Buffer.create 1000000;;

let i = ref 1 in
while Buffer.length buf < 1000000 do
  Buffer.add_string buf (string_of_int !i);
  i := !i + 1
done;;

let pow x y =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n - 1) (acc * x)
  in aux y 1;;

let product = ref 1;;

let str = Buffer.contents buf in
for i = 0 to 6 do
  product := !product * ((int_of_char str.[pow 10 i - 1]) - (int_of_char '0'));
  print_char str.[pow 10 i - 1]; print_newline ()
done;;

print_int !product; print_newline ();;
