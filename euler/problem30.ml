let power x y =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n - 1) (acc * x)
  in aux y 1;;

let pow5 x = power x 5;;

let final_sum = ref 0;;

for d1 = 0 to 9 do
  for d2 = 0 to 9 do
    for d3 = 0 to 9 do
      for d4 = 0 to 9 do
        for d5 = 0 to 9 do
          for d6 = 0 to 2 do
            let number = d1 + 10 * d2 + 100 * d3 + 1000 * d4 + 10000 * d5
                + 100000 * d6 in
            if pow5 d6 + pow5 d5 + pow5 d4 + pow5 d3 + pow5 d2 + pow5 d1 =
                number  && number > 1 then
              (final_sum := !final_sum + number;
               print_int number; print_newline ())
          done
        done
      done
    done
  done
done;;

print_int !final_sum; print_newline ();;
