for n = 10 to 99 do
  for d = (n + 1) to 99 do
    let v = (float_of_int n) /. (float_of_int d) in
    if ((float_of_int (n / 10)) /. (float_of_int (d mod 10)) = v && n mod 10 = d / 10)
        || ((float_of_int (n mod 10)) /. (float_of_int (d / 10)) = v && n / 10 = d mod 10)
        || ((float_of_int (n mod 10)) /. (float_of_int (d mod 10)) = v && n / 10 = d / 10) then
      (print_int n; print_string " "; print_int d; print_newline ())
  done
done;;

