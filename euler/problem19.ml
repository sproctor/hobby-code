let days_in_month month year =
  if month = 4 || month = 6 || month = 9 || month = 11 then 30
  else if month != 2 then 31
  else if year mod 4 = 0 then 29 else 28;;

let end_year = 2000;;
let end_month = 12;;

let sundays_after start_month start_year start_day =
  let rec aux month year first_day acc =
    if year > end_year || (year = end_year && month > end_month) then acc
    else
      let new_month = (month mod 12) + 1 in
      let new_year = if month = 1 then year + 1 else year in
      let total = if first_day mod 7 = 0 then (print_string ((string_of_int month) ^ " " ^ (string_of_int year) ^ "\n"); flush stdout; acc + 1) else acc in
      aux new_month new_year ((first_day + days_in_month month year) mod 7)
        total
  in aux start_month start_year start_day 0;;

print_int (sundays_after 1 1901 2); print_newline ();;
