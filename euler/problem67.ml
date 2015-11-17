let read_file name =
  let file = open_in name in
  let size = in_channel_length file in
  let buf = String.create size in
  really_input file buf 0 size;
  close_in file;
  buf;;

let get str column =
  let sub = String.sub str (column * 3) 2 in
  int_of_string sub;;

let max a b =
  if b > a then b
  else a;;

let list_max l =
  let rec aux li m =
    match li with
      [] -> m
    | x::xs -> aux xs (max x m)
  in aux l 0

let rec run_paths remaining totals =
  let get_total str pos =
    let v = get str pos in
    if pos = 0 then v + List.nth totals pos
    else if pos >= List.length totals then v + List.nth totals (pos - 1)
    else v + max (List.nth totals (pos - 1)) (List.nth totals pos)
  in let rec get_totals str pos acc =
    if pos > List.length totals then acc
    else get_totals str (pos+1) (acc@[get_total str pos])
  in match remaining with
    [] -> list_max totals
  | str::rest -> run_paths rest (get_totals str 0 [])

let run_pyramid p =
  match p with
    [] -> 0
  | str::rest -> run_paths rest [(get str 0)];;

let rec print_list l =
  match l with
    [] -> ""
  | s::rest -> print_string s; print_newline (); print_list rest;;

print_int (run_pyramid (Str.split (Str.regexp "\r\n") (read_file "triangle.txt")));
  print_newline ();;
