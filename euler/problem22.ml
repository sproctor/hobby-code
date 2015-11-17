let read_file name =
  let file = open_in name in
  let size = in_channel_length file in
  let buf = String.create size in
  really_input file buf 0 size;
  close_in file;
  buf;;

let rec print_list l =
  match l with
    [] -> ""
  | s::rest -> print_string s; print_newline (); print_list rest;;

(* print_string (read_file "names.txt"); print_newline ();; *)

let get_names str =
  let rec aux pos names =
    try
      let start_name = String.index_from str pos '"' + 1 in
      let end_name = String.index_from str start_name '"' in
      let name = String.sub str start_name (end_name - start_name) in
      (* print_string ((string_of_int start_name) ^ " "
         ^ (string_of_int end_name) ^ " " ^ name ^ "\n"); *)
      aux (end_name + 1) (name::names)
    with Invalid_argument _ -> names
    | Not_found -> names
  in aux 0 [];;

let get_n str n =
  int_of_char str.[n] - int_of_char 'A' + 1;;

let get_value str pos =
  let rec aux n acc =
    if n >= String.length str then acc
    else aux (n + 1) (acc + get_n str n)
  in pos * aux 0 0;;

print_int (get_value "COLIN" 938); print_newline ();;

let sum_names names =
  let rec aux ns pos acc =
    match ns with
      [] -> acc
    | name::rest -> aux rest (pos + 1) (get_value name pos + acc)
  in aux names 1 0;;
  
let sorted_names = List.sort String.compare (get_names (read_file "names.txt"));;
print_int (sum_names sorted_names); print_newline ();;
