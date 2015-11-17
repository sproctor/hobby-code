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

(* print_string (read_file "words.txt"); print_newline ();; *)

let get_words str =
  let rec aux pos words =
    try
      let start_word = String.index_from str pos '"' + 1 in
      let end_word = String.index_from str start_word '"' in
      let word = String.sub str start_word (end_word - start_word) in
      aux (end_word + 1) (word::words)
    with Invalid_argument _ -> words
    | Not_found -> words
  in aux 0 [];;

let get_n str n =
  int_of_char str.[n] - int_of_char 'A' + 1;;

let get_value str =
  let rec aux n acc =
    if n >= String.length str then acc
    else aux (n + 1) (acc + get_n str n)
  in aux 0 0;;

print_int (get_value "SKY"); print_newline ();;

let is_triangle_word word =
  let v = get_value word in
  let rec aux n =
    let t = n * (n + 1) / 2 in
    if t > v
      then false
      else if t = v then true
      else aux (n + 1)
  in aux 1;;

print_string (string_of_bool (is_triangle_word "SKY")); print_newline ();;

let check_words words =
  let rec aux ws acc =
    match ws with
      [] -> acc
    | w::rest -> aux rest (acc + (if is_triangle_word w then 1 else 0))
  in aux words 0;;

print_int (check_words (get_words (read_file "words.txt"))); print_newline ();;
