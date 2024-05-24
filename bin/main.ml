open Lexer

let tokenizer =
  let () = print_string "main> " in
  let string = read_line () in
  string

let f elem =
  let () = print_endline elem in
  List.iter f Lexer.string_to_token tokenizer
