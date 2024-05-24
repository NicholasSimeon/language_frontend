open Lexer

let tokenizer =
  let () = print_string "main> " in
  let string = read_line () in
  string

let () = print_endline tokenizer
