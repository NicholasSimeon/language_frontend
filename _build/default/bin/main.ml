open! Lexer
open! Parser
open! Token

let tokenizer =
  let () = print_string "main> " in
  let string = read_line () in
  string ^ " "

let tokens = Lexer.string_to_token tokenizer

let () =
  for i = 0 to List.length tokens - 1 do
    let x = Token.token_to_string (List.nth tokens i) in
    print_endline x
  done
