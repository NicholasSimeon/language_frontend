open! Token
open! Parserrules

type state = int
type stack = (state * Parserrules.symbol) list

let parse (grammer: grammer) (action_table: action_table) (goto_table: goto_table) (input: string list) = 
  let rec parse_expression stack input = 
  let state = fst(List.hd stack) in
  match input with
    | [] -> (match Hashtbl.find action_table (state, Parserrules.Terminal "S") with
      |Parserrules.Accept -> print_endline "Accepted"
      | _ -> print_endline "Unexpected end of input")
