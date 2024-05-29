type action = Shift of int | Reduce of int | Accept | Error
type symbol = Terminal of string | NonTerminal of string
type production = { lhs : string; rhs : symbol list }
type item = { lhs : string; rhs : symbol list; dot : int }
type state = item list

type parsing_table = {
  action : (int * string, action) Hashtbl.t;
  goto : (int * string, int) Hashtbl.t;
}
