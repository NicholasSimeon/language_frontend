open! Parserrules


let rec parse_grammer stack (productions: Parserrules.production list) tokens =
  for i = 0 to List.length productions - 1 do
    if stack = (List.nth productions i).rhs then
      (*REDUCE/ RETURN*)

  done
  (*IF THAT DOESNT WORK, TRY SHIFTING*)
  (*If token array is 0, then accept*)
  (*
  Check for reduce, 

  if everything in the stack can be reduced to a single production then reduce
  if not then shift,
  if shift is nothing, then accept.
  *)

  


