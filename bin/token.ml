type token =
  (*Operators*)
  | DIV (* / *)
  | SUB (* - *)
  | MUL (* * *)
  | ADD (* + *)
  | REM (* % *)
  | AND (* & *)
  | XOR (* ^ *)
  | OR (* | *)
  | SRIGHT (* >> *)
  | SLEFT (* << *)
  | LPAREN (* ( *)
  | RPAREN (* ) *)
  | LBRACK (* [ *)
  | RBRACK (* ] *)
  | LBRACE (* { *)
  | RBRACE (* } *)
  | ADDEQ (* += *)
  | MULEQ (* *= *)
  | SUBEQ (* -= *)
  | DIVEQ (* /= *)
  | REMEQ (* %= *)
  | XOREQ (* ^= *)
  | OREQ (* |= *)
  | ANDEQ (* &= *)
  | EQ (* == *)
  | NEQ (* != *)
  | ASSIGN (* = *)
  | DEFINE (* := *)
  | NOT (* ! *)
  | SEMI (* ; *)
  | COLON (* : *)
  | GREAT (* > *)
  | LESS (* < *)
  | GREQ (* >= *)
  | LSEQ (* <= *)
  | LOR (* || *)
  | LAND (* && *)
  | DOT (* . *)
  | COMMA (* , *)
  | INC (* ++ *)
  | DEC (* -- *)
  (* BASIC Tokens*)
  | EOF
  | CHAR
  | STRING
  | INT
  | FLOAT
  | HEX
  | OCT
  | BINARY
  | IDENTIFIER
  (* KEYWORDS *)
  | IF
  | ELSE
  | ELIF
  | FUN
  | FOR
  | IN
  | VAR
  | STRUCT
  | MUT
  | IMPORT
  | AS
  | MOD
  | PUBLIC
  | PRIVATE
  | RETURN
  | TRUE
  | FALSE

let token_to_string token =
  match token with
  (*Operators*)
  | DIV -> " / "
  | SUB -> " - "
  | MUL -> " * "
  | ADD -> " + "
  | REM -> " % "
  | AND -> " & "
  | XOR -> " ^ "
  | SRIGHT -> " >> "
  | SLEFT -> " << "
  | OR -> " | "
  | LPAREN -> " ( "
  | RPAREN -> " ) "
  | LBRACK -> " [ "
  | RBRACK -> " ] "
  | LBRACE -> " { "
  | RBRACE -> " } "
  | ADDEQ -> " += "
  | MULEQ -> " *= "
  | SUBEQ -> " -= "
  | DIVEQ -> " /= "
  | REMEQ -> " %= "
  | XOREQ -> " ^= "
  | OREQ -> " |= "
  | ANDEQ -> " &= "
  | EQ -> " == "
  | NEQ -> " != "
  | ASSIGN -> " = "
  | DEFINE -> " := "
  | NOT -> " ! "
  | SEMI -> " ; "
  | COLON -> " : "
  | GREAT -> " > "
  | LESS -> " < "
  | GREQ -> " >= "
  | LSEQ -> " <= "
  | LOR -> " || "
  | LAND -> " && "
  | DOT -> " . "
  | COMMA -> " , "
  | INC -> " ++ "
  | DEC -> " -- "
  (* BASIC Tokens*)
  | EOF -> "EOF"
  | CHAR -> "Char"
  | STRING -> "String"
  | INT -> "Int"
  | FLOAT -> "Float"
  | HEX -> "Hex"
  | OCT -> "Octal"
  | BINARY -> "Binary"
  | IDENTIFIER -> "Identifier"
  (* KEYWORDS *)
  | IF -> "If"
  | ELSE -> "Else"
  | ELIF -> "Else If"
  | FUN -> "Fun"
  | FOR -> "For"
  | IN -> "In"
  | VAR -> "Var"
  | STRUCT -> "Struct"
  | MUT -> "Mut"
  | IMPORT -> "Import"
  | AS -> "As"
  | MOD -> "Mod"
  | PUBLIC -> "Public"
  | PRIVATE -> "Private"
  | RETURN -> "Return"
  | TRUE -> "True"
  | FALSE -> "False"
