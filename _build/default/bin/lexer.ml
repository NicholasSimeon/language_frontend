(* lexer.ml *)

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
  | DIV -> "Divide"
  | SUB -> "Subtract"
  | MUL -> "Multiply"
  | ADD -> "Add"
  | REM -> "Remainder"
  | AND -> "And"
  | XOR -> "XOR"
  | OR -> "Or"
  | LPAREN -> "Left Paren"
  | RPAREN -> "Right Paren"
  | LBRACK -> "Left Bracket"
  | RBRACK -> "Right Bracket"
  | LBRACE -> "Left Brace"
  | RBRACE -> "Right Brace"
  | ADDEQ -> "Add Equals"
  | MULEQ -> "Multiply Equals"
  | SUBEQ -> "Subtract Equals"
  | DIVEQ -> "Divide Equals"
  | REMEQ -> "Remainder Equals"
  | XOREQ -> "XOR Equals"
  | OREQ -> "Or Equails"
  | ANDEQ -> "And Equals"
  | EQ -> "Equal"
  | NEQ -> "Not Equal"
  | ASSIGN -> "Assign"
  | DEFINE -> "Define"
  | NOT -> "Not"
  | SEMI -> "Semicolon"
  | COLON -> "Colon"
  | GREAT -> "Greater Than"
  | LESS -> "Less than"
  | GREQ -> "Greater than or equal to"
  | LSEQ -> "Less than or equal to"
  | DOT -> "Dot"
  | COMMA -> "Comma"
  | INC -> "Increment"
  | DEC -> "Decrement"
  (* BASIC Tokens*)
  | EOF -> "End of file"
  | CHAR -> "Char"
  | STRING -> "String"
  | INT -> "Int"
  | FLOAT -> "Float"
  | HEX -> "Hex"
  | OCT -> "Octal"
  | BINARY -> "Binary"
  | IDENTIFIER -> "Indentifier"
  (* KEYWORDS *)
  | IF -> "If"
  | ELSE -> "Else"
  | ELIF -> "Elif"
  | FUN -> "Function"
  | FOR -> "For"
  | IN -> "In"
  | VAR -> "Var"
  | STRUCT -> "Struct"
  | MUT -> "Mutable"
  | IMPORT -> "Import"
  | AS -> "As"
  | MOD -> "Mod"
  | PUBLIC -> "Public"
  | PRIVATE -> "Private"
  | RETURN -> "Return"
  | TRUE -> "True"
  | FALSE -> "False"

let rec create_float input pos str =
  if pos < String.length input && '0' <= input.[pos] && input.[pos] <= '9' then
    create_float input (pos + 1) (str ^ String.make 1 input.[pos])
  else (FLOAT, str, pos)

let rec create_hex input pos str =
  let () = print_endline str in
  if
    pos < String.length input
    && (('0' <= input.[pos] && input.[pos] <= '9')
       || ('a' <= input.[pos] && input.[pos] <= 'f')
       || ('A' <= input.[pos] && input.[pos] <= 'F'))
  then create_hex input (pos + 1) (str ^ String.make 1 input.[pos])
  else (HEX, str, pos)

let rec create_octal input pos str =
  if pos < String.length input && '0' <= input.[pos] && input.[pos] <= '7' then
    create_octal input (pos + 1) (str ^ String.make 1 input.[pos])
  else (OCT, pos, str)

let rec create_binary input pos str =
  if pos < String.length input && '0' <= input.[pos] && input.[pos] <= '1' then
    create_binary input (pos + 1) (str ^ String.make 1 input.[pos])
  else (BINARY, pos, str)

let rec create_number input pos str =
  if pos < String.length input && '0' <= input.[pos] && input.[pos] <= '9' then
    create_number input (pos + 1) (str ^ String.make 1 input.[pos])
  else if input.[pos] == '.' then create_float input (pos + 1) (str ^ ".")
  else (INT, str, pos)

let rec create_identifier input pos str =
  if
    pos < String.length input
    && (('a' <= input.[pos] && input.[pos] <= 'z')
       || ('A' <= input.[pos] && input.[pos] <= 'Z')
       || input.[pos] == '_')
  then create_identifier input (pos + 1) (str ^ String.make 1 input.[pos])
  else
    match str with
    | "if" -> (IF, pos, str)
    | "else" -> (ELSE, pos, str)
    | "elif" -> (ELIF, pos, str)
    | "for" -> (FOR, pos, str)
    | "in" -> (IN, pos, str)
    | "var" -> (VAR, pos, str)
    | "struct" -> (STRUCT, pos, str)
    | "mut" -> (MUT, pos, str)
    | "import" -> (IMPORT, pos, str)
    | "as" -> (AS, pos, str)
    | "mod" -> (MOD, pos, str)
    | "public" -> (PUBLIC, pos, str)
    | "private" -> (PRIVATE, pos, str)
    | "return" -> (RETURN, pos, str)
    | "true" -> (TRUE, pos, str)
    | "false" -> (FALSE, pos, str)
    | _ -> (IDENTIFIER, pos, str)

let rec create_operator input pos str =
  if
    pos < String.length input
    && (('!' <= input.[pos] && input.[pos] <= '/')
       || (':' <= input.[pos] && input.[pos] <= '@')
       || ('[' <= input.[pos] && input.[pos] <= '`')
       || ('{' <= input.[pos] && input.[pos] <= '~')
          && (input.[pos] != '"' || input.[pos] != '\''))
  then create_operator input (pos + 1) (str ^ String.make 1 input.[pos])
  else
    match str with
    | "/" -> (DIV, pos, str)
    | "-" -> (SUB, pos, str)
    | "*" -> (MUL, pos, str)
    | "+" -> (ADD, pos, str)
    | "%" -> (REM, pos, str)
    | "&" -> (AND, pos, str)
    | "^" -> (XOR, pos, str)
    | "|" -> (OR, pos, str)
    | "(" -> (LPAREN, pos, str)
    | ")" -> (RPAREN, pos, str)
    | "[" -> (LBRACK, pos, str)
    | "]" -> (RBRACK, pos, str)
    | "{" -> (LBRACE, pos, str)
    | "}" -> (RBRACE, pos, str)
    | "+=" -> (ADDEQ, pos, str)
    | "*=" -> (MULEQ, pos, str)
    | "-=" -> (SUBEQ, pos, str)
    | "/=" -> (DIVEQ, pos, str)
    | "%=" -> (REMEQ, pos, str)
    | "^=" -> (XOREQ, pos, str)
    | "|=" -> (OREQ, pos, str)
    | "&=" -> (ANDEQ, pos, str)
    | "==" -> (EQ, pos, str)
    | "!=" -> (NEQ, pos, str)
    | "=" -> (ASSIGN, pos, str)
    | ":=" -> (DEFINE, pos, str)
    | "!" -> (NOT, pos, str)
    | ";" -> (SEMI, pos, str)
    | ":" -> (COLON, pos, str)
    | ">" -> (GREAT, pos, str)
    | "<" -> (LESS, pos, str)
    | ">=" -> (GREQ, pos, str)
    | "<=" -> (LSEQ, pos, str)
    | "." -> (DOT, pos, str)
    | "," -> (COMMA, pos, str)
    | "++" -> (INC, pos, str)
    | "--" -> (DEC, pos, str)
    | _ -> failwith "Unknown operator"

let rec create_string input pos str =
  if pos < String.length input && input.[pos] != '"' then
    create_string input (pos + 1) (str ^ String.make 1 input.[pos])
  else
    match input.[pos] with
    | '"' -> (STRING, pos + 1, str ^ String.make 1 input.[pos])
    | _ ->
        let () = print_endline "Expected \"" in
        (EOF, pos, str)

let rec create_char input pos str =
  if pos < String.length input && input.[pos] != '\'' then
    create_char input (pos + 1) (str ^ String.make 1 input.[pos])
  else
    match input.[pos] with
    | '\'' -> (CHAR, pos + 1, str ^ String.make 1 input.[pos])
    | _ ->
        let () = print_endline "Expected  ' " in
        (EOF, pos, str)

let string_to_token input =
  let rec create_tokens pos tokens =
    if pos >= String.length input then tokens @ [ EOF ]
    else
      match input.[pos] with
      | ' ' -> create_tokens (pos + 1) tokens
      | '0' .. '9' as c ->
          if c == '0' then
            if pos >= String.length input then
              create_tokens (pos + 1) tokens @ [ EOF ]
            else
              match input.[pos + 1] with
              | 'x' | 'X' ->
                  let _type, _str, _new_pos = create_hex input (pos + 2) "0x" in
                  create_tokens _new_pos (tokens @ [ _type ])
              | 'b' | 'B' ->
                  let _type, _new_pos, _str =
                    create_binary input (pos + 2) "0b"
                  in
                  create_tokens _new_pos (tokens @ [ _type ])
              | 'o' | 'O' ->
                  let _type, _new_pos, _str =
                    create_octal input (pos + 2) "0o"
                  in
                  create_tokens _new_pos (tokens @ [ _type ])
              | _ ->
                  let _type, _num_str, _new_pos =
                    create_number input pos (String.make 1 c)
                  in
                  create_tokens _new_pos (tokens @ [ _type ])
          else
            let _type, _num_str, _new_pos =
              create_number input pos (String.make 1 c)
            in
            create_tokens _new_pos (tokens @ [ _type ])
      | '"' as c ->
          let _type, _new_pos, _str =
            create_string input (pos + 1) (String.make 1 c)
          in
          create_tokens _new_pos (tokens @ [ _type ])
      | '\'' as c ->
          let _type, _new_pos, _str =
            create_char input (pos + 1) (String.make 1 c)
          in
          create_tokens _new_pos (tokens @ [ _type ])
      | ('a' .. 'z' | 'A' .. 'Z' | '_') as c ->
          let _type, _new_pos, _str =
            create_identifier input (pos + 1) (String.make 1 c)
          in
          create_tokens _new_pos (tokens @ [ _type ])
      | '!' .. '/' | ':' .. '@' | '[' .. '`' | '{' .. '~' ->
          let _type, _new_pos, _str = create_operator input pos "" in
          create_tokens _new_pos (tokens @ [ _type ])
      | _ -> failwith "Unknown character"
  in
  create_tokens 0 []
