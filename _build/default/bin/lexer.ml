(* lexer.ml *)
open! Token

let rec create_float input pos str =
  if pos < String.length input && '0' <= input.[pos] && input.[pos] <= '9' then
    create_float input (pos + 1) (str ^ String.make 1 input.[pos])
  else (Token.FLOAT, str, pos)

let rec create_hex input pos str =
  let () = print_endline str in
  if
    pos < String.length input
    && (('0' <= input.[pos] && input.[pos] <= '9')
       || ('a' <= input.[pos] && input.[pos] <= 'f')
       || ('A' <= input.[pos] && input.[pos] <= 'F'))
  then create_hex input (pos + 1) (str ^ String.make 1 input.[pos])
  else (Token.HEX, str, pos)

let rec create_octal input pos str =
  if pos < String.length input && '0' <= input.[pos] && input.[pos] <= '7' then
    create_octal input (pos + 1) (str ^ String.make 1 input.[pos])
  else (Token.OCT, pos, str)

let rec create_binary input pos str =
  if pos < String.length input && '0' <= input.[pos] && input.[pos] <= '1' then
    create_binary input (pos + 1) (str ^ String.make 1 input.[pos])
  else (Token.BINARY, pos, str)

let rec create_number input pos str =
  if pos < String.length input && '0' <= input.[pos] && input.[pos] <= '9' then
    create_number input (pos + 1) (str ^ String.make 1 input.[pos])
  else if input.[pos] == '.' then create_float input (pos + 1) (str ^ ".")
  else (Token.INT, str, pos)

let rec create_identifier input pos str =
  if
    pos < String.length input
    && (('a' <= input.[pos] && input.[pos] <= 'z')
       || ('A' <= input.[pos] && input.[pos] <= 'Z')
       || input.[pos] == '_')
  then create_identifier input (pos + 1) (str ^ String.make 1 input.[pos])
  else
    match str with
    | "if" -> (Token.IF, pos, str)
    | "else" -> (Token.ELSE, pos, str)
    | "elif" -> (Token.ELIF, pos, str)
    | "for" -> (Token.FOR, pos, str)
    | "in" -> (Token.IN, pos, str)
    | "var" -> (Token.VAR, pos, str)
    | "struct" -> (Token.STRUCT, pos, str)
    | "mut" -> (Token.MUT, pos, str)
    | "import" -> (Token.IMPORT, pos, str)
    | "as" -> (Token.AS, pos, str)
    | "mod" -> (Token.MOD, pos, str)
    | "public" -> (Token.PUBLIC, pos, str)
    | "private" -> (Token.PRIVATE, pos, str)
    | "return" -> (Token.RETURN, pos, str)
    | "true" -> (Token.TRUE, pos, str)
    | "false" -> (Token.FALSE, pos, str)
    | _ -> (Token.IDENTIFIER, pos, str)

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
    | "/" -> (Token.DIV, pos, str)
    | "-" -> (Token.SUB, pos, str)
    | "*" -> (Token.MUL, pos, str)
    | "+" -> (Token.ADD, pos, str)
    | "%" -> (Token.REM, pos, str)
    | "&" -> (Token.AND, pos, str)
    | "^" -> (Token.XOR, pos, str)
    | "|" -> (Token.OR, pos, str)
    | "(" -> (Token.LPAREN, pos, str)
    | ")" -> (Token.RPAREN, pos, str)
    | "[" -> (Token.LBRACK, pos, str)
    | "]" -> (Token.RBRACK, pos, str)
    | "{" -> (Token.LBRACE, pos, str)
    | "}" -> (Token.RBRACE, pos, str)
    | "+=" -> (Token.ADDEQ, pos, str)
    | "*=" -> (Token.MULEQ, pos, str)
    | "-=" -> (Token.SUBEQ, pos, str)
    | "/=" -> (Token.DIVEQ, pos, str)
    | "%=" -> (Token.REMEQ, pos, str)
    | "^=" -> (Token.XOREQ, pos, str)
    | "|=" -> (Token.OREQ, pos, str)
    | "&=" -> (Token.ANDEQ, pos, str)
    | "==" -> (Token.EQ, pos, str)
    | "!=" -> (Token.NEQ, pos, str)
    | "=" -> (Token.ASSIGN, pos, str)
    | ":=" -> (Token.DEFINE, pos, str)
    | "!" -> (Token.NOT, pos, str)
    | ";" -> (Token.SEMI, pos, str)
    | ":" -> (Token.COLON, pos, str)
    | ">" -> (Token.GREAT, pos, str)
    | "<" -> (Token.LESS, pos, str)
    | ">=" -> (Token.GREQ, pos, str)
    | "<=" -> (Token.LSEQ, pos, str)
    | "." -> (Token.DOT, pos, str)
    | "," -> (Token.COMMA, pos, str)
    | "++" -> (Token.INC, pos, str)
    | "--" -> (Token.DEC, pos, str)
    | _ -> failwith "Unknown operator"

let rec create_string input pos str =
  if pos < String.length input && input.[pos] != '"' then
    create_string input (pos + 1) (str ^ String.make 1 input.[pos])
  else
    match input.[pos] with
    | '"' -> (Token.STRING, pos + 1, str ^ String.make 1 input.[pos])
    | _ ->
        let () = print_endline "Expected \"" in
        (Token.EOF, pos, str)

let rec create_char input pos str =
  if pos < String.length input && input.[pos] != '\'' then
    create_char input (pos + 1) (str ^ String.make 1 input.[pos])
  else
    match input.[pos] with
    | '\'' -> (Token.CHAR, pos + 1, str ^ String.make 1 input.[pos])
    | _ ->
        let () = print_endline "Expected  ' " in
        (Token.EOF, pos, str)

let string_to_token input =
  let rec create_tokens pos tokens =
    if pos >= String.length input then tokens @ [ Token.EOF ]
    else
      match input.[pos] with
      | ' ' | '\n' | '\t' -> create_tokens (pos + 1) tokens
      | '0' .. '9' as c ->
          if c == '0' then
            if pos >= String.length input then
              create_tokens (pos + 1) tokens @ [ Token.EOF ]
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
