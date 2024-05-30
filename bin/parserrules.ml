type nodes =
  | Program
  | Number
  | Integer
  | Float
  | Hex
  | Octal
  | Binary
  | String
  | Char
  | Identifier
  | Primary
  | Expression
  | Logicalor
  | Logicaland
  | Bitwiseor
  | Bitwiseand
  | Equality
  | Relational
  | Shift
  | Additive
  | Multiplicative
  | Unary
  | FunctionCall
  | ArgumentList
  | Statement
  | Assignment
  | IfStatement
  | WhileStatement
  | FunctionDefinition
  | ParameterList
  | ReturnStatement
  | Block
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

type action = Shift of int | Reduce of int | Accept | Error
type symbol = Terminal of nodes | NonTerminal of nodes
type production = { lhs : string; rhs : symbol list }
type item = { lhs : string; rhs : symbol list; dot : int }
type state = item list

type parsing_table = {
  action : (int * string, action) Hashtbl.t;
  goto : (int * string, int) Hashtbl.t;
}

type production = { lhs : string; rhs : symbol list }

let production_table : production list =
  [
    (* NonTerminal *)
    { lhs = "Integer"; rhs = [ Terminal Integer ] };
    { lhs = "Float"; rhs = [ Terminal Float ] };
    { lhs = "Hex"; rhs = [ Terminal Integer ] };
    { lhs = "Octal"; rhs = [ Terminal Octal ] };
    { lhs = "Binary"; rhs = [ Terminal Binary ] };
    { lhs = "String"; rhs = [ Terminal String ] };
    { lhs = "Char"; rhs = [ Terminal String ] };
    { lhs = "Identifier"; rhs = [ Terminal Identifier ] };
    (* Terminal *)
    { lhs = "Program"; rhs = [ NonTerminal Block ] };
    { lhs = "Number"; rhs = [ NonTerminal Integer ] };
    { lhs = "Number"; rhs = [ NonTerminal Float ] };
    { lhs = "Number"; rhs = [ NonTerminal Hex ] };
    { lhs = "Number"; rhs = [ NonTerminal Octal ] };
    { lhs = "Number"; rhs = [ NonTerminal Binary ] };
    { lhs = "Primary"; rhs = [ NonTerminal Number ] };
    { lhs = "Primary"; rhs = [ NonTerminal Identifier ] };
    {
      lhs = "Primary";
      rhs = [ Terminal LPAREN; NonTerminal Expression; Terminal RPAREN ];
    };
    { lhs = "Primary"; rhs = [ NonTerminal FunctionCall ] };
    { lhs = "Logicalor"; rhs = [ NonTerminal Logicaland ] };
    {
      lhs = "Logicalor";
      rhs = [ NonTerminal Logicaland; Terminal LOR; NonTerminal Logicalor ];
    };
    { lhs = "Logicaland"; rhs = [ NonTerminal Bitwiseor ] };
    {
      lhs = "Logicaland";
      rhs = [ NonTerminal Bitwiseor; Terminal LOR; NonTerminal Logicaland ];
    };
    { lhs = "Bitwiseor"; rhs = [ NonTerminal Bitwiseand ] };
    {
      lhs = "Bitwiseor";
      rhs = [ NonTerminal Bitwiseand; Terminal OR; NonTerminal Bitwiseor ];
    };
    { lhs = "Bitwiseand"; rhs = [ NonTerminal Equality ] };
    {
      lhs = "Bitwiseand";
      rhs = [ NonTerminal Equality; Terminal AND; NonTerminal Bitwiseand ];
    };
    { lhs = "Equality"; rhs = [ NonTerminal Relational ] };
    {
      lhs = "Equality";
      rhs = [ NonTerminal Relational; Terminal EQ; NonTerminal Equality ];
    };
    {
      lhs = "Equality";
      rhs = [ NonTerminal Relational; Terminal NEQ; NonTerminal Equality ];
    };
    { lhs = "Relational"; rhs = [ NonTerminal Shift ] };
    {
      lhs = "Relational";
      rhs = [ NonTerminal Shift; Terminal LESS; NonTerminal Equality ];
    };
    {
      lhs = "Relational";
      rhs = [ NonTerminal Shift; Terminal GREAT; NonTerminal Equality ];
    };
    {
      lhs = "Relational";
      rhs = [ NonTerminal Shift; Terminal GREQ; NonTerminal Equality ];
    };
    {
      lhs = "Relational";
      rhs = [ NonTerminal Shift; Terminal LSEQ; NonTerminal Equality ];
    };
    { lhs = "Shift"; rhs = [ NonTerminal Additive ] };
    {
      lhs = "Shift";
      rhs = [ NonTerminal Additive; Terminal SLEFT; NonTerminal Shift ];
    };
    {
      lhs = "Shift";
      rhs = [ NonTerminal Additive; Terminal SRIGHT; NonTerminal Shift ];
    };
    { lhs = "Additive"; rhs = [ NonTerminal Multiplicative ] };
    {
      lhs = "Additive";
      rhs = [ NonTerminal Multiplicative; Terminal ADD; NonTerminal Additive ];
    };
    {
      lhs = "Additive";
      rhs = [ NonTerminal Multiplicative; Terminal SUB; NonTerminal Additive ];
    };
    { lhs = "Multiplicative"; rhs = [ NonTerminal Unary ] };
    {
      lhs = "Multiplicative";
      rhs = [ NonTerminal Unary; Terminal MUL; NonTerminal Additive ];
    };
    {
      lhs = "Multiplicative";
      rhs = [ NonTerminal Unary; Terminal DIV; NonTerminal Additive ];
    };
    {
      lhs = "Multiplicative";
      rhs = [ NonTerminal Unary; Terminal REM; NonTerminal Additive ];
    };
    { lhs = "Unary"; rhs = [ NonTerminal Primary ] };
    { lhs = "Unary"; rhs = [ Terminal INC; NonTerminal Primary ] };
    { lhs = "Unary"; rhs = [ Terminal DEC; NonTerminal Primary ] };
    { lhs = "Unary"; rhs = [ Terminal NOT; NonTerminal Unary ] };
    {
      lhs = "FunctionCall";
      rhs =
        [
          Terminal Identifier;
          Terminal LPAREN;
          NonTerminal ArgumentList;
          Terminal RPAREN;
        ];
    };
    {
      lhs = "FunctionCall";
      rhs = [ Terminal Identifier; Terminal LPAREN; Terminal RPAREN ];
    };
    { lhs = "ArgumentList"; rhs = [ NonTerminal Expression ] };
    {
      lhs = "ArgumentList";
      rhs = [ NonTerminal Expression; Terminal COMMA; NonTerminal ArgumentList ];
    };
    { lhs = "Statement"; rhs = [ NonTerminal Assignment ] };
    { lhs = "Statement"; rhs = [ NonTerminal Expression; Terminal SEMI ] };
    { lhs = "Statement"; rhs = [ NonTerminal IfStatement ] };
    { lhs = "Statement"; rhs = [ NonTerminal WhileStatement ] };
    { lhs = "Statement"; rhs = [ NonTerminal ReturnStatement ] };
    { lhs = "Block"; rhs = [ NonTerminal Statement ] };
    { lhs = "Block"; rhs = [ NonTerminal Block; NonTerminal Statement ] };
    {
      lhs = "Assignment";
      rhs =
        [
          Terminal Identifier;
          Terminal DEFINE;
          NonTerminal Expression;
          Terminal SEMI;
        ];
    };
    {
      lhs = "IfStatement";
      rhs =
        [
          Terminal IF;
          Terminal LPAREN;
          NonTerminal Expression;
          Terminal RPAREN;
          Terminal LBRACK;
          NonTerminal Block;
          Terminal RBRACK;
        ];
    };
    {
      lhs = "WhileStatement";
      rhs =
        [
          Terminal FOR;
          Terminal LPAREN;
          NonTerminal Expression;
          Terminal RPAREN;
          Terminal LBRACK;
          NonTerminal Block;
          Terminal RBRACK;
        ];
    };
    {
      lhs = "FunctionDefinition";
      rhs =
        [
          Terminal FUN;
          Terminal Identifier;
          Terminal LPAREN;
          Terminal RPAREN;
          Terminal LBRACK;
          NonTerminal Block;
          Terminal RBRACK;
        ];
    };
    {
      lhs = "FunctionDefinition";
      rhs =
        [
          Terminal FUN;
          Terminal Identifier;
          Terminal LPAREN;
          NonTerminal ParameterList;
          Terminal RPAREN;
          Terminal LBRACK;
          NonTerminal Block;
          Terminal RBRACK;
        ];
    };
    { lhs = "ParameterList"; rhs = [ Terminal Identifier ] };
    {
      lhs = "ParameterList";
      rhs = [ Terminal Identifier; Terminal COMMA; NonTerminal ParameterList ];
    };
    {
      lhs = "ReturnStatement";
      rhs = [ Terminal RETURN; NonTerminal Expression; Terminal SEMI ];
    };
  ]
