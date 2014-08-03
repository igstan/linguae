structure Token : TOKEN =
struct
  type linenum = int

  datatype token =
    TYPE of linenum * linenum
  | VAR of linenum * linenum
  | FUNCTION of linenum * linenum
  | BREAK of linenum * linenum
  | OF of linenum * linenum
  | END of linenum * linenum
  | IN of linenum * linenum
  | NIL of linenum * linenum
  | LET of linenum * linenum
  | DO of linenum * linenum
  | TO of linenum * linenum
  | FOR of linenum * linenum
  | WHILE of linenum * linenum
  | ELSE of linenum * linenum
  | THEN of linenum * linenum
  | IF of linenum * linenum
  | ARRAY of linenum * linenum
  | ASSIGN of linenum * linenum
  | OR of linenum * linenum
  | AND of linenum * linenum
  | GE of linenum * linenum
  | GT of linenum * linenum
  | LE of linenum * linenum
  | LT of linenum * linenum
  | NEQ of linenum * linenum
  | EQ of linenum * linenum
  | DIVIDE of linenum * linenum
  | TIMES of linenum * linenum
  | MINUS of linenum * linenum
  | PLUS of linenum * linenum
  | DOT of linenum * linenum
  | RBRACE of linenum * linenum
  | LBRACE of linenum * linenum
  | RBRACK of linenum * linenum
  | LBRACK of linenum * linenum
  | RPAREN of linenum * linenum
  | LPAREN of linenum * linenum
  | SEMICOLON of linenum * linenum
  | COLON of linenum * linenum
  | COMMA of linenum * linenum
  | STRING of string * linenum * linenum
  | INT of int * linenum * linenum
  | ID of string * linenum * linenum
  | EOF of linenum * linenum

  fun toString token =
    case token of
      TYPE (i, j) => "TYPE " ^ Int.toString i ^ ":" ^ Int.toString j
    | VAR (i, j) => "VAR " ^ Int.toString i ^ ":" ^ Int.toString j
    | FUNCTION (i, j) => "FUNCTION " ^ Int.toString i ^ ":" ^ Int.toString j
    | BREAK (i, j) => "BREAK " ^ Int.toString i ^ ":" ^ Int.toString j
    | OF (i, j) => "OF " ^ Int.toString i ^ ":" ^ Int.toString j
    | END (i, j) => "END " ^ Int.toString i ^ ":" ^ Int.toString j
    | IN (i, j) => "IN " ^ Int.toString i ^ ":" ^ Int.toString j
    | NIL (i, j) => "NIL " ^ Int.toString i ^ ":" ^ Int.toString j
    | LET (i, j) => "LET " ^ Int.toString i ^ ":" ^ Int.toString j
    | DO (i, j) => "DO " ^ Int.toString i ^ ":" ^ Int.toString j
    | TO (i, j) => "TO " ^ Int.toString i ^ ":" ^ Int.toString j
    | FOR (i, j) => "FOR " ^ Int.toString i ^ ":" ^ Int.toString j
    | WHILE (i, j) => "WHILE " ^ Int.toString i ^ ":" ^ Int.toString j
    | ELSE (i, j) => "ELSE " ^ Int.toString i ^ ":" ^ Int.toString j
    | THEN (i, j) => "THEN " ^ Int.toString i ^ ":" ^ Int.toString j
    | IF (i, j) => "IF " ^ Int.toString i ^ ":" ^ Int.toString j
    | ARRAY (i, j) => "ARRAY " ^ Int.toString i ^ ":" ^ Int.toString j
    | ASSIGN (i, j) => "ASSIGN " ^ Int.toString i ^ ":" ^ Int.toString j
    | OR (i, j) => "OR " ^ Int.toString i ^ ":" ^ Int.toString j
    | AND (i, j) => "AND " ^ Int.toString i ^ ":" ^ Int.toString j
    | GE (i, j) => "GE " ^ Int.toString i ^ ":" ^ Int.toString j
    | GT (i, j) => "GT " ^ Int.toString i ^ ":" ^ Int.toString j
    | LE (i, j) => "LE " ^ Int.toString i ^ ":" ^ Int.toString j
    | LT (i, j) => "LT " ^ Int.toString i ^ ":" ^ Int.toString j
    | NEQ (i, j) => "NEQ " ^ Int.toString i ^ ":" ^ Int.toString j
    | EQ (i, j) => "EQ " ^ Int.toString i ^ ":" ^ Int.toString j
    | DIVIDE (i, j) => "DIVIDE " ^ Int.toString i ^ ":" ^ Int.toString j
    | TIMES (i, j) => "TIMES " ^ Int.toString i ^ ":" ^ Int.toString j
    | MINUS (i, j) => "MINUS " ^ Int.toString i ^ ":" ^ Int.toString j
    | PLUS (i, j) => "PLUS " ^ Int.toString i ^ ":" ^ Int.toString j
    | DOT (i, j) => "DOT " ^ Int.toString i ^ ":" ^ Int.toString j
    | RBRACE (i, j) => "RBRACE " ^ Int.toString i ^ ":" ^ Int.toString j
    | LBRACE (i, j) => "LBRACE " ^ Int.toString i ^ ":" ^ Int.toString j
    | RBRACK (i, j) => "RBRACK " ^ Int.toString i ^ ":" ^ Int.toString j
    | LBRACK (i, j) => "LBRACK " ^ Int.toString i ^ ":" ^ Int.toString j
    | RPAREN (i, j) => "RPAREN " ^ Int.toString i ^ ":" ^ Int.toString j
    | LPAREN (i, j) => "LPAREN " ^ Int.toString i ^ ":" ^ Int.toString j
    | SEMICOLON (i, j) => "SEMICOLON " ^ Int.toString i ^ ":" ^ Int.toString j
    | COLON (i, j) => "COLON " ^ Int.toString i ^ ":" ^ Int.toString j
    | COMMA (i, j) => "COMMA " ^ Int.toString i ^ ":" ^ Int.toString j
    | STRING (s, i, j) => "STRING(" ^ s ^ ") " ^ Int.toString i ^ ":" ^ Int.toString j
    | INT (c, i, j) => "INT(" ^ Int.toString c ^ ") " ^ Int.toString i ^ ":" ^ Int.toString j
    | ID (s, i, j) => "ID(" ^ s ^ ") " ^ Int.toString i ^ ":" ^ Int.toString j
    | EOF (i, j) => "EOF " ^ Int.toString i ^ ":" ^ Int.toString j
end
