signature TOKEN =
sig
  type linenum

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

  val toString : token -> string
end
