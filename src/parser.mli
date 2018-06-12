
(* The type of tokens. *)

type token = 
  | TRUE
  | TRANS
  | TIMES
  | TAG
  | SKIP
  | SEMI
  | RPAREN
  | RBRACK
  | RBRACE
  | PRINT
  | PLUS
  | OR
  | NUM of (int)
  | NOT
  | NORM
  | MINUS
  | LPAREN
  | LEQ
  | LBRACK
  | LBRACE
  | IS
  | INTTYP
  | IF
  | ID of (string)
  | GETS
  | FLOATTYP
  | FLOAT of (float)
  | FALSE
  | EQ
  | EOL
  | ELSE
  | DOT
  | CTIMES
  | COMMA
  | COLON
  | BOOLTYP
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)
