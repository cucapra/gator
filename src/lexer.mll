{
open Parser
exception SyntaxError of string

}

(* Regex definitons *)

let white = [' ' '\t' '\n' '\r']+
let num = ['+' '-']? ['0'-'9']+
let letter = ['a'-'z' 'A'-'Z']
let mat = "mat" num ['x'] num
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let floatval = ['+' '-']? ((['0'-'9']*['.']['0'-'9']+)|(['0'-'9']+['.']['0'-'9']*))
let newline = ('\r' | '\n' | "\r\n" | eof)
let comment = "//" [^ '\r' '\n']* newline

(* Lexer definition *)

rule read = parse
  | comment { read lexbuf }
  | white { read lexbuf }
  | num as num  { NUM (int_of_string num) }
  | "tag" { TAG }
  | "is"  { IS }
  | "dot" { DOT }
  | "norm" { NORM }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "else" { ELSE }
  | "skip" { SKIP }
  | "print" { PRINT }
  | "int" { INTTYP }
  | "float" { FLOATTYP }
  | mat as mat { MATTYP mat }
  | "bool" { BOOLTYP }
  | "+"   { PLUS }
  | "-"   { MINUS }
  | "*"   { TIMES }
  | "/"   { DIV }
  | ".*"  { CTIMES }
  | "["   { LBRACK }
  | "]"   { RBRACK }
  | "{"   { LBRACE }
  | "}"   { RBRACE }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "->"  { TRANS }
  | "="   { GETS }
  | "=="  { EQ }
  | "<="  { LEQ }
  | "||"  { OR }
  | "&&"  { AND }
  | "!"   { NOT }
  | ","   { COMMA } 
  | ";"   { SEMI }
  | id as id { ID id }
  | floatval as floatval  { FLOAT (float_of_string floatval) }
  | eof   { EOL }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

