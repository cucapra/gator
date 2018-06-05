{
open Parser
}

(* Regex definitons *)

let white = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let num = ['0'-'9'] ['0'-'9']*
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let floatval = ['+' '-']? (['0'-'9']*['.'])?['0'-'9']+
let newline = ('\010' | '\013' | "\013\010")
let comment = "//" [^ '\010' '\013']* newline

(* Lexer definition *)

rule read = parse
  | comment 
  | white { read lexbuf }
  | "tag" { TAG }
  | "is"  { IS }
  | "mat" { MAT }
  | "vec" { VEC }
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
  | "bool" { BOOLTYP }
  | id  as id { ID id }
  | num as num  { NUM (int_of_string num) }
  | floatval as floatval  { FLOAT (float_of_string floatval) }
  | "+"   { PLUS }
  | "*"   { TIMES }
  | ".*"  { CTIMES }
  | ":"   { COLON }
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
  | eof   { EOL }
