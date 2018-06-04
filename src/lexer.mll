{
open Parser
}

(* Regex definitons *)

let white = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = ['+' '-']? (['0'-'9']*['.'])?['0'-'9']+
let newline = ('\010' | '\013' | "\013\010")
let comment = "//" [^ '\010' '\013']* newline

(* Lexer definition *)

rule read = parse
  | comment 
  | white { read lexbuf }
  | "+"   { PLUS }
  | "*"   { TIMES }
  | ".*"  { CTIMES }
  | ":"   { COLON }
  | "["   { LBRACK }
  | "]"   { RBRACK }
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
  | "tag" { TAG }
  | "is"  { IS }
  | "mat" { MAT }
  | "vec" { VEC }
  | "dot" { DOT }
  | "norm" { NORM }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "skip" { SKIP }
  | "print" { PRINT }
  | ";"   { SEMI }
  | "int" { INTTYP }
  | "float" { FLOATTYP }
  | "bool" { BOOLTYP }
  | id    { ID (Lexing.lexeme lexbuf) }
  | int   { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | eof   { EOL }
