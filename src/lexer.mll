{
open Parser
exception SyntaxError of string
}

(* Regex definitons *)

let white = [' ' '\t' '\n' '\r']+
let num = ['0'-'9']+
let letter = ['a'-'z' 'A'-'Z']
let mat = "mat" num ['x'] num
let sampler = "sampler" num ['D']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let floatval = ((['0'-'9']*['.']['0'-'9']+)|(['0'-'9']+['.']['0'-'9']*))
let newline = ('\r' | '\n' | "\r\n" | eof)
let comment = "//" [^ '\r' '\n']* newline

(* Lexer definition *)

rule read = parse
  | comment         { read lexbuf }
  | white           { read lexbuf }
  | num as num      { NUM (int_of_string num) }
  | "tag"           { TAG }
  | "is"            { IS }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "if"            { IF }
  | "else"          { ELSE }
  | "skip"          { SKIP }
  | "print"         { PRINT }
  | "int"           { INTTYP }
  | "float"         { FLOATTYP }
  | "auto"          { AUTOTYP }
  | mat as mat      { MATTYP mat }
  | "bool"          { BOOLTYP }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { TIMES }
  | "/"             { DIV }
  | ".*"            { CTIMES }
  | "["             { LBRACK }
  | "]"             { RBRACK }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "->"            { TRANS }
  | "="             { GETS }
  | "=="            { EQ }
  | "<="            { LEQ }
  | "||"            { OR }
  | "&&"            { AND }
  | "!"             { NOT }
  | ","             { COMMA }
  | ";"             { SEMI }
  | "."             { DOT }
  | sampler as sm   { SAMPLER sm }
  | "void"          { VOID }
  | "return"        { RETURN }
  | "declare"       { DECLARE }
  | "attribute"
  | "const"
  | "uniform"
  | "varying"
  | "break"
  | "continue"
  | "do"
  | "for"
  | "while"
  | "in"
  | "out"
  | "inout"
  | "void"
  | "lowp"
  | "mediump"
  | "highp"
  | "precision"
  | "invariant"
  | "discard"
  | "ivec2"
  | "ivec3"
  | "ivec4"
  | "bvec2"
  | "bvec3"
  | "bvec4"
  | "samplerCube"
  | "asm"
  | "class" 
  | "union" 
  | "enum"
  | "typedef"
  | "template"
  | "this"
  | "packed"
  | "goto"
  | "switch"
  | "default"
  | "inline" 
  | "noinline"
  | "volatile"
  | "public"
  | "static"
  | "extern"
  | "external"
  | "interface"
  | "flat"
  | "long"
  | "short"
  | "double"
  | "half"
  | "fixed"
  | "unsigned"
  | "superp"
  | "input"
  | "output"
  | "hvec2"
  | "hvec3"
  | "hvec4"
  | "dvec2"
  | "dvec3"
  | "dvec4"
  | "fvec2"
  | "fvec3"
  | "fvec4"
  | "sampler1DShadow"
  | "sampler2DShadow"
  | "sampler2DRect"
  | "sampler3DRect"
  | "sampler2DRectShadow"
  | "sizeof"
  | "cast"
  | "namespace"
  | "using" 
  | "struct"        { raise (SyntaxError ("Cannot use reserved GLSL keyword " ^ Lexing.lexeme lexbuf)) }
  | id as id        { ID id }
  | floatval as fl  { FLOAT (float_of_string fl) }
  | eof             { EOL }
  | _               { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

