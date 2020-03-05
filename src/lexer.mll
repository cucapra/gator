{
open Parser
open Printf
exception Eof
exception Err
exception SyntaxError of string
}

(* Regex definitons *)

let white = [' ' '\t']
let num = ['0'-'9']+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let s = ['"'] ['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '-' '.' '_' '/' ';']* ['"'] 
let floatval = ((['0'-'9']*['.']['0'-'9']+)|(['0'-'9']+['.']['0'-'9']*))
let newline = ['\n' '\r']
let comment = "//" [^ '\r' '\n']* 

(* Lexer definition *)

rule read = parse
  | comment         { read lexbuf }
  | white           { read lexbuf }
  | newline         { Lexing.new_line lexbuf; read lexbuf }
  | "using"         { USING }
  | "prototype"     { PROTOTYPE }
  | "object"        { OBJECT }  
  | "coordinate"    { COORDINATE }
  | "dimension"     { DIMENSION }  
  | "frame"         { FRAME }
  | "type"          { TYP }
  | "is"            { IS }
  | "has"           { HAS }
  | "this"          { THIS }
  | "with"          { WITH }
  | "canon"         { CANON }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "if"            { IF }
  | "else"          { ELSE }
  | "elif"          { ELIF }
  | "for"           { FOR }
  | "skip"          { SKIP }
  | "print"         { PRINT }
  | "int"           { INTTYP }
  | "float"         { FLOATTYP }
  | "string"        { STRINGTYP }
  | "auto"          { AUTOTYP }
  | "bool"          { BOOLTYP }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { TIMES }
  | "/"             { DIV }
  | ".*"            { CTIMES }
  | "as!"           { AS }
  | "in"            { IN }
  | "out"           { OUT }
  | "+="            { PLUSEQ }
  | "-="            { MINUSEQ }
  | "*="            { TIMESEQ }
  | "/="            { DIVEQ }
  | ".*="           { CTIMESEQ }
  | "++"            { INC }
  | "--"            { DEC }
  | "["             { LBRACK }
  | "]"             { RBRACK }
  | "<"             { LWICK }
  | ">"             { RWICK }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "="             { GETS }
  | "=="            { EQ }
  | "<="            { LEQ }
  | ">="            { GEQ }
  | "||"            { OR }
  | "&&"            { AND }
  | "!"             { NOT }
  | ","             { COMMA }
  | ";"             { SEMI }
  | "."             { DOT }
  | ":"             { COLON }
  | "#"             { POUND }
  | "void"          { VOID }
  | "return"        { RETURN }
  | "declare"       { DECLARE }
  | "genType"       { GENTYPE }
  | "const"         { CONST }
  | "attribute"     { ATTRIBUTE }
  | "uniform"       { UNIFORM }
  | "varying"       { VARYING }  
  | "break"
  | "continue"
  | "do"
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
  | "asm"
  | "class" 
  | "union" 
  | "enum"
  | "typedef"
  | "template"
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
  | "struct"        { raise (SyntaxError ("Cannot use reserved GLSL keyword " ^ Lexing.lexeme lexbuf)) }
  | num as num      { NUM (int_of_string num) }
  | s as s          { STRING (String.sub s 1 (String.length s - 2)) }
  | id as id        { ID id }
  | floatval as fl  { FLOAT (float_of_string fl) }
  | eof             { EOF }
  | _ as c  {
            let pos = lexbuf.Lexing.lex_curr_p in
            printf "Error at line %d\n" pos.Lexing.pos_lnum;
            printf "Unrecognized character: [%c]\n" c;
            exit 1
          }

