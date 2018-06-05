
%{
open Ast
%}

(* Tokens *)

%token EOL 

%token <int> NUM
%token <float> FLOAT
%token <string> ID
%token PLUS
%token MINUS
%token TIMES
%token CTIMES
%token COLON
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token TRANS
%token GETS
%token EQ
%token LEQ
%token AND
%token OR
%token NOT
%token COMMA
%token TAG
%token IS
%token MAT
%token VEC
%token DOT
%token NORM
%token TRUE
%token FALSE
%token IF
%token ELSE
%token SKIP
%token PRINT
%token SEMI
%token INTTYP
%token FLOATTYP 
%token BOOLTYP
%token LBRACE
%token RBRACE

(* Precedences *)
%left PLUS MINUS
%left TIMES
%left CTIMES


%left AND OR
%nonassoc DOT
%nonassoc NORM

%nonassoc NOT

%left TRANS
%left COLON

(* After declaring associativity and precedence, we need to declare what
   the starting point is for parsing the language.  The following
   declaration says to start with a rule (defined below) named [prog].
   The declaration also says that parsing a [prog] will return an OCaml
   value of type [Ast.expr]. *)

%start main
%type <Ast.prog> main


(* The following %% ends the declarations section of the grammar definition. *)

%%
   
main:
  | t = taglst; e = commlst; EOL { Prog(t, e) }
  | e = commlst; EOL {Prog([], e)}
  | t = taglst; EOL {Prog(t, [])}
;

taglst: 
  | t = tag { t::[] }
  | t1 = taglst; t2 = tag { t1@(t2::[])@[] }
; 

tag:
  | TAG; x = ID; IS; e1 = ltyp; SEMI; { TagDecl(x, LTyp(e1)) }
;

commlst:
  | c = comm { c::[] }
  | c1 = comm; c2 = commlst { c1::c2@[] }
;

comm:
  | SKIP; SEMI;{ Skip } 
  | t = typ; x = ID; GETS; e1 = exp; SEMI; { Decl(t, x, e1) }
  | IF; LPAREN; b1 = exp; RPAREN; LBRACE; c1 = commlst; RBRACE; 
    ELSE; LBRACE; c2 = commlst; RBRACE { If(b1,c1,c2) }
  | PRINT; e = exp; SEMI; { Print(e) }
;

typ:
  | a = atyp { ATyp(a) }
  | b = btyp { BTyp(b) }
;

atyp:
  | FLOATTYP  { FloatTyp }
  | INTTYP    { IntTyp }
  | e = ltyp { LTyp(e) }
;

ltyp: 
  | VEC; i = NUM { VecTyp(i) }
  | MAT; i1 = NUM; TIMES; i2 = NUM { MatTyp(i1,i2) }
  | x = ID { TagTyp(x) }
  | x1 = ltyp; TRANS; x2 = ltyp { TransTyp(x1,x2) }
;

btyp:
  | BOOLTYP { BoolTyp }
;

exp:
  | a = aexp { Aexp(a) }
  | b = bexp { Bexp(b) }
;

aval: 
  | i = NUM { Num i }
  | f = FLOAT { Float f }
  | LBRACK; RBRACK {VecLit([])}
  | LBRACK; v = veclit; RBRACK { VecLit(v@[]) }
  | LBRACK; m = matlit; RBRACK { MatLit(m@[]) }
;

veclit:
  | f = FLOAT { f::[] }
  | f = FLOAT; COMMA; v2 = veclit { f::v2@[] }
;

matlit:
  | LBRACK; v = veclit; RBRACK { [v] }
  | LBRACK; RBRACK { [[]] }
  | LBRACK; v = veclit; RBRACK; COMMA; m2 = matlit { [v]@m2@[] }
  | LBRACK; RBRACK; COMMA; m2 = matlit { [[]]@m2@[] }
;

aexp:
  | v = aval { Const v }  
  | x = ID { Var x }
  | LPAREN; a = aexp; RPAREN { a }
  | e = aexp; COLON; t = ltyp { LExp(e, t) }
  | DOT; e1 = aexp; e2 = aexp { Dot(e1, e2) }
  | NORM; e = aexp { Norm(e) } (* Normie *)
  | e1 = aexp; PLUS; e2 = aexp { Plus(e1,e2) }
  | e1 = aexp; TIMES; e2 = aexp { Times(e1,e2) }
  | e1 = aexp; MINUS; e2 = aexp { Minus(e1,e2) }
  | e1 = aexp; CTIMES; e2 = aexp { CTimes(e1,e2) }
;

bexp:
  | TRUE { True }
  | FALSE { False }
  | LPAREN; b = bexp; RPAREN { b }
  | e1 = aexp; EQ; e2 = aexp { Eq(e1,e2) }
  | e1 = aexp; LEQ; e2 = aexp { Leq(e1,e2) }
  | e1 = bexp; OR; e2 = bexp { Or(e1,e2) }
  | e1 = bexp; AND; e2 = bexp { And(e1,e2) }
  | NOT; e1 = bexp;{ Not(e1) }
;

%%