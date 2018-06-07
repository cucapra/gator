
%{
open Ast
%}

(* Tokens *)

%token EOL 
%token MAT
%token VEC
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
%token DIM

(* Precedences *)

%right PRINT
%left TIMES CTIMES 
%left PLUS MINUS

%left AND OR
%left NOT EQ LEQ
%nonassoc DOT

%nonassoc NORM
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
  | BOOLTYP { BTyp }
;

atyp:
  | FLOATTYP  { FloatTyp }
  | INTTYP    { IntTyp }
  | e = ltyp { LTyp(e) }
;

ltyp: 
  | VEC; i = NUM { VecTyp(i) }
  | MAT; i1 = NUM; DIM; i2 = NUM { MatTyp(i1,i2) }
  | x = ID { TagTyp(x) }
  | x1 = ltyp; TRANS; x2 = ltyp { TransTyp(x1,x2) }
;

aval: 
  | i = NUM { Num i }
  | f = FLOAT { Float f }
  | LBRACK; RBRACK; COLON; t = ltyp{VecLit([], t)}
  | LBRACK; v = veclit; RBRACK; COLON; t = ltyp{ VecLit(v@[], t) }
  | LBRACK; m = matlit; RBRACK; COLON; t = ltyp { MatLit(m@[], t) }
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

bool:
  | TRUE { true }
  | FALSE { false }
;

exp:
  | LPAREN; a = exp; RPAREN { a }
  | a = aval { Aval a }
  | b = bool { Bool b }
  | x = ID { Var x }
  | DOT; e1 = exp; e2 = exp { Dot(e1, e2) }
  | NORM; e = exp { Norm(e) } (* Normie *)
  | e1 = exp; PLUS; e2 = exp { Plus(e1,e2) }
  | e1 = exp; TIMES; e2 = exp { Times(e1,e2) }
  | e1 = exp; MINUS; e2 = exp { Minus(e1,e2) }
  | e1 = exp; CTIMES; e2 = exp { CTimes(e1,e2) }
  | NOT; e1 = exp;{ Not(e1) }
  | e1 = exp; EQ; e2 = exp { Eq(e1,e2) }
  | e1 = exp; LEQ; e2 = exp { Leq(e1,e2) }
  | e1 = exp; OR; e2 = exp { Or(e1,e2) }
  | e1 = exp; AND; e2 = exp { And(e1,e2) }
;

%%