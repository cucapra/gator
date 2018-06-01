
%{
open Ast
%}

(* Tokens *)

%token <int> INT
%token <string> ID
%token PERIOD
%token PLUS
%token MINUS
%token TIMES
%token CTIMES
%token COLON
%token LTIMES
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token LET
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
%token COMP
%token NORM
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SKIP
%token EOF
%token SEMI

(* Precedences *)

%left PLUS MINUS
%left TIMES
%left AND OR
%nonassoc NOT

(* After declaring associativity and precedence, we need to declare what
   the starting point is for parsing the language.  The following
   declaration says to start with a rule (defined below) named [prog].
   The declaration also says that parsing a [prog] will return an OCaml
   value of type [Ast.expr]. *)

%start <Ast.comm> prog

(* The following %% ends the declarations section of the grammar definition. *)

%%
   
prog:
  | e = tags; EOF { e }
  | c1 = comm; SEMI; c2 = comm; EOF { Comp(c1,c2) }
  ;

tagdecl:
  | TAG; x = ID; IS; e1 = ltyp { TagDecl(x, e1) }

tags:
  | e = tagdecl { e }
  | e1 = tags ; SEMI; e2 = tags { TagComp(e1,e2) }

typ:
  | FLOAT

comm:
  | SKIP { Skip } 
  | t = typ; x = ID; GETS; e1 = aexp { Decl(t, x, e1) }
  | IF; LPAREN; b1 = bexp; RPAREN; THEN; c1 = comm; ELSE; c2 = comm { If(b1,c1,c2) }

aexp:
  | i = INT { Num i }
  | x = ID { Var x }
  | e1 = aexp; PLUS; e2 = aexp { Plus(e1,e2) }
  | e1 = aexp; TIMES; e2 = aexp { Times(e1,e2) }
  | e1 = aexp; MINUS; e2 = aexp { Minus(e1,e2) }

bexp:
  | TRUE { True }
  | FALSE { False }
  | e1 = aexp; EQ; e2 = aexp { Eq(e1,e2) }
  | e1 = aexp; LEQ; e2 = aexp { Leq(e1,e2) }
  | e1 = bexp; OR; e2 = bexp { Or(e1,e2) }
  | e1 = bexp; AND; e2 = bexp { And(e1,e2) }
  | NOT; e1 = bexp;{ Not(e1) }
