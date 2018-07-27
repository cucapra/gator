
%{
open CoreAst
open TagAst
open Str

exception ParseException of string

(* let matr = Str.regexp "mat\\([0-9]+\\)x\\([0-9]+\\)" *)
let vec = Str.regexp "vec\\([0-9]+\\)"

%}

(* Tokens *)

%token EOL 
%token <int> NUM
%token <float> FLOAT
%token <string> MATTYP
%token <string> ID
%token <string> SAMPLER
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token CTIMES
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

(* Precedences *)

%left AND OR
%left NOT EQ LEQ

%left PLUS MINUS
%left TIMES DIV CTIMES 

%nonassoc DOT

%nonassoc NORM
(*%left TRANS*)

(* After declaring associativity and precedence, we need to declare what
   the starting point is for parsing the language.  The following
   declaration says to start with a rule (defined below) named [prog].
   The declaration also says that parsing a [prog] will return an OCaml
   value of type [TagAst.expr]. *)

%start main
%type <TagAst.prog> main

(* The following %% ends the declarations section of the grammar definition. *)

%%
   
main:
  | t = taglst; e = fnlst; EOL 
      { Prog(t, e) }
  | e = fnlst; EOL             
      { Prog([], e) }
  | t = taglst; EOL              
      { Prog(t, []) }
;


taglst: 
  | t = tag               
      { t::[] }
  | t1 = taglst; t2 = tag 
      { t1@(t2::[])@[] }
; 

tag:
  | TAG; x = ID; IS; e1 = tagtyp; SEMI; 
      { (x, TagTyp(e1)) }
;

fnlst: 
  | x = fn_decl; LBRACE; c1 = commlst; RBRACE;
      { Fn(x, c1)::[] }
  | x = fn_decl; LBRACE; c1 = commlst; RBRACE; fl = fnlst;
      { Fn(x, c1)::fl@[] }


commlst:
  | c = comm                
      { c::[] }
  | c1 = comm; c2 = commlst 
      { c1::c2@[] }
;

params: 
  | t = typ; x = ID
      { (x, t)::[] }
  | t = typ; x = ID; p = params
      { (x, t)::p@[] }
;

fn_decl:
  | t = typ; x = ID; LPAREN; p = params ; RPAREN;
      { (x, p, t) }
;

comm:
  | SKIP; SEMI;                            
      { Skip }
  | t = typ; x = ID; GETS; e1 = exp; SEMI; 
      { if (Str.string_match vec x 0) then (
        raise (ParseException "invalid id specified for variable declaration")
        ) else Decl(t, x, e1) }
  | x = ID; GETS; e1 = exp; SEMI;          
      { if (Str.string_match vec x 0) then (
        raise (ParseException "invalid id specified for variable declaration")
        ) else Assign(x, e1) }
  | IF; LPAREN; b1 = exp; RPAREN; LBRACE; c1 = commlst; RBRACE; 
    ELSE; LBRACE; c2 = commlst; RBRACE     
      { If(b1, c1, c2) }
  | PRINT; e = exp; SEMI;                  
      { Print(e) }
;

typ:
  | BOOLTYP                         
      { BoolTyp }
  | FLOATTYP                        
      { FloatTyp }
  | INTTYP                          
      { IntTyp }
  | m = MATTYP                      
      { let len = String.length m in
        let dim = String.sub m 3 (len-3) in
        let dim_lst = Str.split_delim (regexp "x") dim in
        TransTyp (TopTyp (int_of_string(List.nth dim_lst 1)),
        TopTyp (int_of_string(List.nth dim_lst 0)))}
  | x1 = tagtyp; TRANS; x2 = tagtyp 
      { TransTyp(x1,x2) }
  | e = tagtyp                      
      { TagTyp(e) }
  | s = SAMPLER                     
      { let len = String.length s in
        let dim = String.sub s 7 (len-7) in 
        let dim_lst = Str.split_delim (regexp "D") dim in
        SamplerTyp (int_of_string(List.nth dim_lst 0)) }
;

tagtyp:
  | x = ID 
      { if (Str.string_match vec x 0) then (
        let len = String.length x in 
        let dim = String.sub x 3 (len-3)in
        TopTyp (int_of_string(dim))
        ) else (VarTyp x) }
;

value:
  | b = bool                    
      { Bool b }
  | i = NUM                     
      { Num i }
  | f = FLOAT                   
      { Float f }
  | LBRACK; RBRACK              
      { VecLit([]) }
  | LBRACK; v = veclit; RBRACK; 
      { VecLit(v@[]) }
  | LBRACK; m = matlit; RBRACK; 
      { MatLit(m@[]) }
;

veclit:
  | f = FLOAT                     
      { f::[] }
  | f = FLOAT; COMMA; v2 = veclit 
      { f::v2@[] }
;

matlit:
  | LBRACK; v = veclit; RBRACK                     
      { [v] }
  | LBRACK; RBRACK                                 
      { [[]] }
  | LBRACK; v = veclit; RBRACK; COMMA; m2 = matlit 
      { [v]@m2@[] }
  | LBRACK; RBRACK; COMMA; m2 = matlit             
      { [[]]@m2@[] }
;

bool:
  | TRUE  
      { true }
  | FALSE 
      { false }
;

exp:
  | LPAREN; a = exp; RPAREN    
      { a }
  | v = value                  
      { Val v }
  | x = ID                     
      { Var x }
  | DOT; e1 = exp; e2 = exp    
      { Binop(Dot,e1, e2) }
  | NORM; e = exp              
      { Unop(Norm,e) } (* Normie *)
  | e1 = exp; PLUS; e2 = exp   
      { Binop(Plus,e1,e2) }
  | e1 = exp; TIMES; e2 = exp  
      { Binop(Times,e1,e2) }
  | e1 = exp; MINUS; e2 = exp  
      { Binop(Minus,e1,e2) }
  | e1 = exp; DIV; e2 = exp    
      { Binop(Div,e1,e2) }
  | e1 = exp; CTIMES; e2 = exp 
      { Binop(CTimes,e1,e2) }
  | NOT; e1 = exp;             
      { Unop(Not,e1) }
  | e1 = exp; EQ; e2 = exp      
      { Binop(Eq,e1,e2) }
  | e1 = exp; LEQ; e2 = exp    
      { Binop(Leq,e1,e2) }
  | e1 = exp; OR; e2 = exp    
      { Binop(Or,e1,e2) }
  | e1 = exp; AND; e2 = exp    
      { Binop(And,e1,e2) }
;

%%
