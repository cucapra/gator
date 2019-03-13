
%{
open CoreAst
open TagAst
open Str

exception ParseException of string

(* let matr = Str.regexp "mat\\([0-9]+\\)x\\([0-9]+\\)" *)
let vec = Str.regexp "vec\\([0-9]+\\)"
let mat = Str.regexp "mat\\([0-9]+\\)"

%}

(* Tokens *)

%token EOL  
%token <int> NUM
%token <float> FLOAT
%token <string> MATTYP
%token <string> ID
%token SAMPLERCUBE
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
%token IN
%token OUT
%token AS
%token INC
%token DEC
%token PLUSEQ
%token MINUSEQ
%token TIMESEQ
%token DIVEQ
%token CTIMESEQ
%token EQ
%token LEQ
%token GEQ
%token AND
%token OR
%token NOT
%token COMMA
%token DOT
%token TAG
%token COORD
%token CANON
%token IS
%token TRUE
%token FALSE
%token IF
%token ELSE
%token ELIF
%token FOR
%token SKIP
%token PRINT
%token SEMI
%token INTTYP
%token FLOATTYP 
%token BOOLTYP
%token AUTOTYP
%token LBRACE
%token RBRACE
%token RETURN
%token VOID
%token DECLARE
%token COLON
%token BACKTICK
%token GENTYPE
%token LWICK
%token RWICK
%token VEC
%token MAT
%token CONST
%token ATTRIBUTE
%token UNIFORM
%token VARYING

(* Precedences *)

%left ID
%left TRANS
%left AS IN
%left AND OR
%left NOT EQ LEQ GEQ LWICK RWICK LBRACK

%left PLUS MINUS
%left TIMES DIV CTIMES 
%left DOT

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
  | t = taglst; d = declarelst; g = globalvarlst; e = fnlst; EOL 
      { (d, t, g, e) }
  | t = taglst; d = declarelst; g = globalvarlst; EOL 
      { (d, t, g, []) }
  | t = taglst; d = declarelst; e = fnlst; EOL 
      { (d, t, [], e) }
  | d = declarelst; g = globalvarlst; e = fnlst; EOL 
      { (d, [], g, e) }
  | t = taglst; g = globalvarlst; e = fnlst; EOL 
      { ([], t, g, e) }
  | t = taglst; d = declarelst; EOL              
      { (d, t, [], []) }
  | d = declarelst; e = fnlst; EOL
      { (d, [], [], e)}
  | g = globalvarlst; e = fnlst; EOL
      { ([], [], g, e)}
  | d = declarelst; g = globalvarlst; EOL
      { (d, [], g, [])}
  | t = taglst; e = fnlst; EOL 
      { ([], t, [], e) }
  | t = taglst; g = globalvarlst; EOL 
      { ([], t, g, []) }
  | d = declarelst; EOL
      { (d, [], [], [])}
  | t = taglst; EOL              
      { ([], t, [], []) }
  | g = globalvarlst; EOL              
      { ([], [], g, []) }
  | e = fnlst; EOL             
      { ([], [], [], e) }
;

modification:
  | CANON 
      { Canon }
  | COORD 
      { Coord }

declarelst: 
  | DECLARE; d = decl_extern; SEMI;
      { d::[] }
  | DECLARE; d = decl_extern; SEMI; dl = declarelst
      { d::dl }

decl_extern:
  | t = typ; x = ID; p = fn_params; 
      { ExternFn((None, x, (fst p, t, snd p))) }
  | t = typ; x = ID;
      { ExternVar(t, Var x) }
  | m = modification; t = typ; x = ID; p = fn_params; 
      { ExternFn((Some m, x, (fst p, t, snd p))) }

taglst: 
  | t = tag               
      { t::[] }
  | t1 = taglst; t2 = tag 
      { t1@(t2::[]) }
; 

tag:
  | TAG; x = ID; IS; t = typ; SEMI; 
      { (None, x, [], t) }
  | TAG; x = ID; LWICK; pt = paramet_decl; RWICK; IS; t = typ; SEMI; 
      { (None, x, pt, t) }
  | TAG; m = modification; x = ID; IS; t = typ; SEMI; 
      { (Some m, x, [], t) }
  | TAG; m = modification; x = ID; LWICK; pt = paramet_decl; RWICK; IS; t = typ; SEMI; 
      { (Some m, x, pt, t) }
;

fnlst: 
  | x = fn_decl; LBRACE; RBRACE;
      { (x, [])::[] }
  | x = fn_decl; LBRACE; RBRACE; fl = fnlst;
      { (x, [])::fl }
  | x = fn_decl; LBRACE; c1 = commlst; RBRACE;
      { (x, c1)::[] }
  | x = fn_decl; LBRACE; c1 = commlst; RBRACE; fl = fnlst;
      { (x, c1)::fl }

commlst:
  | c = comm 
      { c::[] }
  | c1 = comm; c2 = commlst 
      { c1::c2 }
;

globalvar:
  | sq = storagequal; t = typ; x = ID; SEMI 
      { (x, sq, t, None) }
  | sq = storagequal; t = typ; x = ID; GETS; v = value; SEMI 
      { (x, sq, t, Some v) }
;

globalvarlst:
  | gv = globalvar
      { gv::[] }
  | gv1 = globalvar; gv2 = globalvarlst
      { gv1::gv2 }
;


params: 
  | t = typ; x = ID
      { (x, t)::[] }
  | t = typ; x = ID; COMMA; p = params
      { (x, t)::p }
;

parameterization:
  | BACKTICK; t = ID;
      { (t, None, AnyTyp) }
  | BACKTICK; t = ID; COLON; c = constrain;
      { (t, None, c) }
  | BACKTICK; t = ID; COLON; m = modification; c = constrain;
      { (t, Some m, c) }

paramet_decl:
  | p = parameterization;
      { [p] }
  | p = parameterization; COMMA; pl = paramet_decl;
      { p::pl }

fn_decl:
  | t = typ; x = ID; p = fn_params;
      { (None, x, (fst p, t, snd p)) }
  | m = modification; t = typ; x = ID; p = fn_params;
      { (Some m, x, (fst p, t, snd p)) }
;

fn_params:
  | LPAREN; RPAREN;
      { ([], []) }
  | LPAREN; p = params ; RPAREN;
      { (p, []) }
  | LWICK; pt = paramet_decl; RWICK; LPAREN; RPAREN;
      { ([], pt) }
  | LWICK; pt = paramet_decl; RWICK; LPAREN; p = params ; RPAREN;
      { (p, pt) }

elif:
  | ELIF; LPAREN; b = exp; RPAREN; LBRACE; c = commlst; RBRACE;
    { (b, c) }

eliflst:
  | e = elif
      { e::[] }
  | e = elif; a = eliflst
      { e::a@[] }
;

comm:
  | c = comm_block;
    { c }
  | c = comm_element; SEMI;
    { c }

comm_block:
  | IF; LPAREN; b1 = exp; RPAREN; LBRACE; c1 = commlst; RBRACE;   
      { If((b1, c1), [], None) }
  | IF; LPAREN; b1 = exp; RPAREN; LBRACE; c1 = commlst; RBRACE; 
    ELSE; LBRACE; c2 = commlst; RBRACE;
      { If((b1, c1), [], Some c2) }
  | IF; LPAREN; b1 = exp; RPAREN; LBRACE; c1 = commlst; RBRACE; el = eliflst;
      { If((b1, c1), el, None) }
  | IF; LPAREN; b1 = exp; RPAREN; LBRACE; c1 = commlst; RBRACE; el = eliflst; 
    ELSE; LBRACE; c2 = commlst; RBRACE;
      { If((b1, c1), el, Some c2) }
  | FOR; LPAREN; c1 = comm_element; SEMI; b = exp; SEMI; c2 = comm_element; RPAREN;
    LBRACE; cl = commlst; RBRACE; 
      { For(c1, b, c2, cl) }

comm_element:
  | SKIP;                            
      { Skip }
  | t = typ; x = ID; GETS; e1 = exp; 
      { Decl(t, None, x, e1) }
  | t1= typ; x = ID; LWICK; t2 = typ; RWICK; GETS; e1 = exp; 
      { Decl(t1, Some t2, x, e1) }
  | x = ID; GETS; e1 = exp; 
      { Assign(x, e1) }
  | x = ID; PLUSEQ; e1 = exp; 
      { AssignOp(x, Plus, e1) }
  | x = ID; MINUSEQ; e1 = exp; 
      { AssignOp(x, Minus, e1) }
  | x = ID; TIMESEQ; e1 = exp; 
      { AssignOp(x, Times, e1) }
  | x = ID; DIVEQ; e1 = exp; 
      { AssignOp(x, Div, e1) }
  | x = ID; CTIMESEQ; e1 = exp; 
      { AssignOp(x, CTimes, e1) }
  | PRINT; e = exp; 
      { Print(e) }
  | RETURN; e = exp; 
      { Return(Some e) }
  | RETURN; 
      { Return(None) }
  | x = ID; INC; 
      { Inc(x) }
  | x = ID; DEC; 
      { Dec(x) }
  | x = ID; LPAREN; RPAREN; 
      { FnCall(x, [], []) }
  | x = ID; LPAREN; a = arglst; RPAREN; 
      { FnCall(x, a, []) }
  | x = ID; LWICK; p = typlst; RWICK; LPAREN; RPAREN; 
      { FnCall(x, [], p) }
  | x = ID; LWICK; p = typlst; RWICK; LPAREN; a = arglst; RPAREN; 
      { FnCall(x, a, p) }
; 

constrain:
  | VEC 
      { GenVecTyp }
  | MAT 
      { GenMatTyp }
  | GENTYPE 
      { GenTyp }
  | t = typ 
      { TypConstraint t }

typ:
  | AUTOTYP 
      { AutoTyp }    
  | BACKTICK; e = ID
      { AbsTyp(e) }
  | BOOLTYP                         
      { BoolTyp }
  | FLOATTYP                        
      { FloatTyp }
  | INTTYP                          
      { IntTyp }
  | t = typ; LBRACK; n = NUM; RBRACK; 
      { ArrTyp(t, ConstInt n) }
  | t = typ; LBRACK; s = ID; RBRACK; 
      { ArrTyp(t, ConstVar s) }
  | m = MATTYP                      
      { let len = String.length m in
        let dim = String.sub m 3 (len-3) in
        let dim_lst = Str.split_delim (regexp "x") dim in
        TransTyp (TopVecTyp (int_of_string(List.nth dim_lst 1)),
        (TopVecTyp (int_of_string(List.nth dim_lst 0))))}
  | x1 = typ; TRANS; x2 = typ 
      { TransTyp(x1,x2) }
  | x = typ; LWICK; tl = typlst; RWICK; 
      { ParTyp(x,tl) }
  | x = ID 
      { if (Str.string_match vec x 0) then (
        let len = String.length x in 
        let dim = int_of_string (String.sub x 3 (len-3)) in
        TopVecTyp dim
        ) else
        if (Str.string_match mat x 0) then (
        let len = String.length x in 
        let dim = int_of_string (String.sub x 3 (len-3)) in
        TransTyp (TopVecTyp dim, TopVecTyp dim)
        ) 
        else (VarTyp x) }
  | SAMPLERCUBE 
      { SamplerCubeTyp }
  | s = SAMPLER 
      { let len = String.length s in
        let dim = String.sub s 7 (len-7) in 
        let dim_lst = Str.split_delim (regexp "D") dim in
        SamplerTyp (int_of_string(List.nth dim_lst 0)) }
  | VOID 
      { UnitTyp }
;

storagequal:
  | IN 
      { In }
  | OUT 
      { Out }
  | CONST 
      { Const }
  | ATTRIBUTE 
      { Attribute }
  | UNIFORM 
      { Uniform }
  | VARYING 
      { Varying }
;

arr:
  | e = exp 
      { e::[] }
  | e = exp; COMMA; a = arr
      { e::a@[] }
;

value:
  | b = bool                    
      { Bool b }
  | i = NUM                     
      { Num i }
  | f = FLOAT                   
      { Float f }
;

bool:
  | TRUE  
      { true }
  | FALSE 
      { false }
;

arglst:
  | e = exp 
     { e::[] }
  | e = exp; COMMA; a = arglst;
     { e::a@[] }
;
  
typlst: 
  | t = typ 
      { t::[] }
  | t = typ; COMMA; tl = typlst
      { t::tl } 

exp:
  | LPAREN; a = exp; RPAREN    
      { a }
  | v = value                  
      { Val v }
  | x = ID                     
      { Var x }
  | x = ID; LPAREN; RPAREN;
      { FnInv(x, [], []) }
  | x = ID; LPAREN; a = arglst; RPAREN;
      { FnInv(x, a, []) }
  | e1 = exp; LWICK; e2 = exp 
      { Binop(Lt,e1,e2) }
  | e1 = exp; RWICK; e2 = exp 
      { Binop(Gt,e1,e2) }
  | x = ID; LWICK; t = typlst; RWICK; LPAREN; a = arglst; RPAREN;
      { FnInv(x, a, t) }
  | x = ID; LWICK; t = typlst; RWICK; LPAREN; RPAREN;
      { FnInv(x, [], t) }
  | LBRACK; RBRACK;
    { Arr [] }
  | LBRACK; e = arr; RBRACK;
    { Arr e }
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
  | e = exp; AS; t = typ
      { As(e, t) }
  | e = exp; IN; t = typ
      { In(e, t) }
  | MINUS; e1 = exp;
      { Unop(Neg,e1) }
  | NOT; e1 = exp;             
      { Unop(Not,e1) }
  | e1 = exp; EQ; e2 = exp 
      { Binop(Eq,e1,e2) }
  | e1 = exp; LEQ; e2 = exp 
      { Binop(Leq,e1,e2) }
  | e1 = exp; GEQ; e2 = exp 
      { Binop(Geq,e1,e2) }
  | e1 = exp; OR; e2 = exp 
      { Binop(Or,e1,e2) }
  | e1 = exp; AND; e2 = exp 
      { Binop(And,e1,e2) }
  | e1 = exp; DOT; s = ID;
      { Unop(Swizzle s,e1) }
  | x = ID; LBRACK; e2 = exp; RBRACK;
      { Binop(Index,Var(x),e2) }
;

%%