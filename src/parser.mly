
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

%token EOF
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
%token SPACE

(* Precedences *)

%left ID
%left TRANS
%left AS IN
%left AND OR
%left NOT EQ LEQ GEQ LBRACK 
%left LWICK RWICK 
%left LPAREN 

%left PLUS MINUS
%left TIMES DIV CTIMES 
%left DOT

(* After declaring associativity and precedence, we need to declare what
   the starting point is for parsing the language.  The following
   declaration says to start with a rule (defined below) named [prog].
   The declaration also says that parsing a [prog] will return an OCaml
   value of type [TagAst.expr]. *)

%start main
(* The explicit types of some key parser expressions to help with debugging *)
%type <TagAst.prog> main
%type <TagAst.exp> exp
%type <TagAst.comm> comm  
%type <TagAst.term> term 

(* The following %% ends the declarations section of the grammar definition. *)

%%

main:
  | t = aterm+; EOF;
    { t }

let aterm ==
  | t = term;
    { (t, $startpos) }

let term == 
  | TAG; x = ID; IS; t = typ; SEMI; 
    { TagDecl([], x, [], t) }
  | TAG; x = ID; pt = par_decl; IS; t = typ; SEMI; 
    { TagDecl([], x, pt, t) }
  | SPACE; x = ID; IS; t = typ; SEMI; 
    { TagDecl([Space], x, [], t) }
  | SPACE; x = ID; pt = par_decl; IS; t = typ; SEMI; 
    { TagDecl([Space], x, pt, t) }
  | DECLARE; d = decl_extern; SEMI; 
    <ExternDecl>
  | m = modification*; sq = storage_qual; t = typ;
    x = ID; v = preceded(GETS, value)?; SEMI; 
    <GlobalVar>
  | f = fn_decl; LBRACE; cl = list(acomm); RBRACE;
    <Fn>

let modification ==
  | CANON;
    { Canon } 

let decl_extern == 
  | m = modification*; t = typ; x = ID;
    { ExternVar(m, t, (Var x, $startpos)) }
  | m = modification*; t = typ; x = ID; (p1, p2, _) = fn_params; 
    { ExternFn((m, x, (p1, t, p2))) }

let params == 
  | m = modification*; t = typ; x = ID;
    <>

let parameterization ==
  | BACKTICK; t = ID;
    { (t, AnyTyp) }
  | BACKTICK; t = ID; COLON; c = constrain;
    <>

let par_decl == 
  | LWICK; pt = separated_list(COMMA, parameterization); RWICK;
    <>

let fn_decl ==
  | m = modification*; t = typ; x = ID; (p1, p2, _) = fn_params;
    { (m, x, (p1, t, p2)) }

let fn_params ==
  | LPAREN; p = separated_list(COMMA, params); RPAREN;
    { ([], p, $startpos) }
  | pt = par_decl; LPAREN; p = separated_list(COMMA, params); RPAREN;
    { (pt, p, $startpos) }

let acomm ==
  | c = comm;
    { (c, $startpos) }

let comm ==
  | c = comm_block;
    <>
  | c = comm_element; SEMI;
    <>

let if_block(delim) ==
  | delim; LPAREN; b = aexp; RPAREN; LBRACE; c = acomm*; RBRACE;
    <>

let comm_block ==
  | i = if_block(IF); el = if_block(ELIF)*; 
    e = option(preceded(ELSE, delimited(LBRACE, list(acomm), RBRACE)));
    <If>
  | FOR; LPAREN; c1 = acomm_element; SEMI; b = aexp; SEMI; c2 = acomm_element; RPAREN;
    LBRACE; cl = acomm*; RBRACE; 
    <For>

let acomm_element ==
  | ce = comm_element;
    { (ce, $startpos) }

let comm_element == 
  | SKIP;
    { Skip }
  /* | m = modification*; t = typ; x = ID; GETS; e1 = exp; 
    < Decl > */
  | t = typ; x = ID; GETS; e = aexp;
    { Decl([], t, x, e) }
  | m = modification+; t = typ; x = ID; GETS; e = aexp;
    { Decl(m, t, x, e) }
  | x = ID; GETS; e1 = aexp; 
    < Assign >
  | x = ID; PLUSEQ; e1 = aexp; 
    { AssignOp(x, Plus, e1) }
  | x = ID; MINUSEQ; e1 = aexp; 
    { AssignOp(x, Minus, e1) }
  | x = ID; TIMESEQ; e1 = aexp; 
    { AssignOp(x, Times, e1) }
  | x = ID; DIVEQ; e1 = aexp; 
    { AssignOp(x, Div, e1) }
  | x = ID; CTIMESEQ; e1 = aexp; 
    { AssignOp(x, CTimes, e1) }
  | PRINT; e = aexp; 
    < Print >
  | RETURN; e = aexp?;
    < Return >
  | x = ID; INC; 
    < Inc >
  | x = ID; DEC; 
    < Dec >
  | t = typ; LPAREN; a = separated_list(COMMA, aexp); RPAREN; 
    { FnCall(t, [], a) }
  | t = typ; LWICK; p = separated_list(COMMA, typ); RWICK; 
    LPAREN; a = separated_list(COMMA, aexp); RPAREN; 
    <FnCall>

let constrain == 
  | SPACE;
    { GenSpaceTyp }
  | VEC;
    { GenVecTyp }
  | MAT;
    { GenMatTyp }
  | GENTYPE; 
    { GenTyp }
  | t = typ;
    <TypConstraint>

dexp: 
  | d1 = dexp; PLUS; d2 = dexp;
    { DimBinop(Plus, d1, d2) }
  | d1 = dexp; MINUS; d2 = dexp; 
    { DimBinop(Plus, d1, d2) }
  | n = NUM;
    { DimNum n }
  | x = ID;
    { DimVar x }
  
typ:
  | AUTOTYP;
    { AutoTyp }
  | BACKTICK; e = ID;
    { AbsTyp(e) }
  | BOOLTYP;
    { BoolTyp }
  | FLOATTYP;
    { FloatTyp }
  | INTTYP;
    { IntTyp }
  | t = typ; LBRACK; n = NUM; RBRACK;
    { ArrTyp(t, ConstInt n) }
  | t = typ; LBRACK; s = ID; RBRACK;
    { ArrTyp(t, ConstVar s) }
  | m = MATTYP;
    { let len = String.length m in
      let dim = String.sub m 3 (len-3) in
      let dim_lst = Str.split_delim (regexp "x") dim in
      TransTyp (UntaggedVecTyp (int_of_string(List.nth dim_lst 1)),
      (UntaggedVecTyp (int_of_string(List.nth dim_lst 0))))}
  | t1 = typ; TRANS; t2 = typ;
    { TransTyp(t1,t2) }
  | t = typ; LWICK; tl = separated_list(COMMA, typ); RWICK;
    { ParTyp(t, tl) }
  | VEC; LWICK; d = dexp; RWICK;
    { TopVecTyp d } 
  | x = ID;
    { if (Str.string_match vec x 0) then (
      let len = String.length x in 
      let dim = int_of_string (String.sub x 3 (len-3)) in
      UntaggedVecTyp dim
      ) else
      if (Str.string_match mat x 0) then (
      let len = String.length x in 
      let dim = int_of_string (String.sub x 3 (len-3)) in
      TransTyp (UntaggedVecTyp dim, UntaggedVecTyp dim)
      ) 
      else (VarTyp x) }
  | SAMPLERCUBE;
    { SamplerCubeTyp }
  | s = SAMPLER;
    { let len = String.length s in
      let dim = String.sub s 7 (len-7) in 
      let dim_lst = Str.split_delim (regexp "D") dim in
      SamplerTyp (int_of_string(List.nth dim_lst 0)) }
  | VOID;
    { UnitTyp }

let storage_qual ==
  | IN;
    { InQual }
  | OUT;
    { Out }
  | CONST;
    { Const }
  | ATTRIBUTE;
    { Attribute }
  | UNIFORM;
    { Uniform }
  | VARYING;
    { Varying }

let value ==
  | b = bool;
    { Bool b }
  | i = NUM;
    { Num i }
  | f = FLOAT;
    { Float f }

let bool ==
  | TRUE;
    { true }
  | FALSE;
    { false }

let aexp ==
  | e = exp;
    { (e, $startpos) }

exp:
  | LPAREN; a = exp; RPAREN;
    { a }
  | v = value;
    { Val v }
  | x = ID;
    { Var x }
  | x = ID; LPAREN; a = separated_list(COMMA, aexp); RPAREN;
    { FnInv(x, [], a) }
  | e1 = aexp; LWICK; e2 = aexp;
    { Binop(Lt,e1,e2) }
  | e1 = aexp; RWICK; e2 = aexp;
    { Binop(Gt,e1,e2) }
  | x = ID; LWICK; t = separated_list(COMMA, typ); RWICK; 
    LPAREN; a = separated_list(COMMA, aexp); RPAREN;
    { FnInv(x, t, a) }
  | LBRACK; e = separated_list(COMMA, aexp); RBRACK;
    { Arr(e) }
  | e1 = aexp; PLUS; e2 = aexp;
    { Binop(Plus,e1,e2) }
  | e1 = aexp; TIMES; e2 = aexp;
    { Binop(Times,e1,e2) }
  | e1 = aexp; MINUS; e2 = aexp;
    { Binop(Minus,e1,e2) }
  | e1 = aexp; DIV; e2 = aexp;
    { Binop(Div,e1,e2) }
  | e1 = aexp; CTIMES; e2 = aexp;
    { Binop(CTimes,e1,e2) }
  | e = aexp; AS; t = typ;
    { As(e, t) }
  | e = aexp; IN; t = typ;
    { In(e, t) }
  | MINUS; e1 = aexp;
    { Unop(Neg,e1) }
  | NOT; e1 = aexp;
    { Unop(Not,e1) }
  | e1 = aexp; EQ; e2 = aexp;
    { Binop(Eq,e1,e2) }
  | e1 = aexp; LEQ; e2 = aexp;
    { Binop(Leq,e1,e2) }
  | e1 = aexp; GEQ; e2 = aexp;
    { Binop(Geq,e1,e2) }
  | e1 = aexp; OR; e2 = aexp;
    { Binop(Or,e1,e2) }
  | e1 = aexp; AND; e2 = aexp;
    { Binop(And,e1,e2) }
  | e1 = aexp; DOT; s = ID;
    { Unop(Swizzle s,e1) }
  | x = ID; LBRACK; e2 = aexp; RBRACK;
    { Binop(Index, (Var(x), $startpos), e2) }

%%