
%{
open CoreAst
open GatorAst
open Str

exception ParseException of string
%}

(* Tokens *)

%token EOF
%token <int> NUM
%token <float> FLOAT
%token <string> ID
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token CTIMES
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
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
%token PROTOTYPE
%token OBJECT
%token COORDINATE
%token DIMENSION
%token FRAME
%token TYP
%token CANON
%token IS
%token WITH
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
%left AS IN
%left AND OR
%left NOT EQ LEQ GEQ LBRACK 
%left LWICK RWICK 

%left PLUS MINUS
%left TIMES DIV CTIMES 
%left DOT

(* After declaring associativity and precedence, we need to declare what
   the starting point is for parsing the language.  The following
   declaration says to start with a rule (defined below) named [prog].
   The declaration also says that parsing a [prog] will return an OCaml
   value of type [GatorAst.expr]. *)

%start main
(* The explicit types of some key parser expressions to help with debugging *)
%type <GatorAst.prog> main
%type <GatorAst.exp> exp
%type <GatorAst.comm> comm  
%type <GatorAst.term> term 

(* The following %% ends the declarations section of the grammar definition. *)

%%

main:
  | t = aterm+; EOF;
    { t }

let terminated_list(X, Y) ==
  | y = Y;
    { ([], y) }
  | x = X+; y = Y;
    <>

let combined(A, B) == a = A; b = B; { (a, b) }
let fst(T) == (a, b) = T; { a }
let snd(T) == (a, b) = T; { b }

let aterm ==
  | t = term;
    { (t, $startpos) }

let term == 
  | PROTOTYPE; x = ID; LBRACE; p = list(prototype_element); RBRACE;
    <Prototype>
  | COORDINATE; x = ID; COLON; p = ID;
    LBRACE; d = dimension; SEMI; c = list(coordinate_element); RBRACE;
    <Coordinate>
  | FRAME; x = ID; d = dimension; s = snd(combined(IS, ID))?; SEMI;
    <FrameDecl>
  | TYP; x = ID; pm = parameterization(constrained); IS; t = typ; SEMI;
    <TypDecl>
  | DECLARE; d = decl_extern; SEMI; 
    <ExternDecl>
  | m = modification*; sq = storage_qual; t = typ;
    x = ID; v = preceded(GETS, aexp)?; SEMI; 
    <GlobalVar>
  | f = gen_fn(ID);
    <Fn>

let prototype_element ==
  | OBJECT; x = ID; p = parameterization(constrained); SEMI;
    <ProtoObjectDecl>
  | f = gen_fn_decl(ID);
    <ProtoFnDecl>
  | f = gen_fn_decl(binop);
    <ProtoBinopDecl>

let modification ==
  | WITH; w = with_statement+;
    <With>
  | CANON;
    { Canon }

let with_statement ==
  | FRAME; d = delimited(LPAREN, NUM, RPAREN); r = separated_list(COMMA, ID); COLON;
    <>

let dimension ==
  | DIMENSION; n = NUM;
    <>

let coordinate_element ==
  | OBJECT; x = ID; p = parameterization(constrained); IS; t = typ;
    <CoordObjectAssign>
  | f = gen_fn(ID);
    <CoordFnDecl>
  | f = gen_fn(binop);
    <CoordBinopDecl>

let decl_extern == 
  | (m, t) = terminated_list(modification, typ); x = ID;
    { ExternVar(m, t, (Var x, $startpos)) }
  | f = gen_fn_decl(ID);
    <ExternFn>

let constrained ==
  | t = ID;
    { (t, AnyTyp) }
  | t = ID; COLON; c = constrain; <>

let parameterization(T) ==
  | { [] }
  | pt = delimited(LWICK, separated_list(COMMA, T), RWICK); <>

let parameter == 
  | m = modification*; t = typ; x = ID;
    <>

let parameters ==
  | x = delimited(LPAREN, separated_list(COMMA, parameter), RPAREN); <>

let arguments ==
  | x = delimited(LPAREN, separated_list(COMMA, aexp), RPAREN); <>

let gen_fn_decl(A) ==
  | (m, t) = terminated_list(modification, typ); a = A; pm = parameterization(constrained); args = parameters;
    { (m, a, (pm, t, args)) }

let gen_fn(A) ==
  | f = gen_fn_decl(A); LBRACE; cl = list(acomm); RBRACE; <>

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

let assignop ==
  | PLUSEQ;
    { Plus }
  | MINUSEQ;
    { Minus }
  | TIMESEQ;
    { Times }
  | DIVEQ;
    { Div }
  | CTIMESEQ;
    { CTimes }

let comm_element == 
  | SKIP;
    { Skip }
  | (m, t) = terminated_list(modification, typ); x = ID; GETS; e = aexp; 
    < Decl >
  | x = ID; GETS; e = aexp; 
    < Assign >
  | x = ID; a = assignop; e = aexp; 
    < AssignOp >
  | PRINT; e = aexp; 
    < Print >
  | RETURN; e = aexp?;
    < Return >
  | x = ID; INC; 
    < Inc >
  | x = ID; DEC; 
    < Dec >
  | x = ID; p = parameterization(typ); a = arguments;
    < FnCall >

let constrain == 
  | VEC;
    { GenArrTyp(TypConstraint(FloatTyp)) }
  | MAT;
    { GenArrTyp(GenArrTyp(TypConstraint(FloatTyp))) }
  | GENTYPE; 
    { GenTyp }
  | t = typ;
    <TypConstraint>

let dexp :=
  | d1 = dexp; b = binop; d2 = dexp;
    <DimBinop>
  | n = NUM;
    <DimNum>
  | x = ID;
    <DimVar>

let typ :=
  | AUTOTYP;
    { AutoTyp }
  | BOOLTYP;
    { BoolTyp }
  | FLOATTYP;
    { FloatTyp }
  | INTTYP;
    { IntTyp }
  | VOID;
    { UnitTyp }
  | t = typ; LBRACK; dl = separated_list(combined(LBRACK, RBRACK), dexp); RBRACK;
    { List.fold_right (fun d acc -> ArrTyp(acc, d)) dl t }
  | x = ID; DOT; t = typ;
    <CoordTyp>
  | x = ID; pt = parameterization(typ);
    <ParTyp>

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
    <Bool>
  | i = NUM;
    <Num>
  | f = FLOAT;
    <Float>

let bool ==
  | TRUE;
    { true }
  | FALSE;
    { false }

let aexp ==
  | e = exp;
    { (e, $startpos) }

let exp:=
  | LPAREN; a = exp; RPAREN;
    <>
  | v = value;
    <Val>
  | x = ID;
    <Var>
  | e1 = aexp; op = binop; e2 = aexp;
    <Binop>
  | u = unop; e = aexp;
    <Unop>
  | x = ID; p = parameterization(typ); a = arguments; 
    <FnInv>
  | LBRACK; e = separated_list(COMMA, aexp); RBRACK; 
    <Arr>
  | e = aexp; AS; t = typ;
    <As>
  | e = aexp; IN; t = typ;
    <In>
  | e = aexp; DOT; s = ID;
    { Unop(Swizzle s,e) }
  | x = ID; LBRACK; e = aexp; RBRACK;
    { Binop((Var(x), $startpos), Index, e) }

let unop ==
  | MINUS; { Neg }
  | NOT; { Not }

let binop ==
  | PLUS; { Plus }
  | TIMES; { Times }
  | MINUS; { Minus }
  | DIV; { Div }
  | LWICK; { Lt }
  | RWICK; { Gt }
  | CTIMES; { CTimes }
  | EQ; { Eq }
  | LEQ; { Leq }
  | GEQ; { Geq }
  | OR; { Or }
  | AND; { And }

%%