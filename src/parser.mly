
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
%token HAS
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
%left AND OR
%left NOT EQ LEQ GEQ LBRACK 
%left LWICK RWICK 
%left INC DEC

%left PLUS MINUS
%left TIMES DIV CTIMES 
%left AS IN
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
  | t = node(term)+; EOF;
    { t }

let terminated_list(X, Y) ==
  | y = Y;
    { ([], y) }
  | x = X+; y = Y;
    <>

let oplist(X) ==
  | x = X?;
    { match x with | Some y -> y | None -> [] }

let combined(A, B) == a = A; b = B; { (a, b) }
let fst(T) == (a, b) = T; { a }
let snd(T) == (a, b) = T; { b }
let node(T) == t = T; { (t, $startpos) }

let term == 
  | PROTOTYPE; x = ID; LBRACE; p = list(node(prototype_element)); RBRACE;
    <Prototype>
  | COORDINATE; x = ID; COLON; p = ID;
    LBRACE; DIMENSION; d = dexp; SEMI; c = list(node(coordinate_element)); RBRACE;
    <Coordinate>
  | FRAME; x = ID; HAS; DIMENSION; d = dexp; SEMI;
    <Frame>
  | TYP; x = ID; pm = parameterization(constrained); IS; t = typ; SEMI;
    <Typ>
  | DECLARE; d = extern_element; SEMI; 
    <Extern>
  | m = modification*; sq = storage_qual; t = typ;
    x = ID; v = preceded(GETS, node(exp))?; SEMI; 
    <GlobalVar>
  | f = fn;
    <Fn>

let prototype_element ==
  | OBJECT; x = ID; p = oplist(parameters(LWICK, ID, RWICK)); SEMI;
    <ProtoObject>
  | f = fn_typ; SEMI;
    <ProtoFn>

let modification ==
  | WITH; w = with_statement+;
    <With>
  | CANON;
    { Canon }

let with_statement ==
  | FRAME; d = delimited(LPAREN, NUM, RPAREN); r = separated_list(COMMA, ID); COLON;
    <>

let coordinate_element ==
  | OBJECT; x = ID; p = oplist(parameters(LWICK, ID, RWICK)); IS; t = typ; SEMI;
    <CoordObjectAssign>
  | f = fn;
    <CoordFn>

let extern_element == 
  | (m, t) = terminated_list(modification, typ); x = ID;
    { ExternVar(m, t, x, $startpos) }
  | f = fn_typ;
    <ExternFn>

let constrained ==
  | t = ID;
    { (t, AnyTyp) }
  | t = ID; COLON; c = typ; <>

let parameter == 
  | t = typ; x = ID;
    <>

let parameters(L, P, R) ==
  | x = delimited(L, separated_list(COMMA, P), R); <>

let parameterization(T) ==
  | pt = oplist(parameters(LWICK, T, RWICK)); { Assoc.create pt }

let arguments ==
  | x = parameters(LPAREN, node(exp), RPAREN); <>

let id_expanded ==
  | x = ID; <>
  | NOT; { "!" }
  | x = infix; <>

let fn_typ ==
  | (m, t) = terminated_list(modification, typ); x = id_expanded; 
    pm = parameterization(constrained); params = parameters(LPAREN, parameter, RPAREN);
    { (m, t, x, pm, params, $startpos) }

let fn ==
  | f = fn_typ; LBRACE; cl = list(acomm); RBRACE; <>

let acomm ==
  | c = comm;
    { (c, $startpos) }

let comm ==
  | c = comm_block;
    <>
  | c = comm_element; SEMI;
    <>

let if_block(delim) ==
  | delim; LPAREN; b = node(exp); RPAREN; LBRACE; c = acomm*; RBRACE;
    <>

let comm_block ==
  | i = if_block(IF); el = if_block(ELIF)*; 
    e = option(preceded(ELSE, delimited(LBRACE, list(acomm), RBRACE)));
    <If>
  | FOR; LPAREN; c1 = node(comm_element); SEMI; b = node(exp); SEMI; c2 = node(comm_element); RPAREN;
    LBRACE; cl = node(comm)*; RBRACE; 
    <For>

let assignop ==
  | PLUSEQ;
    { "+" }
  | MINUSEQ;
    { "-" }
  | TIMESEQ;
    { "*" }
  | DIVEQ;
    { "/" }
  | CTIMESEQ;
    { ".*" }

let comm_element == 
  | SKIP;
    { Skip }
  | (m, t) = terminated_list(modification, typ); x = ID; GETS; e = node(exp); 
    < Decl >
  | e = node(exp);
    < Exp >
  | x = ID; GETS; e = node(exp); 
    < Assign >
  | x = ID; a = assignop; e = node(exp); 
    < AssignOp >
  | PRINT; e = node(exp); 
    < Print >
  | RETURN; e = node(exp)?;
    < Return >

let dexp :=
  | d1 = dexp; PLUS; d2 = dexp;
    <DimPlus>
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
  | x = ID; pt = parameters(LWICK, typ, RWICK);
    <ParTyp>
  | x = ID; /* explicit for clarity and to help out the parser */
    { ParTyp(x, []) }
  | VEC;
    { GenArrTyp(FloatTyp) }
  | MAT;
    { GenArrTyp(GenArrTyp(FloatTyp)) }
  | GENTYPE; 
    { GenTyp }

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

let exp:=
  | LPAREN; a = exp; RPAREN;
    <>
  | v = value;
    <Val>
  | x = ID;
    <Var>
  | op = unop_prefix; e = node(exp);
    { FnInv(op, [], [e]) }
  | e = node(exp); op = unop_postfix;
    { FnInv(op, [], [e]) }
  | e1 = node(exp); op = infix; e2 = node(exp);
    { FnInv(op, [], [e1; e2]) }
  | x = ID; p = oplist(parameters(LWICK, typ, RWICK)); a = arguments; 
    <FnInv>
  | LBRACK; e = separated_list(COMMA, node(exp)); RBRACK; 
    <Arr>
  | e = node(exp); AS; t = typ;
    <As>
  | e = node(exp); IN; t = typ;
    <In>
  | e = node(exp); DOT; s = ID;
    { FnInv("swizzle",[],[Var s, $startpos; e]) }
  | x = ID; LBRACK; e = node(exp); RBRACK;
    { Index((Var x, $startpos), e) }

let unop_prefix ==
  /* NOTE: if you update this, update id_extended to avoid MINUS conflicts */
  | NOT; { "!" }
  | MINUS; { "-" }

let unop_postfix ==
  | INC; {"++"}
  | DEC; {"--"}

let infix ==
  | PLUS; { "+" }
  | TIMES; { "*" }
  | MINUS; { "-" }
  | DIV; { "/" }
  | LWICK; { "<" }
  | RWICK; { ">" }
  | CTIMES; { ".*" }
  | EQ; { "==" }
  | LEQ; { "<=" }
  | GEQ; { ">=" }
  | OR; { "||" }
  | AND; { "&&" }

%%