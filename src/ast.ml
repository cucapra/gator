(* AST definition of LinGL *)
type vec = float list
type mat = float list list

(* linear types *)
type ltyp = 
    VecTyp of int
    | MatTyp of int * int
    | TagTyp of string
    | TransTyp of ltyp * ltyp

(* arithmetic types *)
type atyp = 
    | IntTyp
    | FloatTyp
    | LTyp of ltyp

(* bool types *)
type btyp = BoolTyp

(* types *)
type typ = 
    UnitTyp
    | BTyp of btyp
    | ATyp of atyp

type avalue =
    Int of int
    | Float of float
    | VecLit of vec
    | MatLit of mat

(* arithmetic expressions *)
type aexp = 
    Const of avalue
    | Var of string
    | Vec of aexp * ltyp
    | Mat of aexp * ltyp
    | Dot of aexp * aexp
    | Norm of aexp
    | Plus of aexp * aexp
    | Times of aexp * aexp
    | Minus of aexp * aexp
    | LCompTimes of aexp * aexp
    | LTrans of aexp * aexp (* Linear Transformation, i.e. matrix mult *)

type bvalue = Bool of bool

(* boolean expressions *)
type bexp =
    True
    | False
    | Eq of aexp * aexp
    | Leq of aexp * aexp
    | Or of bexp * bexp
    | And of bexp * bexp
    | Not of bexp

type exp =
    Aexp of aexp
    | Bexp of bexp

type comm = 
    Skip
    | Print of exp
    | Decl of typ * string * exp
    | Seq of comm * comm
    | If of bexp * comm * comm

type tags = 
    Empty
    | TagDecl of string * atyp
    | TagComp of tags * tags

type prog = 
    Prog of tags * comm
