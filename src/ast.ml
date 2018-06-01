(* AST definition of LinGL *)

(* linear types *)
type ltyp = 
    VecTyp of int
    | MatTyp of int * int
    | TagTyp of string
    | TransTyp of ltyp * ltyp

(* types *)
type typ = 
    FloatTyp
    | IntTyp
    | LtypTyp of ltyp

type scalar = 
    Int of int
    | Float of float

(* arithmetic expressions *)
type aexp = 
    Num of scalar
    | Var of string * ltyp
    | Vec of aexp * ltyp
    | Mat of aexp * ltyp
    | Dot of aexp * aexp
    | Norm of aexp
    | Plus of aexp * aexp
    | Times of aexp * aexp
    | Minus of aexp * aexp
    | LCompTimes of aexp * aexp
    | LTrans of aexp * aexp (* Linear Transformation, i.e. matrix mult *)

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
    | Decl of typ * string * exp
    | Comp of comm * comm
    | If of bexp * comm * comm

type tags = 
    TagDecl of string * ltyp
    | TagComp of tags * tags

type prog = tags * comm