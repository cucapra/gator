(* AST definition of LinGL *)

(* linear types *)
type ltyp = 
    VecTyp of int
    | MatTyp of int * int
    | TagTyp of string
    | TransTyp of ltyp * ltyp

(* types *)
type atyp = 
    UnitTyp
    | IntTyp
    | FloatTyp
    | LtypTyp of ltyp

type avalue =
    Int of int
    | Float of float
    | Vec of float list
    | Mat of float list list

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

type btyp = BoolTyp
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
    | Decl of atyp * string * exp
    | Comp of comm * comm
    | If of bexp * comm * comm

type tags = 
    TagDecl of string * ltyp
    | TagComp of tags * tags

type prog = tags * comm