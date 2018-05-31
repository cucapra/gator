(* AST definition of LinGL *)

type scalar = 
    Int of int
    | Float of float

type vec = scalar list

(* arithmetic expressions *)
type aexp = 
    Num of scalar
    | Vec of vec 
    | Var of string
    | Mat of vec list
    | Plus of aexp * aexp
    | Times of aexp * aexp
    | Minus of aexp * aexp
    | LCompTimes of aexp * aexp
    | LComp of aexp * aexp

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

(* linear types *)
type ltyp = 
    VecTyp of int
    | MatTyp of int * int
    | TagTyp of string
    | ConvTyp of ltyp * ltyp

(* types *)
type typ = 
    FloatTyp of float
    | IntTyp of int
    | LtypTyp of ltyp

type comm = 
    Skip
    | Decl of typ * string * exp
    | Comp of comm * comm
    | If of bexp * comm * comm

type tagdecl = string * ltyp

type prog = tagdecl list * comm