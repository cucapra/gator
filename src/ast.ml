(* AST definition of LinGL *)

type scalar = 
    Int of int
    | Float of float

type vec = scalar list

(* linear expressions *)
type lexp =
    Vec of vec 
    | LVar of string
    | Mat of vec list
    | LCompTimes of lexp * lexp
    | LTimes of lexp * lexp
    | LComp of lexp * lexp

(* arithmetic expressions *)
type aexp = 
    Num of scalar
    | Var of string
    | Plus of aexp * aexp
    | Times of aexp * aexp
    | Minus of aexp * aexp
    
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
    | Aexp of aexp
    | Bexp of bexp
    | Lexp of lexp

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