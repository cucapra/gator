(* AST definition of LinGL *)

(* linear types *)
type ltyp = 
    VecTyp of int
    | MatTyp of int * int
    | TagTyp of string
    | ConvTyp of ltyp * ltyp

(* types *)
type typ = 
    FloatTyp
    | IntTyp
    | LtypTyp of ltyp

type scalar = 
    Int of int
    | Float of float

type vec = scalar list

(* linear literal expressions*)
type linlit =
    Vec of vec * ltyp
    | Mat of vec list * ltyp

(* arithmetic expressions *)
type aexp = 
    Num of scalar
    | Var of string * ltyp
    | LinLit of linlit
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


type comm = 
    Skip
    | Decl of typ * string * exp
    | Comp of comm * comm
    | If of bexp * comm * comm

type tagdecl = string * ltyp

type prog = tagdecl list * comm