(* AST definition of LinGL *)

(* Standard type definitions*)
type id = string
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
    | BTyp of btyp
    | ATyp of atyp

type avalue =
    Num of int
    | Float of float
    | VecLit of vec
    | MatLit of mat

(* arithmetic expressions *)
type aexp = 
    Const of avalue
    | Var of string
    | LExp of aexp * ltyp (* Linear exp *)
    | Dot of aexp * aexp
    | Norm of aexp
    | Plus of aexp * aexp
    | Times of aexp * aexp
    | Minus of aexp * aexp
    | CTimes of aexp * aexp (* Component-wise multiplication*)

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
    
type tagdecl = 
    TagDecl of string * atyp

type prog =
    | Prog of tagdecl list * comm


