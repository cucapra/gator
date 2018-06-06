(* AST definition of LinGL *)

(* Standard type definitions*)
type id = string
type vec = float list
type mat = vec list

(* linear types *)
type ltyp = 
    VecTyp of int
    | MatTyp of int * int
    | TagTyp of id
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

type bvalue = bool

type value =
    | Avalue of avalue
    | Bvalue of bvalue

(* arithmetic expressions *)
type aexp = 
    Const of avalue
    | Var of string
    | Lexp of aexp * ltyp (* Linear exp *)
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
    | Var of string
    | Eq of aexp * aexp
    | Leq of aexp * aexp
    | Or of bexp * bexp
    | And of bexp * bexp
    | Not of bexp    

type exp =
    Aexp of aexp
    | Bexp of bexp
    | Var of id

type comm = 
    Skip
    | Print of exp
    | Decl of typ * string * exp
    | If of exp * comm list * comm list
    
type tagdecl = 
    TagDecl of string * atyp

type prog =
    | Prog of tagdecl list * comm list

