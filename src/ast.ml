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
    | UnitTyp
    | BTyp
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

type exp =
    | Bool of bvalue
    | Aval of avalue 
    | Var of id
    | Eq of exp * exp
    | Leq of exp * exp
    | Or of exp * exp
    | And of exp * exp
    | Not of exp    
    | Lexp of exp * ltyp (* Linear exp *)
    | Dot of exp * exp
    | Norm of exp
    | Plus of exp * exp
    | Times of exp * exp
    | Minus of exp * exp
    | CTimes of exp * exp (* Component-wise multiplication*)

type comm = 
    Skip
    | Print of exp
    | Decl of typ * string * exp
    | If of exp * comm list * comm list
    
type tagdecl = 
    TagDecl of string * atyp

type prog =
    | Prog of tagdecl list * comm list

