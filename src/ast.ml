(* AST definition of LinGL *)

(* Standard type definitions*)
type id = string
type vec = float list
type mat = vec list

(* tag types *)
type tagtyp = 
    | VecTyp of int
    | TagBot of int
    | TagTyp of id

(* types *)
type typ = 
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | TagTyp of tagtyp
    | MatTyp of int * int
    | TransBot of int * int
    | TransTyp of tagtyp * tagtyp

(* arithmetic values *)
type avalue =
    Num of int
    | Float of float
    | VecLit of vec
    | MatLit of mat

(* boolean values *)
type bvalue = bool

(* values *)
type value =
    | Avalue of avalue
    | Bvalue of bvalue

(* expressions *)
type unaryop =
    | Norm
    | Not
type binop = 
    | Eq 
    | Leq 
    | Or 
    | And 
    | Dot 
    | Plus 
    | Times 
    | Div 
    | Minus 
    | CTimes (* Component-wise multiplication *)
type exp =
    | Bool of bvalue
    | Aval of avalue
    | Typ of typ
    | Var of id
    | UnaryOp of unaryop * exp
    | BinOp of binop * exp * exp
    | VecTrans of int * tagtyp (* vec3(<vec4>), vec4(<vec3>) *)

(* commands *)
type comm = 
    Skip
    | Print of exp
    | Decl of typ * string * exp
    | Assign of string * exp
    | If of exp * comm list * comm list
    
(* tag declaration statements *)
type tagdecl = string * typ

(* program *)
type prog =
    | Prog of tagdecl list * comm list

