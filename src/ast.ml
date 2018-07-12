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

(* types *)
type typ = 
    | UnitTyp
    | BTyp
    | ATyp of atyp

(* qualifier types *)
type qualtyp =
    | In 
    | Out

(* arithmetic values *)
type avalue =
    Num of int
    | Float of float
    | VecLit of vec * ltyp
    | MatLit of mat * ltyp

(* boolean values *)
type bvalue = bool

(* values *)
type value =
    | Avalue of avalue
    | Bvalue of bvalue

(* expressions *)
type exp =
    | Bool of bvalue
    | Aval of avalue
    | Typ of typ
    | Var of id
    | Norm of exp
    | Not of exp
    | Eq of exp * exp
    | Leq of exp * exp
    | Or of exp * exp
    | And of exp * exp
    | Dot of exp * exp
    | Plus of exp * exp
    | Times of exp * exp
    | Div of exp * exp
    | Minus of exp * exp
    | CTimes of exp * exp (* Component-wise multiplication *)
    | VecTrans of int * ltyp (* vec3(<vec4>), vec4(<vec3>) *)

(* commands *)
type comm = 
    Skip
    | Print of exp
    | Decl of typ * string * exp
    | Assign of string * exp
    | If of exp * comm list * comm list
    | Store of qualtyp * typ * string 
    
(* tag declaration statements *)
type tagdecl = string * atyp

(* program *)
type prog =
    | Prog of tagdecl list * comm list

