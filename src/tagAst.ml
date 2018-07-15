(* AST definition of LinGL *)

open CoreAst

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

(* expressions *)
type exp =
    | Bool of bvalue
    | Aval of avalue
    | Typ of typ
    | Var of id
    | Unop of unop * exp
    | Binop of binop * exp * exp
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

