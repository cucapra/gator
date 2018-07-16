(* AST definition of LinGL *)

open CoreAst

(* tag types *)
type tagtyp = 
    | TagTopTyp of int
    | TagBotTyp of int
    | TagVarTyp of id

type transtype =
    | TransTopTyp of int * int
    | TransBotTyp of int * int
    | TransVarTyp of tagtyp * tagtyp

(* types *)
type typ = 
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | TagTyp of tagtyp
    | TransTyp of transtype

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

