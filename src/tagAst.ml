(* AST definition of LinGL *)

open CoreAst

(* tag types *)
type tagtyp = 
    | TopTyp of int
    | BotTyp of int
    | VarTyp of id

(* types *)
type typ =
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | TagTyp of tagtyp
    | TransTyp of tagtyp * tagtyp
    | SamplerTyp of int (* i.e. sampler2D *)

(* expressions *)
type exp =
    | Val of value
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

