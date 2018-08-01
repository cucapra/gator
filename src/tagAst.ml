(* AST definition of LinGL *)

open CoreAst

(* tag types *)
type tag_typ = 
    | TopTyp of int
    | BotTyp of int
    | VarTyp of id

(* types *)
type typ =
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | TagTyp of tag_typ
    | TransTyp of tag_typ * tag_typ
    | SamplerTyp of int (* i.e. sampler2D *)
    | VoidTyp

(* expressions *)
type exp =
    | Val of value
    | Var of string
    | Unop of unop * exp
    | Binop of binop * exp * exp
    | VecTrans of int * tag_typ (* vec3(<vec4>), vec4(<vec3>) *)
    | FnInv of string * args (* function invocation *)

(* arguments *)
and args = exp list

(* function parameters *)
type params = (string * typ) list
type ret_type = typ
(* our functions are not first-order *)
type fn_type = params * ret_type
(* function declaration *)
type fn_decl = string * fn_type

(* commands *)
type comm = 
    Skip
    | Print of exp
    | Decl of typ * string * exp
    | Assign of string * exp
    | If of exp * comm list * comm list
    | Return of exp option

type fn = fn_decl * comm list

(* tag declaration statements *)
type tag_decl = string * typ

(* program *)
type prog =
    | Prog of tag_decl list * fn list

