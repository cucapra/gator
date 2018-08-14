(* IR for typed AST *)
open CoreAst

(* type with tags erased *)
type etyp =
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | VecTyp of int
    | MatTyp of int * int
    | SamplerTyp of int
    | PTyp of string

(* expressions *)
type texp = exp * etyp
and exp =
    | Val of value
    | Var of id
    | Unop of unop * texp
    | Binop of binop * texp * texp
    | FnInv of id * args

and args = exp list

(* commands *)
type comm =
    | Skip
    | Print of texp
    | Decl of etyp * id * texp
    | Assign of id * texp
    | If of texp * comm list * comm list
    | Return of texp option
    | FnCall of id * args

type params = (string * etyp) list
type ret_type = etyp
type fn_type = params * ret_type
type fn_decl = string * fn_type
type fn = fn_decl * comm list

type prog = fn list
