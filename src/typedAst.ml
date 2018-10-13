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
    | AbsTyp of string * etyp option
    | GenTyp
    | GenMatTyp
    | GenVecTyp

(* expressions *)
type texp = exp * etyp
and exp =
    | Val of value
    | Var of id
    | Arr of texp list
    | Unop of unop * texp
    | Binop of binop * texp * texp
    | FnInv of id * args

and args = exp list

(* commands *)
type comm =
    | Skip
    | Print of texp
    | Inc of id
    | Dec of id
    | Decl of etyp * id * texp
    | Assign of id * texp
    | AssignOp of string * binop * texp
    | If of if_block * if_block list * (comm list) option  (* if - elif list - else *)
    | For of comm * texp * comm * comm list
    | Return of texp option
    | FnCall of id * args
and if_block = texp * comm list

type params = (string * etyp) list
type ret_type = etyp
type fn_type = params * ret_type
type fn_decl = string * fn_type
type fn = fn_decl * comm list

type prog = fn list
