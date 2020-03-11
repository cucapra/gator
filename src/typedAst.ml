(* IR for typed AST *)
open CoreAst

(* type with tags erased *)
type etyp =
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | StringTyp
    | ArrTyp of etyp * constvar (* i.e. vec3[5] *)
    | ParTyp of string * etyp list
    | AnyTyp
    | GenTyp
    | ExactCodeTyp

(* expressions *)
type texp = exp * etyp
and exp =
    | Val of value
    | Var of id
    | Arr of texp list
    | Index of texp * texp
    | FnInv of id * etyp list * args

and args = texp list

(* commands *)
type comm =
    | Skip
    | Print of texp
    | Exp of texp
    | Decl of etyp * id * texp
    | Assign of texp * texp
    | AssignOp of texp * string * texp
    | If of if_block * if_block list * (comm list) option  (* if - elif list - else *)
    | For of comm * texp * comm * comm list
    | Return of texp option
    | ExactCodeComm of string

and if_block = texp * comm list

type parameterization = etyp Assoc.context
type params = (etyp * string) list
type global_var = storage_qual * etyp * string * texp option
type ret_typ = etyp
type fn_decl = ret_typ * id * parameterization * params
type fn = fn_decl * comm list

type term =
    | GlobalVar of global_var
    | Fn of fn

type prog = term list