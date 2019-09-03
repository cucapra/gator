(* IR for typed AST *)
open CoreAst

(* type with tags erased *)
type etyp =
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | StrTyp
    | VecTyp of int
    | MatTyp of int * int
    | TransTyp of etyp * etyp
    | AbsTyp of string * etyp
    | ArrTyp of etyp * constvar (* i.e. vec3[5] *)
    | AnyTyp
    | GenTyp

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
    | Inc of id * etyp
    | Dec of id * etyp
    | Decl of etyp * id * texp
    | Assign of id * texp
    | AssignOp of (string * etyp) * string * texp
    | If of if_block * if_block list * (comm list) option  (* if - elif list - else *)
    | For of comm * texp * comm * comm list
    | Return of texp option
    | FnCall of id * etyp list * args
and if_block = texp * comm list

type parameterization = etyp Assoc.context
type params = (etyp * string) list
type global_var = storage_qual * etyp * string * texp option
type global_vars = global_var list
type ret_typ = etyp
type fn_typ = params * ret_typ * parameterization
type fn_decl = string * fn_typ
type fn = fn_decl * comm list

type prog = fn list
