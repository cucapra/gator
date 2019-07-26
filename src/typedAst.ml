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
    | TransTyp of etyp * etyp
    | SamplerTyp of int
    | SamplerCubeTyp
    | AbsTyp of string * constrain
    | ArrTyp of etyp * constvar (* i.e. vec3[5] *)

and constrain =
    | AnyTyp
    | GenTyp
    | GenMatTyp
    | GenVecTyp
    | ETypConstraint of etyp

(* expressions *)
type texp = exp * etyp
and exp =
    | Val of value
    | Var of id
    | Arr of texp list
    | Unop of unop * texp
    | Binop of texp * binop * texp
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
    | AssignOp of (string * etyp) * binop * texp
    | If of if_block * if_block list * (comm list) option  (* if - elif list - else *)
    | For of comm * texp * comm * comm list
    | Return of texp option
    | FnCall of id * etyp list * args
and if_block = texp * comm list

type parameterization = constrain Assoc.context
type params = (etyp * string) list
type global_var = storage_qual * etyp * string * texp option
type global_vars = global_var list
type ret_type = etyp
type fn_type = params * ret_type * parameterization
type fn_decl = string * fn_type
type fn = fn_decl * comm list

type prog = fn list
