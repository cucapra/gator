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
    | AbsTyp of string * etyp option (* type abstraction *)
    | AppTyp of string * etyp (* type application *)

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

(* function parameterization,
 * which may extend another type. *)
type parametrization = etyp list

type params = (string * etyp) list
type ret_type = etyp
type fn_type = params * ret_type * parametrization
type fn_decl = string * fn_type
type fn = fn_decl * comm list

type prog = fn list
