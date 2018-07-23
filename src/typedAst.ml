(* IR for typed AST *)

open CoreAst

(* Type with tags erased *)
type etyp = 
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | VecTyp of int
    | MatTyp of int * int

(* expressions  *)
type texp = exp * etyp
and exp =
    | Val of value
    | Var of id 
    | Unop of unop * texp
    | Binop of binop * texp * texp

(* commands *)
type comm = 
    | Skip
    | Print of texp
    | Decl of etyp * string * texp
    | Assign of string * texp
    | If of texp * comm list * comm list

type prog = comm list