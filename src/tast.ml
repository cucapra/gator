(* IR for typed AST *)

(* Type with tags erased *)
type etyp = 
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | VecTyp of int
    | MatTyp of int * int

(* expressions *)
type unaryop =
    | Norm
    | Not
type binop =
    | Eq
    | Leq
    | Or
    | And
    | Dot
    | Plus
    | Times
    | Minus
    | CTimes (* Component-wise multiplication*)
type texp = exp * etyp
and exp =
    | Bool of Ast.bvalue
    | Aval of Ast.avalue  
    | Typ of etyp
    | Var of Ast.id 
    | UnaryOp of unaryop * texp
    | BinOp of binop * texp * texp

(* commands *)
type comm = 
    | Skip
    | Print of texp
    | Decl of etyp * string * texp
    | If of texp * comm list * comm list