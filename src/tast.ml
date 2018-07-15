(* IR for typed AST *)

type id = Ast.id
type vec = Ast.vec
type mat = Ast.mat

(* Type with tags erased *)
type etyp = 
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | VecTyp of int
    | MatTyp of int * int

(* expressions  *)
type unaryop = Ast.unaryop
type binop = Ast.binop
type texp = exp * etyp
and exp =
    | Bool of Ast.bvalue
    | Aval of Ast.avalue  
    | Typ of etyp
    | Var of Ast.id 
    | UnaryOp of unaryop * texp
    | BinOp of binop * texp * texp

type avalue = Ast.avalue
type bvalue = Ast.bvalue
type value = Ast.value

(* commands *)
type comm = 
    | Skip
    | Print of texp
    | Decl of etyp * string * texp
    | Assign of string * texp
    | If of texp * comm list * comm list

type prog = comm list