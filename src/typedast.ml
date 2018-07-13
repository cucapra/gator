(* IR for typed AST *)
open Ast

(* expressions *)
type typexp = exp * typ
and texp =
    | TBool of bvalue
    | TAval of avalue  
    | TTyp of typ
    | TVar of id
    | TNorm of typexp
    | TNot of typexp   
    | TEq of typexp * typexp
    | TLeq of typexp * typexp
    | TOr of typexp * typexp
    | TAnd of typexp * typexp
    | TDot of typexp * typexp
    | TPlus of typexp * typexp
    | TTimes of typexp * typexp
    | TMinus of typexp * typexp
    | TCTimes of typexp * typexp (* Component-wise multiplication*)

(* commands *)
type tcomm = 
    | TSkip
    | TPrint of typexp
    | TDecl of typ * string * typexp
    | TIf of typexp * comm list * comm list