(* IR for typed AST *)
open Ast

(* expressions *)
type texp = exp * typ
and exp =
    | Bool of bvalue
    | Aval of avalue  
    | Typ of typ
    | Var of id
    | Norm of texp
    | Not of texp   
    | Eq of texp * texp
    | Leq of texp * texp
    | Or of texp * texp
    | And of texp * texp
    | Dot of texp * texp
    | Plus of texp * texp
    | Times of texp * texp
    | Minus of texp * texp
    | CTimes of texp * texp (* Component-wise multiplication*)

(* commands *)
type comm = 
    Skip
    | Print of texp
    | Decl of typ * string * texp
    | If of texp * comm list * comm list