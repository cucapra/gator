(* Standard type definitions*)
type id = string
type vec = float list
type mat = vec list

(* values *)
type value =
  | Unit
  | Bool of bool
  | Num of int
  | Float of float
  | VecLit of vec
  | MatLit of mat

type unop =
  | Not
type binop =
  | Eq
  | Leq
  | Or
  | And
  | Plus
  | Minus
  | Times
  | Div
  | CTimes (* Component-wise multiplication *)
