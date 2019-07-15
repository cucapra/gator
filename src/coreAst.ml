(* Standard type definitions*)
type id = string
type vec = float list
type mat = vec list

type constvar =
  | ConstInt of int
  | ConstVar of id

(* values *)
type value =
  | Unit
  | Bool of bool
  | Num of int
  | Float of float
  | VecLit of vec
  | MatLit of mat

type unop =
  | Neg
  | Not
  | Swizzle of id
type binop =
  | Eq
  | Leq
  | Lt
  | Geq
  | Gt
  | Or
  | And
  | Plus
  | Minus
  | Times
  | Div
  | CTimes (* Component-wise multiplication *)
  | Index

(* Storage qualifiers *)
type storage_qual =
  | InQual
  | Out
  | Const
  | Attribute
  | Uniform
  | Varying