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

(* Storage qualifiers *)
type storage_qual =
  | InQual
  | Out
  | Const
  | Attribute
  | Uniform
  | Varying