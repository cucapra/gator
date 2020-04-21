(* Standard type definitions*)
type id = string

type exactCode = string

type constvar =
  | ConstInt of int
  | ConstVar of id

(* values *)
type value =
  | Unit
  | Bool of bool
  | Num of int
  | Float of float
  | StringVal of string

(* Storage qualifiers *)
type storage_qual =
  | InQual
  | Out
  | Const
  | Attribute
  | Uniform
  | Varying
  | BuiltIn