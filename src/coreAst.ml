(* Standard type definitions*)
type id = string
type vec = float list
type mat = vec list

(* arithmetic values *)
type avalue =
  Num of int
  | Float of float
  | VecLit of vec
  | MatLit of mat

(* boolean values *)
type bvalue = bool

(* values *)
type value =
  | Avalue of avalue
  | Bvalue of bvalue

type unop =
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
    | Div 
    | Minus 
    | CTimes (* Component-wise multiplication *)