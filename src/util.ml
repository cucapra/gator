open CoreAst
(* Some utilities, modified from CS6110 assignments *)

exception ElementNotFoundException of string

let flip f x y = f y x
let compose f g = fun x -> f (g x)
let (|-) = compose
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

(* Cause for some reason Option.map doesn't exist? *)
let option_map (f: ('a -> 'b)) (o: 'a option) : 'b option =
  match o with
  | None -> None
  | Some x -> Some (f x)

let tr_fst ((x, _, _): 'a * 'b * 'c) : 'a = x
let tr_snd ((_, x, _): 'a * 'b * 'c) : 'b = x
let tr_thd ((_, _, x): 'a * 'b * 'c) : 'c = x

let string_if_true (b : 'a -> bool) (f : 'a -> string) (x : 'a) : string = 
  if b x then "" else f x
let nonempty x = List.length x > 0
let string_of_option_removed (f : 'a -> string) (o : 'a option) : string =
  match o with
  | Some v -> f v
  | None -> ""

let string_of_pair (a: string) (b: string) : string = 
  "(" ^ a ^ ", " ^ b ^ ")"

let string_of_separated_list (sep : string) (f: 'a -> string) (l : 'a list) : string =
  (String.concat sep (List.map f l))
let string_of_list (f: 'a -> string) (l: 'a list) : string =
  string_of_separated_list ", " f l
let string_of_bounded_list (f: 'a -> string) (lb : string) (rb : string) (l : 'a list) : string =
  lb ^ string_of_list f l ^ rb
let string_of_vec (v: vec) : string =
  string_of_bounded_list string_of_float "[" "]" v
let string_of_mat (m: mat) : string = 
  string_of_bounded_list string_of_vec "[" "]" m

let rec repeat (s : string) (count : int) : string = 
  if count <= 0 then "" else (if count > 1 then (s ^ (repeat s (count-1))) else s)

let get_mat_square_dim (m : mat) = 
    (* Note the transpose to match the glsl column-oriented style *)
    let tm = Lin_ops.transpose m in
    let r = (List.length tm) in
    let c = (if r = 0 then 0 else List.length (List.hd tm)) in
    max r c

let string_of_constvar (c : constvar) : string =
  match c with
  | ConstInt i -> string_of_int i
  | ConstVar s -> s

let rec string_of_value (v: value) : string =
  match v with
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | Num n -> string_of_int n
  | Float f -> string_of_float f
  | VecLit v -> string_of_vec v
  | MatLit m -> string_of_mat m
  | Arr a -> "[" ^ string_of_list string_of_value a ^ "]"

let string_of_unop (op : unop) : string =
  match op with
  | Neg -> "-"
  | Not -> "!"
  | Swizzle s -> "." ^ s

let string_of_binop (op: binop) : string =
  match op with
  | Eq -> "=="
  | Leq -> "<="
  | Lt -> "<"
  | Geq -> ">="
  | Gt -> ">"
  | Or -> "||"
  | And -> "&&"
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | CTimes -> ".*"
  | Index -> "[]"

let string_of_storage_qual (s: storage_qual) : string =
  match s with
  | Const -> "const"
  | InQual -> "in"
  | Out -> "out"
  | Attribute -> "attribute"
  | Uniform -> "uniform"
  | Varying -> "varying"

let string_of_unop_exp (op: unop) (e: string) : string =
  match op with
  | Swizzle s -> e ^ "." ^ s
  | _ -> string_of_unop op ^  " " ^ e

let string_of_binop_exp (left: string) (op: binop) (right: string) : string =
  match op with
  | Index -> left ^ "[" ^ right ^ "]"
  | _ -> left ^ " " ^ string_of_binop op ^ " " ^ right
  

(*****************************************************
 * Debug-printer
 *****************************************************)

 let debug = false

 let debug_print (s: string) : unit = if debug then Printf.printf "%s\n" s