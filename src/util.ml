open CoreAst
(* Some utilities, modified from CS6110 assignments *)

exception ElementNotFoundException of string

(* Cause for some reason Option.map doesn't exist? *)
let option_map (f: ('a -> 'b)) (o: 'a option) : 'b option =
  match o with
  | None -> None
  | Some x -> Some (f x)

let string_of_vec (v: vec) : string = 
  "(" ^ (String.concat ", " (List.map string_of_float v)) ^ ")"

let string_of_mat (m: mat) : string = 
  "(" ^ (String.concat ", " (List.map string_of_vec m)) ^ ")"

let rec repeat (s : string) (count : int) : string = 
  if count <= 0 then "" else (if count > 1 then (s ^ (repeat s (count-1))) else s)

let get_mat_square_dim (m : mat) = 
    (* Note the transpose to match the glsl column-oriented style *)
    let tm = Lin_ops.transpose m in
    let r = (List.length tm) in
    let c = (if r = 0 then 0 else List.length (List.hd tm)) in
    max r c

let string_of_vec (v: vec) : string = 
  "("^(String.concat ", " (List.map string_of_float v))^")"

let string_of_mat (m: mat) : string = 
  "("^(String.concat ", " (List.map string_of_vec m))^")"

let rec string_of_value (v: value) : string =
  match v with
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | Num n -> string_of_int n
  | Float f -> string_of_float f
  | VecLit v -> "vec" ^ (string_of_int (List.length v)) ^ string_of_vec v
  | MatLit m -> string_of_mat m

let binop_string (op: binop) : string =
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

let string_of_unop (op: unop) (e: string) : string =
  match op with
  | Neg -> "-" ^ e
  | Not -> "!" ^ e
  | Swizzle s -> e ^ "." ^ s
let string_of_binop (op: binop) (left: string) (right: string) : string =
  match op with
  | Index -> left ^ "[" ^ right ^ "]"
  | _ -> left ^ " " ^ binop_string op ^ " " ^ right
  

(*****************************************************
 * Debug-printer
 *****************************************************)

 let debug = false

 let debug_print (s: string) : unit = if debug then Printf.printf "\n%s" s