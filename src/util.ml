open CoreAst
(* Some utilities, modified from CS6110 assignments *)

exception ElementNotFoundException of string

let string_of_vec (v: vec) : string = 
  "(" ^ (String.concat ", " (List.map string_of_float v)) ^ ")"

let string_of_mat (m: mat) : string = 
  "(" ^ (String.concat ", " (List.map string_of_vec m)) ^ ")"

let rec repeat (s : string) (count : int) : string = 
  if count <= 0 then "" else (if count > 1 then (s ^ (repeat s (count-1))) else s)

let string_of_vec (v: vec) : string = 
  "("^(String.concat ", " (List.map string_of_float v))^")"

let string_of_mat (m: mat) : string = 
  "("^(String.concat ", " (List.map string_of_vec m))^")"

let rec string_of_value (v: value) : string =
  match v with
  | Bool b -> string_of_bool b
  | Num n -> string_of_int n
  | Float f -> string_of_float f
  | VecLit v -> "vec" ^ string_of_int (List.length v) ^ string_of_vec v
  | MatLit m -> string_of_mat m
  | _ -> failwith "Unimplemented"

let string_of_unop (op: unop) (e: string) : string =
  match op with
  | Not -> "!" ^ e
let string_of_binop (op: binop) (left: string) (right: string) : string =
  let inline_op (op: string) : string =
      left ^ " " ^ op ^ " " ^ right
  in
  match op with
  | Eq -> inline_op "=="
  | Leq -> inline_op "<="
  | Or -> inline_op "||"
  | And -> inline_op "&&"
  | Plus -> inline_op "+"
  | Minus -> inline_op "-"
  | Times -> inline_op "*"
  | Div -> inline_op "/"
  | CTimes -> inline_op ".*"

(*****************************************************
 * Debug-printer
 *****************************************************)

 let debug = false

 let debug_print (s: string) : unit = if debug then Printf.printf "\n%s" s