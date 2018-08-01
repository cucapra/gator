(* Tag AST pretty printer *)

open CoreAst
open Util

(* Note the column parameter for padding the matrix size *)
let string_of_no_paren_vec (v: vec) (padding: int) : string = 
    (String.concat ", " (List.map string_of_float v)) ^ (repeat ", 0." padding)

let string_of_mat_padded (m: mat) (max_dim: int) : string =
    let string_of_vec_padded = (fun v -> (string_of_no_paren_vec v (max_dim - List.length v))) in
    ("(" ^ (String.concat ", " (List.map string_of_vec_padded m)) ^
    (repeat (string_of_no_paren_vec [] max_dim) (max_dim - List.length m)) ^ ")")

let string_of_gl_mat (m: mat) : string = 
    (* Note the transpose to match the glsl column-oriented style *)
    let tm = Lin_ops.transpose m in
    let r = (List.length tm) in
    let c = (if r = 0 then 0 else List.length (List.hd tm)) in
    let dim = max r c in
    ("mat"^(string_of_int dim)^string_of_mat_padded tm dim)

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
    | MatLit m -> string_of_gl_mat m

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
