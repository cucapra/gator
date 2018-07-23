(* Tag AST pretty printer *)

open CoreAst

let string_of_vec (v: vec) : string = 
    "["^(String.concat ", " (List.map string_of_float v))^"]"

let string_of_mat (m: mat) : string = 
    "["^(String.concat ", " (List.map string_of_vec m))^"]"

let rec string_of_value (v: value) : string =
    match v with
    | Bool b -> string_of_bool b
    | Num n -> string_of_int n
    | Float f -> string_of_float f
    | VecLit v -> string_of_vec v
    | MatLit m -> string_of_mat m


let string_of_unop (op: unop) (e: string) : string =
    let funct_op (op: string) : string =
        op ^ "(" ^ e ^ ")"
    in
    match op with
    | Norm -> funct_op "normalize"
    | Not -> "!" ^ e
let string_of_binop (op: binop) (left: string) (right: string) : string =
    let funct_op (op: string) : string =
        op ^ "(" ^ left ^ ", " ^ right ^ ")"
    in
    let inline_op (op: string) : string =
        left ^ " " ^ op ^ " " ^ right
    in
    match op with
    | Eq -> inline_op "=="
    | Leq -> inline_op "<="
    | Or -> inline_op "||"
    | And -> inline_op "&&"
    | Dot -> funct_op "dot("
    | Plus -> inline_op "+"
    | Minus -> inline_op "-"
    | Times -> inline_op "*"
    | Div -> inline_op "/"
    | CTimes -> inline_op ".*"
