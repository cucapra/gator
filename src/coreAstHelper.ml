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


let string_of_unop (op: unop) : string =
    match op with
    | Norm -> "normalize"
    | Not -> "!"
let string_of_binop (op: binop) : string =
    match op with
    | Eq -> "=="
    | Leq -> "<="
    | Or -> "||"
    | And -> "&&"
    | Dot -> "dot"
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | CTimes -> ".*"
