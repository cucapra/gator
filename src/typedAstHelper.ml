(* Tag AST pretty printer *)

open CoreAst
open CoreAstHelper
open TypedAst

let string_of_vec (v: vec) : string = 
    "["^(String.concat ", " (List.map string_of_float v))^"]"

let string_of_mat (m: mat) : string = 
    "["^(String.concat ", " (List.map string_of_vec m))^"]"

let rec string_of_typ (t: etyp) : string = 
    match t with
    | UnitTyp -> "unit"
    | BoolTyp -> "bool"
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | VecTyp v -> "vec" ^ (string_of_int v)
    | MatTyp (m1, m2) -> "mat" ^ (string_of_int m1) ^ "x" ^ (string_of_int m2)
    | SamplerTyp n -> "sampler" ^ (string_of_int n) ^ "D"

let rec string_of_exp (e:exp) : string =
    match e with
    | Val v -> string_of_value v
    | Var v -> v
    | Unop (op, (x, _)) -> (string_of_unop op (string_of_exp x))
    | Binop (op, (l, _), (r, _)) -> 
        let ls = (string_of_exp l) in
        let rs = (string_of_exp r) in
        (match op with
        | Dot -> (string_of_binop op ls rs)
        | _ -> (string_of_binop op ls rs))

let rec string_of_comm (c: comm) : string =
    match c with
    | Skip -> "skip;"
    | Print (e, _) -> "print " ^ (string_of_exp e) ^ ";"
    | Decl (t, s, (e, _)) -> (string_of_typ t)^" " ^ s ^ " = " ^ (string_of_exp e) ^ ";"
    | If ((b, _), c1, c2) -> "if (" ^ (string_of_exp b) ^ ") {\n" ^ (string_of_comm_list c1) ^
        "} else {\n" ^ (string_of_comm_list c2) ^ "}"
    | Assign (b, (x, _)) -> b^" = " ^ (string_of_exp x) ^ ";"
    | Fn _ -> failwith "Unimplemented"

and 
string_of_comm_list (cl : comm list) : string = 
    match cl with
    | [] -> ""
    | h::t -> (string_of_comm h)^"\n"^(string_of_comm_list t)

let string_of_prog (e : prog) : string =
    string_of_comm_list e
