(* Tag AST pretty printer *)

open CoreAst
open CoreAstHelper
open TagAstHelper
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
    | VoidTyp -> "void"

let rec string_of_exp (e: exp) : string =
    match e with
    | Val v -> string_of_value v
    | Var v -> v
    | Unop (op, (x, _)) -> (string_of_unop op (string_of_exp x))
    | Binop (op, (l, _), (r, _)) -> 
        let ls = (string_of_exp l) in
        let rs = (string_of_exp r) in
        (match op with
        | _ -> (string_of_binop op ls rs))
        (* id * args *)
    | FnInv (id, args) -> id ^ "(" ^ (string_of_args args) ^")"
and string_of_args (a: exp list) : string = 
    match a with
    | [] -> ""
    | h::t -> string_of_exp h ^ ", " ^ string_of_args t

let rec string_of_param (p: params) : string = 
    match p with
    | [] -> ""
    | (i, e)::t -> (string_of_typ e) ^ " " ^ i ^ "," ^ (string_of_param t)

let rec string_of_comm (c: comm) : string =
    match c with
    | Skip -> "skip;"
    | Print (e, _) -> "print " ^ (string_of_exp e) ^ ";"
    | Decl (t, s, (e, _)) -> (string_of_typ t)^" " ^ s ^ " = " ^ (string_of_exp e) ^ ";"
    | If ((b, _), c1, c2) -> "if (" ^ (string_of_exp b) ^ ") {\n" ^ (string_of_comm_list c1) ^
        "} else {\n" ^ (string_of_comm_list c2) ^ "}"
    | Assign (b, (x, _)) -> b ^ " = " ^ (string_of_exp x) ^ ";"
    | Return Some (x, _) -> "return" ^ (string_of_exp x) ^ ";"
    | Return None -> "return;"

and string_of_comm_list (cl : comm list) : string = 
    match cl with
    | [] -> ""
    | h::t -> (string_of_comm h)^"\n"^(string_of_comm_list t)

let string_of_fn (((id, (p, rt)), cl) : fn) : string = 
    match id with 
    | "main" -> "void main() {" ^ (string_of_comm_list cl) ^ "}"
    | _ -> (string_of_typ rt) ^ " " ^ id ^ "(" ^ (string_of_param p) ^ "){" ^ (string_of_comm_list cl) ^ "}"
 
let rec string_of_fn_list (f : fn list) : string = 
    match f with 
    | [] -> ""
    | h::t -> (string_of_fn h) ^ (string_of_fn_list t)

let string_of_prog (e : prog) : string =

    string_of_fn_list e
