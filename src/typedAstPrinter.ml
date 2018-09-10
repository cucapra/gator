(* Tag AST pretty printer *)

open CoreAst
open Util
open TagAstPrinter
open TypedAst

let string_of_vec (v: vec) : string = 
    "["^(String.concat ", " (List.map string_of_float v))^"]"

let string_of_mat (m: mat) : string = 
    "["^(String.concat ", " (List.map string_of_vec m))^"]"

let rec string_of_typ (t: etyp) : string = 
    match t with
    | UnitTyp -> "void"
    | BoolTyp -> "bool"
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | VecTyp v -> "vec" ^ (string_of_int v)
    | MatTyp (m1, m2) -> "mat" ^ (string_of_int m1) ^ "x" ^ (string_of_int m2)
    | SamplerTyp n -> "sampler" ^ (string_of_int n) ^ "D"
    | AbsTyp (s, typ) -> "`" ^ s (* TODO *)
    | GenTyp -> "genType"


let rec string_of_exp (e: exp) : string =
    let string_of_arr (a: texp list) : string = 
        "["^(String.concat ", " (List.map string_of_exp (List.map fst a)))^"]"
    in
    match e with
    | Val v -> string_of_value v
    | Arr a -> string_of_arr a
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
    (String.concat ", " (List.map string_of_exp a))

let string_of_param (i, e) : string = 
    (string_of_typ e) ^ " " ^ i

let rec string_of_params (p: params) : string = 
    (String.concat ", " (List.map string_of_param p))

let rec string_of_comm (c: comm) : string =
    match c with
    | Skip -> "skip;"
    | Print (e, _) -> "print " ^ (string_of_exp e) ^ ";"
    | Decl (t, s, (e, _)) -> (string_of_typ t)^" " ^ s ^ " = " ^ (string_of_exp e) ^ ";"
    | If ((b, _), c1, c2) -> "if (" ^ (string_of_exp b) ^ ") {\n" ^ (string_of_comm_list c1) ^
        "} else {\n" ^ (string_of_comm_list c2) ^ "}"
    | Assign (b, (x, _)) -> b ^ " = " ^ (string_of_exp x) ^ ";"
    | Return Some (x, _) -> "return " ^ (string_of_exp x) ^ ";"
    | Return None -> "return;"
    | FnCall (id, args) -> id ^ "(" ^ (string_of_args args) ^")"

and string_of_comm_list (cl : comm list) : string = 
   (String.concat "\n" (List.map string_of_comm cl))

let string_of_fn ((((id, (p, rt)), cl)) : fn) : string = 
    match id with 
    | "main" -> "void main() {" ^ (string_of_comm_list cl) ^ "}"
    | _ -> (string_of_typ rt) ^ " " ^ id ^ "(" ^ (string_of_params p) ^ "){" ^ (string_of_comm_list cl) ^ "}"

let string_of_prog (e : prog) : string =
    (String.concat "" (List.map string_of_fn e))
