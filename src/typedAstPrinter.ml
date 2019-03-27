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
    | VecTyp v -> if v = 1 then "float" else "vec" ^ (string_of_int v)
    | MatTyp (m1, m2) -> "mat" ^ (string_of_int m1) ^ "x" ^ (string_of_int m2)
    | TransTyp (s1, s2) -> (string_of_typ s1) ^ "->" ^ (string_of_typ s2)
    | SamplerTyp n -> "sampler" ^ (string_of_int n) ^ "D"
    | SamplerCubeTyp -> "samplerCube"
    | AbsTyp (s, typ) -> "`" ^ s
    | ArrTyp (t, c) -> string_of_typ t ^ "[" ^ string_of_constvar c ^ "]"

let rec string_of_constraint (t: constrain) : string =
    match t with
    | AnyTyp -> "any"
    | GenTyp -> "genType"
    | GenMatTyp -> "mat"
    | GenVecTyp -> "vec"
    | GenSpaceTyp -> "space"
    | ETypConstraint t -> string_of_typ t

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
    (string_of_constraint e) ^ " " ^ i

let rec string_of_params (p: params) : string =
  String.concat ", " (List.map (fun (i, t) -> (string_of_typ t) ^ " " ^ i) p)

let rec string_of_global_vars (gv: global_vars) : string =
  String.concat "\n" (List.map 
  (fun (i, s, t, v) -> (string_of_storage_qual s) ^ " " ^ (string_of_typ t) ^ " " ^ i 
  ^ string_of_option_removed (fun x -> "= " ^ string_of_value x) v ^ ";") gv)

let string_of_parameterization (pm : parameterization) : string = 
  Assoc.to_string string_of_constraint pm

let rec string_of_comm (c: comm) : string =
    match c with
    | Skip -> "skip;"
    | Print (e, _) -> "print " ^ (string_of_exp e) ^ ";"
    | Inc (x, _) -> x ^ "++"
    | Dec (x, _) -> x ^ "--"
    | Decl (t, s, (e, _)) -> (string_of_typ t)^" " ^ s ^ " = " ^ (string_of_exp e) ^ ";"
    | Assign (b, (x, _)) -> b ^ " = " ^ (string_of_exp x) ^ ";"
    | AssignOp ((x, _), op, (e, _)) -> x ^ " " ^  binop_string op ^ "= " ^ (string_of_exp e)
    | If (((b, _), c1), elif_list, c2) -> 
        let block_string c = "{\n " ^ (string_of_comm_list c) ^ "}" in
        let rec string_of_elif lst = (match lst with 
        | [] -> "" 
        | ((b, _), c)::t -> "elif (" ^ (string_of_exp b) ^ ")" ^ block_string c ^ (string_of_elif t))
        in
        "if (" ^ (string_of_exp b) ^ ") {\n" ^ (string_of_comm_list c1) ^ "} " 
        ^ string_of_elif elif_list ^ (match c2 with | None -> "" | Some c2 -> "else {\n" ^ (string_of_comm_list c2) ^ "}")
    | For (d, (b, _), u, cl) -> "for (" ^ string_of_comm d ^ string_of_exp b ^ "; " ^ string_of_comm u ^ ") {\n" ^ string_of_comm_list cl ^ "}"
    | Return Some (x, _) -> "return " ^ (string_of_exp x) ^ ";"
    | Return None -> "return;"
    | FnCall (id, args) -> id ^ "(" ^ (string_of_args args) ^")"
 
and string_of_comm_list (cl : comm list) : string = 
   (String.concat "\n" (List.map string_of_comm cl))

let string_of_fn ((((id, (p, rt, pm)), cl)) : fn) : string = 
    match id with 
    | "main" -> "void main() {" ^ (string_of_comm_list cl) ^ "}"
    | _ -> (string_of_typ rt) ^ " " ^ id ^ "<" ^ (string_of_parameterization pm) ^ ">(" ^ (string_of_params p) ^ "){" ^ (string_of_comm_list cl) ^ "}"

let string_of_prog (e : prog) : string =
    (String.concat "" (List.map string_of_fn e))
