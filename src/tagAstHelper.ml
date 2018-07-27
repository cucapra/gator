(* Tag AST pretty printer *)

open CoreAst
open CoreAstHelper
open TagAst


let string_of_tag_typ (t: tag_typ) : string =
    match t with
    | TopTyp n -> "vec"^(string_of_int n)
    | BotTyp n -> "vec"^(string_of_int n)^"lit"
    | VarTyp s -> s

let rec string_of_typ (t: typ) : string = 
    match t with
    | UnitTyp -> "unit"
    | BoolTyp -> "bool"
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | TagTyp s -> string_of_tag_typ s
    | TransTyp (s1, s2) -> (string_of_tag_typ s1) ^ "->" ^ (string_of_tag_typ s2)
    | SamplerTyp i -> "sampler" ^ (string_of_int i) ^ "D"

let rec string_of_exp (e:exp) : string =
    match e with
    | Val v -> string_of_value v
    | Var v -> v
    | Unop (op, x) -> (string_of_unop op (string_of_exp x))
    | Binop (op, l, r) -> 
        let ls = (string_of_exp l) in
        let rs = (string_of_exp r) in
        (match op with
        | Dot -> (string_of_binop op ls rs)
        | _ -> (string_of_binop op ls rs))
    | VecTrans (i, t) -> failwith "Unimplemented"

let rec string_of_params (p: (id * typ) list) : string =
    match p with
    | [] -> ""
    | (i1, t1)::t -> i1 ^ (string_of_typ t1) ^ " " ^ (string_of_params t)

let string_of_fn_decl (d: fn_decl) : string = 
    match d with
    | (id, p, r) -> (string_of_typ r) ^ " " ^ id ^ " (" ^ (string_of_params p) ^ ")"

let rec string_of_comm (c: comm) : string =
    match c with
    | Skip -> "skip;"
    | Print e -> "print " ^ (string_of_exp e) ^ ";"
    | Decl (t, s, e) -> (string_of_typ t)^" " ^ s ^ " = " ^ (string_of_exp e) ^ ";"
    | If (b, c1, c2) -> "if (" ^ (string_of_exp b) ^ ") {\n" ^ (string_of_comm_list c1) ^
        "} else {\n" ^ (string_of_comm_list c2) ^ "}"
    | Assign (b, x) -> b^" = " ^ (string_of_exp x) ^ ";"
    | Fn (d, c1) -> string_of_fn_decl d ^ "{" ^ (string_of_comm_list c1) ^"}"

and 
string_of_comm_list (cl : comm list) : string = 
    match cl with
    | [] -> ""
    | h::t -> (string_of_comm h)^"\n"^(string_of_comm_list t)

let rec string_of_tags (t : tag_decl list) : string =
    match t with 
    | [] -> ""
    | (s, a)::t -> "tag " ^ s ^ " is "^(string_of_typ a) ^ ";\n" ^ (string_of_tags t)

let string_of_prog (e : prog) : string =
    match e with
    | Prog (t, c) -> (string_of_tags t) ^ (string_of_comm_list c) 
