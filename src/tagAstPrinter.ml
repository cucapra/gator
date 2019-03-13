(* Tag AST pretty printer *)

open CoreAst
open Util
open TagAst

let rec string_of_typ (t: typ) : string = 
    match t with
    | AutoTyp -> "auto"
    | UnitTyp -> "void"
    | BoolTyp -> "bool"
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | TopVecTyp n -> "vec"^(string_of_int n)
    | BotVecTyp n -> "vec"^(string_of_int n)^"lit"
    | VarTyp s -> s
    | ParTyp (t, tl) -> string_of_typ t ^ "<" ^ (string_of_lst string_of_typ tl) ^ ">"
    | TransTyp (s1, s2) -> (string_of_typ s1) ^ "->" ^ (string_of_typ s2)
    | SamplerTyp i -> "sampler" ^ (string_of_int i) ^ "D "
    | SamplerCubeTyp -> "samplerCube"
    | AbsTyp s -> "`" ^ s
    | ArrTyp (t, c) -> string_of_typ t ^ "[" ^ string_of_constvar c ^ "]"

let string_of_constraint (c: constrain) : string =
    match c with
    | GenTyp -> "genTyp"
    | GenMatTyp -> "mat"
    | GenVecTyp -> "vec"
    | TypConstraint t -> string_of_typ t
    | AnyTyp -> ""

let string_of_modification (m: modification) : string =
    match m with
    | Coord -> "coord"
    | Canon -> "canon"

let string_of_mod_option (m: modification option) : string =
    string_of_option_removed string_of_modification m ^ " "

let string_of_param ((s, t): string * typ) : string =
    (string_of_typ t) ^ " " ^ s
    
let string_of_params (p: params) : string =
    "(" ^ (String.concat ", " (List.map string_of_param p)) ^ ")"

let string_of_parameterization (pm : parameterization) : string = 
    Assoc.to_string string_of_constraint pm
	
let string_of_parameterization_decl (pm : parameterization_decl) : string = 
    String.concat "," 
	(List.map (fun (s, m, c) -> s ^ " : " ^ string_of_mod_option m ^ string_of_constraint c) pm)

let string_of_fn_type ((p, r, pm): fn_type) : string = 
    (string_of_typ r) ^ " <" ^ (string_of_parameterization pm) ^ ">" 
    ^ "(" ^ (string_of_params p) ^ ")"

let string_of_fn_type_decl ((p, r, pmd): fn_type_decl) : string = 
    (string_of_typ r) ^ " <" ^ (string_of_parameterization_decl pmd) ^ ">" 
    ^ "(" ^ (string_of_params p) ^ ")"

let string_of_fn_decl ((fm, id, ft): fn_decl) : string = 
    string_of_mod_option fm ^ id ^ " " ^ string_of_fn_type_decl ft

let rec string_of_exp (e:exp) : string =
    let string_of_arr (a: exp list) : string = 
        "["^(String.concat ", " (List.map string_of_exp a))^"]"
    in
    match e with
    | Val v -> string_of_value v
    | Var v -> v
    | Arr a -> string_of_arr a
    | Unop (op, x) -> (string_of_unop op (string_of_exp x))
    | Binop (op, l, r) -> 
        let ls = (string_of_exp l) in
        let rs = (string_of_exp r) in
        (match op with
        | _ -> (string_of_binop op ls rs))
    | As (e, t) -> (string_of_exp e) ^ " as " ^ (string_of_typ t)
    | In (e, t) -> (string_of_exp e) ^ " in " ^ (string_of_typ t)
    | FnInv (i, args, pr) -> i ^ "<" ^ (string_of_lst string_of_typ pr) ^ ">" ^ "(" ^ (string_of_lst string_of_exp args) ^ ")"

let rec string_of_comm (c: comm) : string =
    match c with
    | Skip -> "skip;"
    | Print e -> "print " ^ (string_of_exp e) ^ ";"
    | Inc x -> x ^ "++"
    | Dec x -> x ^ "--"
    | Decl (t, s, e) -> (string_of_typ t)^" " ^ s ^ " = " ^ (string_of_exp e) ^ ";"
    | Assign (b, x) -> b ^ " = " ^ (string_of_exp x) ^ ";"
    | AssignOp (x, op, e) -> x ^ " " ^  binop_string op ^ "= " ^ (string_of_exp e)
    | If ((b, c1), elif_list, c2) -> 
        let block_string c = "{\n " ^ (string_of_comm_list c) ^ "}" in
        let rec string_of_elif lst = (match lst with 
        | [] -> "" 
        | (b, c)::t -> "elif (" ^ (string_of_exp b) ^ ")" ^ block_string c ^ (string_of_elif t))
        in
        "if (" ^ (string_of_exp b) ^ ") {\n" ^ (string_of_comm_list c1) ^ "} " 
        ^ string_of_elif elif_list ^ (match c2 with | None -> "" | Some c2 -> "else {\n" ^ (string_of_comm_list c2) ^ "}")
    | For (d, b, u, cl) -> "for (" ^ string_of_comm d ^ string_of_exp b ^ "; " ^ string_of_comm u ^ ") {\n" ^ string_of_comm_list cl ^ "}"
    | Return None -> "return;"
    | Return Some e -> "return" ^ (string_of_exp e) ^ ";"
    | FnCall (n, e, _) -> string_of_typ n ^ "(" ^ (String.concat "," (List.map string_of_exp e)) ^ "^" (* TODO *)
    

and 
string_of_comm_list (cl : comm list) : string = 
    string_of_lst string_of_comm cl

let rec string_of_tags (t : tag_decl list) : string =
    match t with | [] -> "" | (m, s, pm, a)::t -> 
    "tag " ^ string_of_mod_option m ^ s ^ "<" ^ (string_of_parameterization_decl pm) ^ ">"
    ^ " is " ^ (string_of_typ a) ^ ";\n" ^ (string_of_tags t)

let string_of_fn ((d, c1) : fn) : string = 
    string_of_fn_decl d ^ "{" ^ (string_of_comm_list c1) ^"}"

let rec string_of_fn_lst (fl : fn list) : string = 
    string_of_lst string_of_fn fl

let string_of_declare (f: fn) : string = 
    "declare " ^ string_of_fn f

let string_of_declare_lst (fl : fn list) : string = 
    string_of_lst string_of_declare fl

let string_of_global_var ((x, sq, t, v) : global_var) : string =
    string_of_storage_qual sq ^ " " ^ string_of_typ t ^ " " ^ x 
    ^ string_of_option_removed (fun x -> "= " ^ string_of_value x) v

let string_of_global_var_lst (gvl : global_var list) : string =
    string_of_lst string_of_global_var gvl

let string_of_prog ((d, t, g, f) : prog) : string =
    (string_of_tags t) ^ (string_of_global_var_lst g) ^ (string_of_fn_lst f) 
