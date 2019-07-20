(* Tag AST pretty printer *)

open CoreAst
open Util
open TagAst

let rec string_of_dexp (d : dexp) : string = 
    match d with
    | DimBinop (op, l, r) -> 
        let ls = (string_of_dexp l) in
        let rs = (string_of_dexp r) in
        (match op with
        | _ -> (string_of_binop op ls rs))
    | DimNum n -> string_of_int n
    | DimVar s -> s

let rec string_of_typ (t: typ) : string = 
    match t with
    | AutoTyp -> "auto"
    | UnitTyp -> "void"
    | BoolTyp -> "bool"
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | TopVecTyp d -> "vec<"^(string_of_dexp d) ^ ">"
    | UntaggedVecTyp n -> "vec"^(string_of_int n)
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
    | GenSpaceTyp -> "space"
    | TypConstraint t -> string_of_typ t
    | AnyTyp -> ""

let string_of_modification (m: modification) : string =
    match m with
    | Canon -> "canon"
    | Space -> "space"

let string_of_mod_list (m: modification list) : string =
    String.concat " " (List.map string_of_modification m)

let string_of_param ((ml, t, s): modification list * typ * string) : string =
    string_of_mod_list ml ^ " " ^ (string_of_typ t) ^ " " ^ s
    
let string_of_params (p: params) : string =
    "(" ^ (String.concat ", " (List.map string_of_param p)) ^ ")"

let string_of_parameterization (pm : parameterization) : string =
    Assoc.to_string string_of_constraint pm
	
let string_of_parameterization_decl (pm : parameterization_decl) : string = 
    String.concat "," 
	(List.map (fun (s, c) -> s ^ " : " ^ string_of_constraint c) pm)

let string_of_fn_type ((p, r, pm): fn_type) : string = 
    (string_of_typ r) ^ " <" ^ (string_of_parameterization pm) ^ ">" 
    ^ "(" ^ (string_of_params p) ^ ")"

let string_of_fn_type_decl ((pmd, r, p): fn_type_decl) : string = 
    (string_of_typ r) ^ " <" ^ (string_of_parameterization_decl pmd) ^ ">" 
    ^ "(" ^ (string_of_params p) ^ ")"

let string_of_fn_decl ((fm, id, ft): fn_decl) : string = 
    string_of_mod_list fm ^ id ^ " " ^ string_of_fn_type_decl ft

let rec string_of_aexp ((e, m): aexp) : string =
    string_of_exp e
and string_of_exp (e: exp) : string = 
    let string_of_arr (a: aexp list) : string = 
        "["^(String.concat ", " (List.map string_of_aexp a))^"]"
    in
    match e with
    | Val v -> string_of_value v
    | Var v -> v
    | Arr a -> string_of_arr a
    | Unop (op, x) -> (string_of_unop op (string_of_aexp x))
    | Binop (op, l, r) -> 
        let ls = (string_of_aexp l) in
        let rs = (string_of_aexp r) in
        (match op with
        | _ -> (string_of_binop op ls rs))
    | As (e, t) -> (string_of_aexp e) ^ " as " ^ (string_of_typ t)
    | In (e, t) -> (string_of_aexp e) ^ " in " ^ (string_of_typ t)
    | FnInv (i, pr, args) -> i ^ "<" ^ (string_of_lst string_of_typ pr) ^ ">" ^ "(" ^ (string_of_lst string_of_aexp args) ^ ")"

let rec string_of_acomm ((c, m) : acomm) : string = 
    string_of_comm c
and string_of_comm (c: comm) : string =
    match c with
    | Skip -> "skip;"
    | Print e -> "print " ^ (string_of_aexp e) ^ ";"
    | Inc x -> x ^ "++"
    | Dec x -> x ^ "--"
    | Decl (ml, t, s, e) -> (string_of_typ t)^" " ^ s ^ " = " ^ (string_of_aexp e) ^ ";"
    | Assign (b, x) -> b ^ " = " ^ (string_of_aexp x) ^ ";"
    | AssignOp (x, op, e) -> x ^ " " ^  binop_string op ^ "= " ^ (string_of_aexp e)
    | If ((b, c1), elif_list, c2) -> 
        let block_string c = "{\n " ^ string_of_lst string_of_acomm c ^ "}" in
        let rec string_of_elif lst = (match lst with 
        | [] -> "" 
        | (b, c)::t -> "elif (" ^ (string_of_aexp b) ^ ")" ^ block_string c ^ (string_of_elif t))
        in
        "if (" ^ (string_of_aexp b) ^ ") {\n" ^ string_of_lst string_of_acomm c1 ^ "} " 
        ^ string_of_elif elif_list ^ (match c2 with | None -> "" | Some c2 -> "else {\n" ^ string_of_lst string_of_acomm c2 ^ "}")
    | For (d, b, u, cl) -> "for (" ^ string_of_acomm d ^ string_of_aexp b ^ "; " ^ string_of_acomm u ^ ") {\n" ^ string_of_lst string_of_acomm cl ^ "}"
    | Return None -> "return;"
    | Return Some e -> "return" ^ (string_of_aexp e) ^ ";"
    | FnCall (n, _, e) -> string_of_typ n ^ "(" ^ (String.concat "," (List.map string_of_aexp e)) ^ "^" (* TODO *)

let rec string_of_tag_decl ((ml, s, pmd, t) : tag_decl) : string =
    string_of_mod_list ml ^ "tag " ^ s ^ "<" ^ (string_of_parameterization_decl pmd) ^ ">"
    ^ " is " ^ (string_of_typ t) ^ ";"

let string_of_fn ((d, c1) : fn_decl * acomm list) : string = 
    string_of_fn_decl d ^ "{" ^  string_of_lst string_of_acomm c1 ^"}"

let string_of_declare (f: fn_decl) : string = 
    "declare " ^ string_of_fn_decl f

let string_of_global_var ((ml, sq, t, x, v) : global_var) : string =
    string_of_mod_list ml ^ " " ^ string_of_storage_qual sq ^ " " ^ string_of_typ t ^ " " ^ x 
    ^ string_of_option_removed (fun x -> "= " ^ string_of_value x) v

let string_of_extern (e : extern_decl) : string = 
    match e with
    | ExternFn f -> string_of_declare f
    | ExternVar (m, t, e) -> string_of_mod_list m ^ " " ^ string_of_typ t ^ " " ^ (string_of_aexp e)
    
let string_of_term(t : term) : string = 
    match t with
    | TagDecl d -> string_of_tag_decl d
    | ExternDecl e -> string_of_extern e
    | GlobalVar g -> string_of_global_var g
    | Fn f -> string_of_fn f
let string_of_aterm((t, m) : aterm) : string = 
    string_of_term t

let string_of_prog (tl : prog) : string =
    string_of_lst (fun t -> string_of_aterm t ^ "\n") tl; 
