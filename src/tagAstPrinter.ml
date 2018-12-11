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
    | VarTyp (s, t) -> s ^ (if List.length t > 0 then "<" ^ (string_of_lst string_of_typ t) ^ ">" else "")
    | TransTyp (s1, s2) -> (string_of_typ s1) ^ "->" ^ (string_of_typ s2)
    | SamplerTyp i -> "sampler" ^ (string_of_int i) ^ "D"
    | SamplerCubeTyp -> "samplerCube"
    | AbsTyp s -> "`" ^ s

let string_of_constraint (c: constrain) : string =
    match c with
    | GenTyp -> "genTyp"
    | GenMatTyp -> "mat"
    | GenVecTyp -> "vec"
    | TypConstraint t -> string_of_typ t
    | AnyTyp -> ""

let string_of_param ((s, t): string * typ) : string =
    (string_of_typ t) ^ " " ^ s
    
let string_of_params (p: params) : string =
    "(" ^ (String.concat ", " (List.map string_of_param p)) ^ ")"

let string_of_parameterization (pm : parameterization) : string = 
    Assoc.to_string string_of_constraint pm

let string_of_fn_type ((fm, p, r, pm): fn_type) : string = (match fm with | Some Canon -> "canon " | None -> "") ^ 
    (string_of_typ r) ^ " <" ^ (string_of_parameterization pm) ^ ">" ^ "(" ^ (string_of_params p) ^ ")"

let string_of_fn_decl (d: fn_decl) : string = 
    match d with
    | (id, (fm, p, r, pm)) -> (match fm with | Some Canon -> "canon " | None -> "") ^
        (string_of_typ r) ^ " " ^ id ^ " <" ^ (string_of_parameterization pm) ^ ">" ^ " (" ^ (string_of_params p) ^ ")"

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
    | Decl (t, None, s, e) -> (string_of_typ t)^" " ^ s ^ " = " ^ (string_of_exp e) ^ ";"
    | Decl (t, _, s, e) -> failwith "unsupported"
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
    | FnCall (id, e, _) -> id ^ "(" ^ (String.concat "," (List.map string_of_exp e)) ^ "^" (* TODO *)
    

and 
string_of_comm_list (cl : comm list) : string = 
    string_of_lst string_of_comm cl

let rec string_of_tags (t : tag_decl list) : string =
    match t with | [] -> "" | (m, s, a)::t -> 
    let ms = match m with | None -> "" | Some Coord -> " coord " in
    "tag " ^ ms ^ s ^ " is "^(string_of_typ a) ^ ";\n" ^ (string_of_tags t)

let string_of_fn (f : fn) : string = 
    match f with
    | (d, c1) -> string_of_fn_decl d ^ "{" ^ (string_of_comm_list c1) ^"}"

let rec string_of_fn_lst (fl : fn list) : string = 
    string_of_lst string_of_fn fl

let string_of_declare (f: fn) : string = 
    "declare " ^ string_of_fn f

let string_of_declare_lst (fl : fn list) : string = 
    string_of_lst string_of_declare fl

let string_of_prog (e : prog) : string =
    match e with
    | Prog (d, t, f) -> (string_of_tags t) ^ (string_of_fn_lst f) 
