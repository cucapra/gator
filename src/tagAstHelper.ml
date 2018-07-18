(* Tag AST pretty printer *)

open CoreAst
open TagAst

let string_of_vec (v: vec) : string = 
    "["^(String.concat ", " (List.map string_of_float v))^"]"

let string_of_mat (m: mat) : string = 
    "["^(String.concat ", " (List.map string_of_vec m))^"]"

let string_of_tag_typ (t: TagAst.tagtyp) : string =
    match t with
    | TopTyp n -> "vec"^(string_of_int n)
    | BotTyp n -> "vec"^(string_of_int n)^"lit"
    | VarTyp s -> s

let rec string_of_typ (t: TagAst.typ) : string = 
    match t with
    | TagAst.UnitTyp -> "unit"
    | TagAst.BoolTyp -> "bool"
    | TagAst.IntTyp -> "int"
    | TagAst.FloatTyp -> "float"
    | TagAst.TagTyp s -> string_of_tag_typ s
    | TagAst.TransTyp (s1, s2) -> (string_of_tag_typ s1) ^ "->" ^ (string_of_tag_typ s2)

let rec string_of_aval (av: avalue) : string = 
    match av with 
    | Num n -> string_of_int n
    | Float f -> string_of_float f
    | VecLit v -> string_of_vec v
    | MatLit m -> string_of_mat m

let rec string_of_exp (e:exp) : string =
    let string_of_unop (op : string) (e': exp) =
        op ^ " " ^ (string_of_exp e')
    in
    let string_of_binop (op : string) (e1 : exp) (e2 : exp) =
        op ^ " " ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2)
    in
    let string_of_inline_binop (op : string) (e1: exp) (e2: exp) =
        (string_of_exp e1) ^ " " ^ op ^ " " ^ (string_of_exp e2)
    in
    match e with
    | Aval av -> string_of_aval av
    | Bool b -> string_of_bool b
    | Var v -> v
    | Unop (op, x) -> (match op with
        | Norm -> string_of_unop "norm" x
        | Not -> string_of_unop "!" x)
    | Binop (op, l, r) -> (match op with
        | Eq -> string_of_inline_binop "==" l r
        | Leq -> string_of_inline_binop "<=" l r
        | Or -> string_of_inline_binop "||" l r
        | And -> string_of_inline_binop "&&" l r
        | Dot -> string_of_binop "dot" l r
        | Plus -> string_of_inline_binop "+" l r
        | Minus -> string_of_inline_binop "-" l r
        | Times -> string_of_inline_binop "*" l r
        | Div -> string_of_inline_binop "/" l r
        | CTimes -> string_of_inline_binop ".*" l r)
    | VecTrans (i, t) -> failwith "Unimplemented"

let rec string_of_comm (c: comm) : string =
    match c with
    | Skip -> "skip;"
    | Print e -> "print " ^ (string_of_exp e) ^ ";"
    | Decl (t, s, e) -> (string_of_typ t)^" " ^ s ^ " = " ^ (string_of_exp e) ^ ";"
    | If (b, c1, c2) -> "if (" ^ (string_of_exp b) ^ ") {\n" ^ (string_of_comm_list c1) ^
        "} else {\n" ^ (string_of_comm_list c2) ^ "}"
    | Assign (b, x) -> b^" = " ^ (string_of_exp x) ^ ";"

and 
string_of_comm_list (cl : comm list) : string = 
    match cl with
    | [] -> ""
    | h::t -> (string_of_comm h)^"\n"^(string_of_comm_list t)

let rec string_of_tags (t : tagdecl list) : string =
    match t with 
    | [] -> ""
    | (s, a)::t -> "tag " ^ s ^ " is "^(string_of_typ a) ^ ";\n" ^ (string_of_tags t)

let string_of_prog (e : prog) : string =
    match e with
    | Prog (t, c) -> (string_of_tags t) ^ (string_of_comm_list c) 
