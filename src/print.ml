(* AST pretty printer *)

open Ast

let print_vec (v: vec) : string = 
    "["^(String.concat ", " (List.map string_of_float v))^"]"

let print_mat (m: mat) : string = 
    "["^(String.concat ", " (List.map print_vec m))^"]"

let rec print_ltyp (lt: ltyp) : string =
    match lt with
    | VecTyp n -> "vec"^(string_of_int n)
    | MatTyp (n1, n2) -> "mat"^(string_of_int n1)^"*"^(string_of_int n2)
    | TagTyp s -> s
    | TransTyp (lt1, lt2) -> (print_ltyp lt1)^"->"^(print_ltyp lt2)

let rec print_atyp (at: atyp) : string = 
    match at with
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | LTyp lt -> print_ltyp lt

let rec print_typ (t: typ) : string = 
    match t with
    | ATyp at -> print_atyp at 
    | BTyp -> "bool"

let rec print_aval (av: avalue) : string = 
    match av with 
    | Num n -> string_of_int n
    | Float f -> string_of_float f
    | VecLit v -> print_vec v
    | MatLit m -> print_mat m
  
let rec print_exp (e:exp) : string = 
    match e with
    | Aval av -> print_aval av
    | Bool b -> (match b with
            | true -> "true"
            | false -> "false")
    | Var v -> v
    | Lexp (a',l) -> (print_exp a')^":"^(print_ltyp l)
    | Dot (a1, a2) -> "dot "^(print_exp a1)^" "^(print_exp a2)
    | Norm a -> "norm "^(print_exp a)
    | Plus (a1, a2) -> (print_exp a1)^" + "^(print_exp a2)
    | Times (a1, a2) -> (print_exp a1)^" * "^(print_exp a2)
    | Minus (a1, a2) -> (print_exp a1)^" - "^(print_exp a2)
    | CTimes (a1, a2) -> (print_exp a1)^" .* "^(print_exp a2)
    | Eq (a1, a2) -> (print_exp a1)^" == "^(print_exp a2)
    | Leq (a1, a2) -> (print_exp a1)^" <= "^(print_exp a2)
    | Or (b1, b2) -> (print_exp b1)^" || "^(print_exp b2)
    | And (b1, b2) -> (print_exp b1)^" && "^(print_exp b2)
    | Not b' -> "!"^(print_exp b')

let rec print_comm (c: comm) : string =
    match c with
    | Skip -> "skip;"
    | Print e -> "print " ^ (print_exp e)^";"
    | Decl (t, s, e) -> (print_typ t)^" "^s^" = "^(print_exp e)^";"
    | If (b, c1, c2) -> "if ("^(print_exp b)^") {\n"^(print_comm_lst c1)^
        "} else {\n"^(print_comm_lst c2)^"}"

and 

print_comm_lst (cl : comm list) : string = 
    match cl with
    | [] -> ""
    | h::t -> (print_comm h)^"\n"^(print_comm_lst t)

let rec print_tags (t : tagdecl list) : string =
    match t with 
    | [] -> ""
    | TagDecl(s, a)::t -> "tag "^s^" is "^(print_atyp a)^";\n"^(print_tags t)

let print_prog (e : prog) : string =
    match e with
    | Prog (t, c) -> (print_tags t) ^ (print_comm_lst c) 
