open Ast
open Context
open Lin_ops
open Util

type delta = (ltyp, ltyp) Context.context

let string_of_no_paren_vec (v: vec) : string = 
    (String.concat ", " (List.map string_of_float v))

let string_of_gl_mat (m: mat) : string = 
    "("^(String.concat ", " (List.map string_of_no_paren_vec m))^")"

let rec get_dim (lt : ltyp) (d : delta) : int =
    match lt with
    | VecTyp i -> i
    | TagTyp t -> get_dim (Context.lookup d lt) d
    | _ -> failwith "Bad use of get_dim"

let string_of_ltyp (lt : ltyp) (d : delta) : string = 
    match lt with
    | VecTyp i -> "vec" ^ (string_of_int i)
    | MatTyp (r, c) -> if r = c then "mat" ^ (string_of_int r) else
        "mat" ^ (string_of_int r) ^ "x" ^ (string_of_int c)
    | TagTyp t -> "vec" ^ (string_of_int (get_dim lt d))
    | TransTyp (t1, t2) -> let r = (get_dim t1 d) in
                            let c = (get_dim t2 d) in
                            (if r = c then "mat" ^ (string_of_int r) else
                            "mat" ^ (string_of_int r) ^ "x" ^ (string_of_int c))

let string_of_atyp (at : atyp) (d : delta) : string =
    match at with
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | LTyp lt -> string_of_ltyp lt d

let string_of_typ (t : typ) (d : delta) : string =
    match t with
    | UnitTyp -> failwith "Unit type is unwriteable in glsl"
    | BTyp -> "bool"
    | ATyp at -> string_of_atyp at d

let attrib_type (var_name : string) : string =
    if (String.get var_name 0) = 'a' then "attribute" else
    (if (String.get var_name 0) = 'v' then "varying" else
    (if (String.get var_name 0) = 'u' then "uniform" else
    failwith "Not a supported glsl attribute"))

(* Ignore original declarations of apptributes and the like *)
let check_name (var_name : string) : bool = 
    let decl_reg = Str.regexp "[auv][A-Z]" in
        Str.string_match decl_reg var_name 0

(* Don't write the type of gl_Position or gl_FragColor *)
let is_core (var_name : string) : bool = 
    var_name = "gl_Position" || var_name = "gl_FragColor"

let rec comp_exp (e : exp) (d : delta) : string =
    let op_wrap (op : exp) (d : delta) : string =
        match op with
        | Aval _ 
        | Bool _ -> comp_exp op d
        | _ -> "(" ^ (comp_exp op d) ^ ")"
    in
    match e with
    | Aval a -> (match a with 
        | Num n -> string_of_int n
        | Float f -> string_of_float f
        | VecLit (v, t) -> (string_of_ltyp t d) ^ (string_of_vec v)
        | MatLit (m, t) -> (string_of_ltyp t d) ^ (string_of_gl_mat m))
    | Bool b -> string_of_bool b 
    | Typ _ -> failwith "Cannot evaluate a type expression?"
    | Var x -> x
    | Dot (e1, e2) -> "dot(" ^ (comp_exp e1 d) ^ ", " ^ (comp_exp e2 d) ^ ")"
    | Norm e -> "norm(" ^ (comp_exp e d) ^ ")"
    | Plus (e1, e2) -> (op_wrap e1 d) ^ " + " ^ (comp_exp e2 d)
    | Times (e1, e2) -> (op_wrap e1 d) ^ " * " ^ (comp_exp e2 d)
    | Minus (e1, e2) -> (op_wrap e1 d) ^ " - " ^ (comp_exp e2 d)
    | CTimes (e1, e2) -> (op_wrap e1 d) ^ " * " ^ (comp_exp e2 d)
    | Eq (e1, e2) -> (op_wrap e1 d) ^ " == " ^ (comp_exp e2 d)
    | Leq (e1, e2) -> (op_wrap e1 d) ^ " <= " ^ (comp_exp e2 d)
    | Or (e1, e2) -> (op_wrap e1 d) ^ " || " ^ (comp_exp e2 d)
    | And (e1, e2) -> (op_wrap e1 d) ^ " && " ^ (comp_exp e2 d)
    | Not e -> "!" ^ (op_wrap e d)

let rec comp_comm (c : comm list) (d : delta) : string =
    match c with
    | [] -> ""
    | h::t -> match h with
        | Skip -> comp_comm t d
        | Print e -> comp_comm t d
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | Decl (ty, x, e) -> if check_name x then comp_comm t d else
            (if is_core x then "" else string_of_typ ty d ^ " ")
            ^ x ^ " = " ^ (comp_exp e d) ^ ";"
            ^ (comp_comm t d)
        | If (e, c1, c2) -> "if " ^ "(" ^ (comp_exp e d) ^ ")"
            ^ "{ " ^ (comp_comm c1 d) ^ " }"
            ^ "{ " ^ (comp_comm c1 d) ^ " }" 
            ^ (comp_comm t d)


let rec decl_attribs (c : comm list) (d : delta) : string = 
    match c with
    | [] -> ""
    | h::t -> match h with
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | Decl (ty, x, e) -> if check_name x then 
            (attrib_type x) ^ " " ^ (string_of_typ ty d) ^ " " ^ x ^ ";" else
            decl_attribs t d
        | _ -> decl_attribs t d

let rec build_delta (tl : tagdecl list) (d : delta) : delta =
    match tl with
    | [] -> d
    | (tag,at)::t -> (match at with
        | LTyp lt -> (build_delta t (Context.update d (TagTyp tag) lt))
        | _ -> failwith "Typechecker failure -- cannot tag a non-linear type"
    )

let rec compile_program (p : prog) : string =
    match p with
    | Prog (tl, c) -> let d = build_delta tl Context.empty in
        "{ \n    \"main\": \"precision mediump float;" 
            ^ (decl_attribs c d) ^ " void main() { " ^ comp_comm c d ^ " }\"\n}"