open Ast
open Context
open Lin_ops
open Util

type delta = (ltyp, ltyp) Context.context

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
        | VecLit (v, _) -> string_of_vec v
        | MatLit (m, _) -> string_of_mat m)
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

let rec get_dim (lt : ltyp) (d : delta) : string =
    match lt with
    | VecTyp i -> (string_of_int i)
    | TagTyp t -> get_dim (Context.lookup d lt) d
    | _ -> failwith "Bad use of get_dim"

let ltyp_to_string (lt : ltyp) (d : delta) : string = 
    match lt with
    | VecTyp i -> "vec" ^ (string_of_int i)
    | MatTyp (r, c) -> "mat" ^ (string_of_int r) ^ "x" ^ (string_of_int c)
    | TagTyp t -> "vec" ^ (get_dim lt d)
    | TransTyp (t1, t2) -> "mat" ^ (get_dim t1 d)
                        ^ "x" ^ (get_dim t2 d)

let atyp_to_string (at : atyp) (d : delta) : string =
    match at with
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | LTyp lt -> ltyp_to_string lt d

let typ_to_string (t : typ) (d : delta) : string =
    match t with
    | UnitTyp -> failwith "Unit type is unwriteable in glsl"
    | BTyp -> "bool"
    | ATyp at -> atyp_to_string at d

(* Ignore original declarations of apptributes and the like *)
let check_name (var_name : string) : bool = 
    let decl_reg = Str.regexp "[auv][A-Z]" in
        Str.string_match decl_reg var_name 0

(* Don't write the type of gl_Position or gl_FragColor *)
let is_core (var_name : string) : bool =
    var_name == "gl_Position" || var_name == "gl_FragColor"

let rec comp_comm (c : comm list) (d : delta) : string =
    match c with
    | [] -> ""
    | h::t -> match h with
        | Skip -> comp_comm t d
        | Print e -> comp_comm t d
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | Decl (ty, x, e) -> if check_name x then comp_comm t d else
            (if is_core x then "" else typ_to_string ty d ^ " ")
            ^ x ^ " = " ^ (comp_exp e d) ^ ";\n"
            ^ (comp_comm t d)
        | If (e, c1, c2) -> "if " ^ "(" ^ (comp_exp e d) ^ ")\n"
            ^ "{ " ^ (comp_comm c1 d) ^ " }\n"
            ^ "{ " ^ (comp_comm c1 d) ^ " }\n" 
            ^ (comp_comm t d)

let rec build_delta (tl : tagdecl list) (d : delta) : delta =
    match tl with
    | [] -> d
    | (tag,at)::t -> (match at with
        | LTyp lt -> (build_delta t (Context.update d (TagTyp tag) lt))
        | _ -> failwith "Typechecker failure -- cannot tag a non-linear type"
    )

let rec compile_program (p : prog) : string =
    match p with
    | Prog (tl, c) -> comp_comm c (build_delta tl Context.empty)