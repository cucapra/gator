open CoreAst
open TypedAst
open TypedAstHelper
open Assoc
open Lin_ops
open Util

(* type epsilon = (id, etyp) Assoc.context *)

type ltyp_top =
    | VecDim of int
    | MatDim of int * int

(* Note the column parameter for padding the matrix size *)
let string_of_no_paren_vec (v: vec) (padding: int) : string = 
    (String.concat ", " (List.map string_of_float v)) ^ (repeat ", 0." padding)

let string_of_mat_padded (m: mat) (max_dim: int) : string =
    let string_of_vec_padded = (fun v -> (string_of_no_paren_vec v (max_dim - List.length v))) in
    ("(" ^ (String.concat ", " (List.map string_of_vec_padded m)) ^
    (repeat (string_of_no_paren_vec [] max_dim) (max_dim - List.length m)) ^ ")")



let string_of_typ (t : etyp) : string =
    match t with
    | UnitTyp -> failwith "Unit type is unwriteable in glsl"
    | BoolTyp -> "bool"
    | IntTyp -> "int"
    | FloatTyp -> "float"
    | VecTyp n -> "vec" ^ (string_of_int n)
    | MatTyp (m, n) -> "mat" ^ string_of_int (max m n)

let attrib_type (var_name : string) : string =
    if (String.get var_name 0) = 'a' then "attribute" else
    (if (String.get var_name 0) = 'v' then "varying" else
    (if (String.get var_name 0) = 'u' then "uniform" else
    failwith "Not a supported glsl attribute"))

(* Ignore original declarations of attributes and the like *)
let check_name (var_name : string) : bool = 
    let decl_reg = Str.regexp "[auv][A-Z]" in
        Str.string_match decl_reg var_name 0

(* Don't write the type of gl_Position or gl_FragColor *)
let is_core (var_name : string) : bool = 
    var_name = "gl_Position" || var_name = "gl_FragColor"

let rec comp_exp (e : exp) : string =
    let op_wrap (op : exp) : string =
        match op with
        | Val _
        | Var _ -> comp_exp op
        | _ -> "(" ^ (comp_exp op) ^ ")"
    in

    (* Handles the string shenanigans for padding during multiplication *)
    let padded_mult (left : texp) (right : texp) : string =
        (* Printf.printf "\t\t\t%s\n" (string_of_exp e);  *)
        match (left, right) with
        | ((le, lt), (re, rt)) -> (match (lt, rt) with
            | (MatTyp (ldim , _), VecTyp rdim) -> 
                        (if ldim = rdim then  (
                        (op_wrap le) ^ " * " ^ (op_wrap re) )
                        else if ldim > rdim then (
                        (op_wrap le) ^ " * " ^ "vec" ^ (string_of_int ldim) ^ "(" ^ 
                        (comp_exp re) ^ (repeat ", 0." (ldim - rdim)) ^ ")" )
                        else  (* dim < rdim *) (
                        "vec" ^ (string_of_int ldim) ^ "(" ^ 
                        (op_wrap le) ^ " * " ^ 
                        (comp_exp re) ^ ")") )
            | (MatTyp (ldim, _), MatTyp (_, rdim)) -> 
                    (if ldim = rdim then ((op_wrap le) ^ " * " ^ (op_wrap re))
                    else (op_wrap le) ^ " * " ^ (comp_exp re))
            | _ -> (op_wrap le) ^ " * " ^ (comp_exp re))
    in
    match e with
    | (Binop (Times, e1, e2)) -> padded_mult e1 e2
    | (Binop (Eq, (le, lt), (re, rt))) -> (op_wrap le) ^ " == " ^ (op_wrap re)
    | (Binop (Leq, (le, lt), (re, rt))) -> (op_wrap le) ^ " <= " ^ (op_wrap re)
    | (Binop (Or, (le, lt), (re, rt))) -> (op_wrap le) ^ " || " ^ (op_wrap re)
    | (Binop (And, (le, lt), (re, rt))) -> (op_wrap le) ^ " && " ^ (op_wrap re)
    | (Binop (Dot, (le, lt), (re, rt))) -> "dot(" ^ (op_wrap le) ^ " ,    " ^ (op_wrap re)^")"
    | (Binop (Plus, (le, lt), (re, rt))) -> (op_wrap le) ^ " + " ^ (op_wrap re)
    | (Binop (Minus, (le, lt), (re, rt))) -> (op_wrap le) ^ " - " ^ (op_wrap re)
    | (Binop (Div, (le, lt), (re, rt))) -> (op_wrap le) ^ " / " ^ (op_wrap re)
    | (Binop (CTimes, (le, lt), (re, rt))) -> (op_wrap le) ^ " ,* " ^ (op_wrap re)
    | _ -> string_of_exp e

let rec comp_comm (c : comm list) : string =
    match c with
    | [] -> ""
    | h::t -> match h with
        | Skip -> comp_comm t
        | Print e -> comp_comm t
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | Decl (ty, x, (e, _)) -> (
            if check_name x then ""^ (comp_comm t) else  
            if is_core x  then x ^ " = " ^ (comp_exp e) ^ ";" ^ (comp_comm t)
            else string_of_typ ty ^ " "^ x ^ " = " ^ (comp_exp e) ^ ";" ^ (comp_comm t))
        | Assign (x, (e, _)) -> x ^ " = " ^ (comp_exp e) ^ ";" ^ (comp_comm t)
        | If ((e, _), c1, c2) -> ("if " ^ "(" ^ (comp_exp e) ^ ")"
            ^ "{ " ^ (comp_comm c1) ^ " }"
            ^ "{ " ^ (comp_comm c2) ^ " }" 
            ^ (comp_comm t))


let rec decl_attribs (c : comm list) : string = 
    match c with
    | [] -> ""
    | h::t -> match h with
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | Decl (ty, x, e) -> if check_name x then 
            (attrib_type x) ^ " " ^ (string_of_typ ty) ^ " " ^ x ^ ";" ^ (decl_attribs t) else
            decl_attribs t
        | _ -> decl_attribs t


let rec compile_program (p : prog) : string =
    "precision highp float;" ^ (decl_attribs p) ^ 
    " void main() { " ^ (comp_comm p) ^ " }"
