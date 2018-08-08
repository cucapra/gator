open CoreAst
open TypedAst
open TypedAstPrinter
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
  
let string_of_gl_mat (m: mat) : string = 
    (* Note the transpose to match the glsl column-oriented style *)
    let tm = Lin_ops.transpose m in
    let r = (List.length tm) in
    let c = (if r = 0 then 0 else List.length (List.hd tm)) in
    let dim = max r c in
    ("mat"^(string_of_int dim)^string_of_mat_padded tm dim)

let string_of_gl_typ (t : etyp) : string =
    match t with
    | UnitTyp -> failwith "Unit type is unwriteable in glsl"
    | MatTyp (m, n) -> "mat" ^ string_of_int (max m n)
    | _ -> string_of_typ t

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

let rec op_wrap (op : exp) : string =
    match op with
    | Val _
    | Var _ -> comp_exp op
    | _ -> "(" ^ (comp_exp op) ^ ")"

(* Handles the string shenanigans for padding during multiplication *)
and padded_mult (left : texp) (right : texp) : string =
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
        
and padded_args (a: exp list) : string = 
    (String.concat ", " (List.map (op_wrap) a))

and comp_exp (e : exp) : string =
    
    match e with
    | Val v -> (match v with 
        | MatLit m -> string_of_gl_mat m
        | _ -> string_of_value v)
    | Var v -> v
    | Binop (op, l, r) -> (match op with
        | Times -> padded_mult l r
        | CTimes -> "(" ^ ((comp_exp (fst l)) ^ " * " ^(comp_exp (fst r))) ^ ")"
        | _ -> "(" ^ (string_of_binop op (comp_exp (fst l)) (comp_exp (fst r))) ^ ")")
    | Unop (op, (x, _)) -> (string_of_unop op ("(" ^ (comp_exp x) ^ ")"))
    | FnInv (id, args) -> id ^ "(" ^ (padded_args args) ^ ")"
 
and comp_comm (c : comm list) : string =
    match c with
    | [] -> ""
    | h::t -> match h with
        | Skip -> comp_comm t
        | Print e -> comp_comm t
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | Decl (ty, x, (e, _)) -> (
            if check_name x then ""^ (comp_comm t) else  
            if is_core x  then x ^ " = " ^ (comp_exp e) ^ ";" ^ (comp_comm t)
            else string_of_gl_typ ty ^ " "^ x ^ " = " ^ (comp_exp e) ^ ";" ^ (comp_comm t))
        | Assign (x, (e, _)) -> x ^ " = " ^ (comp_exp e) ^ ";" ^ (comp_comm t)
        | If ((e, _), c1, c2) -> ("if " ^ "(" ^ (comp_exp e) ^ ")"
            ^ "{ " ^ (comp_comm c1) ^ " }"
            ^ "{ " ^ (comp_comm c2) ^ " }" 
            ^ (comp_comm t))
        | Return _ -> string_of_comm h ^ (comp_comm t)
        | FnCall (id, args) -> id ^ "(" ^ (padded_args args) ^ ")"
        

let comp_fn (((id, (p, rt)), cl) : fn) : string = 
    match id with 
    | "main" -> "void main() {" ^ (comp_comm cl) ^ "}"
    | _ -> (string_of_gl_typ rt) ^ " " ^ id ^ "(" ^ (string_of_params p) ^ "){" ^ (comp_comm cl) ^ "}"

let rec comp_fn_lst (f : fn list) : string =
    match f with 
    | [] -> ""
    | h::t -> (comp_fn h) ^ (comp_fn_lst t)

let rec decl_attribs (p : TypedAst.params) : string = 
    match p with
    | [] -> ""
    | h::t -> match h with
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | (x, et) -> if check_name x then 
            (attrib_type x) ^ " " ^ (string_of_gl_typ et) ^ " " ^ x ^ ";" ^ (decl_attribs t) else
            decl_attribs t

let rec compile_program (prog : prog) (params: TypedAst.params) : string =
    "precision highp float;" ^ (decl_attribs params) ^ 
     (comp_fn_lst prog)
 