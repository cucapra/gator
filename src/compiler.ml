open Ast
open Context
open Lin_ops
open Util

type delta = (ltyp, ltyp) Context.context
type epsilon = (id, ltyp) Context.context

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
    string_of_mat_padded tm (max r c)

let rec get_dim (lt : ltyp) (d : delta) : int =
    match lt with
    | VecTyp i -> i
    | TagTyp t -> get_dim (Context.lookup d lt) d
    | _ -> failwith "Bad use of get_dim"

let rec reduce_ltyp (l : ltyp) (d : delta) : ltyp_top =
    match l with
    | VecTyp s -> VecDim s
    | MatTyp (r, c) -> MatDim (r, c)
    | TagTyp t -> VecDim (get_dim l d)
    | TransTyp (t1, t2) -> MatDim ((get_dim t2 d), (get_dim t1 d))

let string_of_ltyp_top (lt : ltyp_top) : string =
    match lt with
    | VecDim i -> "vec" ^ (string_of_int i)
    (* Note the padding of matrix dimensions for square matrices *)
    | MatDim (r, c) -> "mat" ^ (string_of_int (max r c))

let string_of_ltyp (lt : ltyp) (d : delta) : string = 
    string_of_ltyp_top (reduce_ltyp lt d)

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

let rec exp_ltyp (e : exp) (d : delta) (eps : epsilon) : ltyp_top option =
    let ltyp_meet (left : ltyp_top option) (right : ltyp_top option) : ltyp_top option =
        match left with
        | None -> None
        | Some x -> (match right with
            | None -> None
            | Some y -> if x = y then Some x else failwith "Typechecker failure (bad dimensions)")
    in
    match e with
    | Aval a -> (match a with 
        | VecLit (_, t) -> Some (reduce_ltyp t d)
        | MatLit (_, t) -> Some (reduce_ltyp t d)
        | _ -> None)
    | Var x -> (try Some (reduce_ltyp (Context.lookup eps x) d)
                with | Not_found -> None)
    | Typ _ -> failwith "Cannot evaluate a type expression?"
    | Times (e1, e2) -> (match (exp_ltyp e1 d eps) with
        | None -> None
        | Some left -> (match (exp_ltyp e2 d eps) with
            | None -> None
            | Some right -> (match left with
                | VecDim _ -> failwith "Typechecker failure (cannot multiply a vector by a matrix/vector)"
                | MatDim (r, _) -> (match right with
                    | VecDim _ -> Some (VecDim r)
                    | MatDim (_, c) -> Some (MatDim (r, c))))))
    | Plus (e1, e2) -> ltyp_meet (exp_ltyp e1 d eps) (exp_ltyp e2 d eps)
    | Minus (e1, e2) -> ltyp_meet (exp_ltyp e1 d eps) (exp_ltyp e2 d eps)
    | CTimes (e1, e2) -> ltyp_meet (exp_ltyp e1 d eps) (exp_ltyp e2 d eps)
    | _ -> None

let rec comp_exp (e : exp) (d : delta) (eps : epsilon) : string =
    let op_wrap (op : exp) (d : delta) (eps : epsilon) : string =
        match op with
        | Aval _ 
        | Bool _ 
        | Var _ -> comp_exp op d eps
        | _ -> "(" ^ (comp_exp op d eps) ^ ")"
    in
    (* Handles the string shenanigans for padding during multiplication *)
    let padded_mult (left : exp) (right : exp) (d : delta) (eps : epsilon) : string =
        match ((exp_ltyp left d eps), (exp_ltyp right d eps)) with
        | (Some lt, Some rt) -> (match lt with
            | VecDim _ -> failwith "Strange Failure: cannot multiply a vector by a vector or matrix"
            | MatDim (dim, _) -> (match rt with
                | VecDim rdim -> (if dim = rdim then 
                        (op_wrap left d eps) ^ " * " ^ (op_wrap right d eps)
                        else if dim > rdim then 
                        (op_wrap left d eps) ^ " * " ^ "vec" ^ (string_of_int dim) ^ "(" ^ 
                        (comp_exp right d eps) ^ (repeat ", 0." (dim - rdim)) ^ ")"
                        else  (* dim < rdim *)
                        "vec" ^ (string_of_int dim) ^ "(" ^ 
                        (op_wrap left d eps) ^ " * " ^ 
                        (comp_exp right d eps) ^ ")")
                    
                | MatDim (_, ldim) -> 
                    (if dim = ldim then ((op_wrap left d eps) ^ " * " ^ (op_wrap right d eps))
                    else (op_wrap left d eps) ^ " * " ^ (comp_exp right d eps))))
        | _ -> (op_wrap left d eps) ^ " * " ^ (comp_exp right d eps)
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
    | Dot (e1, e2) -> "dot(" ^ (comp_exp e1 d eps) ^ ", " ^ (comp_exp e2 d eps) ^ ")"
    | Norm e -> "normalize(" ^ (comp_exp e d eps) ^ ")"
    | Plus (e1, e2) -> (op_wrap e1 d eps) ^ " + " ^ (op_wrap e2 d eps)
    | Times (e1, e2) -> padded_mult e1 e2 d eps
    | Minus (e1, e2) -> (op_wrap e1 d eps) ^ " - " ^ (op_wrap e2 d eps)
    | Div (e1, e2) -> (op_wrap e1 d eps) ^ " / " ^ (op_wrap e2 d eps)
    | CTimes (e1, e2) -> (op_wrap e1 d eps) ^ " * " ^ (op_wrap e2 d eps)
    | Eq (e1, e2) -> (op_wrap e1 d eps) ^ " == " ^ (op_wrap e2 d eps)
    | Leq (e1, e2) -> (op_wrap e1 d eps) ^ " <= " ^ (op_wrap e2 d eps)
    | Or (e1, e2) -> (op_wrap e1 d eps) ^ " || " ^ (op_wrap e2 d eps)
    | And (e1, e2) -> (op_wrap e1 d eps) ^ " && " ^ (op_wrap e2 d eps)
    | Not e -> "!" ^ (op_wrap e d eps)

let rec comp_comm (c : comm list) (d : delta) (eps : epsilon) : string =
    let update_eps (eps : epsilon) (ty : typ) (x : id) : epsilon =
        match ty with
        | ATyp a -> (match a with
            | LTyp l -> (Context.update eps x l)
            | _ -> eps)
        | _ -> eps
    in
    match c with
    | [] -> ""
    | h::t -> match h with
        | Skip -> comp_comm t d eps
        | Print e -> comp_comm t d eps
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | Decl (ty, x, e) -> let eps' = (update_eps eps ty x) in
            (if check_name x then comp_comm t d eps' else
            (if is_core x then "" else string_of_typ ty d ^ " ")
            ^ x ^ " = " ^ (comp_exp e d eps') ^ ";"
            ^ (comp_comm t d eps'))
        | Assign (x, e) -> x ^ " = " ^ (comp_exp e d eps) ^ ";" ^ (comp_comm t d eps)
        | If (e, c1, c2) -> ("if " ^ "(" ^ (comp_exp e d eps) ^ ")"
            ^ "{ " ^ (comp_comm c1 d eps) ^ " }"
            ^ "{ " ^ (comp_comm c2 d eps) ^ " }" 
            ^ (comp_comm t d eps))


let rec decl_attribs (c : comm list) (d : delta) : string = 
    match c with
    | [] -> ""
    | h::t -> match h with
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | Decl (ty, x, e) -> if check_name x then 
            (attrib_type x) ^ " " ^ (string_of_typ ty d) ^ " " ^ x ^ ";" ^ (decl_attribs t d) else
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
        "\"precision highp float;" 
            ^ (decl_attribs c d) ^ " void main() { " ^ (comp_comm c d Context.empty) ^ " }\""