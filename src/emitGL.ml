open CoreAst
open TypedAst
open Assoc
open Lin_ops
open Util
open EmitUtil

(* type epsilon = (id, etyp) Assoc.context *)
type delta = (etyp list) Assoc.context

type ltyp_top =
    | VecDim of int
    | MatDim of int * int

(* Don't write the type of gl_Position or gl_FragColor *)
let is_core (var_name : string) : bool = 
    String.length var_name > 3 && Str.string_before var_name 3 = "gl_"

(* Note the column parameter for padding the matrix size *)
let rec string_of_no_paren_vec (v: texp list) (padding: int) : string = 
    string_of_list string_of_texp v ^ (repeat ", 0." padding)
  
and string_of_mat_padded (m: texp list list) (max_dim: int) : string =
    let string_of_vec_padded = fun v -> (string_of_no_paren_vec v (max_dim - List.length v)) in
    "(" ^ string_of_list string_of_vec_padded m ^
    repeat (string_of_no_paren_vec [] max_dim) (max_dim - List.length m) ^ ")"
  
and string_of_glsl_mat (m: texp list list) : string = 
    (* Note the transpose to match the glsl column-oriented style *)
    let tm = Lin_ops.transpose m in
    let r = (List.length tm) in
    let c = (if r = 0 then 0 else List.length (List.hd tm)) in
    let dim = max r c in
    "mat" ^ (string_of_int dim) ^ string_of_mat_padded tm dim

and string_of_typ (t : etyp) : string =
    match t with
    | ArrTyp (FloatTyp, d) -> "vec" ^ string_of_constvar d
    | ArrTyp (ArrTyp (FloatTyp, _), d) -> "mat" ^ string_of_constvar d
    | ParTyp (s, _) -> s
    | _ -> TypedAstPrinter.string_of_typ t

and attrib_type (var_name : string) : string =
    debug_print ">> attrib_type";
    if (String.get var_name 0) = 'a' then "attribute" else
    if (String.get var_name 0) = 'v' then "varying" else
    if (String.get var_name 0) = 'u' then "uniform" else
    failwith "Not a supported glsl attribute"

and op_wrap (op : exp) : string =
    debug_print ">> op_wrap";
    match op with
    | Val _
    | Var _ -> string_of_exp op
    | _ -> "(" ^ string_of_exp op ^ ")"

(* Handles the string shenanigans for padding during multiplication *)
(* and padded_mult (left : texp) (right : texp) : string =
    debug_print ">> padded_mult"; *)
    (* Printf.printf "\t\t\t%s\n" (string_of_exp e);  *)
    (* match (left, right) with
    | ((le, lt), (re, rt)) -> (match (lt, rt) with
        | (MatTyp (ldim , _), VecTyp rdim) -> 
                    if ldim = rdim then  (
                    (op_wrap le) ^ " * " ^ (op_wrap re) )
                    else if ldim > rdim then (
                    (op_wrap le) ^ " * " ^ "vec" ^ (string_of_int ldim) ^ "(" ^ 
                    (string_of_exp re) ^ (repeat ", 0." (ldim - rdim)) ^ ")" )
                    else  (* dim < rdim *) (
                    "vec" ^ (string_of_int ldim) ^ "(" ^ 
                    (op_wrap le) ^ " * " ^ 
                    (string_of_exp re) ^ ")")
        | (MatTyp (ldim, _), MatTyp (_, rdim)) -> 
                if ldim = rdim then ((op_wrap le) ^ " * " ^ (op_wrap re))
                else (op_wrap le) ^ " * " ^ (string_of_exp re)
        | _ -> (op_wrap le) ^ " * " ^ (string_of_exp re)) *)
and string_of_texp ((e, _): texp) : string=
    string_of_exp e
and string_of_exp (e : exp) : string =
    debug_print ">> comp_exp";
    match e with
    | Val v -> string_of_value v
    | Var v -> v
    | Arr a -> (match a with
        | [] -> "vec0()"
        | (_, t)::_ -> (match t with
            | FloatTyp | IntTyp -> "vec" ^ (string_of_int (List.length a)) 
                ^ "(" ^ string_of_list string_of_texp a ^ ")"
            | ArrTyp (FloatTyp, d) -> let as_vec_list = (fun v -> (
                match v with 
                | (Arr a', _) -> a'
                | _ -> failwith "Typechecker error, a matrix must be a list of vectors")) in
                string_of_glsl_mat (List.map as_vec_list a)
            | _ -> failwith "Typechecker error, every array must be a list of ints, floats, or vectors"))
    | Index (l, r) -> string_of_texp l ^ "[" ^ string_of_texp r ^ "]"
    | FnInv (id, tl, args) -> string_of_fn_util id (List.map string_of_texp args)

let rec string_of_comm (c: comm) : string =
    let block_string c = "{\n " ^ string_of_separated_list "" string_of_comm c ^ "}" in
    match c with
    | Skip -> "skip;"
    | Print e -> "print " ^ string_of_texp e ^ ";"
    | Exp e -> string_of_texp e ^ ";"
    | Decl (t, s, e) -> let ts = (if is_core s then "" else string_of_typ t ^ " ") in
        ts ^ s ^ " = " ^ string_of_texp e ^ ";"
    | Assign (b, x) -> b ^ " = " ^ string_of_texp x ^ ";"
    | AssignOp ((x, _), op, e) -> x ^ " " 
        ^ op ^ "= " ^ (string_of_texp e) ^ ";"
    | If ((b, c1), elif_list, c2) -> 
        "if (" ^ string_of_texp b ^ ")" ^ block_string c1 
        ^ string_of_list (fun (b, c) -> "elif (" ^ string_of_texp b ^ ")" ^ block_string c) elif_list
        ^ string_of_option_removed (fun x -> "else " ^ block_string x) c2
    | For (d, b, u, cl) -> let us = string_of_comm u in (* Hack to get rid of the semicolon at the end of the for loop *)
        "for (" ^ string_of_comm d ^ string_of_texp b ^ "; " 
        ^ String.sub us 0 (String.length us - 1) ^ ") " ^ block_string cl
    | Return x -> "return" ^ string_of_option_removed (fun x -> " " ^ string_of_texp x) x ^ ";"
    | ExactCodeComm ec -> ec

let comp_fn (f : fn) : string = 
    debug_print ">> comp_fn";
    let (rt, id, _, p), cl = f in
    match rt with
        | ExactCodeTyp -> id ^ " "
        | _ ->
            let param_string = string_of_list (fun (t, i) -> string_of_typ t ^ " " ^ i) p in
            let type_id_string = match id with
                | "main" -> "void main"
                | _ -> string_of_typ rt ^ " " ^ replace_all_in_name id
            in
            type_id_string ^ "(" ^ param_string ^ "){" ^ string_of_separated_list "" string_of_comm cl ^ "}"

let rec comp_fn_lst (f : fn list) : string =
    debug_print ">> comp_fn_lst";
    match f with 
    | [] -> ""
    | h::t -> (comp_fn h) ^ (comp_fn_lst t)

let decl_attribs (gv : global_vars) : string = 
    debug_print ">> decl_attribs";
    let rec decl_attribs_list (gv : global_vars) : string =
        match gv with
        | [] -> ""
        | (sq, et, x, e)::t -> 
            string_of_storage_qual sq ^ " " ^ string_of_typ et
            ^ " " ^ x ^ string_of_option_removed (fun x -> " = " ^ string_of_texp x) e ^
            ";" ^ decl_attribs_list t
    in
    decl_attribs_list gv

let rec compile_program (prog : prog) (global_vars : global_vars) : string =
    debug_print ">> compile_program";
    (* "precision mediump float;" ^  *)
     (decl_attribs global_vars) ^ 
     (comp_fn_lst prog)
 