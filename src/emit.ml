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

(* Note the column parameter for padding the matrix size *)
let string_of_no_paren_vec (v: vec) (padding: int) : string = 
    debug_print ">> string_of_no_paren_vec";
    (String.concat ", " (List.map string_of_float v)) ^ (repeat ", 0." padding)
  
let string_of_mat_padded (m: mat) (max_dim: int) : string =
    debug_print ">> string_of_mat_padded";
    let string_of_vec_padded = (fun v -> (string_of_no_paren_vec v (max_dim - List.length v))) in
    ("(" ^ (String.concat ", " (List.map string_of_vec_padded m)) ^
    (repeat (string_of_no_paren_vec [] max_dim) (max_dim - List.length m)) ^ ")")
  
let string_of_gl_mat (m: mat) : string = 
    debug_print ">> string_of_gl_mat";
    (* Note the transpose to match the glsl column-oriented style *)
    let tm = Lin_ops.transpose m in
    let r = (List.length tm) in
    let c = (if r = 0 then 0 else List.length (List.hd tm)) in
    let dim = max r c in
    ("mat"^(string_of_int dim)^string_of_mat_padded tm dim)

let string_of_gl_typ (t : etyp) : string =
    debug_print ">> string_of_gl_typ";
    match t with
    | UnitTyp -> failwith "Unit type is unwriteable in glsl"
    | MatTyp (m, n) -> "mat" ^ string_of_int (max m n)
    | _ -> string_of_typ t

let attrib_type (var_name : string) : string =
    debug_print ">> attrib_type";
    if (String.get var_name 0) = 'a' then "attribute" else
    (if (String.get var_name 0) = 'v' then "varying" else
    (if (String.get var_name 0) = 'u' then "uniform" else
    failwith "Not a supported glsl attribute"))

(* Ignore original declarations of attributes and the like *)
let check_name (var_name : string) : bool = 
    debug_print ">> check_name";
    let decl_reg = Str.regexp "[auv][A-Z]" in
        Str.string_match decl_reg var_name 0

(* Don't write the type of gl_Position or gl_FragColor *)
let is_core (var_name : string) : bool = 
    debug_print ">> is_core";
    var_name = "gl_Position" || var_name = "gl_FragColor"

let rec op_wrap (op : exp) : string =
    debug_print ">> op_wrap";
    match op with
    | Val _
    | Var _ -> comp_exp op
    | _ -> "(" ^ (comp_exp op) ^ ")"

(* Handles the string shenanigans for padding during multiplication *)
and padded_mult (left : texp) (right : texp) : string =
    debug_print ">> padded_mult";
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
        
and padded_args (a : exp list) : string = 
    debug_print ">> padded_args";
    (String.concat ", " (List.map (op_wrap) a))

and comp_exp (e : exp) : string =
    debug_print ">> comp_exp";
    let comp_arr (a: texp list) : string = 
        "["^(String.concat ", " (List.map comp_exp (List.map fst a)))^"]"
    in
    match e with
    | Val v -> string_of_value v
    | Var v -> v
    | Arr a -> (match a with
        | [] -> "vec0()"
        | (_, t)::_ -> (match t with
            | FloatTyp | IntTyp -> "vec" ^ (string_of_int (List.length a)) ^ "(" ^ (String.concat ", " (List.map (fun x -> comp_exp (fst x)) a)) ^ ")"
            | VecTyp n -> let as_vec_list = (fun v -> (match v with | (Arr a', _) -> (List.map fst a') | _ -> failwith "Typechecker error, a matrix must be a list of vectors")) in
                string_of_gl_mat (List.map as_vec_list a)
            | _ -> failwith "Typechecker error, every array must be a list of ints, floats, or vectors"))
    | Binop (op, l, r) -> (match op with
        | Times -> padded_mult l r
        | CTimes -> "(" ^ ((comp_exp (fst l)) ^ " * " ^(comp_exp (fst r))) ^ ")"
        | _ -> "(" ^ (string_of_binop op (comp_exp (fst l)) (comp_exp (fst r))) ^ ")")
    | Unop (op, (x, _)) -> (string_of_unop op ("(" ^ (comp_exp x) ^ ")"))
    | FnInv (id, args) -> id ^ "(" ^ (padded_args args) ^ ")"
 
and comp_comm (c : comm list) : string =
    debug_print ">> comp_comm";
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
        | Return Some (e, _) -> "return " ^ (comp_exp e) ^ ";" ^ (comp_comm t)
        | Return None -> "return;" ^ (comp_comm t)
        | FnCall (id, args) -> id ^ "(" ^ (padded_args args) ^ ")"

let check_generics ((p, rt) : fn_type) : (string * etyp option) list = 
    debug_print ">> check_generics";
    let rec check_generics_rt p' acc : (string * etyp option) list = 
        match p' with
        [] -> acc
        | s::t -> 
            match s with
            (_ , AbsTyp (a, b)) -> (a,b)::(check_generics_rt t acc)
            | (_, GenTyp) -> ("genType" , None)::(check_generics_rt t acc)
            | _ -> check_generics_rt t acc
    in 
    match rt with
    AbsTyp (s', e') -> check_generics_rt p ((s', e')::[])
    | GenTyp -> check_generics_rt p (("genType", None)::[])
    | _ -> check_generics_rt p []

type delta = (etyp list) Assoc.context


let rec get_parametrization_generic_types (p: etyp option) = 
    match p with
    | None -> [IntTyp; FloatTyp; MatTyp(2,1); MatTyp(3,1); MatTyp(4,1); 
            VecTyp 2; VecTyp 3; VecTyp 4; BoolTyp; SamplerTyp 2; SamplerTyp 3]
    | Some GenTyp -> [IntTyp; FloatTyp; MatTyp(2,1); MatTyp(3,1); MatTyp(4,1); 
            VecTyp 2; VecTyp 3; VecTyp 4]
    | Some AbsTyp (s, e) -> get_parametrization_generic_types (Some (AbsTyp (s, e)))
    | Some t -> [t]

let rec process_parametrizations (pm: (string * etyp option) list) (gs: (etyp list) context) = 
    match pm with 
    [] -> gs 
    | (s, e)::t -> Assoc.update s (get_parametrization_generic_types e) gs

(* GenTyp - int, float, vec(2,3,4), mat(16 possibilites) *)
let rec generate_fn_generics (((id, (p, rt)), cl) : fn) (pm : (string * etyp option) list) = 
    debug_print (">> generate_fn_generics " ^ id);   
    let gens = Assoc.empty 
        |> Assoc.update "genType" [IntTyp; FloatTyp; 
        MatTyp(2,1); MatTyp(3,1); MatTyp(4,1); VecTyp 2; VecTyp 3; VecTyp 4]
        |> process_parametrizations pm;
    (* TODO: add into gens all the stuff in pm! *)
    in 
    let plain = (string_of_gl_typ rt) ^ " " ^ id ^ "(" ^ (string_of_params p) ^ "){" ^ (comp_comm cl) ^ "}"
    in 
    let rec replace_generic (orig: string) : string = 
        match pm with
        | [] -> orig
        | (s', None)::t -> 
            debug_print (">> generate_fn_generics1 "^ s');
            let con = Assoc.lookup s' gens in 
            let rec replace_generic_helper c =
                match c with 
                [] -> ""
                | s''::t -> Str.global_replace (Str.regexp ("`"^s')) (string_of_gl_typ s'') orig ^ (replace_generic_helper t)
            in replace_generic_helper con
        | (s', Some (AbsTyp (s'', e'')))::t -> 
            debug_print ">> generate_fn_generics2";
            let con = Assoc.lookup s'' gens in 
            let rec replace_generic_helper c =
                match c with 
                [] -> ""
                | s'''::t -> Str.global_replace (Str.regexp ("`"^s')) (string_of_gl_typ s''') orig ^ (replace_generic_helper t)
            in replace_generic_helper con
        | (s', Some (GenTyp))::t -> 
            debug_print ">> generate_fn_generics3";
            let con = Assoc.lookup "genType" gens in 
            let rec replace_generic_helper c =
                match c with 
                [] -> ""
                | s'''::t -> Str.global_replace (Str.regexp ("`"^s')) (string_of_gl_typ s''') orig ^ (replace_generic_helper t)
            in replace_generic_helper con
        | (s', Some k)::t -> debug_print ">> generate_fn_generics3";
            Str.global_replace (Str.regexp ("`"^s')) (string_of_gl_typ k) orig
    in replace_generic plain


let comp_fn (((id, (p, rt)), cl) : fn) : string = 
    debug_print ">> comp_fn";
    match id with 
    | "main" -> "void main() {" ^ (comp_comm cl) ^ "}"
    | _ -> 
        let pm = check_generics (p, rt) in
        if List.length pm = 0 then 
        (string_of_gl_typ rt) ^ " " ^ id ^ "(" ^ (string_of_params p) ^ "){" ^ (comp_comm cl) ^ "}"
        else generate_fn_generics ((id, (p, rt)), cl) pm

let rec comp_fn_lst (f : fn list) : string =
    debug_print ">> comp_fn_lst";
    match f with 
    | [] -> ""
    | h::t -> (comp_fn h) ^ (comp_fn_lst t)

let rec decl_attribs (p : TypedAst.params) : string = 
    debug_print ">> decl_attribs";
    match p with
    | [] -> ""
    | h::t -> match h with
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | (x, et) -> if check_name x then 
            (attrib_type x) ^ " " ^ (string_of_gl_typ et) ^ " " ^ x ^ ";" ^ (decl_attribs t) else
            decl_attribs t

let rec compile_program (prog : prog) (params : TypedAst.params) : string =
    debug_print ">> compile_program";
    "precision highp float;" ^ (decl_attribs params) ^ 
     (comp_fn_lst prog)
 