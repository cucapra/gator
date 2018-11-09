open CoreAst
open TypedAst
open TypedAstPrinter
open Assoc
open Lin_ops
open Util

(* type epsilon = (id, etyp) Assoc.context *)
type delta = (etyp list) Assoc.context

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
let rec string_of_no_paren_vec (v: exp list) (padding: int) : string = 
    (String.concat ", " (List.map comp_exp v)) ^ (repeat ", 0." padding)
  
and string_of_mat_padded (m: exp list list) (max_dim: int) : string =
    let string_of_vec_padded = (fun v -> (string_of_no_paren_vec v (max_dim - List.length v))) in
    ("(" ^ (String.concat ", " (List.map string_of_vec_padded m)) ^
    (repeat (string_of_no_paren_vec [] max_dim) (max_dim - List.length m)) ^ ")")
  
and string_of_glsl_mat (m: exp list list) : string = 
    (* Note the transpose to match the glsl column-oriented style *)
    let tm = Lin_ops.transpose m in
    let r = (List.length tm) in
    let c = (if r = 0 then 0 else List.length (List.hd tm)) in
    let dim = max r c in
    ("mat"^(string_of_int dim)^string_of_mat_padded tm dim)

and string_of_glsl_typ (t : etyp) : string =
    match t with
    | UnitTyp -> failwith "Unit type is unwriteable in glsl"
    | MatTyp (m, n) -> "mat" ^ string_of_int (max m n)
    | _ -> string_of_typ t

and attrib_type (var_name : string) : string =
    debug_print ">> attrib_type";
    if (String.get var_name 0) = 'a' then "attribute" else
    (if (String.get var_name 0) = 'v' then "varying" else
    (if (String.get var_name 0) = 'u' then "uniform" else
    failwith "Not a supported glsl attribute"))

(* Ignore original declarations of attributes and the like *)
and check_name (var_name : string) : bool = 
    debug_print ">> check_name";
    let decl_reg = Str.regexp "[auv][A-Z]" in
        Str.string_match decl_reg var_name 0

(* Don't write the type of gl_Position or gl_FragColor *)
and is_core (var_name : string) : bool = 
    debug_print ">> is_core";
    var_name = "gl_Position" || var_name = "gl_FragColor"

and op_wrap (op : exp) : string =
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
    match e with
    | Val v -> string_of_value v
    | Var v -> v
    | Arr a -> (match a with
        | [] -> "vec0()"
        | (_, t)::_ -> (match t with
            | FloatTyp | IntTyp -> "vec" ^ (string_of_int (List.length a)) ^ "(" ^ (String.concat ", " (List.map (fun x -> comp_exp (fst x)) a)) ^ ")"
            | VecTyp n -> let as_vec_list = (fun v -> (match v with | (Arr a', _) -> (List.map fst a') | _ -> failwith "Typechecker error, a matrix must be a list of vectors")) in
                string_of_glsl_mat (List.map as_vec_list a)
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
        | Inc (x, _) -> x ^ "++;" ^ (comp_comm t)
        | Dec (x, _) -> x ^ "--;" ^ (comp_comm t)
        (* Super janky, but we need to have rules for weird glsl declarations and variables *)
        | Decl (ty, x, (e, _)) -> (
            if check_name x then ""^ (comp_comm t) else  
            if is_core x  then x ^ " = " ^ (comp_exp e) ^ ";" ^ (comp_comm t)
            else string_of_glsl_typ ty ^ " "^ x ^ " = " ^ (comp_exp e) ^ ";" ^ (comp_comm t))
        | Assign (x, (e, _)) -> x ^ " = " ^ (comp_exp e) ^ ";" ^ (comp_comm t)
        | AssignOp ((x, _), op, (e, _)) -> x ^ " " ^ (binop_string op) ^ "= " ^ (comp_exp e) ^ ";" ^ (comp_comm t)
        | If (((b, _), c1), el, c2) -> 
            ("if " ^ "(" ^ (comp_exp b) ^ ")"
            ^ "{ " ^ (comp_comm c1) ^ " }"
            ^ (List.fold_left (fun acc ((b, _), c) -> "if " ^ "(" ^ (comp_exp b) ^ ")"
                ^ "{ " ^ (comp_comm c) ^ " }" ^ acc) "" el)
            ^ (match c2 with | Some c2 -> "{ " ^ (comp_comm c2) ^ " }" | None -> "")
            ^ (comp_comm t))
        | For (c1, (b, _), c2, cl) -> 
            ("for (" ^ (comp_comm [c1]) ^ "; " ^ (comp_exp b) ^ "; " ^ (comp_comm [c2])
            ^ "{ " ^ (comp_comm cl) ^ " }" ^ (comp_comm t))
        | Return Some (e, _) -> "return " ^ (comp_exp e) ^ ";" ^ (comp_comm t)
        | Return None -> "return;" ^ (comp_comm t)
        | FnCall (id, args) -> id ^ "(" ^ (padded_args args) ^ ")"

(* GenTyp - int, float, vec(2,3,4), mat(16 possibilites) *)
let rec strings_of_constraint (c: constrain) : string list =
    match c with
    | AnyTyp -> string_of_glsl_typ BoolTyp :: strings_of_constraint GenTyp
    | GenTyp -> List.map string_of_glsl_typ [IntTyp; FloatTyp] @ 
        (strings_of_constraint GenVecTyp @ strings_of_constraint GenMatTyp)
    | GenVecTyp -> List.map string_of_glsl_typ [VecTyp 2; VecTyp 3; VecTyp 4]
    | GenMatTyp -> List.map string_of_glsl_typ [MatTyp(2,1); MatTyp(3,1); MatTyp(4,1)]
    | ETypConstraint t -> [string_of_glsl_typ t]


let rec generate_fn_generics (orig : string) (((id, (pm, rt)), cl) : fn) : string = 
    debug_print (">> generate_fn_generics " ^ id);
    let pm_list = Assoc.bindings pm in
    let rec replace_generic (orig: string) (pml : (string * constrain) list) : string =
        print_endline orig;
        match pml with
        | [] -> orig
        | (s,c)::t -> 
            let regex = (Str.regexp ("`"^s)) in
            let str_lst = strings_of_constraint c in
            let result = List.fold_left (fun acc r -> Str.global_replace regex r acc) orig str_lst in
            replace_generic result t
    in replace_generic orig pm_list


let comp_fn (f : fn) : string = 
    debug_print ">> comp_fn";
    let ((id, (p, rt)), cl) = f in
    match id with 
    | "main" -> "void main() {" ^ (comp_comm cl) ^ "}"
    | _ -> 
        let fn_str = ((string_of_glsl_typ rt) ^ " " ^ id ^ "(" ^ (string_of_params p) ^ "){" ^ (comp_comm cl) ^ "}") in
        generate_fn_generics fn_str f


let rec comp_fn_lst (f : fn list) : string =
    debug_print ">> comp_fn_lst";
    match f with 
    | [] -> ""
    | h::t -> (comp_fn h) ^ (comp_fn_lst t)

    
let decl_attribs (p : params) : string = 
    debug_print ">> decl_attribs";
    let rec decl_attribs_list (pl : (string * constrain) list) =
        match pl with
        | [] -> ""
        | (x, c)::t -> 
            if check_name x then begin
                match c with
                | ETypConstraint et ->
                    (attrib_type x) ^ " " ^ (string_of_glsl_typ et) ^ " " ^ x ^ ";" ^ (decl_attribs_list t)
                | _ -> failwith "Typechecker error: cannot include generic constraints in main"
            end
            else decl_attribs_list t
    in
    decl_attribs_list (Assoc.bindings p)

let rec compile_program (prog : prog) (params : params) : string =
    debug_print ">> compile_program";
    "precision highp float;" ^ (decl_attribs params) ^ 
     (comp_fn_lst prog)
 