open CoreAst
open TypedAst
open Assoc
open Lin_ops
open Util
open EmitUtil
open CheckUtil
open CheckContexts

let check_contexts : contexts option ref = ref None

(* type epsilon = (id, etyp) Assoc.context *)
type delta = etyp list Assoc.context
type ltyp_top = VecDim of int | MatDim of int * int

(* Don't write the type of gl_Position or gl_FragColor *)
let is_core (var_name : string) : bool =
  String.length var_name > 3 && Str.string_before var_name 3 = "gl_"

(* Note the column parameter for padding the matrix size *)
let rec string_of_no_paren_vec (v : texp list) (padding : int) : string =
  string_of_list string_of_texp v ^ repeat ", 0." padding

and string_of_mat_padded (m : texp list list) (max_dim : int) : string =
  let string_of_vec_padded v =
    string_of_no_paren_vec v (max_dim - List.length v) in
  "("
  ^ string_of_list string_of_vec_padded m
  ^ repeat (string_of_no_paren_vec [] max_dim) (max_dim - List.length m)
  ^ ")"

and string_of_glsl_mat (m : texp list list) : string =
  (* Note the transpose to match the glsl column-oriented style *)
  let tm = Lin_ops.transpose m in
  let r = List.length tm in
  let c = if r = 0 then 0 else List.length (List.hd tm) in
  let dim = max r c in
  "mat" ^ string_of_int dim ^ string_of_mat_padded tm dim

and string_of_typ (t : etyp) : string =
  let is_2_4 d = match d with ConstInt d' -> d' >= 2 && d' <= 4 | _ -> false in
  let arr_string =
    match t with
    | ArrTyp (t', d) -> string_of_typ t' ^ "[" ^ string_of_constvar d ^ "]"
    | _ -> TypedAstPrinter.string_of_typ t in
  let dim_string s d =
    if is_2_4 d then s ^ string_of_constvar d else arr_string in
  match t with
  | ArrTyp (IntTyp, d) -> dim_string "ivec" d
  | ArrTyp (FloatTyp, d) -> dim_string "vec" d
  | ArrTyp (BoolTyp, d) -> dim_string "bvec" d
  | ArrTyp (ArrTyp (FloatTyp, _), d) -> dim_string "mat" d
  | ArrTyp _ -> arr_string
  | ParTyp (s, _) -> s
  | _ -> TypedAstPrinter.string_of_typ t

and attrib_type (var_name : string) : string =
  debug_print ">> attrib_type" ;
  if var_name.[0] = 'a' then "attribute"
  else if var_name.[0] = 'v' then "varying"
  else if var_name.[0] = 'u' then "uniform"
  else failwith "Not a supported glsl attribute"

and op_wrap (op : exp) : string =
  debug_print ">> op_wrap" ;
  match op with
  | Val _ | Var _ -> string_of_exp op
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
and string_of_texp ((e, _) : texp) : string = string_of_exp e

and string_of_exp (e : exp) : string =
  debug_print ">> comp_exp" ;
  match e with
  | Val v -> string_of_value v
  | Var v -> v
  | Arr a -> (
    match a with
    | [] -> "vec0()"
    | (_, t) :: _ -> (
      match t with
      | FloatTyp ->
          "vec"
          ^ string_of_int (List.length a)
          ^ "("
          ^ string_of_list string_of_texp a
          ^ ")"
      | IntTyp ->
          "ivec"
          ^ string_of_int (List.length a)
          ^ "("
          ^ string_of_list string_of_texp a
          ^ ")"
      | BoolTyp ->
          "bvec"
          ^ string_of_int (List.length a)
          ^ "("
          ^ string_of_list string_of_texp a
          ^ ")"
      | ArrTyp (FloatTyp, d) | ArrTyp (IntTyp, d) ->
          let as_vec_list v =
            match v with
            | Arr a', _ -> a'
            | _ ->
                failwith "Typechecker error, a matrix must be a list of vectors"
          in
          string_of_glsl_mat (List.map as_vec_list a)
      | _ ->
          failwith
            "Typechecker error, every array must be a list of ints, floats, \
             vectors, or bools" ) )
  | Index (l, r) -> string_of_texp l ^ "[" ^ string_of_texp r ^ "]"
  | FnInv (id, tl, args) -> string_of_fn_util id (List.map string_of_texp args)
  | MethodInv (lhs, id, tl, args, class_name) ->
    let cx = (match !check_contexts with
      | Some con -> con
      | None -> failwith "No check contexts!") in
    let class_string, super_string = (match class_method_lookup_deep cx
        (get_class cx class_name) id with
      | Some (_, num_parents, class_found) ->
          (class_found, string_multiply ("." ^ super_keyword) num_parents)
      | None -> failwith ("Invalid method invocation: " ^ id)) in
    let first_arg = (match lhs with
      | Some e' -> string_of_exp e'
      | None -> this_keyword) in
    string_of_fn_util (method_to_function_name class_string id)
      ([first_arg ^ super_string] @ (List.map string_of_texp args))
  | FieldSelect (lhs, s, class_name) ->
    let glsl_lhs = (match lhs with
      | Some e' -> string_of_exp e'
      | None -> this_keyword) in
    if String.equal class_name "" then (* Evil hack. Need to properly signal
    that the field select is for a struct *)
    glsl_lhs ^ "." ^ s
    else
    let cx = (match !check_contexts with
      | Some con -> con
      | None -> failwith "No check contexts!") in
    let super_string = (match class_field_lookup_deep cx (get_class cx class_name) s with
      | Some (_, num_parents) -> string_multiply ("." ^ super_keyword) num_parents
      | None -> failwith "Invalid field select: " ^ s
    ) in
    glsl_lhs ^ super_string ^ "." ^ s

let rec string_of_comm (c : comm) : string =
  let block_string c =
    "{ " ^ string_of_separated_list "" string_of_comm c ^ "}" in
  match c with
  | Skip -> "skip;"
  | Print e -> "print " ^ string_of_texp e ^ ";"
  | Exp e -> string_of_texp e ^ ";"
  | Decl (t, s, e) ->
      let ts = if is_core s then "" else string_of_typ t ^ " " in
      ts ^ s ^ " = " ^ string_of_texp e ^ ";"
  | Assign (b, x) -> string_of_texp b ^ " = " ^ string_of_texp x ^ ";"
  | AssignOp (x, op, e) ->
      string_of_texp x ^ " " ^ op ^ "= " ^ string_of_texp e ^ ";"
  | If ((b, c1), elif_list, c2) ->
      "if (" ^ string_of_texp b ^ ")" ^ block_string c1
      ^ string_of_list
          (fun (b, c) -> "elif (" ^ string_of_texp b ^ ")" ^ block_string c)
          elif_list
      ^ Option.fold ~none:"" ~some:(fun x -> "else " ^ block_string x) c2
  | For (d, b, u, cl) ->
      let us = string_of_comm u in
      (* Hack to get rid of the semicolon at the end of the for loop *)
      "for (" ^ string_of_comm d ^ string_of_texp b ^ "; "
      ^ String.sub us 0 (String.length us - 1)
      ^ ") " ^ block_string cl
  | Return x ->
      "return"
      ^ Option.fold ~none:"" ~some:(fun x -> " " ^ string_of_texp x) x
      ^ ";"
  | ExactCodeComm ec -> ec

let string_of_structure (id, ml) : string =
  (List.fold_left
  (fun s (t, id) -> 
    s ^ " " ^ string_of_typ t ^ " " ^ id ^ ";"
  ) ("struct " ^ id ^ " {") ml) ^ " };"
  ^ (if !pretty_printer then "\n" else "")

let comp_fn (f : fn) : string =
  debug_print ">> comp_fn" ;
  let (rt, id, _, p), cl = f in
  match rt with
  | ExactCodeTyp -> id ^ " "
  | _ ->
      let param_string =
        string_of_list (fun (sq, t, i) -> string_of_list string_of_storage_qual sq ^ " " ^
        string_of_typ t ^ " " ^ i) p in
      let type_id_string =
        match id with
        | "main" -> "void main"
        | _ -> string_of_typ rt ^ " " ^ replace_all_in_name id in
      if !pretty_printer then
        type_id_string ^ "(" ^ param_string ^ "){"
        ^ string_of_separated_list "" string_of_comm cl
        ^ "}" ^ "\n\n"
      else
        type_id_string ^ "(" ^ param_string ^ "){"
        ^ string_of_separated_list "" string_of_comm cl
        ^ "}"

let string_of_class (c : _class) : string =
  let name, parent, mems = c in
  let fields = List.filter
  (fun m -> match m with | Field _ -> true | Method _ -> false) mems in
  let methods = List.filter
  (fun m -> match m with | Method _ -> true | Field _ -> false) mems in

  let struct_mems =
    (match parent with
    | Some par -> [(ParTyp(class_to_struct_name par, []), "super")]
    | None -> []) @
    List.map (fun f -> match f with
    | Field (_, typ, id) -> (typ, id)
    | _ -> failwith "not a field") fields in
  let struct_str = string_of_structure ((class_to_struct_name name), struct_mems) in

  let constructor_types = List.map (fun f -> match f with
  | Field (_, typ, _) -> typ
  | _ -> failwith "not a field") fields in

  let constructor_defaults = List.map (fun f -> match f with
  | Field (_, typ, _) -> default_value typ
  | _ -> failwith "not a field") fields in

  let class_type = ParTyp(name, []) in
  let function_strs = List.map
  (fun m -> match m with
  | Method (_, (fn_decl, fn_comms)) -> 
    let ret_typ, fn_name, pmt, params  = fn_decl in
    if String.equal fn_name "init" then (
      let new_fn_name = class_to_constructor_name name in
      let new_ret_typ = class_type in
      let new_fn_comms =
        (*[Decl (class_type, this_keyword, ((
          FnInv (class_to_struct_name name, constructor_types, constructor_defaults)), class_type))] @*)
        fn_comms @
        [Return (Some (Var this_keyword, class_type))]
      in
      comp_fn ((new_ret_typ, new_fn_name, pmt, params), new_fn_comms)
    ) else (
      let new_fn_name = method_to_function_name name fn_name in
      let new_params = [([], class_type, this_keyword)] @ params in
      comp_fn ((ret_typ, new_fn_name, pmt, new_params), fn_comms)
    )
  | _ -> failwith "not a method"
  ) methods in
  let concatenated_functions = String.concat " " function_strs in
  struct_str ^ concatenated_functions

let rec comp_prog (f : term list) : string =
  debug_print ">> comp_fn_lst" ;
  match f with
  | [] -> ""
  | Fn h :: t -> comp_fn h ^ comp_prog t
  | GlobalVar (sq, et, x, e) :: t ->
      string_of_list string_of_storage_qual sq ^ " " ^ string_of_typ et ^ " " ^ x
      ^ Option.fold ~none:"" ~some:(fun x -> " = " ^ string_of_texp x) e
      ^ ";" ^ comp_prog t
  | Structure s :: t -> string_of_structure s ^ comp_prog t
  | Class c :: t -> string_of_class c ^ comp_prog t

let rec compile_program (cx : contexts) (prog : prog) : string =
  debug_print ">> compile_program" ;
  check_contexts := Some cx;
  comp_prog prog
