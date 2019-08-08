open CoreAst
open GatorAst
open GatorAstPrinter
open Util
open Printf
open Str
open CheckUtil
open Contexts

let rec get_frame_top (cx : contexts) (x : string) : int =
    match get_frame cx x with
    | FrameDim s -> get_frame_top cx s
    | FrameNum n -> n

let rec reduce_dexp (cx: contexts) (d : dexp) : int =
    debug_print (">> reduce_dexp " ^ string_of_dexp d);
    match d with
    | DimNum n -> n
    | DimVar x -> get_frame_top cx x
    | DimBinop (l, b, r) -> match b with
            | Plus -> reduce_dexp cx l + reduce_dexp cx r
            | Minus -> reduce_dexp cx l - reduce_dexp cx r
            | _ -> error cx ("Invalid binary operation to dimension expression " ^ string_of_binop b)

let rec unwrap_abstyp (cx: contexts) (s: string) : typ =
    debug_print ">> unwrap_abstyp";
    match get_pm cx s with
        | ParTyp (s, tl) -> failwith "unimplemented partyp unwrapping"
        | p -> p

let rec replace_abstype (c: typ Assoc.context) (t: typ) : typ =
    debug_print ">> replace_abstype";
    match t with
    | ParTyp (s, tl) -> failwith "unimplemented abstract type replacement"
    (* ParTyp (s, List.map (replace_abstype c) tl) *)
    | _ -> t

let rec is_typ_eq (cx : contexts) (t1: typ) (t2: typ) : bool =
    match (t1, t2) with
    | UnitTyp, UnitTyp
    | BoolTyp, BoolTyp
    | IntTyp, IntTyp
    | FloatTyp, FloatTyp -> true
    | Literal t1, Literal t2 -> is_typ_eq cx t1 t2
    | ArrTyp (t1, d1), ArrTyp (t2, d2) -> is_typ_eq cx t1 t2 && reduce_dexp cx d1 = reduce_dexp cx d2
    | CoordTyp (c1, ParTyp (o1, f1)), CoordTyp(c2, ParTyp (o2, f2)) -> 
        c1 = c2 && is_typ_eq cx (ParTyp (o1, f1)) (ParTyp (o2, f2))
    | ParTyp (s1, tl1), ParTyp (s2, tl2) -> s1 = s2 && 
        (if (List.length tl1 = List.length tl2) 
        then list_typ_eq cx tl1 tl2
        else false)    
    | _ -> false

and list_typ_eq (cx : contexts) (tl1: typ list) (tl2: typ list) : bool 
    = List.fold_left2 (fun acc x y -> acc && is_typ_eq cx x y) true tl1 tl2

let rec is_subtype (cx: contexts) (to_check : typ) (target : typ) : bool =
    debug_print (">> is_subtype " ^ (string_of_pair (string_of_typ to_check) (string_of_typ target)));
    if is_typ_eq cx to_check target then true else
    match (to_check, target) with
    | BotTyp, _ -> true
    | _, BotTyp -> false
    | _, AnyTyp -> true
    | AnyTyp, _ -> false
    | BoolTyp, GenTyp -> false
    | _, GenTyp -> true
    | GenArrTyp t1, GenArrTyp t2 -> is_subtype cx t1 t2
    | ArrTyp (t, _), GenArrTyp c -> is_subtype cx t c

    | ArrTyp (t1, d1), ArrTyp (t2, d2) ->
        reduce_dexp cx d1 = reduce_dexp cx d2 
        && is_subtype cx t1 t2
    | ParTyp (s1, tl1), ParTyp (s2, tl2) -> (s1 = s2 && List.length tl1 = List.length tl2
        && list_typ_eq cx tl1 tl2)
        || is_subtype cx (tau_lookup cx s1 tl1) target
    | CoordTyp (c1, ParTyp (o1, f1)), CoordTyp (c2, ParTyp (o2, f2)) ->
        (c1 = c2 && list_typ_eq cx f1 f2)
        || is_subtype cx (chi_object_lookup cx c1 o1 f1) target
    
    (* Type lookup cases *)
    | Literal t, _ -> is_subtype cx target t
    | ParTyp (s, tl), _ -> is_subtype cx (tau_lookup cx s tl) target
    | CoordTyp (c, ParTyp (o, f)), _ -> is_subtype cx (chi_object_lookup cx c o f) target

    | _ -> false

(* Given a parameterization and a list of types being invoked on that parameterization *)
(* Returns the appropriate concretized context if one exists *)
and match_parameterization (cx: contexts) (pml : typ list)
: typ Assoc.context =
    debug_print ">> match_parameterization";
    let pmb = Assoc.bindings cx.pm in
    if List.length pmb == List.length pml
        && List.fold_left2 (fun acc (s, c) t -> is_subtype cx t c && acc) true pmb pml
    then List.fold_left2 (fun tcacc (s, c) t -> Assoc.update s t tcacc)
        Assoc.empty (Assoc.bindings cx.pm) pml
    else error cx ("Invalid parameterization provided to " ^ string_of_parameterization cx.pm
        ^ " by <" ^ string_of_separated_list "," string_of_typ pml ^ ">")
 
(* Looks up a supertype without checking the bounds on the provided parameters (hence, 'unsafe') *)
and tau_lookup (cx: contexts) (x: id) (pml: typ list) : typ =
    (* If the given type evaluates to a declared tag, return it *)
    (* If the return type would be a top type, resolve the dimension to a number *)
    debug_print ">> tau_lookup";
    let pmd, t = get_typ cx x in
    let tc = match_parameterization (with_pm cx pmd) pml in
    replace_abstype tc t

(* Looks up an object 'o' definition from a coordinate scheme 'c' without checking parameter bounds *)
and chi_object_lookup (cx: contexts) (c : id) (o: id) (f: typ list) : typ =
    match get_coordinate_element cx c o with
    | CoordObjectAssign (_, pmd, t) -> 
        let tc = match_parameterization (with_pm cx pmd) f in
        replace_abstype tc t
    | _ -> error cx ("")

(* Steps types up a level in the subtyping tree *)
(* Fails if given a primitive type, illegal geometric type, or external type (they have no supertype) *)
let rec typ_step (cx : contexts) (t : typ) : typ option =
    debug_print (">> typ_step" ^ string_of_typ t);
    match t with
    | ParTyp (s, tl) -> Some (tau_lookup cx s tl)
    | CoordTyp (c, ParTyp (o, f)) -> Some (chi_object_lookup cx c o f)
    | Literal t -> Some t
    | _ -> None

(* Produces a primitive type (boolean, int, float, array, or array literal) *)
let rec primitive (cx : contexts) (t : typ) : typ =
    debug_print (">> primitive" ^ string_of_typ t);
    match typ_step cx t with
    | Some t' -> primitive cx t'
    | None -> t

let rec greatest_common_child (cx: contexts) (t1: typ) (t2: typ): typ =
    debug_print ">> greatest_common_child";    
    if is_subtype cx t1 t2 then t1 else 
    if is_subtype cx t2 t1 then t2 else 
    let top = primitive cx t1 in
    if is_typ_eq cx top (primitive cx t2) then Literal top else
    error cx ("Cannot unify " ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2))

let rec least_common_parent (cx: contexts) (t1: typ) (t2: typ): typ =
    debug_print (">> least_common_parent" ^ (string_of_pair (string_of_typ t1) (string_of_typ t2)));
    if is_subtype cx t1 t2 then t2 else if is_subtype cx t2 t1 then t1 
    else match typ_step cx t1 with 
    | Some t1' -> least_common_parent cx t1' t2
    | None -> error cx ("Cannot unify " ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2))

(* Least common parent which will not raise an exception (used for typechecking of inferred/internal types) *)
let least_common_parent_safe (cx: contexts) (t1: typ) (t2: typ): typ option =
    try Some (least_common_parent cx t1 t2) with
    | TypeExceptionMeta (t, _) -> None
    | t -> raise t

(* Given a function and list of arguments to that function *)
(* Attempts to produce a list of valid types for the parameterization of the function *)
let infer_pml (cx: contexts) (pr : params) (args : typ list) : (typ list) option =
    debug_print ">> infer_pml";
    let is_abs s = List.mem s (Assoc.keys cx.pm) in
    let update_inference (t : typ) (s : string) (fpm : typ Assoc.context option) : typ Assoc.context option =
        match fpm with | None -> None | Some p ->
        if Assoc.mem s p then match least_common_parent_safe cx t (Assoc.lookup s p) with
            | None -> None
            | Some t' -> Some (Assoc.update s t' p)
        else Some (Assoc.update s t p)
    in
    let rec unify_param (fpm : (typ Assoc.context) option) (arg_typ : typ) (par_typ : typ) : (typ Assoc.context) option =
        (* Only update our inference if we are working on an abstract type *)
        let new_fpm s = if is_abs s then update_inference arg_typ s fpm else fpm in
        match arg_typ, par_typ with
        | ParTyp (_, tl1), ParTyp (s, tl2) ->
            (* Abstract params may have unspecified parameterizations provided by the arguments *)
            if List.length tl1 != List.length tl2 then new_fpm s else
            List.fold_left2 unify_param (new_fpm s) tl1 tl2
        | CoordTyp (_, t1), CoordTyp(s, t2) ->
            unify_param (new_fpm s) t1 t2
        | _ -> fpm
    in
    let inferred = List.fold_left2 unify_param (Some Assoc.empty) args (List.map fst pr) in
    option_map Assoc.values inferred

let check_subtype_list (cx: contexts) (l: typ list) (t: typ) : bool =
    debug_print ">> check_subtype_list";
    List.fold_left (fun acc t' -> acc || (is_subtype cx t t')) false l
    
let check_typ_valid (cx: contexts) (ogt: typ) : unit =
    let rec check_typ_valid_rec (t: typ) : unit =
        debug_print ">> check_typ_valid";
        match t with
        | ParTyp (s, tl) -> 
            ignore_typ (tau_lookup cx s tl);
            List.fold_left (fun _ -> check_typ_valid_rec) () tl
        | CoordTyp (c, ParTyp(o, f)) ->
            ignore_typ (chi_object_lookup cx c o f);
        | CoordTyp _ ->
            error cx ("All types a.b must be geometric (i.e. of the form scheme.object<frames>) and "
                ^ string_of_typ t ^ " fails to adhere to this form")
        | _ -> ()
    in check_typ_valid_rec ogt

let rec typ_erase_param (cx: contexts) (t: typ) : TypedAst.etyp = 
    debug_print ">> tag_erase_param";
    match t with
    | ParTyp(s, tl) -> if Assoc.mem s cx.pm then 
        let c = Assoc.lookup s cx.pm in 
        TypedAst.AbsTyp (s, constrain_erase cx c)
        else error cx ("AbsTyp " ^ s ^ " was not found in function parameterization definition")
    | _ -> typ_erase cx t

and typ_erase (cx: contexts) (t : typ) : TypedAst.etyp =
    debug_print ">> tag_erase";
    let d_to_c opd = match opd with
    | DimNum i -> ConstInt(i) 
    | DimVar s -> ConstVar(s)
    | _ -> error cx ("No valid concrete interpretation of " ^ string_of_typ t) in
    match t with
    | UnitTyp -> TypedAst.UnitTyp
    | BoolTyp -> TypedAst.BoolTyp
    | IntTyp -> TypedAst.IntTyp
    | FloatTyp -> TypedAst.FloatTyp
    | ArrTyp (t', d) -> TypedAst.ArrTyp (typ_erase cx t', d_to_c d)
    | CoordTyp _ | ParTyp _ | Literal _ -> typ_erase cx (primitive cx t)
    | AutoTyp -> error cx ("Illegal use of auto (cannot use auto as part of a function call)")
    | _ -> debug_fail cx "Invalid use of typ_erase"

and constrain_erase (cx: contexts) (t: typ) : TypedAst.constrain =
    debug_print ">> constrain_erase";
    match t with
    | AnyTyp -> TypedAst.AnyTyp
    | GenTyp -> TypedAst.GenTyp
    | GenArrTyp _ -> debug_fail cx "unimplemented genarrtyp erasure"
    | _ -> TypedAst.ETypConstraint(typ_erase cx t)

let rec etyp_to_typ (e : TypedAst.etyp) : typ =
    debug_print ">> etyp_to_typ";
    match e with 
    | TypedAst.UnitTyp -> UnitTyp
    | TypedAst.BoolTyp -> BoolTyp
    | TypedAst.IntTyp -> IntTyp
    | TypedAst.FloatTyp -> FloatTyp
    | TypedAst.VecTyp n -> ArrTyp (FloatTyp, DimNum n)
    | TypedAst.MatTyp (n1, n2) -> ArrTyp(ArrTyp(FloatTyp, DimNum n1), DimNum n2)
    | TypedAst.TransTyp (s1, s2) -> failwith "unimplemented removal of transtyp from typedast"
    | TypedAst.AbsTyp (s, c) -> ParTyp(s, [])
    | TypedAst.ArrTyp (t, c) -> ArrTyp (etyp_to_typ t, 
        match c with | ConstInt i -> DimNum i | ConstVar v -> DimVar v)

and constrain_to_typ (c : TypedAst.constrain) : typ =
    debug_print ">> constrain_to_constrain";
    match c with
    | TypedAst.AnyTyp -> AnyTyp
    | TypedAst.GenTyp -> GenTyp
    | TypedAst.GenMatTyp -> GenArrTyp(FloatTyp)
    | TypedAst.GenVecTyp -> GenArrTyp(GenArrTyp(FloatTyp))
    | TypedAst.ETypConstraint t -> etyp_to_typ t

let rec check_val (cx: contexts) (v: value) : typ = 
    debug_print ">> check_aval";
    match v with
    | Bool b -> Literal BoolTyp
    | Num n -> Literal IntTyp
    | Float f -> Literal FloatTyp
    | ArrLit v -> Literal 
        (ArrTyp (List.fold_left (least_common_parent cx) BotTyp (List.map (check_val cx) v), 
        DimNum (List.length v)))
    | _ -> error cx ("Unexpected typechecker value " ^ (string_of_value v))

(* "scalar linear exp", (i.e. ctimes) returns generalized MatTyp *)
let check_ctimes_exp (cx: contexts) (t1: typ) (t2: typ) : typ = 
    debug_print ">> check_ctimes_exp";
    if is_bounded_by cx t1 GenVecTyp
    then (least_common_parent cx t1 t2 |> typ_ignore; UntaggedVecTyp (vec_dim cx t1))
    else if is_bounded_by cx t1 GenMatTyp 
    then (least_common_parent cx t1 t2 |> typ_ignore; 
        (let (left, right) = as_matrix_pair cx t1 in trans_top (vec_dim cx left) (vec_dim cx right)))
    else raise (TypeExceptionMeta ("Invalid expressions for component wise multiplication: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2), cx.meta))

(* Type check binary bool operators (i.e. &&, ||) *)
let check_bool_binop (cx: contexts) (t1: typ) (t2: typ) : typ = 
    debug_print ">> check_bool_binop";
    if is_subtype cx t1 BoolTyp then least_common_parent cx t1 t2
    else raise (TypeExceptionMeta ("Expected boolean expression for binop", cx.meta))

(* Type check unary number operators (i.e. -) *)
let check_num_unop (cx: contexts) (t: typ) : typ =
    debug_print ">> check_num_unop";
    if is_non_bool cx t then t
    else raise (TypeExceptionMeta ("Expected integer, float, vector, or matrix expression", cx.meta))

(* Type check unary bool operators (i.e. !) *)
let check_bool_unop (cx: contexts) (t: typ) : typ =
    debug_print ">> check_bool_unop";
    if is_subtype cx t BoolTyp then t
    else raise (TypeExceptionMeta ("Expected boolean expression for boolean operator", cx.meta))

(* Type check unary bool operators (i.e. !) *)
let check_swizzle (cx: contexts) (s : id) (t: typ) : typ =
    debug_print ">> check_swizzle";
    let check_reg = if Str.string_match (Str.regexp "[xyzwrgbastpq]+") s 0 
        then if String.length s == 1 then FloatTyp else UntaggedVecTyp (String.length s)
        else raise (TypeExceptionMeta ("Invalid characters used for swizzling in " ^ s, cx.meta)) in
    if is_bounded_by cx t GenVecTyp then check_reg
    else raise (TypeExceptionMeta ("Expected vector for swizzling", cx.meta))

(* Type check equality (==) *)
(* Only bool, int, float are comparable *)
let check_equality_exp (cx: contexts) (t1: typ) (t2: typ) : typ = 
    debug_print ">> check_comp_binop";
    let check_typs = check_subtype_list cx [BoolTyp; IntTyp; FloatTyp] in
    if check_typs t1
    then (least_common_parent cx t1 t2 |> typ_ignore; BoolTyp)
    else raise (TypeExceptionMeta ("Equality checks must be between booleans, integers, or floats", cx.meta))

(* Type check comparative binary operators (i.e. <. <=) *)
(* Only int and float are comparable *)
let check_comp_binop (cx: contexts) (t1: typ) (t2: typ) : typ = 
    debug_print ">> check_comp_binop";
    let check_typs = check_subtype_list cx [IntTyp; FloatTyp] in
    if check_typs t1
    then (least_common_parent cx t1 t2 |> typ_ignore; BoolTyp)
    else raise (TypeExceptionMeta ("Comparison checks must be between integers or floats", cx.meta))

let check_as_exp (cx: contexts) (t1: typ) (t2: typ) : typ =
    debug_print (">> check_as_exp " ^ string_of_typ t1 ^ " " ^ string_of_typ t2);
    least_common_parent cx t1 t2 |> typ_ignore; t2

(* Type checking addition operations on scalar (int, float) expressions *)
(* Types are closed under addition and scalar multiplication *)
let check_addition_exp (cx: contexts) (t1: typ) (t2: typ) : typ =
    debug_print ">> check_addition";
    if is_non_bool cx t1 then least_common_parent cx t1 t2
    else raise (TypeExceptionMeta ("Invalid expressions for addition: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2), cx.meta))

(* Type checking times operator - on scalar mult & matrix transformations *)
let check_times_exp (cx: contexts) (t1: typ) (t2: typ) : typ = 
    debug_print ">> check_times_exp";
    let fail () = raise (TypeExceptionMeta ("Invalid expressions for multiplication: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2), cx.meta)) in
    let check_typs = check_subtype_list cx [IntTyp; FloatTyp] in
    if is_subtype cx t1 IntTyp && is_subtype cx t2 IntTyp then 
        least_common_parent cx t1 t2
    (* Scalar multiplication *)
    else if check_typs t1 && is_non_bool cx t2 then t2
    else if check_typs t2 && is_non_bool cx t1 then t1
    (* Matrix-vector multiplication *)
    else if is_bounded_by cx t1 GenMatTyp then
        (let (t1l, t1r) = as_matrix_pair cx t1 in
        if is_bounded_by cx t2 GenVecTyp then
            (if is_subtype cx t2 t1l then t1r else fail ())
        else if is_bounded_by cx t2 GenMatTyp then
            (let (t2l, t2r) = as_matrix_pair cx t2 in
            least_common_parent cx t1l t2r |> typ_ignore; TransTyp(t2l, t1r))
        else fail ())
    else fail ()

(* Type checking division operations (/) *)
(* Types are closed under scalar division *)
let check_division_exp (cx: contexts) (t1: typ) (t2: typ) : typ =
    debug_print ">> check_division";
    let check_typs = check_subtype_list cx [IntTyp; FloatTyp] in
    if check_typs t1
        then least_common_parent cx t1 t2
    else if check_typs t2 && is_bounded_by cx t1 GenVecTyp
        then t1
    else raise (TypeExceptionMeta ("Invalid expressions for division: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2), cx.meta))

let check_index_exp (cx: contexts) (t1: typ) (t2: typ) : typ =
    debug_print ">> check_index_exp";
    let fail _ = raise (TypeExceptionMeta ("Invalid expressions for indexing: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2), cx.meta)) in
    if is_subtype cx t2 IntTyp then
        if is_bounded_by cx t1 GenVecTyp then FloatTyp
        else if is_bounded_by cx t1 GenMatTyp then
            UntaggedVecTyp (vec_dim cx (fst (as_matrix_pair cx t1)))
        else begin
        match t1 with
        | ArrTyp (t, _) -> t
        | _ -> fail ()
        end
    else fail ()

let check_parameterization_decl (cx: contexts) (pmd: parameterization_decl) : unit =
    debug_print ">> check_parameterization_decl";
    let rec check_para_list param found : unit = 
        match param with
        | [] -> ()
        | (s, c)::t -> if Assoc.mem s found then raise (TypeExceptionMeta ("Duplicate parameter `" ^ s, cx.meta)) 
            else 
            let updated_found = (Assoc.update s c found) in
            (match c with
            | TypConstraint t' -> check_typ_valid (w_pm cx found) t'; ()
            | _ -> ());
            (check_para_list t updated_found);
        in
    check_para_list pmd Assoc.empty; ()

let as_par_typ (cx: contexts) (t: typ) : string * typ list =
    match t with
    | VarTyp s -> (s, [])
	| ParTyp (VarTyp s, tl) -> (s, tl)
	| AbsTyp s -> ("`" ^ s, [])
	| _ -> failwith ("Unexpected type " ^ string_of_typ t ^ " provided to manipulating psi")
	
let update_psi (cx: contexts) (ml: modification list) (start: typ) 
(target: typ) ((f, pml) : string * typ list) : psi =
    (* Update psi, raising errors in case of a duplicate *)
    (* If the given type is not valid in psi, psi is returned unmodified *)
    (* Will raise a failure if a non-concrete vartyp is used *)
    debug_print ">> update_psi";
    let is_valid (t: typ) : bool =
        match t with
        | VarTyp _ 
        | ParTyp _
        | AbsTyp _ -> true
        | _ -> false
    in
    if not (is_valid start) || not (is_valid target) then cx.ps else
    let rec check_var_typ_eq (t1: typ) (t2: typ) : bool =
        match (t1, t2) with
        | TopVecTyp n1, TopVecTyp n2 -> n1 = n2
        | VarTyp s1, VarTyp s2 -> s1 = s2
        | ParTyp (t1, tl1), ParTyp (t2, tl2) -> check_var_typ_eq t1 t2 && 
            (if List.length tl1 = List.length tl2
            then List.fold_left2 (fun acc t1' t2' -> acc && check_var_typ_eq t1' t2') true tl1 tl2
            else false)
        | _ -> false
    in
	let (s1, stl) = as_par_typ cx start in
	let (s2, ttl) = as_par_typ cx target in
    let start_index = string_of_typ start in
    let to_add = (target, (f, pml)) in
    if List.mem Canon ml then
        if Assoc.mem start_index cx.ps then 
        (let start_list = Assoc.lookup start_index cx.ps in
            if (List.fold_left (fun acc (lt, (_, _)) -> acc ||
                    (let (s2, tl2) = as_par_typ cx lt in
                    if (List.length ttl = List.length tl2) 
                    then List.fold_left2 (fun acc' t1 t2 -> acc' || (check_var_typ_eq t1 t2)) false ttl tl2
                    else false))
                false start_list)
            then raise (TypeExceptionMeta ("Duplicate transformation for " ^ 
                start_index ^ "->" ^ string_of_typ (ParTyp(VarTyp s1, ttl)) ^
                " in the declaration of " ^ f, cx.meta))
            else Assoc.update start_index (to_add :: start_list) cx.ps
        )
        else Assoc.update start_index [to_add] cx.ps
    else cx.ps

let update_psi_matrix (cx: contexts) (ml: modification list) (t: typ) (f: string) : psi =
    match t with
    | TransTyp (t1, t2) -> update_psi cx ml t1 t2 (f, []) 
    | _ -> cx.ps

(* Type check parameter; make sure there are no name-shadowed parameter names *)
(* TODO : parametrized types *)
let check_param (cx: contexts) ((ml, t, id): (modification list * typ * string)) : gamma * psi = 
    debug_print ">> check_param";
    if Assoc.mem id cx.g 
    then raise (TypeExceptionMeta ("Duplicate parameter name in function declaration: " ^ id, cx.meta))
    else check_typ_valid cx t; 
    Assoc.update id t cx.g, update_psi_matrix cx ml t id
    
(* Get list of parameters from param list *)
let check_params (cx: contexts) (pl : params) : gamma * psi * TypedAst.params = 
    debug_print ">> check_params";
    let wrap_gps (g, ps) = w_g (w_ps cx ps) g  in
    let (g', ps') = List.fold_left (fun acc x -> check_param (wrap_gps acc) x) (cx.g, cx.ps) pl in 
    let p = (List.map (fun (_, t, i) -> (tag_erase cx t, i)) pl) in 
    (g', ps', p)
    
let exp_to_texp (cx: contexts) (checked_exp : TypedAst.exp * typ) : TypedAst.texp = 
    debug_print ">> exp_to_texp";
    ((fst checked_exp), (tag_erase cx (snd checked_exp)))

(* Super expensive.  We're essentially relying on small contexts *)
let check_in_exp (cx: contexts) (start_exp: aexp) (start: typ) (target: typ) : aexp = 
    debug_print ">> check_in_exp";
    let meta = snd start_exp in
    let rec psi_path_rec (to_search: (typ * aexp) Queue.t) (found: typ list) : aexp =
        let search_phi (tl: typ) (ps_lst : (typ * fn_inv) list) : (typ * fn_inv) list =
            (* This function searches phi for canonical abstract functions that map from the given type *)
            (* A list of the types these functions map with the inferred type parameters is returned *)
            (* If multiple functions are possible, then ambiguities are resolved with the following priorities *)
            (* 1. Minimize upcasting requirements (actually handled by use of this function) *)
            (* 2. Minimize number of type parameters *)
            (* 3. Minimize constraint bounds *)            
            let rec search_phi_rec (fns : (string * fn_typ) list) : (typ * (id * typ list * constrain list)) list =
                match fns with
                (* Note that matrices are always selected over canonical function invocations *)
                | [] -> List.map (fun (t, (x, y)) -> (t, (x, y, []))) ps_lst 
                | (id, (params, rt, pr, meta')) :: t -> 
                    let cx' = w_meta cx meta' in
                    if List.mem Canon (Assoc.lookup id cx.m) then
                        let pt = match params with | [(_,pt,_)] -> pt | _ -> failwith ("function " ^ id ^ " with non-one argument made canonical") in
                        match infer_pml cx (params, rt, pr, meta) [tl] with | None -> search_phi_rec t | Some pml ->
                        let pr1 = List.map snd (Assoc.bindings pr) in
                        let rtr = replace_abstype (fst (match_parameterization_unsafe cx' pr pml)) rt in
                        let ptr = replace_abstype (fst (match_parameterization_unsafe cx' pr pml)) pt in
                        let fail id2 s = raise (TypeExceptionMeta ("Ambiguity between viable canonical functions " 
                            ^ id ^ " and " ^ id2 ^ " (" ^ s ^ ")", cx.meta)) in
                        let compare_parameterizations id2 (acc : bool option) c1 c2 : bool option = 
                            let result = is_sub_constraint cx' c1 c2 in match acc with | None -> Some result
                            | Some b -> if b = result then acc else fail id2 
                            ("ambiguous constraint ordering between " ^ string_of_constraint c1 ^ " and " ^ string_of_constraint c2)
                        in
                        if not (is_subtype cx tl ptr) then search_phi_rec t else
                        match rtr with
                        | TopVecTyp _ -> search_phi_rec t
                        | AbsTyp _
                        | VarTyp _ 
                        | ParTyp (VarTyp _, _) -> let rec_result = search_phi_rec t in
                            if List.fold_left (fun acc (rt, _) -> is_typ_eq cx rt rtr || acc) false rec_result then
                                List.map (fun (rt, (id2, pml2, pr2)) -> 
                                if (List.length pr1 = List.length pr2) && (List.length pr1 = 0) then
                                fail id2 ("duplicate concrete paths from " ^ string_of_typ tl ^ " to " ^ string_of_typ rtr)
                                else if not (is_typ_eq cx' rt rtr) then (rt, (id2, pml2, pr2))
                                else if List.length pr1 < List.length pr2 then (rt, (id, pml, pr1))
                                else if List.length pr2 < List.length pr1 then (rt, (id2, pml2, pr2))
                                else if (match List.fold_left2 (compare_parameterizations id2) None pr1 pr2 with
                                    | None -> failwith "Unexpected concrete function type duplicates in phi" 
                                    | Some b -> b) then (rt, (id2, pml2, pr2))
                                else (rtr, (id, pml, pr1))) rec_result
                            (* No duplicate type result found, just add this function to the list *)
                            else (rtr, (id, pml, pr1)) :: rec_result
                        | _ -> raise (TypeExceptionMeta ("Canonical function " ^ id ^ " resulted in type "
                            ^ (string_of_typ rtr) ^
                            ", while canonical functions should always result in an abs or vartyp", cx.meta))
                    else search_phi_rec t
            in
            List.map (fun (t, (x, y, z)) -> (t, (x, y))) (search_phi_rec (Assoc.bindings cx.p))
        in
        let rec psi_lookup_rec (nt: typ) : (typ * fn_inv) list =
            (* NOTE: paths which would send to a type with more than 5 generic levels are rejected to avoid infinite spirals *)
            let rec check_typ_ignore (t: typ) (count: int) : bool =
                if count > 5 then true else
                match t with
                | ParTyp (_, tl) -> List.fold_left (fun acc t -> acc || check_typ_ignore t (count + 1)) false tl
                | _ -> false
            in
            if check_typ_ignore nt 0 then [] else
            let s_lookup = string_of_typ nt in
            let ps_lst = if Assoc.mem s_lookup cx.ps then Assoc.lookup s_lookup cx.ps else [] in
            let to_return = search_phi nt ps_lst in
            let (ns, ntl) = as_par_typ cx nt in
            let next_step = match nt with | VarTyp _ | ParTyp _ -> delta_lookup_unsafe cx ntl ns | _ -> nt in
            (match next_step with
            | VarTyp _
            | ParTyp _ -> 
                to_return @ psi_lookup_rec next_step
            | _ -> to_return)
        in 
        let rec update_search_and_found (vals: (typ * fn_inv) list) (e: aexp) : typ list =
            match vals with
            | [] -> found
            | (t1, (v, pml))::t -> 
                if List.fold_left (fun acc t2 -> acc || is_typ_eq cx t1 t2) false found 
                then update_search_and_found t e 
                else 
                let e' = 
                    if Assoc.mem v cx.g then (Binop ((Var v, snd e), Times, e), snd e)
                    else if Assoc.mem v cx.p then (FnInv (v, pml, [e]), snd e)
                    else failwith ("Typechecker error: unknown value " ^ v ^ " loaded into psi") in
                (* Note the update to the stateful queue *)
                (Queue.push (t1, e') to_search;  t1 :: update_search_and_found t e)
        in
        let (nt, e) = if Queue.is_empty to_search 
            then (raise (TypeExceptionMeta ("Cannot find a path from " ^
                string_of_typ start ^ " to " ^ string_of_typ target, cx.meta)))
            else Queue.pop to_search 
        in 
        (* We use the 'with_strictness' version to avoid throwing an exception *)
        if is_subtype cx nt target then e
        else psi_path_rec to_search (update_search_and_found (psi_lookup_rec nt) e)
    in	
	if string_of_typ start = string_of_typ target then start_exp else
	let q = Queue.create () in Queue.push (start, start_exp) q;
	psi_path_rec q []

let rec check_aexp (cx: contexts) ((e, meta) : aexp) : TypedAst.exp * typ =
    check_exp (w_meta cx meta) e

and check_exp (cx: contexts) (e : exp) : TypedAst.exp * typ = 
    debug_print ">> check_exp";
    let build_unop (op : unop) (e': aexp) (check_fun: typ->typ)
        : TypedAst.exp * typ =
        let result = check_aexp cx e' in
            (TypedAst.Unop(op, exp_to_texp cx result), check_fun (snd result))
    in
    let build_binop (op : binop) (e1: aexp) (e2: aexp) (check_fun: typ->typ->typ)
        : TypedAst.exp * typ =
        let e1r = check_aexp cx e1 in
        let e2r = check_aexp cx e2 in
            (TypedAst.Binop(exp_to_texp cx e1r, op, exp_to_texp cx e2r), check_fun (snd e1r) (snd e2r))
    in
    match e with
    | Val v -> (TypedAst.Val v, check_val cx v)
    | Var v -> "\tVar "^v |> debug_print;
        if Assoc.mem v cx.g then (TypedAst.Var v, Assoc.lookup v cx.g) else
        raise (TypeExceptionMeta ("Unknown variable " ^ v, cx.meta))
    | Arr a -> check_arr cx a
    | As (e', t) -> let (er, tr) = check_aexp cx e' in (er, check_as_exp cx tr t)
    | In (e', t) -> let (_, tr) = check_aexp cx e' in 
        check_aexp cx (check_in_exp cx e' tr t)
    | Unop (op, e') -> let f = match op with
            | Neg -> check_num_unop cx
            | Not -> check_bool_unop cx
            | Swizzle s -> check_swizzle cx s in
        build_unop op e' f
    | Binop (e1, op, e2) -> let f = match op with
            | Eq -> check_equality_exp
            | Leq | Lt | Geq | Gt -> check_comp_binop
            | Or | And -> check_bool_binop
            | Plus | Minus -> check_addition_exp
            | Times -> check_times_exp
            | Div  -> check_division_exp
            | CTimes -> check_ctimes_exp
            | Index -> check_index_exp
        in build_binop op e1 e2 (f cx)
    | FnInv (i, args, pr) -> let ((i, tpl, args_exp), rt) = check_fn_inv cx args i pr in 
        (FnInv (i, tpl, args_exp), rt)
        
and check_arr (cx: contexts) (a : aexp list) : (TypedAst.exp * typ) =
    debug_print ">> check_arr";
    let is_vec (v: TypedAst.texp list) : bool =
        List.fold_left (fun acc (_, t) -> match t with
            | TypedAst.IntTyp | TypedAst.FloatTyp -> acc | _ -> false) true v
    in
    let is_mat (v: TypedAst.texp list) : int option =
        match List.hd v with
        | (_, TypedAst.VecTyp size) ->
        List.fold_left (fun acc (_, t) -> match t with
            | TypedAst.VecTyp n -> if (n == size) then acc else None | _ -> None) (Some size) v
        | _ -> None
    in
    let checked_a = List.map (fun e -> (exp_to_texp cx (check_aexp cx e))) a in
    let length_a = List.length a in
    if is_vec checked_a then (TypedAst.Arr checked_a, BotVecTyp length_a) else 
    (match is_mat checked_a with
    | Some n -> (TypedAst.Arr checked_a, trans_bot n length_a)
    | None ->  raise (TypeExceptionMeta ("Invalid array definition for " ^ 
        (string_of_exp (Arr a)) ^ ", must be a matrix or vector", cx.meta)))


and check_fn_inv (cx: contexts) (pml: typ list) (x : id) (args : args) 
: (string * TypedAst.etyp list * TypedAst.args) * typ = 
    debug_print (">> check_fn_inv " ^ x);
    let fn_invocated = if Assoc.mem x cx.p
        then Assoc.lookup x cx.p
        else raise (TypeExceptionMeta ("Invocated function " ^ x ^ " not found", cx.meta)) in
    let (_, rt, _, _) = fn_invocated in
    let args' = List.map (fun a -> check_aexp cx a) args in 
    let args_typ = List.map snd args' in
    (* find definition for function in phi *)
    (* looks through all possible overloaded definitions of the function *)
    let find_fn_inv ((params, rt, pr, meta') : fn_typ) : (typ Assoc.context) option =
        let cx' = w_meta cx meta' in
        debug_print ">> find_fn_inv";
        (* This function asserts whether or not the function invocation matches the function given *)
        (* In particular, this checks whether the given function matches the given parameterization and parameters *)
        (* If it is valid, this returns (Some 'map from parameter to type'), otherwise returns 'None' *)

        (* If we have the wrong number of arguments, then no match for sure *)
        if List.length args != List.length params then None else
        (* Work out the parameter inference if one is needed *)
        let inferred_pml = 
            (if Assoc.size pr == List.length pml then Some pml
            else if List.length pml == 0 then infer_pml cx' (params, rt, pr, meta') args_typ
            else None)
        in
        match inferred_pml with
         | None -> None | Some pml' ->
        (* Helper function for using the function parameters as they are constructed *)
        let apply_fpm (c: constrain) (fpm : typ Assoc.context) : constrain =
            let rec in_function_t t = 
                match t with
                | AbsTyp s -> Assoc.lookup s fpm
                | TransTyp (t1, t2) -> TransTyp (in_function_t t1, in_function_t t2)
                | ParTyp (t, tl) -> ParTyp (in_function_t t, List.map in_function_t tl)
                | _ -> t
            in
            match c with
            | TypConstraint t -> TypConstraint (in_function_t t)
            | _ -> c
        in
        (* Check that the parameterization conforms to the bounds provided *)
        let param_check = 
            debug_print ">> param_check";
            List.fold_left2 (fun acc given_pm (s, c) -> 
            (match acc with 
            | None -> None
            | Some fpm -> let bound = apply_fpm c fpm in 
                if is_bounded_by cx given_pm bound
                then Some (Assoc.update s given_pm fpm) else None))
            (Some Assoc.empty) pml' (Assoc.bindings pr)
        in
        match param_check with | None -> None | Some pm_map ->
        (* Get the parameters types and replace them in params_typ *)
        let params_typ = List.map (fun (_,a,_) -> a) params in
        let rec read_pm (t : typ) : typ =
            match t with
            | AbsTyp s -> Assoc.lookup s pm_map
            | TransTyp (t1, t2) -> TransTyp (read_pm t1, read_pm t2)
            | ParTyp (t, tl) -> ParTyp (read_pm t, List.map read_pm tl)
            | _ -> t
        in
        let params_typ_corrected = List.map read_pm params_typ in
        (* Finally, check that the arg and parameter types match *)
        if List.length args_typ == List.length params_typ then
            List.fold_left2 (fun acc arg param -> if (is_subtype cx arg param) then acc else None)
            param_check args_typ params_typ_corrected
        else None
    in
    (match find_fn_inv fn_invocated with
    | Some l -> ((x, List.rev (List.map (fun p -> tag_erase cx (snd p)) (Assoc.bindings l)), 
        List.map (fun a -> exp_to_texp cx a) args'), replace_abstype l rt)
    | None -> raise (TypeExceptionMeta ("No overloaded function declaration of " ^ x
    ^ (if List.length pml > 0 then "<" ^ (String.concat "," (List.map string_of_typ pml)) ^ ">" else "")
    ^ " matching types (" ^ (String.concat "," (List.map string_of_typ args_typ)) ^ ") found", cx.meta))) 

and check_acomm (cx: contexts) ((c, meta): acomm) : gamma * psi * TypedAst.comm =
    check_comm (w_meta cx meta) c

and wrap_comm cx (g, ps, c) = (w_g (w_ps cx ps) g, c)
and check_comm (cx: contexts) (c: comm) : gamma * psi * TypedAst.comm =
    debug_print ">> check_comm";
    let wrap_gps (g, ps, c) = (w_g (w_ps cx ps) g, c) in
    match c with
    | Skip -> cx.g, cx.ps, TypedAst.Skip
    | Print e -> (
        let (e, t) = exp_to_texp cx (check_aexp cx e) in 
        match t with
        | UnitTyp -> raise (TypeExceptionMeta ("Print function cannot print void types", cx.meta))
        | _ -> cx.g, cx.ps, TypedAst.Print (e, t)
    )
    | Inc x -> let x_typ = (Assoc.lookup x cx.g) in (match x_typ with
        | IntTyp -> cx.g, cx.ps, TypedAst.Inc (x, TypedAst.IntTyp)
        | FloatTyp -> cx.g, cx.ps, TypedAst.Inc (x, TypedAst.FloatTyp)
        | _ -> raise (TypeExceptionMeta ("increment must be applied to an integer or float", cx.meta)))
    | Dec x -> let x_typ = (Assoc.lookup x cx.g) in (match x_typ with
        | IntTyp -> cx.g, cx.ps, TypedAst.Dec (x, TypedAst.IntTyp)
        | FloatTyp -> cx.g, cx.ps, TypedAst.Dec (x, TypedAst.FloatTyp)
        | _ -> raise (TypeExceptionMeta ("decrement must be applied to an integer or float", cx.meta)))
    | Decl (ml, t, s, e) -> 
        if Assoc.mem s cx.g then raise (TypeExceptionMeta ("variable name shadowing is illegal", cx.meta))
        else 
        (check_typ_valid cx t; 
        let result = check_aexp cx e in
        let t' = (match t with | AutoTyp -> 
            (match (snd result) with
                | BotVecTyp _ -> raise (TypeExceptionMeta ("Cannot infer the type of a vector literal", cx.meta))
                | TransTyp (TopVecTyp _, BotVecTyp _) -> raise (TypeExceptionMeta ("Cannot infer the type of a matrix literal", cx.meta))
                | t' -> t')
            | TopVecTyp _ -> raise (TypeExceptionMeta ("Cannot declare a variable of the top vec type", cx.meta))
            | TransTyp (TopVecTyp _, _)
            | TransTyp (_, TopVecTyp _) -> raise (TypeExceptionMeta ("Cannot declare a transformation matrix with the top vec type", cx.meta))
            | _ -> t) in
            check_assign cx t' s (snd result), update_psi_matrix cx ml t s,
                TypedAst.Decl (tag_erase cx t', s, (exp_to_texp cx result)))
    | Assign (s, e) ->
        if Assoc.mem s cx.g then
            let t = Assoc.lookup s cx.g in
            let result = check_aexp cx e in
            check_assign cx t s (snd result), cx.ps, TypedAst.Assign (s, (exp_to_texp cx result))
        else raise (TypeExceptionMeta ("Assignment to undeclared variable: " ^ s, cx.meta))
    | AssignOp (s, b, e) -> 
        let (g', ps', c') = check_acomm cx 
            (Assign (s, (Binop((Var s, snd e), b, e), cx.meta)), cx.meta) in
        (match c' with
        | TypedAst.Assign (_, (TypedAst.Binop ((_, st), _, e), _)) -> 
            g', ps', TypedAst.AssignOp((s, st), b, e)
        | _ -> failwith "Assign must return an assign?")
    | If ((b, c1), el, c2) ->
        let check_if b c =
            let er = (check_aexp cx b) in
            let _, _, cr = check_comm_lst cx c in
            (match snd er with 
            | BoolTyp -> ((exp_to_texp cx er), cr)
            | _ -> raise (TypeExceptionMeta ("Expected boolean expression for if condition", cx.meta)))
        in
        let c2r = (match c2 with | Some e -> Some (tr_thd (check_comm_lst cx e)) | None -> None) in
        cx.g, cx.ps, TypedAst.If (check_if b c1, List.map (fun (b, c) -> check_if b c) el, c2r)
    | For (c1, b, c2, cl) ->
        let cx', c1r = wrap_gps (check_acomm cx c1) in
        let br, brt = check_aexp cx' b in
        let btexp = exp_to_texp cx (br, brt) in
        let cx'', c2r = wrap_gps (check_acomm cx' c2) in
        cx.g, cx.ps, TypedAst.For (c1r, btexp, c2r, (tr_thd (check_comm_lst cx'' cl)))
    | Return e ->
        cx.g, cx.ps, TypedAst.Return(option_map (exp_to_texp cx |- check_aexp cx) e)
    | FnCall (it, args, pml) -> (match it with
        | VarTyp i -> 
            let ((i, tpl, args_exp), _) = check_fn_inv cx args i pml in 
            cx.g, cx.ps, TypedAst.FnCall (i, tpl, args_exp)
        | _ -> raise (TypeExceptionMeta ("Cannot treat the type " ^ string_of_typ it ^ " as a function call", cx.meta)))

and check_comm_lst (cx: contexts) (cl : acomm list) : gamma * psi * TypedAst.comm list = 
    debug_print ">> check_comm_lst";
    let wrap_gps (g, ps, c) = (w_g (w_ps cx ps) g, c) in
    match cl with
    | [] -> cx.g, cx.ps, []
    | h::t -> let cx', c' = wrap_gps (check_acomm cx h) in
        let g', ps', cl' = check_comm_lst cx' t  in 
        g', ps', c' :: cl'

and check_assign (cx: contexts) (t: typ) (s: string) (etyp : typ) : gamma =
    debug_print (">> check_assign <<"^s^">>");
    debug_print (string_of_typ t);
    (* Check that t, if not a core type, is a registered tag *)
    let rec check_tag (t: typ) : unit =
        match t with
        | VarTyp s -> delta_lookup cx [] s |> typ_ignore; ()
        | ParTyp (VarTyp s, pml) -> delta_lookup cx pml s |> typ_ignore; ()
        | TransTyp (t1, t2) -> check_tag t1; check_tag t2; ()
        | _ -> ()
    in
    check_tag t;
    let check_name regexp = if Str.string_match regexp s 0 then raise (TypeExceptionMeta ("Invalid variable name " ^ s, cx.meta)) in
    check_name (Str.regexp "int$");
    check_name (Str.regexp "float$");
    check_name (Str.regexp "bool$");
    check_name (Str.regexp "vec[0-9]+$");
    check_name (Str.regexp "mat[0-9]+$");
    check_name (Str.regexp "mat[0-9]+x[0-9]+$");
    if Assoc.mem s cx.d then 
        raise (TypeExceptionMeta ("Variable " ^ s ^ " has the name of a tag", cx.meta))
    else if Assoc.mem s cx.p then
        raise (TypeExceptionMeta ("Variable " ^ s ^ " has the name of a function", cx.meta))
    else
        if is_subtype cx etyp t then Assoc.update s t cx.g
        else raise (TypeExceptionMeta ("Mismatched types for var decl for " ^ s ^
            ": expected " ^ (string_of_typ t) ^ ", found " ^ (string_of_typ etyp), cx.meta))

let check_fn_decl (cx: contexts) ((fm, id, (pmd, rt, pl)): fn_decl) : 
phi * (gamma * psi * TypedAst.params) * TypedAst.parameterization =
    debug_print (">> check_fn_decl : " ^ id);
    check_parameterization_decl cx pmd;
    let pm = collapse_parameterization_decl pmd in
    let pr = check_params cx pl in 
    check_typ_valid cx rt;
    let pme = Assoc.gen_context (List.map (fun (s, c) -> (s, constrain_erase cx c)) (Assoc.bindings pm)) in
    if Assoc.mem id cx.p 
    then raise (TypeExceptionMeta ("Function of duplicate name has been found: " ^ id, cx.meta))
    else Assoc.update id (pl, rt, pm, cx.meta) cx.p, pr, pme

(* Helper function for type checking void functions. 
 * Functions that return void can have any number of void return statements 
 * anywhere. *)
let check_void_return (cx : contexts) (c: acomm) : unit =
    debug_print ">> check_void_return";
    match c with
    | (Return Some _, _) -> raise (TypeExceptionMeta ("Void functions cannot return a value", cx.meta))
    | _ -> ()

let check_return (cx: contexts) (t: typ) (c: acomm) : unit = 
    debug_print ">> check_return";
    match c with
    | (Return None, meta) -> raise (TypeExceptionMeta ("Expected a return value instead of void", meta))
    | (Return Some r, meta) -> (
        let (_, rt) = check_aexp cx r in
        (* raises return exception of given boolean exp is false *)
        if is_subtype cx rt t then () 
        else raise (TypeExceptionMeta ("Mismatched return types, expected: " ^ 
        (string_of_typ t) ^ ", found: " ^ (string_of_typ rt), meta))
        )
    | _ -> ()

let update_mu_with_function (cx: contexts) (((fm, id, (pmd, r, pr))): fn_decl) : mu =
    let m' = (Assoc.update id fm cx.m) in
    let cx' = w_m cx m' in
    if List.mem Canon fm then
        match pr with
        (* Only update if it is a canon function with exactly one argument *)
        (* TODO: add to phi, not to psi unless it is concrete *)
        | [(_,t,_)] ->
        begin
            if is_typ_eq cx' t r then raise (TypeExceptionMeta 
                ("Canonical function " ^ id ^ " cannot be a map from a type to itself", cx.meta)) else
            let fail _ = raise (TypeExceptionMeta 
            ("Canonical functions must be between tag or abstract types", cx.meta)) in
            match t with
            | VarTyp _
            | ParTyp (VarTyp _, _)
            | AbsTyp _ ->  
            begin
                match r with
                | VarTyp _
                | ParTyp (VarTyp _, _)
                | AbsTyp _ ->  m'
                | _ -> fail ()
            end
            | _ -> fail ()
        end
        | _ -> raise (TypeExceptionMeta ("Cannot have a canonical function with zero or more than one arguments", cx.meta))
    else m'

let check_typ_decl (cx: contexts) ((s, pmd, t) : typ_decl) : delta =
    debug_print ">> check_tag_decl";
    let pm = collapse_parameterization_decl pmd in
    let rec check_valid_supertype (t: typ) : constrain =
        match t with
        | TopVecTyp _ -> TypConstraint t
        | VarTyp s -> 
            if not (Assoc.mem s cx.d) then raise (TypeExceptionMeta ("Unknown tag " ^ s, cx.meta)) else TypConstraint t
        | ParTyp (VarTyp s, pml) -> 
            if not (Assoc.mem s cx.d) then raise (TypeExceptionMeta ("Unknown tag " ^ s, cx.meta))
            else let (tpm, _) = Assoc.lookup s cx.d in
            let pmb = Assoc.bindings tpm in
            if List.length pmb == List.length pml
            then (List.fold_left2 (fun acc (s, c) t -> if is_sub_constraint cx t c then () else
                raise (TypeExceptionMeta ("Invalid constraint used for parameterization of " ^ s, cx.meta)))
                () (Assoc.bindings tpm) (List.map check_valid_supertype pml); TypConstraint t)
            else raise (TypeExceptionMeta ("Invalid number of parameters provided to parameterized type " ^ s, cx.meta))
        | AbsTyp s -> if Assoc.mem s pm then Assoc.lookup s pm else raise (TypeExceptionMeta ("Unknown type " ^ (string_of_typ t), cx.meta))
        | _ -> raise (TypeExceptionMeta ("Invalid type for tag declaration " ^ (string_of_typ t) ^
            ", expected vector (not an untagged vector)", cx.meta))
    in
    let rec check_param_vec_bounds (cl : constrain list) : unit =
        match cl with
        | [] -> ()
        | h::t -> if is_sub_constraint cx h GenVecTyp then check_param_vec_bounds t
            else raise (TypeExceptionMeta ("Invalid declaration of " ^ s ^ " -- must parameterize on vectors only", cx.meta))
    in
    check_valid_supertype t |> constrain_ignore;
    check_param_vec_bounds (List.map snd (Assoc.bindings pm));
    if Assoc.mem s cx.d then raise (TypeExceptionMeta ("Cannot redeclare tag", cx.meta))
    else ();
    Assoc.update s (pm, t) cx.d

let check_decls (cx: contexts) (ed : extern_decl) : gamma * mu * phi =
    match ed with
    | ExternFn f -> let (p', _, _) = check_fn_decl cx f in 
        let m' = update_mu_with_function cx f 
        in cx.g, m', p'
    | ExternVar (ml, t, (Var x, meta)) -> Assoc.update x t cx.g, Assoc.update x ml cx.m, cx.p
    | _ -> raise (TypeExceptionMeta ("Invalid declaration, must be a function or variable", cx.meta))

(* Type check global variable *)
let check_global_variable (cx: contexts) ((ml, sq, t, id, e): global_var) 
: gamma * psi * TypedAst.global_var =
    debug_print ">> check_global_variable";
    let e' = option_map (fun x -> check_aexp cx x) e in
    if Assoc.mem id cx.g
    then raise (TypeExceptionMeta ("Duplicate global variable: " ^ id, cx.meta))
    else check_typ_valid cx t; 
        Assoc.update id t cx.g, update_psi_matrix cx ml t id,
        (sq, tag_erase cx t, id, option_map (fun x -> exp_to_texp cx x) e')

let check_fn (cx: contexts) (((fm, id, (pmd, r, pr)), cl): fn) 
: mu * phi * psi * TypedAst.fn = 
    let wrap_gps (g, ps, c) = (w_g (w_ps cx ps) g, c) in
    debug_print (">> check_fn : " ^ id);
    (* update phi with function declaration *)
    let (p', (g', ps', pl'), pm') = check_fn_decl cx (fm, id, (pmd, r, pr)) in
    let pm = collapse_parameterization_decl pmd in
    (* Note that we don't use our updated phi to avoid recursion *)
    let cx' = w_pm (w_g (w_ps cx ps') g') pm in
    let (cx'', cl') = wrap_gps(check_comm_lst cx' cl) in 
    let m' = update_mu_with_function cx (fm, id, (pmd, r, pr)) in
    (* check that the last command is a return statement *)
    match r with
    | UnitTyp -> List.iter (fun c -> check_void_return cx c) cl; 
        m', p', ps', (((id, (pl', TypedAst.UnitTyp, pm')), cl'))
    (* TODO: might want to check that there is exactly one return statement at the end *)
    | t -> List.iter (check_return cx'' t) cl; 
        m', p', ps', (((id, (pl', tag_erase cx t, pm')), cl'))

let check_term (cx: contexts) (t: term) 
: contexts * TypedAst.fn option * TypedAst.global_var option =
    match t with    
    | Prototype p -> failwith ""
    | Coordinate c -> failwith ""
    | FrameDecl t -> let cx' = w_d cx (check_typ_decl cx t) in
        cx', None, None
    | TypDecl t -> let cx' = w_d cx (check_typ_decl cx t) in
        cx', None, None
    | ExternDecl ed -> let (g, m, p) = check_decls cx ed in
        w_g (w_m (w_p cx p) m) g, None, None
    | GlobalVar gv -> let (g, ps, gv') = check_global_variable cx gv in
        w_g (w_ps cx ps) g, None, Some gv'
    | Fn f -> let (m, p, ps, f') = check_fn cx f in
        w_m (w_p (w_ps cx ps) p) m, Some f', None
    
let check_aterm (cx: contexts) ((t, meta): aterm) 
: contexts * TypedAst.fn option * TypedAst.global_var option =
    check_term (w_meta cx meta) t

let rec check_term_list (tl: aterm list) :
phi * TypedAst.prog * TypedAst.global_vars =
    debug_print ">> check_global_var_or_fn_lst";
    (* Annoying bootstrapping hack *)
    if List.length tl == 0 then fresh, [], [] else
    let app_maybe o l = match o with | Some v -> v::l | None -> l in
    let cx, f, gv = List.fold_left (fun acc t -> let (cx', f', gv') = check_aterm (tr_fst acc) t in
        (cx', app_maybe f' (tr_snd acc), app_maybe gv' (tr_thd acc)))
        (init_contexts (snd (List.hd tl)), [], []) tl in
    cx.p, List.rev f, List.rev gv

(* Check that there is a void main() defined *)
let check_main_fn (p: phi) : unit =
    debug_print ">> check_main_fn";
    let (params, ret_type, pm, meta) = Assoc.lookup "main" p in 
    debug_print (">> check_main_fn_2" ^ (string_of_list string_of_param params) ^ (string_of_parameterization pm));
    if (List.length params) > 0 || (Assoc.size pm) > 0 then raise (TypeExceptionMeta ("Cannot provide parameters to main", meta)) else
    match ret_type with
        | UnitTyp -> ()
        | _ -> raise (TypeException "Expected main function to return void")

exception TypeException of string

(* Returns the list of fn's which represent the program 
 * and params of the void main() fn *)
let check_prog (tl: prog) : TypedAst.prog * TypedAst.global_vars =
    (try
    debug_print ">> check_prog";
    (*(d: delta) ((id, t): fn_decl) (p: phi) *)
    (* delta from tag declarations *)
    let (p, typed_prog, typed_gvs) = check_term_list tl in
    check_main_fn p;
    debug_print "===================";
    debug_print "Type Check Complete";
    debug_print "===================\n";
    (typed_prog, typed_gvs)
    with TypeExceptionMeta (s, meta) -> raise (TypeException 
        (line_number meta ^ " " ^ s)))
    