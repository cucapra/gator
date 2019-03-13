open CoreAst
open TagAst
open TagAstPrinter
open Util
open Printf
open Str

exception TypeException of string
exception DimensionException of int * int

(* For readability, especially with psi *)
type fn_inv = string * typ list 

(* Variable definitions *)
(* Maps from variable names to the type of that variable *)
type gamma = typ Assoc.context

(* Tag definitions *)
(* Stores supertype and possible parameterization information for the tag *)
type delta = (parameterization * typ) Assoc.context

(* Function definitions *)
(* Stores the full type and parameterization of each function *)
type phi = fn_type Assoc.context

(* Tag and function modifiers *)
(* Specifically the coord and canonical keywords *)
(* Is currently only modified during tag and function declaration steps *)
type mu = (modification option) Assoc.context

(* Transformation context *)
(* Effectively has the type 'start->(target, f<pml>) list' for types start and target (both restricted implicitely to var types), function/matrix name f, and function parameter list pml *)
(* Note that the resulting thing could be a call with a concrete parameterization, hence the typ list (which is empty for matrices) *)
type psi = ((typ * fn_inv) list) Assoc.context

let string_of_delta (d : delta) = Assoc.to_string (fun (pm, t) -> "(" ^ string_of_parameterization pm ^ ", " ^ string_of_typ t ^ ")") d
let string_of_fn_inv ((s, tl) : fn_inv) = s ^ "<" ^ string_of_lst string_of_typ tl ^ ">"
let string_of_psi (ps : psi) = Assoc.to_string (fun x -> string_of_arr (fun (t, p) -> "(" ^ string_of_typ t ^ ", " ^ string_of_fn_inv p ^ ")") x) ps

let trans_top (n1: int) (n2: int) : typ =
    TransTyp (BotVecTyp n1, TopVecTyp n2)

let trans_bot (n1: int) (n2: int) : typ =
    TransTyp (TopVecTyp n1, BotVecTyp n2)

let rec unwrap_abstyp (s: string) (pm : parameterization) : constrain =
    debug_print ">> unwrap_abstyp";
    if Assoc.mem s pm 
    then match (Assoc.lookup s pm) with 
        | TypConstraint(AbsTyp s) ->  unwrap_abstyp s pm
        | p -> p
    else raise (TypeException ("AbsTyp " ^ s ^ " not found in parameterization"))

let as_matrix_pair (t: typ) (d: delta) (pm: parameterization) : typ * typ =
    debug_print ">> as_matrix_pair";
    (* Checks whether the given abstract type is a vector *)
    let fail _ = failwith ("Cannot treat " ^ (string_of_typ t) ^ " as a matrix") in
    match t with
    | TransTyp (t1, t2) -> (t1, t2)
    | AbsTyp s -> (match unwrap_abstyp s pm with
        | TypConstraint (TransTyp (t1, t2)) -> (t1, t2)
        | _ -> fail ())
    | _ -> fail ()

(* Given a parameterization and a list of types being invoked on that parameterization *)
(* Returns the appropriate concretized context if one exists *)
(* Should only be used on previously verified parameterized type invokations *)
let match_parameterization_unsafe (pm: parameterization) (pml : typ list) : typ Assoc.context =
    debug_print ">> match_parameterization_unsafe";
    let pmb = Assoc.bindings pm in
    if List.length pmb == List.length pml
    then List.fold_left2 (fun acc (s, _) t -> Assoc.update s t acc)
    (Assoc.empty) (Assoc.bindings pm) pml
    else raise (TypeException ("Invalid parameterization provided in " ^ (string_of_arr string_of_typ pml)))

let rec replace_abstype (t: typ) (c: typ Assoc.context) : typ =
    debug_print ">> replace_abstype";
    match t with
    | ParTyp (s, tl) -> ParTyp (s, List.map (fun x -> replace_abstype x c) tl)
    | AbsTyp s -> Assoc.lookup s c
    | TransTyp (t1, t2) -> TransTyp (replace_abstype t1 c, replace_abstype t2 c)
    | _ -> t

(* Looks up delta without checking the bounds on the pml *)
let delta_lookup_unsafe (s: id) (pml: typ list) (d: delta) : typ =
    (* If the given type evaluates to a declared a tag, return it *)
    debug_print ">> delta_lookup_unsafe";
    if Assoc.mem s d then
    let (pm, t) = Assoc.lookup s d in
    replace_abstype t (match_parameterization_unsafe pm pml)
    
    else raise (TypeException ("Unknown tag " ^ s))

let rec etyp_to_typ (e : TypedAst.etyp) : typ =
    debug_print ">> etyp_to_typ";
    match e with 
    | TypedAst.UnitTyp -> UnitTyp
    | TypedAst.BoolTyp -> BoolTyp
    | TypedAst.IntTyp -> IntTyp
    | TypedAst.FloatTyp -> FloatTyp
    | TypedAst.VecTyp n -> BotVecTyp n
    | TypedAst.MatTyp (n1, n2) -> TransTyp(BotVecTyp n1, BotVecTyp n2)
    | TypedAst.TransTyp (s1, s2) -> TransTyp(etyp_to_typ s1, etyp_to_typ s2)
    | TypedAst.SamplerTyp n -> SamplerTyp n
    | TypedAst.SamplerCubeTyp -> SamplerCubeTyp
    | TypedAst.AbsTyp (s, c) -> AbsTyp s
    | TypedAst.ArrTyp (t, c) -> ArrTyp (etyp_to_typ t, c)

and constrain_to_constrain (c : TypedAst.constrain) : constrain =
    debug_print ">> constrain_to_constrain";
    match c with
    | TypedAst.AnyTyp -> AnyTyp
    | TypedAst.GenTyp -> GenTyp
    | TypedAst.GenMatTyp -> GenMatTyp
    | TypedAst.GenVecTyp -> GenVecTyp
    | TypedAst.ETypConstraint t -> TypConstraint (etyp_to_typ t)

type dim_constrain_typ = | UnconInt of int | ConString of string
let rec vec_dim_constrain (t: typ) (d: delta) (pm: parameterization) : dim_constrain_typ option =
    debug_print ">> vec_dim";
    match t with
    | TopVecTyp n
    | BotVecTyp n -> Some (UnconInt n)
    | ParTyp (VarTyp s, pml) -> vec_dim_constrain (delta_lookup_unsafe s pml d) d pm 
    | VarTyp s -> vec_dim_constrain (delta_lookup_unsafe s [] d) d pm
    | AbsTyp s -> (match unwrap_abstyp s pm with
        | TypConstraint t' -> vec_dim_constrain t' d pm
        | GenVecTyp -> Some (ConString s)
        | _ -> None)
    | _ -> None

let vec_dim_safe (t: typ) (d: delta) (pm: parameterization) : int option =
    match vec_dim_constrain t d pm with
    | Some (UnconInt i) -> Some i 
    | Some (ConString _) -> raise (TypeException ("Vector " ^ string_of_typ t ^ " does not have a concrete dimension"))
    | None -> None

let vec_dim (t: typ) (d: delta) (pm: parameterization) : int =
    match vec_dim_safe t d pm with
    | Some i -> i
    | None -> failwith ("Expected vector for computing dimension, got " ^ string_of_typ t)

let rec tag_erase_param (t: typ) (d: delta) (pm: parameterization) : TypedAst.etyp = 
    debug_print ">> tag_erase_param";
    match t with 
    | AbsTyp s -> if Assoc.mem s pm then 
        let p = (Assoc.lookup s pm) in 
        TypedAst.AbsTyp (s, constrain_erase p d pm)
        else raise (TypeException ("AbsTyp " ^ s ^ " was not found in function parameterization definition"))
    | _ -> tag_erase t d pm 

and tag_erase (t : typ) (d : delta) (pm: parameterization) : TypedAst.etyp =
    debug_print ">> tag_erase";
    match t with
    | UnitTyp -> TypedAst.UnitTyp
    | BoolTyp -> TypedAst.BoolTyp
    | IntTyp -> TypedAst.IntTyp
    | FloatTyp -> TypedAst.FloatTyp
    | TopVecTyp _
    | BotVecTyp _
    | VarTyp _
    | ParTyp (VarTyp _, _) -> (match vec_dim_constrain t d pm with
        | Some (UnconInt i) -> TypedAst.VecTyp i 
        | Some (ConString s) -> TypedAst.AbsTyp (s, TypedAst.GenVecTyp)
        | None -> raise (TypeException ("No valid vector interpretation of " ^ string_of_typ t)))
    (* Convert to the strange glsl style of doing things *)
    (* TODO: standardize this and handle switching in the emitter *)
    | ParTyp (t', _) -> tag_erase t' d pm
    | TransTyp (t1, t2) -> 
    begin
        match (vec_dim_constrain t1 d pm, vec_dim_constrain t2 d pm) with
        | (Some (UnconInt n1), Some (UnconInt n2)) -> TypedAst.MatTyp (n2, n1)
        | _ -> TypedAst.TransTyp (tag_erase t1 d pm, tag_erase t2 d pm)
    end
    | SamplerTyp i -> TypedAst.SamplerTyp i
    | SamplerCubeTyp -> TypedAst.SamplerCubeTyp
    | AbsTyp s -> tag_erase_param t d pm 
    | ArrTyp (t, c) -> ArrTyp (tag_erase t d pm, c)
    | AutoTyp -> raise (TypeException "Illegal use of auto (cannot use auto as part of a function call)")

and constrain_erase (c: constrain) (d : delta) (pm : parameterization) : TypedAst.constrain =
    debug_print ">> constrain_erase";
    match c with
    | AnyTyp -> TypedAst.AnyTyp
    | GenTyp -> TypedAst.GenTyp
    | GenMatTyp -> TypedAst.GenMatTyp
    | GenVecTyp -> TypedAst.GenVecTyp
    | TypConstraint t -> TypedAst.ETypConstraint (tag_erase t d pm)

let rec is_typ_eq (t1: typ) (t2: typ) : bool =
    match (t1, t2) with
    | UnitTyp, UnitTyp
    | BoolTyp, BoolTyp
    | IntTyp, IntTyp
    | FloatTyp, FloatTyp
    | SamplerCubeTyp, SamplerCubeTyp -> true
    | TopVecTyp n1, TopVecTyp n2
    | BotVecTyp n1, BotVecTyp n2
    | SamplerTyp n1, SamplerTyp n2 -> n1 = n2
    | VarTyp s1, VarTyp s2
    | AbsTyp s1, AbsTyp s2 -> s1 = s2
    | TransTyp (l1, r1), TransTyp (l2, r2) -> is_typ_eq l1 l2 && is_typ_eq r1 r2
    | ParTyp (t1, tl1), ParTyp (t2, tl2) -> is_typ_eq t1 t2 && 
        (if (List.length tl1 = List.length tl2) 
        then List.fold_left2 (fun acc t1' t2' -> acc && is_typ_eq t1' t2') true tl1 tl2
        else false)
    | ArrTyp (t1, n1), ArrTyp (t2, n2) -> n1 = n2 && is_typ_eq t1 t2
    | _ -> false

let rec is_subtype_with_strictness (to_check : typ) (target : typ) (d : delta) (pm: parameterization) (strict : bool) : bool =
    debug_print (">> is_subtype " ^ (string_of_typ to_check) ^ ", " ^(string_of_typ target));
    let abstyp_step (s: string) : bool =
        if Assoc.mem s pm 
        then match (Assoc.lookup s pm) with
        | TypConstraint t -> is_subtype_with_strictness t target d pm strict
        | _ -> false
        else raise (TypeException ("AbsTyp " ^ s ^ " not found in parameterization"))
    in
    match (to_check, target) with 
    | BotVecTyp n1, BotVecTyp n2
    | BotVecTyp n1, TopVecTyp n2
    | TopVecTyp n1, TopVecTyp n2 -> n1 = n2
    | TopVecTyp _, _ -> false
    | BotVecTyp n, VarTyp _ 
    | BotVecTyp n, ParTyp (VarTyp _, _) -> n = (vec_dim target d pm)
    | BotVecTyp _, ArrTyp (IntTyp, _) -> true
    | BotVecTyp _, ArrTyp (FloatTyp, _) -> true
    | ParTyp (t1, tl1), ParTyp (t2, tl2) -> (List.length tl1 = List.length tl2
        && is_typ_eq t1 t2 && List.fold_left2 (fun acc x y -> acc && is_typ_eq x y) true tl1 tl2)
        || (match t1 with 
        | VarTyp s -> is_subtype_with_strictness (delta_lookup_unsafe s tl1 d) target d pm strict
        | _ -> false)
    | ParTyp (VarTyp s, tl), VarTyp _ -> is_subtype_with_strictness (delta_lookup_unsafe s tl d) target d pm strict
    | ParTyp (VarTyp s, tl), TopVecTyp n -> not strict && (vec_dim to_check d pm) =  n
    | VarTyp s, ParTyp _ -> is_subtype_with_strictness (delta_lookup_unsafe s [] d) target d pm strict
    | VarTyp s1, VarTyp s2 -> s1 = s2 || is_subtype_with_strictness (delta_lookup_unsafe s1 [] d) target d pm strict
    | VarTyp _, TopVecTyp n -> not strict && (vec_dim to_check d pm) =  n
    | SamplerTyp i1, SamplerTyp i2 -> i1 = i2 
    | BoolTyp, BoolTyp
    | IntTyp, IntTyp
    | FloatTyp, FloatTyp
    | SamplerCubeTyp, SamplerCubeTyp -> true
    | TransTyp (t1, t2), TransTyp (t3, t4) -> 
        (is_subtype_with_strictness t3 t1 d pm false && is_subtype_with_strictness t2 t4 d pm strict)
    | AbsTyp s1, AbsTyp s2 -> 
        s1 = s2 || abstyp_step s1
    | AbsTyp s, _ -> 
        abstyp_step s
    (* Necessary because we have a lattice and the bottype is less than EVERYTHING in that lattice *)
    | BotVecTyp n, AbsTyp _ -> (is_subtype_with_strictness target (TopVecTyp n) d pm false)
    | _ -> false

and is_subtype (to_check : typ) (target : typ) (d : delta) (pm: parameterization) : bool =
    debug_print ">> is_subtype";
    is_subtype_with_strictness to_check target d pm true

let rec is_sub_constraint (to_check : constrain) (target : constrain) (d : delta) (pm: parameterization): bool =
    debug_print ">> is_sub_constraint";
    match (to_check, target) with
    | _, AnyTyp -> true
    | AnyTyp, _ -> false
    | (TypConstraint t1, TypConstraint t2) -> is_subtype_with_strictness t1 t2 d pm false
    | (TypConstraint (AbsTyp s), _) -> is_sub_constraint (unwrap_abstyp s pm) target d pm
    | (_, TypConstraint _) -> false
    | (TypConstraint (BoolTyp), GenTyp) -> false
    | (TypConstraint (TransTyp _), GenTyp) -> false
    | (_, GenTyp) -> true
    | (GenVecTyp, GenVecTyp)
    | (TypConstraint (BotVecTyp _), GenVecTyp)
    | (TypConstraint (VarTyp _), GenVecTyp)
    | (TypConstraint (TopVecTyp _), GenVecTyp)
    | (TypConstraint (ParTyp (VarTyp _, _)), GenVecTyp) -> true
    | (_, GenVecTyp) -> false
    | (GenMatTyp, GenMatTyp)
    | (TypConstraint (TransTyp _), GenMatTyp) -> true
    | (_, GenMatTyp) -> false

let is_bounded_by (t: typ) (c: constrain) (d: delta) (pm: parameterization): bool =
    debug_print ">> is_bounded_by";
    is_sub_constraint (TypConstraint t) c d pm

(* Special case that comes up a bunch -- previously covered incorrectly by 'genType' *)
let is_non_bool (t: typ) (d: delta) (pm: parameterization) : bool =
    is_bounded_by t GenTyp d pm || is_bounded_by t GenMatTyp d pm

let match_parameterization (d: delta) (pmb: (string * constrain) list) (pml : typ list) (pm: parameterization) : (typ Assoc.context) option =
    debug_print ">> match_parametrization";
    if List.length pmb == List.length pml
    then List.fold_left2 (fun acc (s, c) t -> match acc with | None -> None 
        | Some tl -> if is_bounded_by t c d pm then Some (Assoc.update s t tl) else None)
        (Some (Assoc.empty)) pmb pml
    else None

let delta_lookup (s: id) (pml: typ list) (d: delta) (pm: parameterization) : typ =
    (* The safe version, where we check the validity of abstract type resolution *)
    debug_print ">> delta_lookup";
    if Assoc.mem s d then
    let (pm_to_match, t) = Assoc.lookup s d in
    match (match_parameterization d (Assoc.bindings pm_to_match) pml pm) with
    | Some cont -> replace_abstype t cont
    | None -> raise (TypeException ("Invalid parameters <" ^ (string_of_lst string_of_typ pml) ^ "> to " ^ s))
    else raise (TypeException ("Unknown tag " ^ s))

let rec greatest_common_child (t1: typ) (t2: typ) (d: delta) (pm: parameterization): typ =
    debug_print ">> greatest_common_child";
    let fail _ = raise (TypeException ("Cannot unify " ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2))) in
    if (is_subtype t1 t2 d pm) then t1 else 
    if (is_subtype t2 t1 d pm ) then t2 else 
    let n1 = match (vec_dim_safe t1 d pm) with | Some n -> n | None -> fail () in
    let n2 = match (vec_dim_safe t2 d pm) with | Some n -> n | None -> fail () in
    (* So this definition of n1 and n2 actually means non-vector types have already been checked *)
    if not (n1 = n2) then fail () else
    match (t1, t2) with                                                                                                                                                                                                                                                                                                                                                                                                                    
    | TopVecTyp _, _ -> t2
    | _, TopVecTyp _ -> t1
    | _ -> BotVecTyp n1 (* This works since both t1 and t2 are vectors with the same dimension and are not subtypes of each other *)

let rec least_common_parent_with_strictness (t1: typ) (t2: typ) (d: delta) (pm: parameterization) (strict: bool): typ =
    debug_print ">> least_common_parent";
    let fail _ = raise (TypeException ("Cannot unify " ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2))) in
    let check_dim (n1: int) (n2: int) : unit =
        if n1 = n2 then () else (raise (DimensionException (n1, n2)))
    in
    let rec step_abstyp s1 s2 =
        begin
            match (Assoc.lookup s2 pm) with
            | TypConstraint (AbsTyp s') -> if (is_subtype t1 (AbsTyp s') d pm) then (AbsTyp s') else step_abstyp s1 s'
            | TypConstraint t -> least_common_parent_with_strictness t t2 d pm strict
            | c -> fail ()
        end
    in
    if (is_subtype_with_strictness t1 t2 d pm strict) then t2 else if (is_subtype_with_strictness t2 t1 d pm strict) then t1 
    else match (t1, t2) with
        | VarTyp s, TopVecTyp n1
        | TopVecTyp n1, VarTyp s ->
            check_dim (vec_dim (VarTyp s) d pm) n1;
            if strict then
            raise (TypeException ("Cannot implicitly cast " ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2) ^ " to the top vector type"  ))
            else TopVecTyp n1
        | VarTyp s, _
        | _, VarTyp s ->
            (* Just go up a step -- the end condition is the is_subtype check earlier *)
            least_common_parent_with_strictness (delta_lookup s [] d pm) t2 d pm strict
        | ParTyp (VarTyp s, tl), _
        | _, ParTyp (VarTyp s, tl) ->
            least_common_parent_with_strictness (delta_lookup s tl d pm) t2 d pm strict
        | (AbsTyp s1, AbsTyp s2) -> (step_abstyp s1 s2)
        | (TransTyp (t1, t2), TransTyp(t3, t4)) -> 
            (TransTyp (greatest_common_child t1 t3 d pm, least_common_parent_with_strictness t2 t4 d pm strict))
        (* Note that every other possible pair of legal joins would be caught by the is_subtype calls above *)
        | _ -> fail ()

let least_common_parent (t1: typ) (t2: typ) (d: delta) (pm: parameterization): typ =
    least_common_parent_with_strictness t1 t2 d pm true

let least_common_parent_safe (t1: typ) (t2: typ) (d: delta) (pm: parameterization): typ option =
    try Some (least_common_parent t1 t2 d pm) with
    | TypeException t -> None
    | DimensionException _ -> None
    | t -> raise t

let collapse_parameterization_decl (pmd: parameterization_decl) : parameterization =
    Assoc.gen_context (List.map (fun (x, y, z) -> (x, z)) pmd)

let infer_pml (d : delta) ((params, rt, pr) : fn_type) (args_typ : typ list) 
(pm : parameterization): (typ list) option =
    let update_inference (t : typ) (s : string) (fpm : (typ Assoc.context) option) : (typ Assoc.context) option =
        match fpm with | None -> None | Some p ->
        if Assoc.mem s p then (match least_common_parent_safe t (Assoc.lookup s p) d pm with
            | None -> None
            | Some t' -> (Some (Assoc.update s t' p)))
        else (Some (Assoc.update s t p)) in
    let rec unify_param (arg_typ : typ) (par_typ : typ) (fpm : (typ Assoc.context) option) : (typ Assoc.context) option =
        match (arg_typ, par_typ) with
        | (_, AbsTyp s) -> update_inference arg_typ s fpm
        (* Note that transtyp order doesn't matter; lots of commutivity *)
        | (TransTyp (al, ar), TransTyp (pl, pr)) -> unify_param ar pr (unify_param al pl fpm)
        | (ParTyp (_, tl1), ParTyp (_, tl2)) -> if List.length tl1 = List.length tl2 then
            List.fold_left2 (fun acc l r -> unify_param l r acc) fpm tl1 tl2 else fpm
        | _ -> fpm
    in
    let gen_pml (inferred : (typ Assoc.context) option) : (typ list) option =
        let rec reduce_typ (t : typ) (fpm : typ Assoc.context) : typ =
            match t with
            (* Don't have to check if t is valid *)
            (* already handled by the typechecker when checking that 
             * the function declaration parameter dependencies were valid *)
            | AbsTyp s -> Assoc.lookup s fpm
            | TransTyp (l, r) -> TransTyp(reduce_typ l fpm, reduce_typ r fpm)
            | ParTyp (t, tl) -> ParTyp(t, List.map (fun x -> reduce_typ x fpm) tl)
            | _ -> t
        in
        (* Correctly orders the resulting pml to match the parameters of the function parameterization list *)
        match inferred with | None -> None | Some tc -> 
        option_map (fun c -> List.rev (List.map snd (Assoc.bindings c))) 
            (List.fold_left (fun acc (s, c) -> match acc with | None -> None | Some a -> 
                if Assoc.mem s tc then Some (Assoc.update s (Assoc.lookup s tc) a) else
                (match c with | TypConstraint t' -> Some (Assoc.update s (reduce_typ t' a) a) | _ -> None))
            (Some Assoc.empty) (Assoc.bindings pr))
    in
    gen_pml (List.fold_left2 (fun fpm arg_typ (_, par_typ) -> unify_param arg_typ par_typ fpm) 
                (Some Assoc.empty) args_typ params)
    

let check_subtype_list (t: typ) (l: typ list) (d: delta) (pm: parameterization) : bool =
    debug_print ">> check_subtype_list";
    List.fold_left (fun acc t' -> acc || (is_subtype t t' d pm)) false l

let check_bounds_list (t: typ) (l: constrain list) (d: delta) (pm: parameterization) : bool =
    debug_print ">> check_bounds_list";
    List.fold_left (fun acc t' -> acc || (is_bounded_by t t' d pm)) false l
    
let check_typ_valid (ogt: typ) (d: delta) (pm: parameterization) : unit =
    let rec check_typ_valid_rec (t: typ) : unit =
        debug_print ">> check_typ_valid";
        match t with
        | VarTyp s -> if Assoc.mem s d then () else raise (TypeException ("Unknown tag " ^ s))
        | ParTyp (t, tl) -> check_typ_valid_rec t; List.fold_left (fun acc t' -> check_typ_valid_rec t') () tl;
        | AbsTyp s -> if Assoc.mem s pm then () else raise (TypeException ("Unknown abstract type `" ^ s))
        | TransTyp (t1, t2) -> check_typ_valid_rec t1; check_typ_valid_rec t2;
            if is_bounded_by t1 GenVecTyp d pm && is_bounded_by t2 GenVecTyp d pm then ()
            else raise (TypeException ("Invalid matrix type " ^ (string_of_typ ogt) ^ " (must be a map from vectors to vectors)"))
        | _ -> ()
    in check_typ_valid_rec ogt

let check_val (v: value) (d: delta) : typ = 
    debug_print ">> check_aval";
    match v with
    | Bool b -> BoolTyp
    | Num n -> IntTyp
    | Float f -> FloatTyp
    | VecLit v -> BotVecTyp (List.length v)
    | MatLit m ->
        (let rows = List.length m in
        if rows = 0 then trans_bot 0 0 else
        let cols = List.length (List.hd m) in
        if List.for_all (fun v -> List.length v = cols) m then trans_bot cols rows
        else (raise (TypeException ("Matrix must have the same number of elements in each row"))))
    | _ -> raise (TypeException ("Unexpected typechecker value " ^ (string_of_value v)))

let rec check_typ_exp (t: typ) (d: delta) : unit =
    debug_print ">> check_typ";
    match t with
    | AutoTyp -> raise (TypeException "Cannot use type auto as a tag type")
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp
    | SamplerCubeTyp
    | SamplerTyp _ 
    | ArrTyp _ -> ()
    | TopVecTyp n
    | BotVecTyp n -> (if (n > 0) then ()
        else raise (TypeException "Cannot declare a type with dimension less than 0"))
    | VarTyp s -> delta_lookup s [] d |> ignore; ()
    | ParTyp (VarTyp s, pml) -> delta_lookup s pml d |> ignore; ()
    | ParTyp _ -> ()
    | TransTyp (t1, t2) -> check_typ_exp t1 d; check_typ_exp t2 d; ()
    | AbsTyp s -> raise  (TypeException "Cannot use a generic type as a tag argument yet")

(* "scalar linear exp", (i.e. ctimes) returns generalized MatTyp *)
let check_ctimes_exp (t1: typ) (t2: typ) (d: delta) (pm : parameterization): typ = 
    debug_print ">> check_ctimes_exp";
    if is_bounded_by t1 GenVecTyp d pm
    then (least_common_parent t1 t2 d pm |> ignore; TopVecTyp (vec_dim t1 d pm))
    else if is_bounded_by t1 GenMatTyp d pm 
    then (least_common_parent t1 t2 d pm |> ignore; 
        (let (left, right) = as_matrix_pair t1 d pm in trans_top (vec_dim left d pm) (vec_dim right d pm)))
    else raise (TypeException ("Invalid expressions for component wise multiplication: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2)))

(* Type check binary bool operators (i.e. &&, ||) *)
let check_bool_binop (t1: typ) (t2: typ) (d: delta) (pm: parameterization): typ = 
    debug_print ">> check_bool_binop";
    if is_subtype t1 BoolTyp d pm then least_common_parent t1 t2 d pm
    else raise (TypeException "Expected boolean expression for binop")

(* Type check unary number operators (i.e. -) *)
let check_num_unop (t: typ) (d: delta) (pm: parameterization) : typ =
    debug_print ">> check_num_unop";
    if is_non_bool t d pm then t
    else raise (TypeException "Expected integer, float, vector, or matrix expression")

(* Type check unary bool operators (i.e. !) *)
let check_bool_unop (t: typ) (d: delta) (pm: parameterization) : typ =
    debug_print ">> check_bool_unop";
    if is_subtype t BoolTyp d pm then t
    else raise (TypeException "Expected boolean expression for boolean operator")

(* Type check unary bool operators (i.e. !) *)
let check_swizzle (s : id) (t: typ) (d: delta) (pm: parameterization) : typ =
    debug_print ">> check_swizzle";
    let check_reg = if Str.string_match (Str.regexp "[xyzwrgbastpq]+") s 0 
        then if String.length s == 1 then FloatTyp else TopVecTyp (String.length s)
        else raise (TypeException ("Invalid characters used for swizzling in " ^ s)) in
    if is_bounded_by t GenVecTyp d pm then check_reg
    else raise (TypeException "Expected vector for swizzling")

(* Type check equality (==) *)
(* Only bool, int, float are comparable *)
let check_equality_exp (t1: typ) (t2: typ) (d: delta) (pm: parameterization) : typ = 
    debug_print ">> check_comp_binop";
    let subtype_list = [BoolTyp; IntTyp; FloatTyp] in
    if check_subtype_list t1 subtype_list d pm
    then (least_common_parent t1 t2 d pm |> ignore; BoolTyp)
    else raise (TypeException "Equality checks must be between booleans, integers, or floats")

(* Type check comparative binary operators (i.e. <. <=) *)
(* Only int and float are comparable *)
let check_comp_binop (t1: typ) (t2: typ) (d: delta) (pm: parameterization) : typ = 
    debug_print ">> check_comp_binop";
    let subtype_list = [IntTyp; FloatTyp] in
    if check_subtype_list t1 subtype_list d pm
    then (least_common_parent t1 t2 d pm |> ignore; BoolTyp)
    else raise (TypeException "Comparison checks must be between integers or floats")

let check_as_exp (t1: typ) (t2: typ) (d: delta) (pm: parameterization) : typ =
    least_common_parent_with_strictness t1 t2 d pm false |> ignore; t2

(* Type checking addition operations on scalar (int, float) expressions *)
(* Types are closed under addition and scalar multiplication *)
let check_addition_exp (t1: typ) (t2: typ) (d: delta) (pm: parameterization): typ =
    debug_print ">> check_addition";
    if is_non_bool t1 d pm then least_common_parent t1 t2 d pm
    else raise (TypeException ("Invalid expressions for addition: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2)))

(* Type checking times operator - on scalar mult & matrix transformations *)
let check_times_exp (t1: typ) (t2: typ) (d: delta) (pm: parameterization): typ = 
    debug_print ">> check_times_exp";
    let fail () = raise (TypeException ("Invalid expressions for multiplication: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2))) in
    let intfloat_list = [IntTyp; FloatTyp] in
    if is_subtype t1 IntTyp d pm && is_subtype t2 IntTyp d pm then 
        least_common_parent t1 t2 d pm
    (* Scalar multiplication *)
    else if check_subtype_list t1 intfloat_list d pm && is_non_bool t2 d pm then t2
    else if check_subtype_list t2 intfloat_list d pm && is_non_bool t1 d pm then t1
    (* Matrix-vector multiplication *)
    else if is_bounded_by t1 GenMatTyp d pm then
        (let (t1l, t1r) = as_matrix_pair t1 d pm in
        if is_bounded_by t2 GenVecTyp d pm then
            (if is_subtype t2 t1l d pm then t1r else fail ())
        else if is_bounded_by t2 GenMatTyp d pm then
            (let (t2l, t2r) = as_matrix_pair t2 d pm in
            least_common_parent t1l t2r d |> ignore; TransTyp(t2l, t1r))
        else fail ())
    else fail ()

(* Type checking division operations (/) *)
(* Types are closed under scalar division *)
let check_division_exp (t1: typ) (t2: typ) (d: delta) (pm: parameterization) : typ =
    debug_print ">> check_division";
    let subtype_list = [IntTyp; FloatTyp] in
    if check_subtype_list t1 subtype_list d pm
        then least_common_parent t1 t2 d pm
    else if check_subtype_list t2 subtype_list d pm && is_bounded_by t1 GenVecTyp d pm
        then t1
    else raise (TypeException ("Invalid expressions for division: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2)))

let check_index_exp (t1: typ) (t2: typ) (d: delta) (pm: parameterization): typ =
    debug_print ">> check_index_exp";
    let fail _ = raise (TypeException ("Invalid expressions for indexing: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2))) in
    if is_subtype t2 IntTyp d pm then
        if is_bounded_by t1 GenVecTyp d pm then FloatTyp
        else if is_bounded_by t1 GenMatTyp d pm then
            TopVecTyp (vec_dim (fst (as_matrix_pair t1 d pm)) d pm)
        else begin
        match t1 with
        | ArrTyp (t, _) -> t
        | _ -> fail ()
        end
    else fail ()

let check_parameterization (d: delta) (m: mu) (pmd: parameterization_decl) : mu =
    let rec check_para_list param found : mu = 
    match param with
    | [] -> m
    | (s, mo, c)::t -> if Assoc.mem s found then raise (TypeException ("Duplicate parameter `" ^ s)) 
        else 
        let updated_found = (Assoc.update s c found) in
        (match c with
        | TypConstraint t' -> check_typ_valid t' d found; 
            Assoc.update ("`" ^ s) mo (check_para_list t updated_found)
        | _ -> Assoc.update ("`" ^ s) mo (check_para_list t updated_found))
    in
    check_para_list pmd Assoc.empty

let as_par_typ (t: typ) : string * typ list =
    match t with
    | VarTyp s -> (s, [])
	| ParTyp (VarTyp s, tl) -> (s, tl)
	| AbsTyp s -> ("`" ^ s, [])
	| _ -> failwith ("Unexpected type " ^ string_of_typ t ^ " provided to manipulating psi")
	
let update_psi (start: typ) (target: typ) ((f, pml) : string * typ list) (m: mu) (ps: psi) : psi =
    (* Update psi, raising errors in case of a duplicate *)
    (* If the given type is not valid in psi, psi is returned unmodified *)
    (* Will raise a failure if a non-concrete vartyp is used *)
    debug_print ">> update_psi";
    let is_valid (t: typ) : bool =
        match t with
        | VarTyp _ 
        | AbsTyp _ -> true
        | _ -> false
    in
    if not (is_valid start) || not (is_valid target) then ps else
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
    let are_coord (t1: typ) (t2: typ) : bool =
        match t1, t2 with
        | VarTyp s1, VarTyp s2
        | ParTyp (VarTyp s1, _), VarTyp s2
        | VarTyp s1, ParTyp (VarTyp s2, _)
        | ParTyp (VarTyp s1, _), ParTyp (VarTyp s2, _) ->
            (if Assoc.mem s1 m && Assoc.mem s2 m
            then (match (Assoc.lookup s1 m, Assoc.lookup s2 m) with
            | (Some Coord, Some Coord) -> true
            | _ -> false)
            else failwith ("Expected to find " ^ s1 ^ " and " ^ s2 ^ " in mu"))
        | _ -> failwith ("Unexpected lookup of " ^ string_of_typ t1 ^ " or " ^ string_of_typ t2 ^ " to mu")
    in
	let (s1, stl) = as_par_typ start in
	let (s2, ttl) = as_par_typ target in
    let start_index = string_of_typ start in
    let to_add = (target, (f, pml)) in
    if  are_coord (VarTyp s1) (VarTyp s2) then
        if Assoc.mem start_index ps then 
        (let start_lst = Assoc.lookup start_index ps in
            if (List.fold_left (fun acc (lt, (_, _)) -> acc ||
                    (let (s2, tl2) = as_par_typ lt in
					if (List.length ttl = List.length tl2) 
                    then List.fold_left2 (fun acc' t1 t2 -> acc' || (check_var_typ_eq t1 t2)) false ttl tl2
                    else false))
                false start_lst)
            then raise (TypeException ("Duplicate transformation for " ^ start_index ^ "->" ^ string_of_typ (ParTyp(VarTyp s1, ttl)) ^ " in the declaration of " ^ f))
            else Assoc.update start_index (to_add :: start_lst) ps
        )
        else Assoc.update start_index [to_add] ps 
    else ps

let update_psi_matrix (f: string) (t: typ) (m: mu) (ps: psi) : psi =
    match t with
    | TransTyp (t1, t2) -> update_psi t1 t2 (f, []) m ps
    | _ -> ps

(* Type check parameter; make sure there are no name-shadowed parameter names *)
(* TODO : parametrized types *)
let check_param ((id, t): (string * typ)) (g: gamma) (d: delta) (m: mu) 
    (pm : parameterization) (ps: psi) : gamma * psi = 
    debug_print ">> check_param";
    if Assoc.mem id g 
    then raise (TypeException ("Duplicate parameter name in function declaration: " ^ id))
    else check_typ_valid t d pm; (Assoc.update id t g, update_psi_matrix id t m ps)
    
(* Get list of parameters from param list *)
let check_params (pl : (string * typ) list) (g: gamma) (d : delta) (m: mu) 
(pm : parameterization) (ps: psi) : TypedAst.params * gamma * psi = 
    debug_print ">> check_params";
    let (g', ps') = List.fold_left (fun (g', ps') p -> check_param p g' d m pm ps') (g, ps) pl in 
    let p = (List.map (fun (i, t) -> (i, tag_erase t d pm)) pl) in 
    (p, g', ps')

(* Type check global variable *)
let check_global_variable ((id, sq, t, v): (string * storage_qual * typ * value option)) (g: gamma)
    (d: delta) (m:mu) (ps: psi) : gamma * psi =
    debug_print ">> check_global_variable";
    if Assoc.mem id g
    then raise (TypeException ("Duplicate global variable: " ^ id))
    else check_typ_valid t d Assoc.empty; (Assoc.update id t g, update_psi_matrix id t m ps)

(* Get list of variables from global variable list *)
let check_global_variables (gvl: (string * storage_qual * typ * value option) list)
    (g: gamma) (d: delta) (m: mu) (ps: psi) : TypedAst.global_vars * gamma * psi =
    debug_print ">> check_global_variables";
    let (g', ps') = List.fold_left (fun (g', ps') gv -> check_global_variable gv g' d m ps') (g, ps) gvl in
    let gv = (List.map (fun (i, sq, t, v) -> (i, sq, tag_erase t d Assoc.empty, v)) gvl) in
    (gv, g', ps')
    
let exp_to_texp (checked_exp : TypedAst.exp * typ) (d : delta) (pm : parameterization) : TypedAst.texp = 
    debug_print ">> exp_to_texp";
    ((fst checked_exp), (tag_erase (snd checked_exp) d pm))

(* Super expensive.  We're essentially relying on small contexts *)
let check_in_exp (start_exp: exp) (start: typ) (target: typ) (m: mu) (g: gamma) (d: delta) 
(pm: parameterization) (p: phi) (ps: psi) : exp = 
    debug_print ">> check_in_exp";
    print_endline (string_of_psi ps);
    let rec psi_path_rec (to_search: (typ * exp) Queue.t) (found: typ list) : exp =
        let search_phi (tl: typ) (ps_lst : (typ * fn_inv) list) : (typ * fn_inv) list =
            (* This function searches phi for anonical abstract functions that map from the given type *)
            (* A list of the types these functions map with the inferred type parameters is returned *)
            (* If multiple functions are possible, then ambiguities are resolved with the following priorities *)
            (* 1. Minimize upcasting requirements (actually handled by use of this function) *)
            (* 2. Minimize number of type parameters *)
            (* 3. Minimize constraint bounds *)
            let rec search_phi_rec (fns : (string * fn_type) list) : (typ * (id * typ list * constrain list)) list =
                match fns with
                (* Note that matrices are always selected over canonical function invocations *)
                | [] -> List.map (fun (t, (x, y)) -> (t, (x, y, []))) ps_lst 
                | (id, (params, rt, pr)) :: t -> 
                begin
                    match Assoc.lookup id m with
                    | Some Canon -> 
                    begin
                        let pt = match params with | [(_, pt)] -> pt | _ -> failwith ("function " ^ id ^ " with non-one argument made canonical") in
                        match infer_pml d (params, rt, pr) [tl] pm with | None -> search_phi_rec t | Some pml ->
                        let pr1 = List.map snd (Assoc.bindings pr) in
                        let rtr = replace_abstype rt (match_parameterization_unsafe pr pml) in
                        let ptr = replace_abstype pt (match_parameterization_unsafe pr pml) in
                        let fail id2 s = raise (TypeException ("Ambiguity between viable canonical functions " 
                            ^ id ^ " and " ^ id2 ^ " (" ^ s ^ ")")) in
                        let compare_parameterizations id2 (acc : bool option) c1 c2 : bool option = 
                            let result = is_sub_constraint c1 c2 d pm in match acc with | None -> Some result
                            | Some b -> if b = result then acc else fail id2 
                            ("ambiguous constraint ordering between " ^ string_of_constraint c1 ^ " and " ^ string_of_constraint c2)
                        in
                        if not (is_subtype tl ptr d pm) then search_phi_rec t else
                        match rtr with
                        | TopVecTyp _ -> search_phi_rec t
                        | AbsTyp _
                        | VarTyp _ 
                        | ParTyp (VarTyp _, _) -> let rec_result = search_phi_rec t in
                            if List.fold_left (fun acc (rt, _) -> is_typ_eq rt rtr || acc) false rec_result then
                            List.map (fun (rt, (id2, pml2, pr2)) -> 
                            if (List.length pr1 = List.length pr2) && (List.length pr1 = 0) then
                                fail id2 ("duplicate concrete paths from " ^ string_of_typ tl ^ " to " ^ string_of_typ rtr)
                            else if not (is_typ_eq rt rtr) then (rt, (id2, pml2, pr2))
                            else if List.length pr1 < List.length pr2 then (rt, (id, pml, pr1))
                            else if List.length pr2 < List.length pr1 then (rt, (id2, pml2, pr2))
                            else if (match List.fold_left2 (compare_parameterizations id2) None pr1 pr2 with
                            | None -> failwith "Unexpected concrete function type duplicates in phi" 
                            | Some b -> b) then (rt, (id2, pml2, pr2))
                            else (rtr, (id, pml, pr1))) rec_result
                            (* No duplicate type result found, just add this function to the list *)
                            else (rtr, (id, pml, pr1)) :: rec_result
                        | _ -> raise (TypeException ("Canonical function " ^ id ^ " resulted in type "
                            ^ (string_of_typ rtr) ^ ", while canonical functions should always result in an abs or vartyp"))
                    end
                    | _ -> search_phi_rec t
                end
            in
            List.map (fun (t, (x, y, z)) -> (t, (x, y))) (search_phi_rec (Assoc.bindings p))
        in
        let rec psi_lookup_rec (nt: typ) : (typ * fn_inv) list =
            (* NOTE: paths which would send to a type with more than 5 generic levels are rejected to avoid infinite spirals *)
            let rec check_ignore (t: typ) (count: int) : bool =
                if count > 5 then true else
                match t with
                | ParTyp (_, tl) -> List.fold_left (fun acc t -> acc || check_ignore t (count + 1)) false tl
                | _ -> false
            in
            if check_ignore nt 0 then [] else
            let s_lookup = string_of_typ nt in
            let ps_lst = if Assoc.mem s_lookup ps then Assoc.lookup s_lookup ps else [] in
            let to_return = search_phi nt ps_lst in
            let (ns, ntl) = as_par_typ nt in
            let next_step = match nt with | VarTyp _ -> delta_lookup_unsafe ns ntl d | _ -> nt in
            (match next_step with
            | VarTyp _
            | ParTyp _ -> 
                to_return @ psi_lookup_rec next_step
            | _ -> to_return)
        in 
        let rec update_search_and_found (vals: (typ * fn_inv) list) (e: exp) : typ list =
            match vals with
            | [] -> found
            | (t1, (v, pml))::t -> 
                if List.fold_left (fun acc t2 -> acc || is_typ_eq t1 t2) false found 
                then update_search_and_found t e 
                else 
                let e' = 
                    if Assoc.mem v g then (Binop (Times, Var v, e))
                    else if Assoc.mem v p then (FnInv (v, [e], pml))
                    else failwith ("Typechecker error: unknown value " ^ v ^ " loaded into psi") in
                (* Note the update to the stateful queue *)
                (Queue.push (t1, e') to_search;  t1 :: update_search_and_found t e)
        in
        let (nt, e) = if Queue.is_empty to_search 
            then (raise (TypeException ("Cannot find a path from " ^
                string_of_typ start ^ " to " ^ string_of_typ target)))
            else Queue.pop to_search 
        in 
        (* We use the 'with_strictness' version to avoid throwing an exception *)
        if is_subtype_with_strictness nt target d pm false then e
        else psi_path_rec to_search (update_search_and_found (psi_lookup_rec nt) e)
    in	
	if string_of_typ start = string_of_typ target then start_exp else
	let q = Queue.create () in Queue.push (start, start_exp) q;
	psi_path_rec q []

let rec check_exp (e : exp) (d : delta) (m: mu) (g : gamma) (pm : parameterization) (p : phi) (ps: psi)
 : TypedAst.exp * typ = 
    debug_print ">> check_exp";
    let build_unop (op : unop) (e': exp) (check_fun: typ->delta->parameterization->typ) (pm: parameterization)
        : TypedAst.exp * typ =
        let result = check_exp e' d m g pm p ps in
            (TypedAst.Unop(op, exp_to_texp result d pm), check_fun (snd result) d pm)
    in
    let build_binop (op : binop) (e1: exp) (e2: exp) (check_fun: typ->typ->delta->parameterization->typ) (pm: parameterization)
        : TypedAst.exp * typ =
        let e1r = check_exp e1 d m g pm p ps in
        let e2r = check_exp e2 d m g pm p ps in
            (TypedAst.Binop(op, exp_to_texp e1r d pm, exp_to_texp e2r d pm), check_fun (snd e1r) (snd e2r) d pm)
    in 
    let req_parameterizations f =
        fun a b c d -> f a b c d
    in
    let req_parameterizations2 f =
     fun a b c  -> f a b c
    in
    match e with
    | Val v -> (TypedAst.Val v, check_val v d)
    | Var v -> "\tVar "^v |> debug_print;
        if Assoc.mem v g then (TypedAst.Var v, Assoc.lookup v g) else
        raise (TypeException ("Unknown variable " ^ v))
    | Arr a -> check_arr d m g p a pm ps
    | As (e, t) -> let (er, tr) = check_exp e d m g pm p ps in (er, check_as_exp tr t d pm)
    | In (e, t) -> let (_, tr) = check_exp e d m g pm p ps in 
        (check_exp (check_in_exp e tr t m g d pm p ps) d m g pm p ps)
    | Unop (op, e') -> (match op with
        | Neg -> build_unop op e' (req_parameterizations2 check_num_unop) pm
        | Not -> build_unop op e' (req_parameterizations2 check_bool_unop) pm
        | Swizzle s -> build_unop op e' (check_swizzle s) pm)
    | Binop (op, e1, e2) -> (match op with
        | Eq -> build_binop op e1 e2 (req_parameterizations check_equality_exp) pm
        | Leq -> build_binop op e1 e2 (req_parameterizations check_comp_binop) pm
        | Lt -> build_binop op e1 e2 (req_parameterizations check_comp_binop) pm
        | Geq -> build_binop op e1 e2 (req_parameterizations check_comp_binop) pm
        | Gt -> build_binop op e1 e2 (req_parameterizations check_comp_binop) pm
        | Or | And -> build_binop op e1 e2 check_bool_binop pm
        | Plus | Minus -> build_binop op e1 e2 check_addition_exp pm
        | Times -> build_binop op e1 e2 check_times_exp pm
        | Div  -> build_binop op e1 e2 (req_parameterizations check_division_exp) pm
        | CTimes -> build_binop op e1 e2 check_ctimes_exp pm
        | Index -> build_binop op e1 e2 check_index_exp pm
    )
    | FnInv (i, args, pr) -> let ((i, args_exp), rt) = check_fn_inv d m g p args i pr pm ps in 
        (FnInv (i, args_exp), rt)
        
and check_arr (d : delta) (m: mu) (g : gamma) (p : phi) (a : exp list) (pm : parameterization) (ps: psi)
 : (TypedAst.exp * typ) =
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
    let checked_a = List.map (fun e -> (exp_to_texp (check_exp e d m g pm p ps) d pm )) a in
    let length_a = List.length a in
    if is_vec checked_a then (TypedAst.Arr checked_a, BotVecTyp length_a) else 
    (match is_mat checked_a with
    | Some n -> (TypedAst.Arr checked_a, trans_bot n length_a)
    | None ->  raise (TypeException ("Invalid array definition for " ^ (string_of_exp (Arr a)) ^ ", must be a matrix or vector")))


and check_fn_inv (d : delta) (m: mu) (g : gamma) (p : phi) (args : args) (i : string) (pml: typ list) (pm : parameterization) (ps: psi)
 : (string * TypedAst.args) * typ = 
    debug_print ">> check_fn_inv";
    let fn_invocated = if Assoc.mem i p
        then Assoc.lookup i p
        else raise (TypeException ("Invocated function " ^ i ^ " not found")) in
    let (_, rt, _) = fn_invocated in
    let args' = List.map (fun a -> check_exp a d m g pm p ps) args in 
    let args_exp = List.map fst args' in
    let args_typ = List.map snd args' in
    (* find definition for function in phi *)
    (* looks through all possible overloaded definitions of the function *)
    let find_fn_inv ((params, rt, pr) : fn_type) : (typ Assoc.context) option =
        debug_print ">> find_fn_inv";
        (* This function asserts whether or not the function invocation matches the function given *)
        (* In particular, this checks whether the given function matches the given parameterization and parameters *)
        (* If it is valid, this returns (Some 'map from parameter to type'), otherwise returns 'None' *)

        (* If we have the wrong number of arguments, then no match for sure *)
        if List.length args != List.length params then None else
        (* Work out the parameter inference if one is needed *)
        let inferred_pml = 
            (if Assoc.size pr == List.length pml then Some pml
            else if List.length pml == 0 then infer_pml d (params, rt, pr) args_typ pm 
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
                if is_bounded_by given_pm bound d pm 
                then Some (Assoc.update s given_pm fpm) else None))
            (Some Assoc.empty) pml' (Assoc.bindings pr)
        in
        match param_check with | None -> None | Some pm_map ->
        (* Get the parameters types and replace them in params_typ *)
        let params_typ = List.map (fun (_,a) -> a) params in
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
            List.fold_left2 (fun acc arg param -> if (is_subtype arg param d pm) then acc else None)
            param_check args_typ params_typ_corrected
        else None
    in
    (match find_fn_inv fn_invocated with
    | Some l -> ((i, args_exp), replace_abstype rt l)
    | None -> raise (TypeException ("No overloaded function declaration of " ^ i
    ^ (if List.length pml > 0 then "<" ^ (String.concat "," (List.map string_of_typ pml)) ^ ">" else "")
    ^ " matching types (" ^ (String.concat "," (List.map string_of_typ args_typ)) ^ ") found"))) 

and check_comm (c: comm) (d: delta) (m: mu) (g: gamma) (pm: parameterization) (p: phi) (ps: psi) : TypedAst.comm * gamma * psi = 
    debug_print ">> check_comm";
    match c with
    | Skip -> (TypedAst.Skip, g, ps)
    | Print e -> (
        let (e, t) = exp_to_texp (check_exp e d m g pm p ps) d pm in 
        match t with
        | UnitTyp -> raise (TypeException "Print function cannot print void types")
        | _ -> (TypedAst.Print (e, t), g, ps)
    )
    | Inc x -> let x_typ = (Assoc.lookup x g) in (match x_typ with
        | IntTyp -> (TypedAst.Inc (x, TypedAst.IntTyp), g, ps)
        | FloatTyp -> (TypedAst.Inc (x, TypedAst.FloatTyp), g, ps)
        | _ -> raise (TypeException "increment must be applied to an integer or float"))
    | Dec x -> let x_typ = (Assoc.lookup x g) in (match x_typ with
        | IntTyp -> (TypedAst.Dec (x, TypedAst.IntTyp), g, ps)
        | FloatTyp -> (TypedAst.Dec (x, TypedAst.FloatTyp), g, ps)
        | _ -> raise (TypeException "decrement must be applied to an integer or float"))
    | Decl (t, s, e) -> (* TODO: code insertion *)
        if Assoc.mem s g then raise (TypeException "variable name shadowing is illegal")        
        else 
        (check_typ_valid t d pm;
        let result = check_exp e d m g pm p ps in
        let t' = (match t with | AutoTyp -> 
            (match (snd result) with
                | BotVecTyp _ -> raise (TypeException "Cannot infer the type of a vector literal")
                | TransTyp (TopVecTyp _, BotVecTyp _) -> raise (TypeException "Cannot infer the type of a matrix literal")
                | t' -> t')
            | _ -> t) in
            (TypedAst.Decl (tag_erase t' d pm, s, (exp_to_texp result d pm)), 
            (check_assign t' s (snd result) d g p pm), update_psi_matrix s t m ps))
    | Assign (s, e) ->
        if Assoc.mem s g then
            let t = Assoc.lookup s g in
            let result = check_exp e d m g pm p ps in
            (TypedAst.Assign (s, (exp_to_texp result d pm)), check_assign t s (snd result) d g p pm, ps)
        else raise (TypeException ("Assignment to undeclared variable: " ^ s))
    | AssignOp (s, b, e) -> 
        let (c', g', ps') = check_comm (Assign (s, Binop(b, Var s, e))) d m g pm p ps in
        (match c' with
        | TypedAst.Assign (_, (TypedAst.Binop (_, (_, st), e), _)) -> (TypedAst.AssignOp((s, st), b, e), g', ps')
        | _ -> failwith "Assign must return an assign?")
    | If ((b, c1), el, c2) ->
        let check_if b c =
            let er = (check_exp b d m g pm p ps) in
            let (cr, _, _) = check_comm_lst c d m g pm p ps in
            (match (snd er) with 
            | BoolTyp -> ((exp_to_texp er d pm), cr)
            | _ -> raise (TypeException "Expected boolean expression for if condition"))
        in
        let c2r = (match c2 with | Some e -> Some (tr_fst (check_comm_lst e d m g pm p ps)) | None -> None) in
        (TypedAst.If (check_if b c1, List.map (fun (b, c) -> check_if b c) el, c2r), g, ps)
    | For (c1, b, c2, cl) ->
        let (c1r, g', ps') = check_comm c1 d m g pm p ps in
        let (br, brt) = check_exp b d m g' pm p ps in
        let btexp = exp_to_texp (br, brt) d pm in
        let (c2r, _, _) = check_comm c2 d m g' pm p ps' in
        (TypedAst.For (c1r, btexp, c2r, (tr_fst (check_comm_lst cl d m g' pm p ps'))), g, ps)
        (* (match c1r with
        | Skip
        | Decl _
        | Assign _ -> (match (br, c2r) with
            | (Binop (Leq, (Var x, TypedAst.IntTyp), (Val _, TypedAst.IntTyp)), Inc (y, TypedAst.IntTyp))
            | (Binop (Lt, (Var x, TypedAst.IntTyp), (Val _, TypedAst.IntTyp)), Inc (y, TypedAst.IntTyp))
            | (Binop (Geq, (Val _, TypedAst.IntTyp), (Var x, TypedAst.IntTyp)), Inc (y, TypedAst.IntTyp))
            | (Binop (Gt, (Val _, TypedAst.IntTyp), (Var x, TypedAst.IntTyp)), Inc (y, TypedAst.IntTyp))
            | (Binop (Geq, (Var x, TypedAst.IntTyp), (Val _, TypedAst.IntTyp)), Dec (y, TypedAst.IntTyp))
            | (Binop (Gt, (Var x, TypedAst.IntTyp), (Val _, TypedAst.IntTyp)), Dec (y, TypedAst.IntTyp))
            | (Binop (Leq, (Val _, TypedAst.IntTyp), (Var x, TypedAst.IntTyp)), Dec (y, TypedAst.IntTyp))
            | (Binop (Lt, (Val _, TypedAst.IntTyp), (Var x, TypedAst.IntTyp)), Dec (y, TypedAst.IntTyp)) -> 
                if x = y then (TypedAst.For (c1r, btexp, c2r, (tr_fst (check_comm_lst cl d m g' pm p ps'))), g, ps)
                else raise (TypeException "Must use the same variable when checking and progressing toward termination")
            | _ -> raise (TypeException "For loop must progress toward termination with a comparative expression between an id and constant using precisely the increment or decrement operator"))
        | _ -> raise (TypeException "First statement in for loop must be a skip, declaration, or assignment")) *)
    | Return Some e ->
        let (e, t) = exp_to_texp (check_exp e d m g pm p ps) d pm in
        (TypedAst.Return (Some (e, t)), g, ps)
    | Return None -> (TypedAst.Return None, g, ps)
    | FnCall (it, args, pml) -> (match it with | VarTyp i -> 
        let ((i, args_exp), _) = check_fn_inv d m g p args i pml pm ps in 
        (TypedAst.FnCall (i, args_exp), g, ps)
        | _ -> raise (TypeException ("Cannot treat the type " ^ string_of_typ it ^ " as a function call")))

and check_comm_lst (cl : comm list) (d: delta) (m: mu) (g: gamma) (pm : parameterization) (p: phi) (ps: psi) : TypedAst.comm list * gamma * psi = 
    debug_print ">> check_comm_lst";
    match cl with
    | [] -> ([], g, ps)
    | h::t -> let (c', g', ps') = check_comm h d m g pm p ps in
        let (cl', g'', ps'') = check_comm_lst t d m g' pm p ps' in 
        (c' :: cl', g'', ps'')

and check_assign (t: typ) (s: string) (etyp : typ)  (d: delta) (g: gamma) (p: phi) (pm: parameterization): gamma =
    debug_print (">> check_assign <<"^s^">>");
    (* Check that t, if not a core type, is a registered tag *)
    let rec check_tag (t: typ) : unit =
        match t with
        | VarTyp s -> delta_lookup s [] d pm |> ignore; ()
        | ParTyp (VarTyp s, pml) -> delta_lookup s pml d pm |> ignore; ()
        | TransTyp (t1, t2) -> check_tag t1; check_tag t2; ()
        | _ -> ()
    in
    check_tag t;
    let check_name regexp = if Str.string_match regexp s 0 then raise (TypeException ("Invalid variable name " ^ s)) in
    check_name (Str.regexp "int$");
    check_name (Str.regexp "float$");
    check_name (Str.regexp "bool$");
    check_name (Str.regexp "vec[0-9]+$");
    check_name (Str.regexp "mat[0-9]+$");
    check_name (Str.regexp "mat[0-9]+x[0-9]+$");
    if Assoc.mem s d then 
        raise (TypeException ("Variable " ^ s ^ " has the name of a tag"))
    else if Assoc.mem s p then
        raise (TypeException ("Variable " ^ s ^ " has the name of a function"))
    else
        if is_subtype etyp t d pm then Assoc.update s t g
        else raise (TypeException ("Mismatched types for var decl for " ^ s ^  ": expected " ^ (string_of_typ t) ^ ", found " ^ (string_of_typ etyp)))

let check_tag (s: string) (pm: parameterization) (tm : modification option) (t: typ) (d: delta) (m: mu) : delta * mu = 
    debug_print ">> check_tag";
    let rec check_valid_supertype (t: typ) : constrain =
        match t with
        | TopVecTyp _ -> TypConstraint t
        | VarTyp s -> 
            if not (Assoc.mem s d) then raise (TypeException ("Unknown tag " ^ s)) else TypConstraint t
        | ParTyp (VarTyp s, pml) -> 
            if not (Assoc.mem s d) then raise (TypeException ("Unknown tag " ^ s))
            else let (tpm, _) = Assoc.lookup s d in
            let pmb = Assoc.bindings tpm in
            if List.length pmb == List.length pml
            then (List.fold_left2 (fun acc (s, c) t -> if is_sub_constraint t c d pm then () else
                raise (TypeException ("Invalid constraint used for parameterization of " ^ s)))
                () (Assoc.bindings tpm) (List.map check_valid_supertype pml); TypConstraint t)
            else raise (TypeException ("Invalid number of parameters provided to parameterized type " ^ s))
        | AbsTyp s -> if Assoc.mem s pm then Assoc.lookup s pm else raise (TypeException ("Unknown type " ^ (string_of_typ t)))
        | _ -> raise (TypeException ("Invalid type for tag declaration " ^ (string_of_typ t) ^ ", expected vector"))
    in
    let rec check_param_vec_bounds (cl : constrain list) : unit =
        match cl with
        | [] -> ()
        | h::t -> if is_sub_constraint h GenVecTyp d pm then check_param_vec_bounds t
            else raise (TypeException ("Invalid declaration of " ^ s ^ " -- must parameterize on vectors only"))
    in
    let rec check_coord (t : typ) : unit =
        match t with
        | VarTyp s -> (match Assoc.lookup s m with 
            | None -> check_coord (delta_lookup s [] d pm)
            | Some Coord -> raise (TypeException "Cannot declare a coord as a subtype of another coord")
            | Some Canon -> failwith "Cannot have a canonical tag?")
        | ParTyp (VarTyp s, pml) -> (match Assoc.lookup s m with 
            | None -> check_coord (delta_lookup s pml d pm)
            | Some Coord -> raise (TypeException "Cannot declare a coord as a subtype of another coord")
            | Some Canon -> failwith "Cannot have a canonical tag?")
        | _ -> ()
    in
    check_valid_supertype t |> ignore;
    check_param_vec_bounds (List.map snd (Assoc.bindings pm));
    if Assoc.mem s d then raise (TypeException "Cannot redeclare tag")
    else (match tm with | Some Coord -> check_coord t | _ -> ());
    (Assoc.update s (pm, t) d, Assoc.update s tm m)

let rec check_tags (t: tag_decl list) (d: delta) (m: mu): delta * mu =
    debug_print ">> check_tags";
    match t with 
    | [] -> (d, m)
    | (tm, s, pmd, a)::t ->
        check_typ_exp a |> ignore;
        let (d', m') = check_tag s (collapse_parameterization_decl pmd) tm a d m in
        check_tags t d' m'

let check_fn_decl (g: gamma) (d: delta) (m: mu) ((fm, id, (pl, rt, pmd)): fn_decl) (p: phi) (ps: psi) : 
(TypedAst.params * gamma * psi) * TypedAst.parameterization * mu * phi =
    debug_print (">> check_fn_decl : " ^ id);
    let pm = collapse_parameterization_decl pmd in
    let m' = check_parameterization d m pmd in
    let pr = check_params pl g d m' pm ps in 
    check_typ_valid rt d pm;
    let pme = Assoc.gen_context (List.map (fun (s, c) -> (s, constrain_erase c d pm)) (Assoc.bindings pm)) in
    if Assoc.mem id p 
    then raise (TypeException ("Function of duplicate name has been found: " ^ id))
    else (pr, pme, m', Assoc.update id (pl, rt, pm) p)

(* Helper function for type checking void functions. 
 * Functions that return void can have any number of void return statements 
 * anywhere. *)
let check_void_return (c: comm) =
    debug_print ">> check_void_return";
    match c with
    | Return Some _ -> raise (TypeException ("Void functions cannot return a value"))
    | _ -> ()

let check_return (t: typ) (d: delta) (m: mu) (g: gamma) (pm: parameterization) (p: phi) (ps: psi) (c: comm) = 
    debug_print ">> check_return";
    match c with
    | Return None -> raise (TypeException ("Expected a return value instead of void"))
    | Return Some r -> (
        let (_, rt) = check_exp r d m g pm p ps in
        (* raises return exception of given boolean exp is false *)
        if is_subtype rt t d pm then () 
        else raise (TypeException ("Mismatched return types, expected: " ^ 
        (string_of_typ t) ^ ", found: " ^ (string_of_typ rt)))
        )
    | _ -> ()

let update_mu_with_function (((fm, id, (pr, r, pmd))): fn_decl) (d: delta) (m: mu) (ps: psi) : mu =
    let m' = Assoc.update id fm m in
    match (fm, pr) with
    (* Only update if it is a canon function with exactly one argument *)
    (* TODO: add to phi, not to psi unless it is concrete *)
    | (Some Canon, [(_, t)]) ->
    begin
        if is_typ_eq t r then raise (TypeException ("Canonical function " ^ id ^ " cannot be a map from a type to itself")) else
        let fail _ = raise (TypeException "Canonical functions must be between tag or abstract types") in
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
    | (Some Canon, _) -> raise (TypeException "Cannot have a canonical function with zero or more than one arguments")
    | _ -> m'

let rec check_fn (((fm, id, (pr, r, pmd)), cl): fn) (g: gamma) (d: delta) (m: mu) (p: phi) (ps: psi) : psi * mu * TypedAst.fn * phi = 
    debug_print (">> check_fn : " ^ id);
    (* update phi with function declaration *)
    let pm = collapse_parameterization_decl pmd in
    let ((pl', g', ps'), pm', m', p') = check_fn_decl g d m (fm, id, (pr, r, pmd)) p ps in 
    let (cl', g'', ps'') = check_comm_lst cl d m' g' pm p ps' in 
    let m_ret = update_mu_with_function (fm, id, (pr, r, pmd)) d m ps in
    (* check that the last command is a return statement *)
    match r with
    | UnitTyp -> List.iter check_void_return cl; (ps, m_ret, (((id, (pl', TypedAst.UnitTyp, pm')), cl')), p')
    (* TODO: might want to check that there is exactly one return statement at the end *)
    | t -> List.iter (check_return t d m g'' pm p ps'') cl; (ps, m_ret, (((id, (pl', tag_erase t d pm, pm')), cl')), p')

and check_fn_lst (fl: fn list) (g: gamma) (d: delta) (m: mu) (p: phi) (ps: psi) : TypedAst.prog * phi =
    debug_print ">> check_fn_lst";
    match fl with
    | [] -> ([], p)
    | h::t -> let (ps', m', fn', p') = check_fn h g d m p ps in
        let (fn'', p'') = check_fn_lst t g d m' p' ps' in 
        (fn' :: fn'', p'')

(* Check that there is a void main() defined *)
let check_main_fn (p: phi) : unit =
    debug_print ">> check_main_fn";
    let (params, ret_type, pm) = Assoc.lookup "main" p in 
    debug_print (">> check_main_fn_2" ^ (string_of_params params) ^ (string_of_parameterization pm));
    if (List.length params) > 0 || (Assoc.size pm) > 0 then raise (TypeException "Cannot provide parameters to main") else
    match ret_type with
        | UnitTyp -> ()
        | _ -> raise (TypeException ("Expected main function to return void"))

let check_decls (g: gamma) (d: delta) (m: mu) (dl : extern_decl) (p: phi) (ps: psi) : (gamma * mu * phi)=
    match dl with
    | ExternFn f -> let (_, _, _, p') = (check_fn_decl g d m f p ps) in 
        let m' = update_mu_with_function f d m ps in
        (g, m', p')
    | ExternVar (t, Var x) -> (Assoc.update x t g, m, p)
    | _ -> raise (TypeException ("Invalid declaration, must be a function or variable"))

(* Returns the list of fn's which represent the program 
 * and params of the void main() fn *)
let check_prog ((dl, t, gv, f): prog) : TypedAst.prog * TypedAst.global_vars =
    debug_print ">> check_prog";
    (*(d: delta) ((id, t): fn_decl) (p: phi) *)
    (* delta from tag declarations *)
    let (d, m) = check_tags t Assoc.empty Assoc.empty in 
    (* TODO: Why is gamma getting carried over here?  I'm a bit suspicious *)
    let (g, m', p) = List.fold_left 
        (fun (g', m', p') (dl': extern_decl) -> check_decls g' d m' dl' p' Assoc.empty) 
        (Assoc.empty, m, Assoc.empty) dl in
    let (gvr, g', ps) = check_global_variables gv g d m' Assoc.empty in
    let (e', p') = check_fn_lst f g' d m' p ps in
    check_main_fn p';
    debug_print "===================";
    debug_print "Type Check Complete";
    debug_print "===================\n";
    (e', gvr)
