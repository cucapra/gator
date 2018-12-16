open CoreAst
open TagAst
open TagAstPrinter
open Util
open Printf
open Str

exception TypeException of string
exception DimensionException of int * int

(* For readability, especially with psi *)
type param_inv = id * typ list 

(* Variable defs *)
type gamma = typ Assoc.context

(* Tags defs *)
type delta = (parameterization * typ) Assoc.context

(* Function defs *)
type phi = fn_type Assoc.context

(* Tag modifications *)
type mu = (tag_mod option) Assoc.context

(* Transformation context *)
(* Effectively has the type 'start->(target, f<pml>) list' for types start and target (both restricted implicitely to var types), function/matrix name f, and function parameter list pml *)
(* Note that the resulting thing could be a function with a concrete parameterization, hence the typ list (which is empty for matrices) *)
type psi = ((param_inv * param_inv) list) Assoc.context

let string_of_delta (d : delta) = Assoc.to_string (fun (pm, t) -> "(" ^ string_of_parameterization pm ^ ", " ^ string_of_typ t ^ ")") d
let string_of_param_inv ((s, tl) : param_inv) = string_of_typ (VarTyp (s, tl))
let string_of_psi (ps : psi) = Assoc.to_string (fun x -> string_of_arr (fun (p1, p2) -> "(" ^ string_of_param_inv p1 ^ ", " ^ string_of_param_inv p2 ^ ")") x) ps

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
    | VarTyp (s, tl) -> VarTyp (s, List.map (fun x -> replace_abstype x c) tl)
    | AbsTyp s -> Assoc.lookup s c
    | TransTyp (t1, t2) -> TransTyp (replace_abstype t1 c, replace_abstype t2 c)
    | _ -> t

(* Looks up delta without checking the bounds on the pml *)
let delta_lookup_unsafe (s: id) (pml: typ list) (d: delta) : typ =
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

and constrain_to_constrain (c : TypedAst.constrain) : constrain =
    debug_print ">> constrain_to_constrain";
    match c with
    | TypedAst.AnyTyp -> AnyTyp
    | TypedAst.GenTyp -> GenTyp
    | TypedAst.GenMatTyp -> GenMatTyp
    | TypedAst.GenVecTyp -> GenVecTyp
    | TypedAst.ETypConstraint t -> TypConstraint (etyp_to_typ t)

type dim_constrain_typ = | UnconInt of int | ConString of string
let rec vec_dim_constrain (t: typ) (d: delta) (pm: parameterization) : dim_constrain_typ =
    debug_print ">> vec_dim";
    let fail _ = failwith ("Expected a vector for computing the dimension, got " ^ (string_of_typ t)) in
    match t with
    | TopVecTyp n
    | BotVecTyp n -> UnconInt n
    | VarTyp (s, pml) -> vec_dim_constrain (delta_lookup_unsafe s pml d) d pm
    | AbsTyp s -> (match unwrap_abstyp s pm with
        | TypConstraint t' -> vec_dim_constrain t' d pm
        | GenVecTyp -> ConString s
        | _ -> fail ())
    | _ -> fail ()

let vec_dim (t: typ) (d: delta) (pm: parameterization) : int =
    match vec_dim_constrain t d pm with
    | UnconInt i -> i | _ -> raise (TypeException ("Vector " ^ string_of_typ t ^ " does not have a concrete dimension"))

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
    | VarTyp _ -> (match vec_dim_constrain t d pm with
        | UnconInt i -> TypedAst.VecTyp i | ConString s -> TypedAst.AbsTyp (s, TypedAst.GenVecTyp))
    (* Convert to the strange glsl style of doing things *)
    (* TODO: standardize this and handle switching in the emitter *)
    | TransTyp (t1, t2) -> 
    begin
        match (vec_dim_constrain t1 d pm, vec_dim_constrain t2 d pm) with
        | (UnconInt n1, UnconInt n2) -> TypedAst.MatTyp (n2, n1)
        | _ -> TypedAst.TransTyp (tag_erase t1 d pm, tag_erase t2 d pm)
    end
    | SamplerTyp i -> TypedAst.SamplerTyp i
    | SamplerCubeTyp -> TypedAst.SamplerCubeTyp
    | AbsTyp s -> tag_erase_param t d pm 
    | AutoTyp -> raise (TypeException "Illegal use of auto (cannot use auto as part of a function call)")

and constrain_erase (c: constrain) (d : delta) (pm : parameterization) : TypedAst.constrain =
    debug_print ">> constrain_erase";
    match c with
    | AnyTyp -> TypedAst.AnyTyp
    | GenTyp -> TypedAst.GenTyp
    | GenMatTyp -> TypedAst.GenMatTyp
    | GenVecTyp -> TypedAst.GenVecTyp
    | TypConstraint t -> TypedAst.ETypConstraint (tag_erase t d pm)

let rec is_subtype_with_strictness (to_check : typ) (target : typ) (d : delta) (pm: parameterization) (strict : bool) : bool =
    debug_print (">> is_subtype " ^ (string_of_typ to_check) ^ ", " ^(string_of_typ target));
    let rec tag_typ_equality (t1: typ) (t2: typ) : bool =
        match (t1, t2) with
        | BotVecTyp n1, BotVecTyp n2 
        | TopVecTyp n1, TopVecTyp n2 -> n1 = n2
        | VarTyp (s1, pml1), VarTyp (s2, pml2) -> s1 = s2 &&
                if List.length pml1 = List.length pml2
                then List.fold_left2 (fun acc t1 t2 -> tag_typ_equality t1 t2 && acc) true pml1 pml2
                else false
        | AbsTyp s1, AbsTyp s2 -> s1 = s2
        | _ -> false
    in
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
    | BotVecTyp n, VarTyp _ -> n = (vec_dim target d pm)
    | VarTyp _, BotVecTyp _ -> false
    | VarTyp (s, pml), VarTyp _ -> if tag_typ_equality to_check target then true
        else is_subtype_with_strictness (delta_lookup_unsafe s pml d) target d pm strict
    | VarTyp _, TopVecTyp n -> not strict && (vec_dim to_check d pm) = n
    | (SamplerTyp i1, SamplerTyp i2) -> i1 = i2 
    | (BoolTyp, BoolTyp)
    | (IntTyp, IntTyp)
    | (FloatTyp, FloatTyp)
    | (SamplerCubeTyp, SamplerCubeTyp) -> true
    | (TransTyp (t1, t2), TransTyp (t3, t4)) -> 
        (is_subtype_with_strictness t3 t1 d pm false && is_subtype_with_strictness t2 t4 d pm strict)
    | (AbsTyp s1, AbsTyp s2) -> 
        s1 = s2 || abstyp_step s1
    | (AbsTyp s, _) -> 
        abstyp_step s
    (* Necessary because we have a lattice and the bottype is less than EVERYTHING in that lattice *)
    | (BotVecTyp n, AbsTyp _) -> (is_subtype_with_strictness target (TopVecTyp n) d pm false)
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
    | (_, GenTyp) -> true
    | (GenVecTyp, GenVecTyp)
    | (TypConstraint (BotVecTyp _), GenVecTyp)
    | (TypConstraint (VarTyp _), GenVecTyp)
    | (TypConstraint (TopVecTyp _), GenVecTyp) -> true
    | (_, GenVecTyp) -> false
    | (GenMatTyp, GenMatTyp)
    | (TypConstraint (TransTyp _), GenMatTyp) -> true
    | (_, GenMatTyp) -> false

let rec is_bounded_by (t: typ) (c: constrain) (d: delta) (pm: parameterization): bool =
    debug_print ">> is_bounded_by";
    is_sub_constraint (TypConstraint t) c d pm

let match_parameterization (d: delta) (pmb: (string * constrain) list) (pml : typ list) (pm: parameterization) : (typ Assoc.context) option =
    debug_print ">> match_parametrization";
    if List.length pmb == List.length pml
    then List.fold_left2 (fun acc (s, c) t -> match acc with | None -> None 
        | Some tl -> if is_bounded_by t c d pm then Some (Assoc.update s t tl) else None)
        (Some (Assoc.empty)) pmb pml
    else None

let delta_lookup (s: id) (pml: typ list) (d: delta) (pm: parameterization) : typ =
    debug_print ">> delta_lookup";
    if Assoc.mem s d then
    let (pm_to_match, t) = Assoc.lookup s d in
    match (match_parameterization d (Assoc.bindings pm_to_match) pml pm) with
    | Some cont -> replace_abstype t cont
    | None -> raise (TypeException ("Invalid parameters <" ^ (string_of_lst string_of_typ pml) ^ "> to " ^ s))
    else raise (TypeException ("Unknown tag " ^ s))

let rec greatest_common_child (t1: typ) (t2: typ) (d: delta) (pm: parameterization): typ =
    debug_print ">> greatest_common_child";
    let check_dim (n1: int) (n2: int) : unit =
        if n1 = n2 then () else (raise (DimensionException (n1, n2)))
    in
    if (is_subtype t1 t2 d pm) then t1 else if (is_subtype t2 t1 d pm ) then t2 
    else match (t1, t2) with
    | BotVecTyp n1, BotVecTyp n2
    | BotVecTyp n1, TopVecTyp n2
    | TopVecTyp n1, BotVecTyp n2 ->
        check_dim n1 n2; BotVecTyp n1                                                                                                                                                                                                                                                                                                                                                                                                                              
    | TopVecTyp n1, TopVecTyp n2 ->
        check_dim n1 n2; TopVecTyp n1
    | VarTyp (s, pml), TopVecTyp n1
    | TopVecTyp n1, VarTyp (s, pml) ->
        check_dim (vec_dim (VarTyp (s, pml)) d pm) n1; VarTyp (s, pml)
    | VarTyp (s, pml), BotVecTyp n1
    | BotVecTyp n1, VarTyp (s, pml) ->
        check_dim (vec_dim (VarTyp (s, pml)) d pm) n1; BotVecTyp n1
    | VarTyp _, VarTyp _ ->
        begin
            let bot_dim = vec_dim t1 d pm in
            check_dim bot_dim (vec_dim t2 d pm); BotVecTyp bot_dim
            (* This works since each tag can only have one parent and we did subtype checks earlier *)
        end
    | _ -> raise (TypeException ("Cannot unify " ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2)))

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
        | VarTyp (s, pml), TopVecTyp n1
        | TopVecTyp n1, VarTyp (s, pml) ->
            check_dim (vec_dim (VarTyp (s, pml)) d pm) n1;
            if strict then
            raise (TypeException ("Cannot implicitly cast " ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2) ^ " to the top vector type"  ))
            else TopVecTyp n1
        | VarTyp (s, pml), _
        | _, VarTyp (s, pml) ->
            (* Just go up a step -- the end condition is the is_subtype check earlier *)
            least_common_parent_with_strictness (delta_lookup s pml d pm) t2 d pm strict
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

let check_subtype_list (t: typ) (l: typ list) (d: delta) (pm: parameterization) : bool =
    debug_print ">> check_subtype_list";
    List.fold_left (fun acc t' -> acc || (is_subtype t t' d pm)) false l

let check_bounds_list (t: typ) (l: constrain list) (d: delta) (pm: parameterization) : bool =
    debug_print ">> check_bounds_list";
    List.fold_left (fun acc t' -> acc || (is_bounded_by t t' d pm)) false l
    

let rec check_typ_valid (t: typ) (d: delta) (pm: parameterization) : unit =
    debug_print ">> check_typ_valid";
    match t with
    | VarTyp (s, _) -> if Assoc.mem s d then () else raise (TypeException ("Unknown tag " ^ s))
    | AbsTyp s -> if Assoc.mem s pm then () else raise (TypeException ("Unknown abstract type `" ^ s))
    | TransTyp (t1, t2) -> check_typ_valid t1 d pm; check_typ_valid t2 d pm; 
        if is_bounded_by t1 GenVecTyp d pm && is_bounded_by t2 GenVecTyp d pm then ()
        else raise (TypeException ("Invalid matrix type " ^ (string_of_typ t) ^ " (must be a map from vectors to vectors)"))
    | _ -> ()

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
    | SamplerTyp _ -> ()
    | TopVecTyp n
    | BotVecTyp n -> (if (n > 0) then ()
        else raise (TypeException "Cannot declare a type with dimension less than 0"))
    | VarTyp (s, pml) -> delta_lookup s pml d |> ignore; ()
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
    if is_bounded_by t GenTyp d pm then t
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
    if is_bounded_by t1 GenTyp d pm then least_common_parent t1 t2 d pm
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
    else if check_subtype_list t1 intfloat_list d pm && is_bounded_by t2 GenTyp d pm then t2
    else if check_subtype_list t2 intfloat_list d pm && is_bounded_by t1 GenTyp d pm then t1
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
        else fail () 
    else fail ()

let check_parameterization (d: delta) (pm: parameterization) : unit =
    let rec check_para_list param found = 
    match param with
    | [] -> ()
    | (s,c)::t -> if Assoc.mem s found then raise (TypeException ("Duplicate parameter `" ^ s)) 
        else (match c with
        | TypConstraint t' -> check_typ_valid t' d found
        | _ -> ()); 
        check_para_list t (Assoc.update s c found)
    in
    check_para_list (Assoc.bindings pm) Assoc.empty

let update_psi ((start_string, stl): param_inv) ((target_string, ttl): param_inv) ((f, pml): param_inv) (m: mu) (ps: psi) : psi =
    (* Update psi, raising errors in case of a duplicate *)
    (* If the given type is not valid in psi, psi is returned unmodified *)
    (* Will raise a failure if a non-concrete vartyp is used *)
    debug_print ">> update_psi";
    let rec check_var_typ_eq (t1: typ) (t2: typ) : bool =
        match (t1, t2) with
        | TopVecTyp n1, TopVecTyp n2 -> n1 = n2
        | VarTyp (s1, tl1), VarTyp (s2, tl2) -> s1 = s2 && 
            (if List.length tl1 = List.length tl2
            then List.fold_left2 (fun acc t1' t2' -> acc && check_var_typ_eq t1' t2') true tl1 tl2
            else false)
        | _ -> false
    in
    let are_coord (s1: string) (s2: string) : bool =
        (if Assoc.mem s1 m && Assoc.mem s2 m
        then (match (Assoc.lookup s1 m, Assoc.lookup s2 m) with
        | (Some Coord, Some Coord) -> true
        | _ -> false)
        else false)
    in
    let start = string_of_param_inv (start_string, stl) in
    let to_add = ((target_string, ttl), (f, pml)) in
    if are_coord start_string target_string then
        if Assoc.mem start ps then 
        (let start_lst = Assoc.lookup start ps in
            if (List.fold_left (fun acc ((s2, tl2), (_, _)) -> acc ||
                    (if (List.length ttl = List.length tl2) 
                    then List.fold_left2 (fun acc' t1 t2 -> acc' || (check_var_typ_eq t1 t2)) false ttl tl2
                    else false))
                false start_lst)
            then raise (TypeException ("Duplicate transformation for " ^ start ^ "->" ^ string_of_param_inv (target_string, ttl) ^ " in the declaration of " ^ f))
            else Assoc.update start (to_add :: start_lst) ps
        )
        else Assoc.update start [to_add] ps 
    else ps

let update_psi_matrix (f: string) (t: typ) (m: mu) (ps: psi) : psi =
    match t with
    | TransTyp ((VarTyp (s1, pml1)), (VarTyp (s2, pml2))) ->
        update_psi (s1, pml1) (s2, pml2) (f, [])  m ps
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
    
let exp_to_texp (checked_exp : TypedAst.exp * typ) (d : delta) (pm : parameterization) : TypedAst.texp = 
    debug_print ">> exp_to_texp";
    ((fst checked_exp), (tag_erase (snd checked_exp) d pm))

(* Super expensive.  We're essentially relying on small contexts *)
let check_in_exp (start_exp: exp) (start_typ: typ) (target: typ) (g: gamma) (d: delta) 
(pm: parameterization) (p: phi) (ps: psi) : exp = 
    debug_print ">> check_in_exp";
    let rec psi_path_rec ((ts, ttl): param_inv) (to_search: (param_inv * exp) Queue.t) (found: param_inv list) : exp =
        let rec check_var_typ_eq (t1: typ) (t2: typ) : bool =
            match (t1, t2) with
            | TopVecTyp n1, TopVecTyp n2 -> n1 = n2
            | VarTyp (s1, tl1), VarTyp (s2, tl2) -> s1 = s2 && 
                (if (List.length tl1 = List.length tl2) 
                then List.fold_left2 (fun acc t1' t2' -> acc && check_var_typ_eq t1' t2') true tl1 tl2
                else false)
            | _ -> false
        in
        let rec psi_lookup_rec ((s, tl): param_inv) : ((param_inv * param_inv) list) option =
            let s_lookup = string_of_typ (VarTyp (s, tl)) in
            if Assoc.mem s_lookup ps then Some (Assoc.lookup s_lookup ps)
            else match delta_lookup_unsafe s tl d with
            | VarTyp (s', tl') -> 
                psi_lookup_rec (s', tl')
            | _ -> None
        in
        let rec update_search_and_found (vals: (param_inv * param_inv) list) (e: exp) : param_inv list =
            match vals with
            | [] -> found
            | ((s, tl), (v, pml))::t -> 
                if List.fold_left (fun acc (s2, tl2) -> acc || check_var_typ_eq (VarTyp (s, tl)) (VarTyp (s2, tl2))) false found 
                then update_search_and_found t e 
                else 
                let e' = 
                    if Assoc.mem v g then (Binop (Times, Var v, e))
                    else if Assoc.mem v p then (FnInv (v, [e], pml))
                    else failwith ("Typechecker error: unknown value " ^ v ^ " loaded into psi") in
                (* Note the update to the stateful queue *)
                (Queue.push ((s, tl), e') to_search;  (s, tl) :: update_search_and_found t e)
        in
        let ((ns, ntl), e) = if Queue.is_empty to_search 
            then (raise (TypeException ("Cannot find a path from " ^ (string_of_param_inv (List.hd (List.rev found))) ^ " to " ^ string_of_param_inv (ts, ttl))))
            else Queue.pop to_search 
        in 
        (* We use the 'with_strictness' version to avoid throwing an exception *)
        if is_subtype_with_strictness (VarTyp (ns, ntl)) (VarTyp (ts, ttl)) d pm false then e
        else (match psi_lookup_rec (ns, ntl) with 
            | None -> psi_path_rec (ts, ttl) to_search found
            | Some next_cont -> psi_path_rec (ts, ttl) to_search (update_search_and_found next_cont e))
    in
    match (start_typ, target) with
    | (VarTyp (s1, tl1), VarTyp (s2, tl2)) -> 
        if not (Assoc.mem s1 d) then
        raise (TypeException ("Unknown tag " ^ (string_of_typ start_typ)))
        else if not (Assoc.mem s2 d) then
        raise (TypeException ("Unknown tag " ^ (string_of_typ target)))
        else if s1 = s2 then start_exp else
        let q = Queue.create () in Queue.push ((s1, tl1), start_exp) q;
        psi_path_rec (s2, tl2) q [(s1, tl1)]
    | _ -> raise (TypeException 
    ("Invalid in expression between " ^ (string_of_typ start_typ) ^ " and " ^ (string_of_typ target)))

let rec check_exp (e : exp) (d : delta) (g : gamma) (pm : parameterization) (p : phi) (ps: psi) : TypedAst.exp * typ = 
    debug_print ">> check_exp";
    let build_unop (op : unop) (e': exp) (check_fun: typ->delta->parameterization->typ) (pm: parameterization)
        : TypedAst.exp * typ =
        let result = check_exp e' d g pm p ps in
            (TypedAst.Unop(op, exp_to_texp result d pm), check_fun (snd result) d pm)
    in
    let build_binop (op : binop) (e1: exp) (e2: exp) (check_fun: typ->typ->delta->parameterization->typ) (pm: parameterization)
        : TypedAst.exp * typ =
        let e1r = check_exp e1 d g pm p ps in
        let e2r = check_exp e2 d g pm p ps in
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
        (TypedAst.Var v, Assoc.lookup v g)
    | Arr a -> check_arr d g p a pm ps
    | As (e, t) -> let (er, tr) = check_exp e d g pm p ps in (er, check_as_exp tr t d pm)
    | In (e, t) -> let (er, tr) = check_exp e d g pm p ps in (check_exp (check_in_exp e tr t g d pm p ps) d g pm p ps)
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
    | FnInv (i, args, pr) -> let ((i, args_exp), rt) = check_fn_inv d g p args i pr pm ps in 
        (FnInv (i, args_exp), rt)
        
and check_arr (d : delta) (g : gamma) (p : phi) (a : exp list) (pm : parameterization) (ps: psi) : (TypedAst.exp * typ) =
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
    let checked_a = List.map (fun e -> (exp_to_texp (check_exp e d g pm p ps) d pm )) a in
    let length_a = List.length a in
    if is_vec checked_a then (TypedAst.Arr checked_a, BotVecTyp length_a) else 
    (match is_mat checked_a with
    | Some n -> (TypedAst.Arr checked_a, trans_bot n length_a)
    | None ->  raise (TypeException ("Invalid array definition for " ^ (string_of_exp (Arr a)) ^ ", must be a matrix or vector")))


and check_fn_inv (d : delta) (g : gamma) (p : phi) (args : args) (i : string) (pml: typ list) (pm : parameterization) (ps: psi)
 : (string * TypedAst.args) * typ = 
    debug_print ">> check_fn_inv";
    let fn_invocated = if Assoc.mem i p
        then Assoc.lookup i p
        else raise (TypeException ("Invocated function " ^ i ^ " not found")) in
    let (_, _, rt, _) = fn_invocated in
    let args' = List.map (fun a -> check_exp a d g pm p ps) args in 
    let args_exp = List.map fst args' in
    let args_typ = List.map snd args' in
    (* find definition for function in phi *)
    (* looks through overloaded all possible definitions of the function *)
    let find_fn_inv ((_, params, rt, pr) : fn_type) : (typ Assoc.context) option =
        debug_print ">> find_fn_inv";
        (* This function asserts whether or not the function invocation matches the function given *)
        (* In particular, this checks whether the given function matches the given parameterization and parameters *)
        (* If it is valid, this returns (Some 'map from parameter to type'), otherwise returns 'None' *)

        (* If we have the wrong number of arguments, then no match for sure *)
        if List.length args != List.length params then None else
        (* Work out the parameter inference if one is needed *)
        let pml_infer = if Assoc.size pr == List.length pml then Some pml
            else if List.length pml == 0 then 
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
                    | (VarTyp (s1, tl1), VarTyp (s2, tl2)) -> if List.length tl1 = List.length tl2 then
                        List.fold_left2 (fun acc l r -> unify_param l r acc) fpm tl1 tl2 else fpm
                    | _ -> fpm
                in
                let gen_pml (inferred : (typ Assoc.context) option) : (typ list) option =
                    let rec reduce_typ (t : typ) (fpm : typ Assoc.context) : typ =
                        match t with
                        (* Don't have to check if t is valid *)
                        (* already handled by the typechecker when checking that the function declaration parameter dependencies were valid *)
                        | AbsTyp s -> Assoc.lookup s fpm
                        | TransTyp (l, r) -> TransTyp(reduce_typ l fpm, reduce_typ r fpm)
                        | VarTyp (s, tl) -> VarTyp(s, List.map (fun x -> reduce_typ x fpm) tl)
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
                gen_pml (List.fold_left2 
                    (fun fpm arg_typ (_, par_typ) -> unify_param arg_typ par_typ fpm) 
                    (Some Assoc.empty) args_typ params)
            else None 
        in
        
        match pml_infer with | None -> None | Some pml' ->
        (* Helper function for using the function parameters as they are constructed *)
        let apply_fpm (c: constrain) (fpm : typ Assoc.context) =
            let rec in_function_t t = 
                match t with
                | AbsTyp s -> Assoc.lookup s fpm
                | TransTyp (t1, t2) -> TransTyp (in_function_t t1, in_function_t t2)
                | VarTyp (s, tl) -> VarTyp (s, List.map in_function_t tl)
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
            | TransTyp (s1, s2) -> TransTyp (read_pm s1, read_pm s2)
            | VarTyp (s, tl) -> VarTyp (s, List.map read_pm tl)
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
        let (e, t) = exp_to_texp (check_exp e d g pm p ps) d pm in 
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
    | Decl (t, tp, s, e) -> (* TODO: code insertion *)
        if Assoc.mem s g then raise (TypeException "variable name shadowing is illegal")        
        else 
        (check_typ_valid t d pm;
        let result = check_exp e d g pm p ps in
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
            let result = check_exp e d g pm p ps in
            (TypedAst.Assign (s, (exp_to_texp result d pm)), check_assign t s (snd result) d g p pm, ps)
        else raise (TypeException ("Assignment to undeclared variable: " ^ s))
    | AssignOp (s, b, e) -> 
        let (c', g', ps') = check_comm (Assign (s, Binop(b, Var s, e))) d m g pm p ps in
        (match c' with
        | TypedAst.Assign (_, (TypedAst.Binop (_, (_, st), e), _)) -> (TypedAst.AssignOp((s, st), b, e), g', ps')
        | _ -> failwith "Assign must return an assign?")
    | If ((b, c1), el, c2) ->
        let check_if b c =
            let er = (check_exp b d g pm p ps) in
            let (cr, _, _) = check_comm_lst c d m g pm p ps in
            (match (snd er) with 
            | BoolTyp -> ((exp_to_texp er d pm), cr)
            | _ -> raise (TypeException "Expected boolean expression for if condition"))
        in
        let c2r = (match c2 with | Some e -> Some (tr_fst (check_comm_lst e d m g pm p ps)) | None -> None) in
        (TypedAst.If (check_if b c1, List.map (fun (b, c) -> check_if b c) el, c2r), g, ps)
    | For (c1, b, c2, cl) ->
        let (c1r, g', ps') = check_comm c1 d m g pm p ps in
        let (br, brt) = check_exp b d g' pm p ps in
        let btexp = exp_to_texp (br, brt) d pm in
        let (c2r, _, _) = check_comm c2 d m g' pm p ps' in
        (match c1r with
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
        | _ -> raise (TypeException "First statement in for loop must be a skip, declaration, or assignment"))
    | Return Some e ->
        let (e, t) = exp_to_texp (check_exp e d g pm p ps) d pm in
        (TypedAst.Return (Some (e, t)), g, ps)
    | Return None -> (TypedAst.Return None, g, ps)
    | FnCall (i, args, pml) -> let ((i, args_exp), _) = check_fn_inv d g p args i pml pm ps in 
        (TypedAst.FnCall (i, args_exp), g, ps)

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
        | VarTyp (s, pml) -> delta_lookup s pml d pm |> ignore; ()
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

let check_tag (s: string) (pm: parameterization) (tm : tag_mod option) (t: typ) (d: delta) (m: mu) : delta * mu = 
    debug_print ">> check_tag";
    let rec check_valid_supertype (t: typ) : constrain =
        match t with
        | TopVecTyp _ -> TypConstraint t
        | VarTyp (s, pml) -> if not (Assoc.mem s d) then raise (TypeException ("Unknown tag" ^ s))
            else let (tpm, _) = Assoc.lookup s d in
            let pmb = Assoc.bindings pm in
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
        | VarTyp (s, pml) -> (match Assoc.lookup s m with 
            | None -> check_coord (delta_lookup s pml d pm)
            | Some Coord -> raise (TypeException "Cannot declare a coord as a subtype of another coord"))
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
    (* TODO: add a context or update delta to lookup tag modifications *)
    | (tm, s, pm, a)::t ->
        check_typ_exp a |> ignore;
        let (d', m') = check_tag s pm tm a d m in
        check_tags t d' m'

let check_fn_decl (g: gamma) (d: delta) (m: mu) ((id, (fm, pl, rt, pm)): fn_decl) (p: phi) (ps: psi) : (TypedAst.params * gamma * psi) * TypedAst.parameterization * phi =
    debug_print (">> check_fn_decl : " ^ id);
    check_parameterization d pm;
    let pr = check_params pl g d m pm ps in 
    check_typ_valid rt d pm;
    let pme = Assoc.gen_context (List.map (fun (s, c) -> (s, constrain_erase c d pm)) (Assoc.bindings pm)) in
    if Assoc.mem id p 
    then raise (TypeException ("Function of duplicate name has been found: " ^ id))
    else (pr, pme, Assoc.update id (fm, pl, rt, pm) p)

(* Helper function for type checking void functions. 
 * Functions that return void can have any number of void return statements 
 * anywhere. *)
let check_void_return (c: comm) =
    debug_print ">> check_void_return";
    match c with
    | Return Some _ -> raise (TypeException ("Void functions cannot return a value"))
    | _ -> ()

let check_return (t: typ) (d: delta) (g: gamma) (pm: parameterization) (p: phi) (ps: psi) (c: comm) = 
    debug_print ">> check_return";
    match c with
    | Return None -> raise (TypeException ("Expected a return value instead of void"))
    | Return Some r -> (
        let (_, rt) = check_exp r d g pm p ps in
        (* raises return exception of given boolean exp is false *)
        if is_subtype rt t d pm then () 
        else raise (TypeException ("Mismatched return types, expected: " ^ 
        (string_of_typ t) ^ ", found: " ^ (string_of_typ rt)))
        )
    | _ -> ()

let update_psi_with_function (((id, (fm, pr, r, pm))): fn_decl) (d: delta) (m: mu) (ps: psi) : psi =
    let is_coord t = match Assoc.lookup t m with | Some Coord -> true | _ -> false in
    let rec replace_abs (t : typ) (strict_pm : typ Assoc.context) : typ =
        match t with
        | AbsTyp s -> if Assoc.mem s strict_pm then Assoc.lookup s strict_pm else t
        | TransTyp (t1, t2) -> TransTyp (replace_abs t1 strict_pm, replace_abs t2 strict_pm)
        | _ -> t
    in
    let gen_valid_pms (_ : unit) : (typ Assoc.context) list =
        let rec gen_valid_pm_lists (pmt : typ Assoc.context) (pml : ((string * constrain) list)) : ((string * typ) list) list =
            let replace_abs_constrain c sp = match c with | TypConstraint t -> TypConstraint (replace_abs t sp) | _ -> c in
            let rec try_tag (s: string) (tag: string) (c: constrain) tail : ((string * typ) list) list =
                if not (is_coord tag) then [] else
                let tag' = VarTyp (tag, []) in
                if not (is_bounded_by tag' c d pm) then []
                else match tail with 
                (* We have to check if there's more recursion to be done so something eventually gets loaded into the list *)
                | [] -> [[(s, tag')]] 
                | _ -> List.map (fun x -> (s, tag')::x) (gen_valid_pm_lists (Assoc.update s tag' pmt) tail)
            in
            match pml with
            | [] -> []
            | (s, c)::tail -> let c' = replace_abs_constrain c pmt in
                List.fold_left (fun acc t -> try_tag s (fst t) c' tail @ acc) [] (Assoc.bindings d);
        in
        List.map Assoc.gen_context (gen_valid_pm_lists Assoc.empty (Assoc.bindings pm))
    in
    match (fm, pr) with
    (* Only update if it is a canon function with exactly one argument *)
    | (Some Canon, [(_, t)]) -> failwith "unimplemented1"
    (* begin
        match (t, r) with
        | ((TagTyp (VarTyp ts)), TagTyp (VarTyp rs)) -> 
        begin
            if ts = rs then raise (TypeException "Cannot apply canon to a function mapping a type to itself") else
            if is_coord ts && is_coord rs then update_psi (id, []) ts rs m ps
            else raise (TypeException "Cannon functions must be maps from coords to coords")
        end
        | _ -> let valid_pms = gen_valid_pms () in
        let as_tag_str t = match t with | TagTyp (VarTyp s) -> s | _ -> raise (TypeException ("Invalid type argument for canon function in " ^ (string_of_typ t))) in
        (* Remove the identity type to avoid adding it to psi *)
        let valid_pms' = List.fold_left 
            (fun acc pml -> let (ts, rs) = (as_tag_str (replace_abs t pml)), (as_tag_str (replace_abs r pml)) in 
                if ts = rs then acc else (pml, ts, rs) :: acc) [] valid_pms in
        if (List.length valid_pms' == 0) then raise (TypeException ("No valid canon declaration for " ^ id))
        else List.fold_left (fun acc (pml, ts, rs) -> update_psi (id, (List.map snd (Assoc.bindings pml))) ts rs m acc) ps valid_pms'
    end *)
    | (Some Canon, _) -> raise (TypeException "Cannot have a canon function with zero or more than one arguments")
    | _ -> ps

let rec check_fn (((id, (fm, pr, r, pm)), cl): fn) (g: gamma) (d: delta) (m: mu) (p: phi) (ps: psi) : psi * TypedAst.fn * phi = 
    debug_print (">> check_fn : " ^ id);
    (* update phi with function declaration *)
    let ((pl', g', ps'), pm', p') = check_fn_decl g d m (id, (fm, pr, r, pm)) p ps in 
    let (cl', g'', ps'') = check_comm_lst cl d m g' pm p ps' in 
    let ps_ret = update_psi_with_function (id, (fm, pr, r, pm)) d m ps in
    (* check that the last command is a return statement *)
    match r with
    | UnitTyp -> List.iter check_void_return cl; (ps_ret, (((id, (pl', TypedAst.UnitTyp, pm')), cl')), p')
    (* TODO: might want to check that there is exactly one return statement at the end *)
    | t -> List.iter (check_return t d g'' pm p ps'') cl; (ps_ret, (((id, (pl', tag_erase t d pm, pm')), cl')), p')

and check_fn_lst (fl: fn list) (g: gamma) (d: delta) (m: mu) (p: phi) (ps: psi) : TypedAst.prog * phi =
    debug_print ">> check_fn_lst";
    match fl with
    | [] -> ([], p)
    | h::t -> let (ps', fn', p') = check_fn h g d m p ps in
        let (fn'', p'') = check_fn_lst t g d m p' ps' in 
        ((fn' :: fn''), p'')

(* Check that there is a void main() defined *)
let check_main_fn (g: gamma) (d: delta) (p: phi) : TypedAst.params =
    debug_print ">> check_main_fn";
    let (fm, params, ret_type, paramet) = Assoc.lookup "main" p in 
    debug_print (">> check_main_fn_2" ^ (string_of_params params) ^ (string_of_parameterization paramet));
    if (Assoc.size paramet) > 0 then raise (TypeException "Cannot provide generic parameters to main") else
    (match fm with | Some _ -> raise (TypeException "Cannot assign function modifications to main") | None -> ());
    match ret_type with
        | UnitTyp -> check_params params g d Assoc.empty paramet Assoc.empty |> tr_fst
        | _ -> raise (TypeException ("Expected main function to return void"))

let check_decls (g: gamma) (d: delta) (m: mu) (dl : extern_decl) (p: phi) (ps: psi) : (gamma * phi)=
    match dl with
    | ExternFn f -> let (_, _, p') = (check_fn_decl g d m f p ps) in (g, p')
    | ExternVar (t, Var x) -> (Assoc.update x t g, p)
    | _ -> raise (TypeException ("Invalid declaration, must be a function or variable"))

(* Returns the list of fn's which represent the program 
 * and params of the void main() fn *)
let check_prog (e: prog) : TypedAst.prog * TypedAst.params =
    debug_print ">> check_prog";
    match e with
    | Prog (dl, t, f) -> (*(d: delta) ((id, t): fn_decl) (p: phi) *)
        (* delta from tag declarations *)
        let (d, m) = check_tags t Assoc.empty Assoc.empty in 
        (* TODO: Why is gamma getting carried over here?  I'm a bit suspicious *)
        let (g, p) = List.fold_left 
            (fun (g', p') (dl': extern_decl) -> check_decls g' d m dl' p' Assoc.empty) 
            (Assoc.empty, Assoc.empty) dl in
        let (e', p') = check_fn_lst f g d m p Assoc.empty in 
        let pr = check_main_fn g d p' in 
        debug_print "===================";
        debug_print "Type Check Complete";
        debug_print "===================\n";
        (e', pr)
