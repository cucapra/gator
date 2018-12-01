open CoreAst
open TagAst
open TagAstPrinter
open Util
open Printf
open Str

exception TypeException of string
exception DimensionException of int * int

(* Variable defs *)
type gamma = typ Assoc.context

(* Tags defs *)
type delta = tag_typ Assoc.context

(* Function defs *)
type phi = fn_type Assoc.context

(* Tag modifications *)
type mu = (tag_mod option) Assoc.context

(* Transformation context *)
(* Effectively has the type 't1->t2->v' for tags t1,t2 and variable name v *)
(* Note that the resulting thing can be a function with a concrete parameterization, hence the typ list *)
type psi = ((string * (typ list)) Assoc.context) Assoc.context

let string_of_psi ps = Assoc.to_string (fun x -> Assoc.to_string (fun (s, t) -> s ^ (string_of_list string_of_typ t)) x) ps

let trans_top (n1: int) (n2: int) : typ =
    TransTyp (TagTyp (BotTyp n1), TagTyp (TopTyp n2))

let trans_bot (n1: int) (n2: int) : typ =
    TransTyp (TagTyp (TopTyp n1), TagTyp (BotTyp n2))

let rec unwrap_abstyp (s: string) (pm : parametrization) : constrain =
    if Assoc.mem s pm 
    then match (Assoc.lookup s pm) with 
        | TypConstraint(AbsTyp s) ->  unwrap_abstyp s pm
        | p -> p
    else raise (TypeException ("AbsTyp " ^ s ^ " not found in parametrization"))

let rec etyp_to_typ (e : TypedAst.etyp) : typ =
    debug_print ">> etyp_to_typ";
    match e with 
    | TypedAst.UnitTyp -> UnitTyp
    | TypedAst.BoolTyp -> BoolTyp
    | TypedAst.IntTyp -> IntTyp
    | TypedAst.FloatTyp -> FloatTyp
    | TypedAst.VecTyp n -> TagTyp(BotTyp n)
    | TypedAst.MatTyp (n1, n2) -> TransTyp(TagTyp(BotTyp n1), TagTyp(BotTyp n2))
    | TypedAst.TransTyp (s1, s2) -> TransTyp(etyp_to_typ s1, etyp_to_typ s2)
    | TypedAst.SamplerTyp n -> SamplerTyp n
    | TypedAst.AbsTyp (s, c) -> AbsTyp s

and constrain_to_constrain (c : TypedAst.constrain) : constrain =
    match c with
    | TypedAst.AnyTyp -> AnyTyp
    | TypedAst.GenTyp -> GenTyp
    | TypedAst.GenMatTyp -> GenMatTyp
    | TypedAst.GenVecTyp -> GenVecTyp
    | TypedAst.ETypConstraint t -> TypConstraint (etyp_to_typ t)

let rec vec_dim (t: tag_typ) (d: delta) (pm : parametrization): int =
    debug_print ">> vec_dim";
    match t with
    | TopTyp n
    | BotTyp n -> n
    | VarTyp s -> begin 
        if (Assoc.mem s d) then vec_dim (Assoc.lookup s d) d pm 
        else raise (TypeException ("Unknown tag " ^ (string_of_tag_typ t))) end

let typ_vec_dim (t: typ) (d: delta) (pm : parametrization): int =
    let fail _ = failwith ("Expected a vector for computing the dimension, got " ^ (string_of_typ t)) in
    match t with
    | TagTyp t -> vec_dim t d pm
    | AbsTyp s -> (match unwrap_abstyp s pm with
        | TypConstraint (TagTyp t) -> vec_dim t d pm
        | _ -> fail ())
    | _ -> fail ()

let rec tag_erase_param (t: typ) (d: delta) (pm: parametrization) : TypedAst.etyp = 
    debug_print ">> tag_erase_param";
    match t with 
    | AbsTyp s -> if Assoc.mem s pm then 
        let p = (Assoc.lookup s pm) in 
        TypedAst.AbsTyp (s, constrain_erase p d pm)
        else raise (TypeException ("AbsTyp " ^ s ^ " was not found in function parametrization definition"))
    | _ -> tag_erase t d pm 

and tag_erase (t : typ) (d : delta) (pm: parametrization) : TypedAst.etyp =
    debug_print ">> tag_erase";
    match t with
    | UnitTyp -> TypedAst.UnitTyp
    | BoolTyp -> TypedAst.BoolTyp
    | IntTyp -> TypedAst.IntTyp
    | FloatTyp -> TypedAst.FloatTyp
    | TagTyp tag -> 
        begin
            match tag with
            | TopTyp n
            | BotTyp n -> TypedAst.VecTyp n
            | VarTyp _ -> TypedAst.VecTyp (vec_dim tag d pm)
        end
    | TransTyp (TagTyp s1, TagTyp s2) -> TypedAst.MatTyp ((vec_dim s2 d pm), (vec_dim s1 d pm))
    | TransTyp (s1, s2) -> 
        begin
            match (s1, s2) with
            | (AbsTyp _, TagTyp _)
            | (TagTyp _, AbsTyp _)
            | (AbsTyp _, AbsTyp _) -> TypedAst.TransTyp (tag_erase s1 d pm, tag_erase s2 d pm)
            | _ -> raise (TypeException "Function type must be from vec to vec")
        end
    | SamplerTyp i -> TypedAst.SamplerTyp i
    | AbsTyp s -> tag_erase_param t d pm 
    | AutoTyp -> raise (TypeException "Illegal use of auto (cannot use auto as part of a function call)")

and constrain_erase (c: constrain) (d : delta) (pm : parametrization) : TypedAst.constrain =
    debug_print ">> constrain_erase";
    match c with
    | AnyTyp -> TypedAst.AnyTyp
    | GenTyp -> TypedAst.GenTyp
    | GenMatTyp -> TypedAst.GenMatTyp
    | GenVecTyp -> TypedAst.GenVecTyp
    | TypConstraint t -> TypedAst.ETypConstraint (tag_erase t d pm)

(* TODO: This is correct, but ineffecient.  Just do stepwise as with abstype *)
let rec get_ancestor_list (t: tag_typ) (d: delta) : id list =
    debug_print ">> get_ancestor_list";
    match t with 
    | TopTyp _ -> []
    | BotTyp _ -> raise (TypeException "Bad failure -- Ancestor list somehow includes the bottom type")
    | VarTyp s -> s :: (get_ancestor_list (Assoc.lookup s d) d)

let is_tag_subtype_with_strictness (to_check: tag_typ) (target: tag_typ) (d: delta) (pm: parametrization) (strict : bool) : bool =
    debug_print ">> is_tag_subtype";
    match (to_check, target) with
    | BotTyp n1, BotTyp n2
    | BotTyp n1, TopTyp n2
    | TopTyp n1, TopTyp n2 -> n1 = n2
    | TopTyp _, _ -> false
    | BotTyp n, VarTyp s -> n = (vec_dim target d pm)
    | VarTyp _, BotTyp _ -> false
    | VarTyp _, VarTyp s2 -> List.mem s2 (get_ancestor_list to_check d)
    | VarTyp s, TopTyp n -> (vec_dim to_check d pm) = n && not strict
    (* | VarTyp s, TopTyp n -> n = (vec_dim to_check d pm) *)

let is_tag_subtype (to_check: tag_typ) (target: tag_typ) (d: delta) (pm: parametrization) : bool =
    is_tag_subtype_with_strictness to_check target d pm true

let rec is_subtype_with_strictness (to_check : typ) (target : typ) (d : delta) (pm: parametrization) (strict : bool) : bool =
    debug_print (">> is_subtype" ^ (string_of_typ to_check) ^ ", " ^(string_of_typ target));
    let abstyp_step (s: string) : bool =
        if Assoc.mem s pm 
        then match (Assoc.lookup s pm) with
        | TypConstraint t -> is_subtype_with_strictness t target d pm strict
        | _ -> false
        else raise (TypeException ("AbsTyp " ^ s ^ " not found in parametrization"))
    in
    match (to_check, target) with 
    | (TagTyp t1, TagTyp t2) -> is_tag_subtype_with_strictness t1 t2 d pm strict (* MARK *)
    | (SamplerTyp i1, SamplerTyp i2) -> i1 = i2 
    | (BoolTyp, BoolTyp)
    | (IntTyp, IntTyp)
    | (FloatTyp, FloatTyp) -> true
    | (TransTyp (t1, t2), TransTyp (t3, t4)) -> 
        (is_subtype_with_strictness t3 t1 d pm false && is_subtype_with_strictness t2 t4 d pm strict)
    | (AbsTyp s1, AbsTyp s2) -> s1 = s2 || abstyp_step s1
    | (AbsTyp s, _) -> abstyp_step s
    (* Necessary because we have a lattice and the bottype is less than EVERYTHING in that lattice *)
    | (TagTyp (BotTyp n), AbsTyp _) -> (is_subtype_with_strictness target (TagTyp (TopTyp n)) d pm false)
    | _ -> false

and is_subtype (to_check : typ) (target : typ) (d : delta) (pm: parametrization) : bool =
    is_subtype_with_strictness to_check target d pm true

let rec is_sub_constraint (to_check : constrain) (target : constrain) (d : delta) (pm: parametrization): bool =
    match (to_check, target) with
    | _, AnyTyp -> true
    | AnyTyp, _ -> false
    | (TypConstraint t1, TypConstraint t2) -> is_subtype_with_strictness t1 t2 d pm false
    | (TypConstraint (AbsTyp s), _) -> is_sub_constraint (unwrap_abstyp s pm) target d pm
    | (_, TypConstraint _) -> false
    | (TypConstraint (BoolTyp), GenTyp) -> false
    | (_, GenTyp) -> true
    | (GenVecTyp, GenVecTyp)
    | (TypConstraint (TagTyp _), GenVecTyp) -> true
    | (_, GenVecTyp) -> false
    | (GenMatTyp, GenMatTyp)
    | (TypConstraint (TransTyp _), GenMatTyp) -> true
    | (_, GenMatTyp) -> false

let rec is_bounded_by (t: typ) (c: constrain) (d: delta) (pm: parametrization): bool =
    is_sub_constraint (TypConstraint t) c d pm

let as_vector (t: typ) (d: delta) (pm: parametrization) : tag_typ =
    (* Checks whether the given abstract type is a vector *)
    let fail _ = failwith ("Cannot treat " ^ (string_of_typ t) ^ " as a vector") in
    match t with
    | TagTyp t' -> t' 
    | AbsTyp s -> (match unwrap_abstyp s pm with
        | TypConstraint (TagTyp t') -> t'
        | _ -> fail ())
    | _ -> fail ()

let as_matrix_pair (t: typ) (d: delta) (pm: parametrization) : typ * typ =
    (* Checks whether the given abstract type is a vector *)
    let fail _ = failwith ("Cannot treat " ^ (string_of_typ t) ^ " as a matrix") in
    match t with
    | TransTyp (t1, t2) -> (t1, t2)
    | AbsTyp s -> (match unwrap_abstyp s pm with
        | TypConstraint (TransTyp (t1, t2)) -> (t1, t2)
        | _ -> fail ())
    | _ -> fail ()

let least_common_vector_parent (t1: tag_typ) (t2: tag_typ) (d: delta) (pm: parametrization) (strict: bool): tag_typ =
    debug_print ">> least_common_vector_parent";
    let check_dim (n1: int) (n2: int) : unit =
        if n1 = n2 then () else (raise (DimensionException (n1, n2)))
    in
    let rec lub (anc_list1: id list) (anc_list2: id list) : id option =
        match anc_list1 with
        | [] -> None
        | h::t -> 
            (try Some (List.find (fun x -> x=h) anc_list2) with Not_found -> lub t anc_list2)
    in
    match (t1, t2) with
    | BotTyp n1, BotTyp n2 ->
        check_dim n1 n2; BotTyp n1
    | BotTyp n1, TopTyp n2
    | TopTyp n1, BotTyp n2
    | TopTyp n1, TopTyp n2 ->
        check_dim n1 n2; TopTyp n1
    | VarTyp s, TopTyp n1
    | TopTyp n1, VarTyp s ->
        check_dim (vec_dim (VarTyp s) d pm) n1;
        if strict then
        raise (TypeException ("Cannot implicitly cast " ^ (string_of_tag_typ t1) ^ " and " ^ (string_of_tag_typ t2) ^ " to the top vector type"  ))
        else TopTyp n1
    | VarTyp s, BotTyp n1
    | BotTyp n1, VarTyp s ->
        check_dim (vec_dim (VarTyp s) d pm) n1; VarTyp s
    | VarTyp s1, VarTyp s2 ->
        check_dim (vec_dim (VarTyp s1) d pm) (vec_dim (VarTyp s2) d pm);
        (if s1 = s2 then VarTyp s1
        else (match lub (get_ancestor_list t1 d) (get_ancestor_list t2 d) with
        | None -> if strict 
            then raise (TypeException ("Cannot implicitly cast " ^ (string_of_tag_typ t1) ^ " and " ^ (string_of_tag_typ t2) ^ " to the top vector type"))
            else TopTyp (vec_dim (VarTyp s1) d pm)
        | Some t -> VarTyp t))

let greatest_common_vector_child (t1: tag_typ) (t2: tag_typ) (d: delta) (pm: parametrization) : tag_typ =
    debug_print "greatest_common_child";
    let check_dim (n1: int) (n2: int) : unit =
        if n1 = n2 then () else (raise (DimensionException (n1, n2)))
    in
    match (t1, t2) with
    | BotTyp n1, BotTyp n2
    | BotTyp n1, TopTyp n2
    | TopTyp n1, BotTyp n2 ->
        check_dim n1 n2; BotTyp n1                                                                                                                                                                                                                                                                                                                                                                                                                              
    | TopTyp n1, TopTyp n2 ->
        check_dim n1 n2; TopTyp n1
    | VarTyp s, TopTyp n1
    | TopTyp n1, VarTyp s ->
        check_dim (vec_dim (VarTyp s) d pm) n1; VarTyp s
    | VarTyp s, BotTyp n1
    | BotTyp n1, VarTyp s ->
        check_dim (vec_dim (VarTyp s) d pm) n1; BotTyp n1
    | VarTyp s1, VarTyp s2 ->
        begin
            let bot_dim = vec_dim (VarTyp s1) d pm in
            check_dim bot_dim (vec_dim (VarTyp s2) d pm);
            (* This works since each tag can only have one parent *)
            (if is_tag_subtype t1 t2 d pm then t1
            else if is_tag_subtype t2 t1 d pm then t2
            else BotTyp bot_dim)
        end

let abstype_as_vec (s: string) (pm: parametrization) : tag_typ =
    match unwrap_abstyp s pm with
    | TypConstraint (TagTyp t') -> t'
    | _ -> failwith "is_vector failed!!"

let rec greatest_common_child (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ =
    if (is_subtype t1 t2 d pm) then t1 else if (is_subtype t2 t1 d pm ) then t2 
    else match (t1, t2) with
    | (TagTyp t1', TagTyp t2') -> (TagTyp (greatest_common_vector_child t1' t2' d pm))
    | _ -> raise (TypeException ("Cannot unify " ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2)))

let rec least_common_parent_with_strictness (t1: typ) (t2: typ) (d: delta) (pm: parametrization) (strict: bool): typ =
    debug_print ">> least_common_parent";
    let fail _ = raise (TypeException ("Cannot unify " ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2))) in
    let rec step_abstyp s1 s2 =
        begin
            match (Assoc.lookup s2 pm) with
            | TypConstraint (AbsTyp s') -> if (is_subtype t1 (AbsTyp s') d pm) then (AbsTyp s') else step_abstyp s1 s'
            | TypConstraint (t) -> least_common_parent_with_strictness t t2 d pm strict
            | c -> fail ()
        end
    in
    if (is_subtype t1 t2 d pm) then t2 else if (is_subtype t2 t1 d pm ) then t1 
    else match (t1, t2) with
        | (TagTyp t1', TagTyp t2') -> (TagTyp (least_common_vector_parent t1' t2' d pm strict))
        | (AbsTyp s1, AbsTyp s2) -> (step_abstyp s1 s2)
        | (AbsTyp s, TagTyp t) 
        | (TagTyp t, AbsTyp s) -> (TagTyp(least_common_vector_parent (abstype_as_vec s pm) t d pm strict))
        | (TransTyp (t1, t2), TransTyp(t3, t4)) -> 
            (TransTyp (greatest_common_child t1 t3 d pm, least_common_parent_with_strictness t2 t4 d pm strict))
        (* Note that every other possible pair of legal joins would be caught by the is_subtype calls above *)
        | _ -> fail ()

let least_common_parent (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ =
    least_common_parent_with_strictness t1 t2 d pm true

let least_common_parent_safe (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ option =
    try Some (least_common_parent t1 t2 d pm) with
    | TypeException t -> None
    | DimensionException _ -> None
    | t -> raise t

let check_subtype_list (t: typ) (l: typ list) (d: delta) (pm: parametrization) : bool =
    List.fold_left (fun acc t' -> acc || (is_subtype t t' d pm)) false l

let check_bounds_list (t: typ) (l: constrain list) (d: delta) (pm: parametrization) : bool =
    List.fold_left (fun acc t' -> acc || (is_bounded_by t t' d pm)) false l
    

let rec check_typ_valid (t: typ) (d: delta) (pm: parametrization) : unit =
    match t with
    | TagTyp (VarTyp s) -> if Assoc.mem s d then () else raise (TypeException ("Unknown tag " ^ s))
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
    | VecLit v -> TagTyp (BotTyp (List.length v))
    | MatLit m ->
        (let rows = List.length m in
        if rows = 0 then trans_bot 0 0 else
        let cols = List.length (List.hd m) in
        if List.for_all (fun v -> List.length v = cols) m then trans_bot cols rows
        else (raise (TypeException ("Matrix must have the same number of elements in each row"))))
    | _ -> raise (TypeException ("Unexpected typechecker value " ^ (string_of_value v)))

let check_tag_typ (tag: tag_typ) (d: delta) : unit =
    debug_print ">> check_tag_typ";
    match tag with
    | TopTyp n
    | BotTyp n -> (if (n > 0) then ()
        else raise (TypeException "Cannot declare a type with dimension less than 0"))
    | VarTyp s -> (if Assoc.mem s d then ()
        else raise (TypeException ("Undeclared tag" ^ s)))

let rec check_typ_exp (t: typ) (d: delta) : unit =
    debug_print ">> check_typ";
    match t with
    | AutoTyp -> raise (TypeException "Cannot use type auto as a tag type")
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp 
    | SamplerTyp _ -> ()
    | TagTyp s -> check_tag_typ s d; ()
    | TransTyp (t1, t2) -> check_typ_exp t1 d; check_typ_exp t2 d; ()
    | AbsTyp s -> raise  (TypeException "Cannot use a generic type as a tag argument yet")

(* "scalar linear exp", (i.e. ctimes) returns generalized MatTyp *)
let check_ctimes_exp (t1: typ) (t2: typ) (d: delta) (pm : parametrization): typ = 
    debug_print ">> check_scalar_linear_exp";
    (* TODO correctness, doesn't match rn *)
    match (t1, t2) with 
    | TransTyp (TagTyp m1, TagTyp m2), TransTyp (TagTyp m3, TagTyp m4) ->
        let left = (vec_dim m1 d pm) in
        let right = (vec_dim m2 d pm) in
        if left = (vec_dim m3 d pm) && right = (vec_dim m4 d pm)
        then trans_top left right
        else (raise (TypeException "Dimension mismatch in ctimes operator"))
    | TagTyp l, TagTyp r -> (
        check_tag_typ l d; check_tag_typ r d;
        let ldim = vec_dim l d pm in
        let rdim = vec_dim r d pm in 
        if ldim = rdim 
        then TagTyp (TopTyp (vec_dim l d pm))
        else (raise (TypeException "Dimension mismatch in ctimes operator"))
    )
    | _ -> (raise (TypeException ("Expected linear types for ctimes operator, found: "^(string_of_typ t1)^", "^(string_of_typ t2))))

(* Type check binary bool operators (i.e. &&, ||) *)
let check_bool_binop (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ = 
    debug_print ">> check_bool_binop";
    if is_subtype t1 BoolTyp d pm then least_common_parent t1 t2 d pm
    else raise (TypeException "Expected boolean expression for binop")

(* Type check unary number operators (i.e. -) *)
let check_num_unop (t: typ) (d: delta) (pm: parametrization) : typ =
    debug_print ">> check_num_unop";
    if is_bounded_by t GenTyp d pm then t
    else raise (TypeException "Expected integer, float, vector, or matrix expression")

(* Type check unary bool operators (i.e. !) *)
let check_bool_unop (t: typ) (d: delta) (pm: parametrization) : typ =
    debug_print ">> check_bool_unop";
    if is_subtype t BoolTyp d pm then t
    else raise (TypeException "Expected boolean expression for boolean operator")

(* Type check unary bool operators (i.e. !) *)
let check_swizzle (s : id) (t: typ) (d: delta) (pm: parametrization) : typ =
    debug_print ">> check_swizzle";
    let check_reg = if Str.string_match (Str.regexp "[xyzwrgbastpq]+") s 0 
        then if String.length s == 1 then FloatTyp else TagTyp (TopTyp (String.length s))
        else raise (TypeException ("Invalid characters used for swizzling in " ^ s)) in
    if is_bounded_by t GenVecTyp d pm then check_reg
    else raise (TypeException "Expected vector for swizzling")

(* Type check equality (==) *)
(* Only bool, int, float are comparable *)
let check_equality_exp (t1: typ) (t2: typ) (d: delta) (pm: parametrization) : typ = 
    debug_print ">> check_comp_binop";
    let subtype_list = [BoolTyp; IntTyp; FloatTyp] in
    if check_subtype_list t1 subtype_list d pm
    then (least_common_parent t1 t2 d pm |> ignore; BoolTyp)
    else raise (TypeException "Equality checks must be between booleans, integers, or floats")

(* Type check comparative binary operators (i.e. <. <=) *)
(* Only int and float are comparable *)
let check_comp_binop (t1: typ) (t2: typ) (d: delta) (pm: parametrization) : typ = 
    debug_print ">> check_comp_binop";
    let subtype_list = [IntTyp; FloatTyp] in
    if check_subtype_list t1 subtype_list d pm
    then (least_common_parent t1 t2 d pm |> ignore; BoolTyp)
    else raise (TypeException "Comparison checks must be between integers or floats")

let check_as_exp (t1: typ) (t2: typ) (d: delta) (pm: parametrization) : typ =
    least_common_parent_with_strictness t1 t2 d pm false |> ignore; t2

(* Type checking addition operations on scalar (int, float) expressions *)
(* Types are closed under addition and scalar multiplication *)
let check_addition_exp (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ =
    debug_print ">> check_addition";
    if is_bounded_by t1 GenTyp d pm then least_common_parent t1 t2 d pm
    else raise (TypeException ("Invalid expressions for addition: "
    ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2)))

(* Type checking times operator - on scalar mult & matrix transformations *)
let check_times_exp (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ = 
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
let check_division_exp (t1: typ) (t2: typ) (d: delta) (pm: parametrization) : typ =
    debug_print ">> check_division";
    let subtype_list = [IntTyp; FloatTyp] in
    if check_subtype_list t1 subtype_list d pm
        then least_common_parent t1 t2 d pm
    else if check_subtype_list t2 subtype_list d pm && is_bounded_by t1 GenVecTyp d pm
        then t1
    else raise (TypeException ("Invalid expressions for division: "
    ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2)))

let check_index_exp (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ =
    debug_print ">> check_index_exp";
    let fail _ = raise (TypeException ("Invalid expressions for indexing: "
    ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2))) in
    if is_subtype t2 IntTyp d pm then
        if is_bounded_by t1 GenVecTyp d pm then FloatTyp
        else if is_bounded_by t1 GenMatTyp d pm then
            TagTyp (TopTyp (typ_vec_dim (fst (as_matrix_pair t1 d pm)) d pm))
        else fail () 
    else fail ()

let check_parametrization (d: delta) (pm: parametrization) : unit =
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

let update_psi (v: string * (typ list)) (start: string) (target: string) (m: mu) (ps: psi) : psi =
    (* Update psi, raising errors in case of a duplicate *)
    (* If the given type is not valid in psi, psi is returned unmodified *)
    let psi_update (s1: string) (s2: string) : psi =
        if Assoc.mem s1 ps then 
        (let s1_cont = Assoc.lookup s1 ps in
            if Assoc.mem s2 s1_cont 
            then raise (TypeException ("Duplicate transformation for " ^ start ^ "->" ^ target ^ " in the declaration of " ^ (fst v)))
            else Assoc.update s1 (Assoc.update s2 v s1_cont) ps
        )
        else Assoc.update s1 (Assoc.update s2 v Assoc.empty) ps
    in
    let are_coord (s1: string) (s2: string) : bool =
        if Assoc.mem s1 m && Assoc.mem s2 m
        then (match (Assoc.lookup s1 m, Assoc.lookup s2 m) with
        | (Some Coord, Some Coord) -> true
        | _ -> false)
        else false
    in
    if are_coord start target then psi_update start target else ps

let update_psi_matrix (v: string) (t: typ) (m: mu) (ps: psi) : psi =
    match t with
    | TransTyp ((TagTyp (VarTyp s1)), (TagTyp (VarTyp s2))) ->
        update_psi (v, []) s1 s2 m ps
    | _ -> ps

(* Type check parameter; make sure there are no name-shadowed parameter names *)
(* TODO : parametrized types *)
let check_param ((id, t): (string * typ)) (g: gamma) (d: delta) (m: mu) 
    (pm : parametrization) (ps: psi) : gamma * psi = 
    debug_print ">> check_param";
    if Assoc.mem id g 
    then raise (TypeException ("Duplicate parameter name in function declaration: " ^ id))
    else check_typ_valid t d pm; (Assoc.update id t g, update_psi_matrix id t m ps)
    
(* Get list of parameters from param list *)
let check_params (pl : (string * typ) list) (g: gamma) (d : delta) (m: mu) 
(pm : parametrization) (ps: psi) : TypedAst.params * gamma * psi = 
    debug_print ">> check_params";
    let (g', ps') = List.fold_left (fun (g', ps') p -> check_param p g' d m pm ps') (g, ps) pl in 
    let p = (List.map (fun (i, t) -> (i, tag_erase t d pm)) pl) in 
    (p, g', ps')
    
let exp_to_texp (checked_exp : TypedAst.exp * typ) (d : delta) (pm : parametrization) : TypedAst.texp = 
    debug_print ">> exp_to_texp";
    ((fst checked_exp), (tag_erase (snd checked_exp) d pm))

(* Super expensive.  We're essentially relying on small contexts *)
let check_in_exp (start_exp: exp) (start_typ: typ) (target: typ) (g: gamma) (d: delta) 
(pm: parametrization) (p: phi) (ps: psi) : exp = 
    let rec psi_path_rec (target: string) (to_search: (string * exp) Queue.t) (found: string list) : exp =
        let rec psi_lookup_rec (next: string) : ((string * (typ list)) Assoc.context) option =
            if Assoc.mem next ps then Some (Assoc.lookup next ps)
            else match Assoc.lookup next d with
            | VarTyp s -> psi_lookup_rec s
            | _ -> None
        in
        let rec update_search_and_found (vals: (string * (string * (typ list))) list) (e: exp) : string list =
            match vals with
            | [] -> found
            | (s, (v, pml))::t -> if List.mem s found then update_search_and_found t e 
                else 
                let e' = 
                    if Assoc.mem v g then (Binop (Times, Var v, e))
                    else if Assoc.mem v p then (FnInv (v, [e], pml))
                    else failwith ("Typechecker error: unknown value " ^ v ^ " loaded into psi") in
                (* Note the update to the stateful queue *)
                (Queue.push (s, e') to_search;  s :: update_search_and_found t e)
        in
        let (next, e) = if Queue.is_empty to_search 
            then (raise (TypeException ("Cannot find a path from " ^ (List.hd (List.rev found)) ^ " to " ^ target)))
            else Queue.pop to_search 
        in 
        (* We use the 'with_strictness' version to avoid throwing an exception *)
        if is_subtype_with_strictness (TagTyp (VarTyp next)) (TagTyp (VarTyp target)) d pm false then e
        else (match psi_lookup_rec next with 
            | None -> psi_path_rec target to_search found
            | Some next_cont -> psi_path_rec target to_search (update_search_and_found (Assoc.bindings next_cont) e))
    in
    match (start_typ, target) with
    | (TagTyp (VarTyp s1), TagTyp (VarTyp s2)) -> 
        if not (Assoc.mem s1 d) then
        raise (TypeException ("Unknown tag " ^ (string_of_typ start_typ)))
        else if not (Assoc.mem s2 d) then
        raise (TypeException ("Unknown tag " ^ (string_of_typ target)))
        else if s1 = s2 then start_exp else
        let q = Queue.create () in Queue.push (s1, start_exp) q;
        psi_path_rec s2 q [s1]
    | _ -> raise (TypeException 
    ("Invalid in expression between " ^ (string_of_typ start_typ) ^ " and " ^ (string_of_typ target)))

let rec check_exp (e : exp) (d : delta) (g : gamma) (pm : parametrization) (p : phi) (ps: psi) : TypedAst.exp * typ = 
    debug_print ">> check_exp";
    let build_unop (op : unop) (e': exp) (check_fun: typ->delta->parametrization->typ) (pm: parametrization)
        : TypedAst.exp * typ =
        let result = check_exp e' d g pm p ps in
            (TypedAst.Unop(op, exp_to_texp result d pm), check_fun (snd result) d pm)
    in
    let build_binop (op : binop) (e1: exp) (e2: exp) (check_fun: typ->typ->delta->parametrization->typ) (pm: parametrization)
        : TypedAst.exp * typ =
        let e1r = check_exp e1 d g pm p ps in
        let e2r = check_exp e2 d g pm p ps in
            (TypedAst.Binop(op, exp_to_texp e1r d pm, exp_to_texp e2r d pm), check_fun (snd e1r) (snd e2r) d pm)
    in 
    let req_parametrizations f =
        fun a b c d -> f a b c d
    in
    let req_parametrizations2 f =
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
        | Neg -> build_unop op e' (req_parametrizations2 check_num_unop) pm
        | Not -> build_unop op e' (req_parametrizations2 check_bool_unop) pm
        | Swizzle s -> build_unop op e' (check_swizzle s) pm)
    | Binop (op, e1, e2) -> (match op with
        | Eq -> build_binop op e1 e2 (req_parametrizations check_equality_exp) pm
        | Leq -> build_binop op e1 e2 (req_parametrizations check_comp_binop) pm
        | Lt -> build_binop op e1 e2 (req_parametrizations check_comp_binop) pm
        | Geq -> build_binop op e1 e2 (req_parametrizations check_comp_binop) pm
        | Gt -> build_binop op e1 e2 (req_parametrizations check_comp_binop) pm
        | Or | And -> build_binop op e1 e2 check_bool_binop pm
        | Plus | Minus -> build_binop op e1 e2 check_addition_exp pm
        | Times -> build_binop op e1 e2 check_times_exp pm
        | Div  -> build_binop op e1 e2 (req_parametrizations check_division_exp) pm
        | CTimes -> build_binop op e1 e2 check_ctimes_exp pm
        | Index -> build_binop op e1 e2 check_index_exp pm
    )
    | FnInv (i, args, pr) -> let ((i, args_exp), rt) = check_fn_inv d g p args i pr pm ps in 
        (FnInv (i, args_exp), rt)
        
and check_arr (d : delta) (g : gamma) (p : phi) (a : exp list) (pm : parametrization) (ps: psi) : (TypedAst.exp * typ) =
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
    if is_vec checked_a then (TypedAst.Arr checked_a, TagTyp (BotTyp length_a)) else 
    (match is_mat checked_a with
    | Some n -> (TypedAst.Arr checked_a, trans_bot n length_a)
    | None ->  raise (TypeException ("Invalid array definition for " ^ (string_of_exp (Arr a)) ^ ", must be a matrix or vector")))


and check_fn_inv (d : delta) (g : gamma) (p : phi) (args : args) (i : string) (pml: typ list) (pm : parametrization) (ps: psi)
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
                    | _ -> fpm
                in
                let gen_pml (inferred : (typ Assoc.context) option) : (typ list) option =
                    let rec reduce_typ (t : typ) (fpm : typ Assoc.context) : typ =
                        match t with
                        (* Don't have to check if t is valid *)
                        (* already handled by the typechecker when checking that the function declaration parameter dependencies were valid *)
                        | AbsTyp s -> Assoc.lookup s fpm
                        | TransTyp (l, r) -> TransTyp(reduce_typ l fpm, reduce_typ r fpm)
                        | _ -> t
                    in
                    (* Correctly orders the resulting pml to match the parameters of the function parametrization list *)
                    match inferred with | None -> None | Some tc -> 
                    option_map (fun c -> List.rev (List.map snd (Assoc.bindings c))) (List.fold_left 
                        (fun acc (s, c) -> match acc with | None -> None | Some a -> 
                        if Assoc.mem s tc then Some (Assoc.update s (Assoc.lookup s tc) a) else
                        (match c with | TypConstraint t' -> Some (Assoc.update s (reduce_typ t' a) a) | _ -> None) )
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
                | _ -> t
            in
            match c with
            | TypConstraint t -> TypConstraint (in_function_t t)
            | _ -> c
        in
        (* Check that the parametrization conforms to the bounds provided *)
        let param_check = List.fold_left2 (fun acc given_pm (s, c) -> 
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
    | Some l -> ((i, args_exp), match rt with
        | AbsTyp rt' -> Assoc.lookup rt' l
        | _ -> rt)
    | None -> raise (TypeException ("No overloaded function declaration of " ^ i
    ^ (if List.length pml > 0 then "<" ^ (String.concat "," (List.map string_of_typ pml)) ^ ">" else "")
    ^ " matching types (" ^ (String.concat "," (List.map string_of_typ args_typ)) ^ ") found"))) 

and check_comm (c: comm) (d: delta) (m: mu) (g: gamma) (pm: parametrization) (p: phi) (ps: psi) : TypedAst.comm * gamma * psi = 
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
                | TagTyp (BotTyp _) -> raise (TypeException "Cannot infer the type of a vector literal")
                | TransTyp (TagTyp TopTyp _, TagTyp BotTyp _) -> raise (TypeException "Cannot infer the type of a matrix literal")
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

and check_comm_lst (cl : comm list) (d: delta) (m: mu) (g: gamma) (pm : parametrization) (p: phi) (ps: psi) : TypedAst.comm list * gamma * psi = 
    debug_print ">> check_comm_lst";
    match cl with
    | [] -> ([], g, ps)
    | h::t -> let (c', g', ps') = check_comm h d m g pm p ps in
        let (cl', g'', ps'') = check_comm_lst t d m g' pm p ps' in 
        (c' :: cl', g'', ps'')

and check_assign (t: typ) (s: string) (etyp : typ)  (d: delta) (g: gamma) (p: phi) (pm: parametrization): gamma =
    debug_print (">> check_assign <<"^s^">>");
    (* Check that t, if not a core type, is a registered tag *)
    begin
    match t with
    | TransTyp (TagTyp VarTyp t1, TagTyp VarTyp t2) -> if not (Assoc.mem t1 d)
        then raise (TypeException ("Unknown tag " ^ t2))
        else if not (Assoc.mem t2 d) then raise (TypeException ("unknown tag " ^ t1))
    | TagTyp (VarTyp t')
    | TransTyp (TagTyp VarTyp t', _)
    | TransTyp (_, TagTyp VarTyp t') ->
        if not (Assoc.mem t' d) then raise (TypeException ("unknown tag " ^ t'))
    | _ -> ()
    end;
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

let check_tag (s: string) (tm : tag_mod option) (l: tag_typ) (d: delta) (m: mu) : delta * mu = 
    debug_print ">> check_tag";
    let rec check_coord (t : tag_typ) : unit =
        match t with
        | VarTyp t' -> (match Assoc.lookup t' m with 
            | None -> check_coord (Assoc.lookup t' d)
            | Some Coord -> raise (TypeException "Cannot declare a coord as a subtype of another coord"))
        | _ -> ()
    in
    if Assoc.mem s d then raise (TypeException "Cannot redeclare tag")
    else (match tm with | Some Coord -> check_coord l | _ -> ());
    (Assoc.update s l d, Assoc.update s tm m)

let rec check_tags (t: tag_decl list) (d: delta) (m: mu): delta * mu =
    debug_print ">> check_tags";
    match t with 
    | [] -> (d, m)
    (* TODO: add a context or update delta to lookup tag modifications *)
    | (tm, s, a)::t ->
        check_typ_exp a |> ignore;
        match a with 
        | (TagTyp l) -> let (d', m') = check_tag s tm l d m in (
            match l with 
            | VarTyp s' -> (
                if Assoc.mem s' d then check_tags t d' m'
                else raise (TypeException ("Tag undefined " ^ s'))
            )
            | _ -> check_tags t d' m'
        )
        | _ -> raise (TypeException "Expected linear type for tag declaration")

let check_fn_decl (g: gamma) (d: delta) (m: mu) ((id, (fm, pl, rt, pm)): fn_decl) (p: phi) (ps: psi) : (TypedAst.params * gamma * psi) * TypedAst.parametrization * phi =
    debug_print (">> check_fn_decl : " ^ id);
    check_parametrization d pm;
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

let check_return (t: typ) (d: delta) (g: gamma) (pm: parametrization) (p: phi) (ps: psi) (c: comm) = 
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
                let tag' = TagTyp (VarTyp tag) in
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
    | (Some Canon, [(_, t)]) -> 
    begin
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
    end
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
let check_main_fn (g: gamma) (d: delta) (p: phi) =
    debug_print ">> check_main_fn";
    let (fm, params, ret_type, paramet) = Assoc.lookup "main" p in 
    debug_print (">> check_main_fn_2" ^ (string_of_params params) ^ (string_of_parametrization paramet));
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
