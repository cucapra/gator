open CoreAst
open TagAst
open TagAstPrinter
open Util
open Printf
open Str

exception TypeException of string
exception DimensionException of int * int
exception AbstractTypeException

(* Variable defs *)
type gamma = (typ) Assoc.context

(* Tags defs *)
type delta = (tag_typ) Assoc.context

(* Function defs *)
type phi = (fn_type) Assoc.context

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
    | VarTyp s, TopTyp n -> not strict (* Cannot upcast a variable to the toptyp *)
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

let least_common_vector_parent (t1: tag_typ) (t2: tag_typ) (d: delta) (pm: parametrization): tag_typ =
    debug_print ">> least_common_vector_parent";
    let check_dim (n1: int) (n2: int) : unit =
        if n1 = n2 then () else (raise (DimensionException (n1, n2)))
    in
    let rec lub (anc_list1: id list) (anc_list2: id list) : id =
        match anc_list1 with
        | [] -> raise (TypeException ("Cannot implicitly cast " ^ (string_of_tag_typ t1) ^ " and " ^ (string_of_tag_typ t2) ^ " to the top vector type"  ))
        | h::t -> 
            (try (List.find (fun x -> x=h) anc_list2) with Not_found -> lub t anc_list2)
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
        raise (TypeException ("Cannot implicitly cast " ^ (string_of_tag_typ t1) ^ " and " ^ (string_of_tag_typ t2) ^ " to the top vector type"  ))
    | VarTyp s, BotTyp n1
    | BotTyp n1, VarTyp s ->
        check_dim (vec_dim (VarTyp s) d pm) n1; VarTyp s
    | VarTyp s1, VarTyp s2 ->
        check_dim (vec_dim (VarTyp s1) d pm) (vec_dim (VarTyp s2) d pm);
        (if s1 = s2 then VarTyp s1
        else VarTyp (lub (get_ancestor_list t1 d) (get_ancestor_list t2 d)))

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

let rec least_common_parent (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ =
    debug_print ">> least_common_parent";
    let fail _ = raise (TypeException ("Cannot unify " ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2))) in
    let rec step_abstyp s1 s2 =
        begin
            match (Assoc.lookup s2 pm) with
            | TypConstraint (AbsTyp s') -> if (is_subtype t1 (AbsTyp s') d pm) then (AbsTyp s') else step_abstyp s1 s'
            | TypConstraint (t) -> least_common_parent t t2 d pm
            | c -> fail ()
        end
    in
    if (is_subtype t1 t2 d pm) then t2 else if (is_subtype t2 t1 d pm ) then t1 
    else match (t1, t2) with
        | (TagTyp t1', TagTyp t2') -> (TagTyp (least_common_vector_parent t1' t2' d pm))
        | (AbsTyp s1, AbsTyp s2) -> (step_abstyp s1 s2)
        | (AbsTyp s, TagTyp t) 
        | (TagTyp t, AbsTyp s) -> (TagTyp(least_common_vector_parent (abstype_as_vec s pm) t d pm))
        | (TransTyp (t1, t2), TransTyp(t3, t4)) -> 
            (TransTyp (greatest_common_child t1 t3 d pm, least_common_parent t2 t4 d pm))
        (* Note that every other possible pair of legal joins would be caught by the is_subtype calls above *)
        | _ -> fail ()

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

let check_typ_exp (t: typ) (d: delta) : unit =
    debug_print ">> check_typ";
    match t with
    | AutoTyp -> raise (TypeException "Cannot use type auto as a function argument")
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp 
    | SamplerTyp _ -> ()
    | TagTyp s -> check_tag_typ s d; ()
    | TransTyp (TagTyp s1, TagTyp s2) -> check_tag_typ s1 d; check_tag_typ s2 d; ()
    | _ -> failwith "Check_typ_exp Unimplemented"

(* "scalar linear exp", (i.e. ctimes) returns generalized MatTyp *)
let check_ctimes_exp (t1: typ) (t2: typ) (d: delta) (pm : parametrization): typ = 
    debug_print ">> check_scalar_linear_exp";
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

(* Type check parameter; make sure there are no name-shadowed parameter names *)
(* TODO : parametrized types *)
let check_param ((id, t, t'): (string * typ * constrain)) (g: gamma) (d: delta) (pm : parametrization) : gamma = 
    debug_print ">> check_param";
    if Assoc.mem id g 
    then raise (TypeException ("Duplicate parameter name in function declaration: " ^ id))
    else check_typ_valid t d pm; Assoc.update id t g
    
(* Get list of parameters from param list *)
let check_params (pl : (string * typ * constrain) list) (g: gamma) (d : delta) (pm : parametrization) 
: TypedAst.params * gamma = 
    debug_print ">> check_params";
    let g' = List.fold_left (fun (g: gamma) p -> check_param p g d pm) g pl in 
    let p = (List.map (fun (i, t, t') -> (i, tag_erase t d pm)) pl) in 
    (p, g')

let exp_to_texp (checked_exp : TypedAst.exp * typ) (d : delta) (pm : parametrization) : TypedAst.texp = 
    debug_print ">> exp_to_texp";
    ((fst checked_exp), (tag_erase (snd checked_exp) d pm))
    
let rec check_exp (e : exp) (d : delta) (g : gamma) (pm : parametrization) (p : phi) : TypedAst.exp * typ = 
    debug_print ">> check_exp";
    let build_unop (op : unop) (e': exp) (check_fun: typ->delta->parametrization->typ) (pm: parametrization)
        : TypedAst.exp * typ =
        let result = check_exp e' d g pm p in
            (TypedAst.Unop(op, exp_to_texp result d pm), check_fun (snd result) d pm)
    in
    let build_binop (op : binop) (e1: exp) (e2: exp) (check_fun: typ->typ->delta->parametrization->typ) (pm: parametrization)
        : TypedAst.exp * typ =
        let e1r = check_exp e1 d g pm p in
        let e2r = check_exp e2 d g pm p in
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
    | Arr a -> check_arr d g p a pm
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
    | FnInv (i, args, pr) -> let ((i, args_exp), rt) = check_fn_inv d g p args i pr pm in 
        (FnInv (i, args_exp), rt)
        
and check_arr (d : delta) (g : gamma) (p : phi) (a : exp list) (pm : parametrization) : (TypedAst.exp * typ) =
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
    let checked_a = List.map (fun e -> (exp_to_texp (check_exp e d g pm p) d pm )) a in
    let length_a = List.length a in
    if is_vec checked_a then (TypedAst.Arr checked_a, TagTyp (BotTyp length_a)) else 
    (match is_mat checked_a with
    | Some n -> (TypedAst.Arr checked_a, trans_bot n length_a)
    | None ->  raise (TypeException ("Invalid array definition for " ^ (string_of_exp (Arr a)) ^ ", must be a matrix or vector")))
    

and check_fn_inv (d : delta) (g : gamma) (p : phi) (args : args) (i : string) (pml: typ list) (pm' : parametrization)
 : (string * TypedAst.args) * typ =    
    debug_print ">> check_fn_inv";
    let fn_invocated = if Assoc.mem i p
        then Assoc.lookup i p
        else raise (TypeException ("Invocated function " ^ i ^ " not found")) in
    let (_, rt, pm) = fn_invocated in
    let args' = List.map (fun a -> check_exp a d g pm' p) args in 
    let args_exp = List.map fst args' in
    let args_typ = List.map snd args' in
    (* find definition for function in phi *)
    (* looks through overloaded all possible definitions of the function *)
    let find_fn_inv ((params, rt, pr) : fn_type) : (typ Assoc.context) option =
        let params_typ = List.map (fun (_,a,_) -> a) params in
        (* Check that the parameterization is valid *)
        if Assoc.size pr != List.length pml then None else
        
        if not (List.fold_left2 (fun acc given_pm (_, bound) -> acc && is_bounded_by given_pm bound d pm') 
            true pml (Assoc.bindings pr)) then None else
        (* raise (TypeException "Mismatched number of parametrizations") *) 
        (* ^^ put this back in if we want the invariant that every overloaded function has the same number of generic parameters *)

        (* Get the parameters types and replace them in params_typ *)
        let pm_map = 
            List.fold_left2 (fun acc x y -> Assoc.update x y acc) Assoc.empty (List.map fst (Assoc.bindings pr)) pml 
        in
        let rec read_pm (t : typ) : typ =
            match t with
            | AbsTyp s -> Assoc.lookup s pm_map
            | TransTyp (s1, s2) -> TransTyp (read_pm s1, read_pm s2)
            | _ -> t
        in
        let params_typ_corrected = List.map read_pm params_typ in
        (* check arg and param types match *)
        if List.length args_typ == List.length params_typ then
            List.fold_left2 (fun acc arg param -> if (is_subtype arg param d pm) then acc else None)
            (Some pm_map) args_typ params_typ_corrected
        else None
    in
    (match find_fn_inv fn_invocated with
    | Some l -> ((i, args_exp), match rt with
        | AbsTyp rt' -> Assoc.lookup rt' l
        | _ -> rt)
    | None -> raise (TypeException ("No overloaded function declaration of " ^ i
    ^ (if List.length pml > 0 then "<" ^ (String.concat "," (List.map string_of_typ pml)) ^ ">" else "")
    ^ " matching types (" ^ (String.concat "," (List.map string_of_typ args_typ)) ^ ") found"))) 

and check_comm (c: comm) (d: delta) (g: gamma) (pm: parametrization) (p: phi) : TypedAst.comm * gamma = 
    debug_print ">> check_comm";
    match c with
    | Skip -> (TypedAst.Skip, g)
    | Print e -> (
        let (e, t) = exp_to_texp (check_exp e d g pm p) d pm in 
        match t with
        | UnitTyp -> raise (TypeException "Print function cannot print void types")
        | _ -> (TypedAst.Print (e, t), g)
    )
    | Inc x -> let x_typ = (Assoc.lookup x g) in (match x_typ with
        | IntTyp -> (TypedAst.Inc (x, TypedAst.IntTyp), g)
        | FloatTyp -> (TypedAst.Inc (x, TypedAst.FloatTyp), g)
        | _ -> raise (TypeException "increment must be applied to an integer or float"))
    | Dec x -> let x_typ = (Assoc.lookup x g) in (match x_typ with
        | IntTyp -> (TypedAst.Dec (x, TypedAst.IntTyp), g)
        | FloatTyp -> (TypedAst.Dec (x, TypedAst.FloatTyp), g)
        | _ -> raise (TypeException "decrement must be applied to an integer or float"))
    | Decl (t, tp, s, e) -> (* TODO: tp *)
        if Assoc.mem s g then raise (TypeException "variable name shadowing is illegal")        
        else 
        (check_typ_valid t d pm;
        let result = check_exp e d g pm p in
        let t' = (match t with | AutoTyp -> 
            (match (snd result) with
                | TagTyp (BotTyp _) -> raise (TypeException "Cannot infer the type of a vector literal")
                | TransTyp (TagTyp TopTyp _, TagTyp BotTyp _) -> raise (TypeException "Cannot infer the type of a matrix literal")
                | t' -> t')
            | _ -> t) in
        begin
            try
            (TypedAst.Decl (tag_erase t' d pm, s, (exp_to_texp result d pm)), (check_assign t' s (snd result) d g p pm))
            with 
            | AbstractTypeException -> (TypedAst.Decl (tag_erase (snd result) d pm, s, (exp_to_texp result d pm)), (check_assign t' s (snd result) d g p pm))
            | e -> raise e
        end)
    | Assign (s, e) ->
        if Assoc.mem s g then
            let t = Assoc.lookup s g in
            let result = check_exp e d g pm p in
            (TypedAst.Assign (s, (exp_to_texp result d pm)), check_assign t s (snd result) d g p pm)
        else raise (TypeException "Assignment to undeclared variable")
    | AssignOp (s, b, e) -> 
        let result = check_comm (Assign (s, Binop(b, Var s, e))) d g pm p in
        (match (fst result) with
        | TypedAst.Assign (_, (TypedAst.Binop (_, (_, st), e), _)) -> (TypedAst.AssignOp((s, st), b, e), snd result)
        | _ -> failwith "Assign must return an assign?")
    | If ((b, c1), el, c2) ->
        let check_if b c =
            let er = (check_exp b d g pm p) in
            let cr = check_comm_lst c d g pm p in
            (match (snd er) with 
            | BoolTyp -> ((exp_to_texp er d pm), (fst cr))
            | _ -> raise (TypeException "Expected boolean expression for if condition"))
        in
        let c2r = (match c2 with | Some e -> Some (fst (check_comm_lst e d g pm p)) | None -> None) in
        (TypedAst.If (check_if b c1, List.map (fun (b, c) -> check_if b c) el, c2r), g)
    | For (c1, b, c2, cl) ->
        let (c1r, g') = check_comm c1 d g pm p in
        let (br, brt) = check_exp b d g' pm p in
        let btexp = exp_to_texp (br, brt) d pm in
        let (c2r, _) = check_comm c2 d g' pm p in
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
                if x = y then (TypedAst.For (c1r, btexp, c2r, (fst (check_comm_lst cl d g' pm p))), g)
                else raise (TypeException "Must use the same variable when checking and progressing toward termination")
            | _ -> raise (TypeException "For loop must progress toward termination with a comparative expression between an id and constant using precisely the increment or decrement operator"))
        | _ -> raise (TypeException "First statement in for loop must be a skip, declaration, or assignment"))
    | Return Some e ->
        let (e, t) = exp_to_texp (check_exp e d g pm p) d pm in
        (TypedAst.Return (Some (e, t)), g)
    | Return None -> (TypedAst.Return None, g)
    | FnCall (i, args, pml) -> let ((i, args_exp), _) = check_fn_inv d g p args i pml pm in 
        (TypedAst.FnCall (i, args_exp), g)

and check_comm_lst (cl : comm list) (d: delta) (g: gamma) (pm : parametrization) (p: phi) : TypedAst.comm list * gamma = 
    debug_print ">> check_comm_lst";
    match cl with
    | [] -> ([], g)
    | h::t -> let context = check_comm h d g pm p in
        let result = check_comm_lst t d (snd context) pm p in 
        ((fst context) :: (fst result), (snd result))

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

let check_tag (s: string) (l: tag_typ) (d: delta) : delta = 
    debug_print ">> check_tag";
    if Assoc.mem s d then raise (TypeException "Cannot redeclare tag")
            else Assoc.update s l d

let rec check_tags (t: tag_decl list) (d: delta): delta =
    debug_print ">> check_tags";
    match t with 
    | [] -> d
    | (s, a)::t ->
        check_typ_exp a |> ignore;
        match a with 
        | (TagTyp l) -> (
            match l with 
            | VarTyp s' -> (
                if Assoc.mem s' d then check_tag s l d |> check_tags t
                else raise (TypeException "Tag undefined")
            )
            | _ -> check_tag s l d |> check_tags t
        )
        | _ -> raise (TypeException "Expected linear type for tag declaration")

let check_fn_decl (g: gamma) (d: delta) ((id, (pl, rt, pm)): fn_decl) (p: phi) : (TypedAst.params * gamma) * TypedAst.parametrization * phi =
    debug_print (">> check_fn_decl : " ^ id);
    check_typ_valid rt d pm;
    let pr = check_params pl g d pm in 
    let pme = Assoc.gen_context (List.map (fun (s, c) -> (s, constrain_erase c d pm)) (Assoc.bindings pm)) in
    if Assoc.mem id p 
    then raise (TypeException ("Function of duplicate name has been found: " ^ id))
    else (pr, pme, Assoc.update id (pl, rt, pm) p)

(* Helper function for type checking void functions. 
 * Functions that return void can have any number of void return statements 
 * anywhere. *)
let check_void_return (c: comm) =
    debug_print ">> check_void_return";
    match c with
    | Return Some _ -> raise (TypeException ("Void functions cannot return a value"))
    | _ -> ()

let check_return (t: typ) (d: delta) (g: gamma) (pm: parametrization) (p: phi) (c: comm) = 
    debug_print ">> check_return";
    match c with
    | Return None -> raise (TypeException ("Expected a return value instead of void"))
    | Return Some r -> (
        let (_, rt) = check_exp r d g pm p in
        (* raises return exception of given boolean exp is false *)
        if is_subtype rt t d pm then () 
        else raise (TypeException ("Mismatched return types, expected: " ^ 
        (string_of_typ t) ^ ", found: " ^ (string_of_typ rt)))
        )
    | _ -> ()

let rec check_fn (((id, (pl, r, pr)), cl): fn) (g: gamma) (d: delta) (p: phi) : TypedAst.fn * phi = 
    debug_print (">> check_fn : " ^ id);
    (* fn := fn_decl * comm list *)
    (* update phi with function declaration *)
    let ((pl', g'), pm, p') = check_fn_decl g d (id, (pl, r, pr)) p in 
    let (cl', g'') = check_comm_lst cl d g' pr p in 
    (* check that the last command is a return statement *)
    (* print_endline (TypedAstPrinter.string_of_parametrization pm); *)
    (* print_endline (TypedAstPrinter.string_of_params pl'); *)
    match r with
    | UnitTyp -> List.iter check_void_return cl; ((((id, (pl', TypedAst.UnitTyp, pm)), cl')), p')
    (* TODO: might want to check that there is exactly one return statement at the end *)
    | t -> List.iter (check_return t d g'' pr p) cl; ((((id, (pl', tag_erase t d pr, pm)), cl')), p')

and check_fn_lst (fl: fn list) (g: gamma) (d: delta) (p: phi) : TypedAst.prog * phi =
    debug_print ">> check_fn_lst";
    match fl with
    | [] -> ([], p)
    | h::t -> let (fn', p') = check_fn h g d p in
        let (fn'', p'') = check_fn_lst t g d p' in 
        ((fn' :: fn''), p'')

(* Check that there is a void main() defined *)
let check_main_fn (g: gamma) (d: delta) (p: phi) =
    debug_print ">> check_main_fn";
    let (params, ret_type, paramet) = Assoc.lookup "main" p in 
    debug_print (">> check_main_fn_2" ^ (string_of_params params) ^ (string_of_parametrization paramet));
    if (Assoc.size paramet) > 0 then raise (TypeException "Cannot provide generic parameters to main") else
    match ret_type with
        | UnitTyp -> check_params params g d paramet |> fst
        | _ -> raise (TypeException ("Expected main function to return void"))

let check_decls (g: gamma) (d: delta) (dl : extern_decl) (p: phi) : (gamma * phi)=
    match dl with
    | ExternFn f -> let (_, _, p') = (check_fn_decl g d f p) in (g, p')
    | ExternVar (t, Var x) -> (Assoc.update x t g, p)
    | _ -> raise (TypeException ("Invalid declaration, must be a function or variable"))

(* Returns the list of fn's which represent the program 
 * and params of the void main() fn *)
let check_prog (e: prog) : TypedAst.prog * TypedAst.params =
    debug_print ">> check_prog";
    match e with
    | Prog (dl, t, f) -> (*(d: delta) ((id, t): fn_decl) (p: phi) *)
        (* delta from tag declarations *)
        let d = check_tags t Assoc.empty in 
        let (g, p) = List.fold_left 
            (fun (g', p') (dl': extern_decl) -> check_decls g' d dl' p') 
            (Assoc.empty, Assoc.empty) dl in
        let (e', p') = check_fn_lst f g d p in 
        let pr = check_main_fn g d p' in 
        debug_print "===================";
        debug_print "Type Check Complete";
        debug_print "===================\n";
        (e', pr)
