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
    TransTyp ((BotTyp n1), (TopTyp n2))

let trans_bot (n1: int) (n2: int) : typ =
    TransTyp ((TopTyp n1), (BotTyp n2))

let rec etyp_to_typ (e : TypedAst.etyp) : typ =
    debug_print ">> etyp_to_typ";
    match e with 
    | TypedAst.UnitTyp -> UnitTyp
    | TypedAst.BoolTyp -> BoolTyp
    | TypedAst.IntTyp -> IntTyp
    | TypedAst.FloatTyp -> FloatTyp
    | TypedAst.VecTyp n -> TagTyp(BotTyp n)
    | TypedAst.MatTyp (n1, n2) -> TransTyp(BotTyp n1, BotTyp n2)
    | TypedAst.SamplerTyp n -> SamplerTyp n
    | TypedAst.AbsTyp (s, None) -> AbsTyp s
    | TypedAst.AbsTyp (s, Some e') -> etyp_to_typ e'
    | TypedAst.GenTyp -> GenTyp
    | TypedAst.GenMatTyp -> GenMatTyp
    | TypedAst.GenVecTyp -> GenVecTyp

let rec vec_dim (t: tag_typ) (d: delta) (pm : parametrization): int =
    debug_print ">> vec_dim";
    match t with
    | TopTyp n
    | BotTyp n -> n
    | VarTyp s -> begin try vec_dim (Assoc.lookup s d) d pm with _ -> failwith (string_of_tag_typ t) end
    | TAbsTyp s ->
        begin
        let s' = tag_erase_param (TagTyp t) d pm in 
        match s' with 
        | TypedAst.VecTyp n -> n
        | TypedAst.AbsTyp (_, Some s'') -> 
            begin
            let s''' = etyp_to_typ s'' in
            match s''' with
            | TagTyp a -> vec_dim a d pm 
            | _ -> raise AbstractTypeException
            end
        | _ -> raise AbstractTypeException
        end
and tag_erase_param (t: typ) (d: delta) (pm: parametrization) : TypedAst.etyp = 
    debug_print ">> tag_erase_param";
    match t with 
    AbsTyp s -> if List.mem_assoc t pm then 
        let p = (List.assoc t pm) in 
        match p with 
        Some e -> TypedAst.AbsTyp (s, Some (tag_erase e d pm))
        | None -> TypedAst.AbsTyp (s, None)
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
            | _ -> raise AbstractTypeException
        end
    | TransTyp (s1, s2) -> TypedAst.MatTyp ((vec_dim s2 d pm), (vec_dim s1 d pm))
    | SamplerTyp i -> TypedAst.SamplerTyp i
    | AbsTyp s -> tag_erase_param t d pm 
    | GenTyp -> TypedAst.GenTyp
    | GenVecTyp -> TypedAst.GenVecTyp
    | GenMatTyp -> TypedAst.GenMatTyp
    | AutoTyp -> raise (TypeException "Illegal use of auto (cannot use auto as part of a function call)")

let rec get_ancestor_list (t: tag_typ) (d: delta) : id list =
    debug_print ">> get_ancestor_list";
    match t with 
    | TopTyp _ -> []
    | BotTyp _ -> raise (TypeException "Bad failure -- Ancestor list somehow includes the bottom type")
    | VarTyp s -> s :: (get_ancestor_list (Assoc.lookup s d) d)
    | TAbsTyp s -> failwith "Unimplemented get_ancestor_list"

let is_tag_subtype (to_check: tag_typ) (target: tag_typ) (d: delta) (pm: parametrization): bool =
    debug_print ">> is_tag_subtype";
    match (to_check, target) with
    | BotTyp n1, BotTyp n2
    | BotTyp n1, TopTyp n2
    | TopTyp n1, TopTyp n2 -> n1 = n2
    | BotTyp n, VarTyp s -> n = (vec_dim target d pm)
    | VarTyp _, BotTyp _ -> false
    | VarTyp _, VarTyp s2 -> List.mem s2 (get_ancestor_list to_check d)
    | VarTyp s, TopTyp n -> n = (vec_dim to_check d pm)
    | TopTyp _, _ -> false
    | TAbsTyp s1, TAbsTyp s2 -> s1 = s2 (* TODO : more than string equality *)
    | TAbsTyp _, _ 
    | _, TAbsTyp _ -> true

let subsumes_to (to_check: tag_typ) (target: tag_typ) (d: delta) (pm: parametrization) : bool =
    debug_print ">> subsumes_to";
    match (to_check, target) with
    | VarTyp s, TopTyp n -> false (* Cannot upcast a variable to the toptyp *)
    | _ -> is_tag_subtype to_check target d pm

(* Note that the 'strict' parameter controls whether or not we allow casting to the top type *)
let rec is_subtype (to_check : typ) (target : typ) (d : delta) (pm: parametrization): bool =
    debug_print (">> is_subtype" ^ (string_of_typ to_check) ^ ", " ^(string_of_typ target));
    match (to_check, target) with 
    | (TagTyp t1, TagTyp t2) -> subsumes_to t1 t2 d pm (* MARK *)
    | (SamplerTyp i1, SamplerTyp i2) -> i1 = i2 
    | (BoolTyp, BoolTyp)
    | (IntTyp, IntTyp)
    | (FloatTyp, FloatTyp) -> true
    | (TransTyp (t1, t2), TransTyp (t3, t4)) -> 
        (is_tag_subtype t3 t1 d pm && is_tag_subtype t2 t4 d pm )
    | (IntTyp, GenTyp)
    | (FloatTyp, GenTyp)
    | (TagTyp _, GenTyp)
    | (TransTyp _, GenTyp) -> true (* todo: is transtyp a subtype of gentyp? *)
    | (TransTyp _, GenMatTyp) -> true
    | (TagTyp _, GenVecTyp) -> true 
    | (AbsTyp s1, AbsTyp s2) -> s1 = s2
    | (_, AbsTyp s) -> 
        if List.mem_assoc target pm 
        then let p = (List.assoc target pm) in 
            match p with 
            | Some p' ->  is_subtype to_check p' d pm
            | None -> true (* todo *)
        else raise (TypeException ("AbsTyp " ^ s ^ " not found in parametrization"))
    | _ -> false

let least_common_parent (t1: tag_typ) (t2: tag_typ) (d: delta) (pm: parametrization): tag_typ =
    debug_print ">> least_common_parent";
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
    | _ -> failwith "Unimplemented least_common_parent"

let greatest_common_child (t1: tag_typ) (t2: tag_typ) (d: delta) (pm: parametrization) : tag_typ =
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
            (if subsumes_to t1 t2 d pm then t1
            else if subsumes_to t2 t1 d pm then t2
            else BotTyp bot_dim)
        end
    | _ -> failwith "Unimplemented greatest_common_child"

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
    | _ -> failwith "Unimplemented check_tag_typ"

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
    | TransTyp (s1, s2) -> check_tag_typ s1 d; check_tag_typ s2 d; ()
    | _ -> failwith "Check_typ_exp Unimplemented"

(* "scalar linear exp", (i.e. ctimes) returns generalized MatTyp *)
let check_ctimes_exp (t1: typ) (t2: typ) (d: delta) (pm : parametrization): typ = 
    debug_print ">> check_scalar_linear_exp";
    match (t1, t2) with 
    | TransTyp (m1, m2), TransTyp (m3, m4) ->
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

(* Type check norm expressions *)
let rec check_norm_exp (t: typ) (d: delta) : typ = 
    debug_print ">> check_norm_exp";
    match t with
    | TagTyp a -> t
    | _ -> (raise (TypeException "Expected linear type for norm operator"))

(* Type check binary bool operators (i.e. &&, ||) *)
let check_bool_binop (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ = 
    debug_print ">> check_bool_binop";
    let check_bool_abs t =
        match tag_erase_param t d pm with
        AbsTyp (_, None) -> true
        | AbsTyp (_, Some t) -> 
            begin
                match etyp_to_typ t with 
                BoolTyp 
                | AbsTyp _ -> true
                | _ -> false
            end
        | _ -> false
    in 
    debug_print ">> check_bool_binop";
    match (t1, t2) with 
    | BoolTyp, BoolTyp -> BoolTyp
    | AbsTyp a, BoolTyp
    | BoolTyp, AbsTyp a -> 
        if check_bool_abs t1 then BoolTyp
        else raise (TypeException "Expected boolean expression for binop")
    | AbsTyp a, AbsTyp a' ->
        if check_bool_abs t1 && check_bool_abs t2 then BoolTyp
        else raise (TypeException "Expected boolean expression for binop")
    | _ -> raise (TypeException "Expected boolean expression for binop")

(* Type check unary number operators (i.e. -) *)
let check_num_unop (t1: typ) (d: delta) : typ =
    debug_print ">> check_num_unop";
    match t1 with 
    | IntTyp
    | FloatTyp
    | TagTyp _
    | TransTyp _ -> t1
    | _ -> raise (TypeException "Expected integer, float, vector, or matrix expression")

(* Type check unary bool operators (i.e. !) *)
let check_bool_unop (t1: typ) (d: delta) : typ =
    debug_print ">> check_bool_unop";
    match t1 with 
    | BoolTyp -> BoolTyp
    | _ -> raise (TypeException "Expected boolean expression")

(* Type check unary bool operators (i.e. !) *)
let check_swizzle (s : id) (t1: typ) (d: delta) (pm: parametrization) : typ =
    debug_print ">> check_swizzle";
    let check_reg valid_set = if Str.string_match valid_set s 0 
        then if String.length s == 1 then FloatTyp else TagTyp (TopTyp (String.length s))
        else raise (TypeException ("Invalid characters used for swizzling in " ^ s)) in
    let valid_length_1 = Str.regexp "[xrs]+" in
    let valid_length_2 = Str.regexp "[xyrgst]+" in
    let valid_length_3 = Str.regexp "[xyzrgbstp]+" in
    let valid_length_4 = Str.regexp "[xyzwrgbastpq]+" in
    match t1 with
    | TagTyp v -> 
        let dim = vec_dim v d pm in
        if dim == 1 then check_reg valid_length_1 else
        if dim == 2 then check_reg valid_length_2 else
        if dim == 3 then check_reg valid_length_3 else
        if dim >= 4 then check_reg valid_length_4 else
        raise (TypeException "Cannot swizzle a vector of length 0")
    | _ -> raise (TypeException "Expected boolean expression")

(* Type check equality (==) *)
(* Only bool, int, float are comparable *)
let check_equality_exp (t1: typ) (t2: typ) (d: delta) : typ = 
    debug_print ">> check_comp_binop";
    match (t1, t2) with
    | BoolTyp, BoolTyp -> BoolTyp
    | IntTyp, IntTyp -> BoolTyp
    | FloatTyp, FloatTyp -> BoolTyp
    | _ -> raise (TypeException "Unexpected type for binary comparator operations")

(* Type check comparative binary operators (i.e. <. <=) *)
(* Only int and float are comparable *)
let check_comp_binop (t1: typ) (t2: typ) (d: delta) : typ = 
    debug_print ">> check_comp_binop";
    match (t1, t2) with
    | IntTyp, IntTyp -> BoolTyp
    | FloatTyp, FloatTyp -> BoolTyp
    | _ -> raise (TypeException "Unexpected type for binary comparator operations")

(* Type checking addition operations on scalar (int, float) expressions *)
(* Types are closed under addition and scalar multiplication *)
let check_addition_exp (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ =
    debug_print ">> check_addition";
    match (t1, t2) with 
    | IntTyp, IntTyp -> IntTyp
    | FloatTyp, IntTyp
    | IntTyp, FloatTyp
    | FloatTyp, FloatTyp -> FloatTyp
    | TagTyp a1, TagTyp a2 -> TagTyp (least_common_parent a1 a2 d pm)
    | TransTyp (m1, m2), TransTyp (m3, m4) -> 
        TransTyp (greatest_common_child m1 m3 d pm, least_common_parent m2 m4 d pm)
    (* TODO - etyp conversion before abstyp comparison*)
    | AbsTyp a, AbsTyp a' -> 
        if a = a'
        then 
            begin
                match tag_erase_param t1 d pm with
                AbsTyp (_, Some t) -> etyp_to_typ t
                | _ -> failwith "Unexpected reach in addition" 
            end
        else (raise (TypeException ("Invalid expressions for addition: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2))))
    | _ -> 
        (raise (TypeException ("Invalid expressions for addition: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2))))

(* Type checking times operator - on scalar mult & matrix transformations *)
let check_times_exp (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ = 
    debug_print ">> check_times_exp";
    match (t1, t2) with
    | IntTyp, IntTyp -> IntTyp
    | FloatTyp, IntTyp
    | IntTyp, FloatTyp
    | FloatTyp, FloatTyp -> FloatTyp
    | (TagTyp _, TagTyp _) -> raise (TypeException "Cannot multiply vectors together")

    (* Scalar Multiplication *)
    | IntTyp, TagTyp t
    | TagTyp t, IntTyp
    | FloatTyp, TagTyp t
    | TagTyp t, FloatTyp -> TagTyp t

    | IntTyp, TransTyp (m1, m2)
    | TransTyp  (m1, m2), IntTyp
    | FloatTyp, TransTyp  (m1, m2)
    | TransTyp  (m1, m2), FloatTyp -> TransTyp (m1, m2)

    (* Matrix * Vector Multiplication *)
    | TagTyp _, TransTyp _ -> 
        raise(TypeException "Cannot multiply a vector * matrix (did you mean matrix * vector?)")
    | TransTyp (m1, m2), TagTyp t -> 
        if subsumes_to t m1 d pm then (TagTyp m2)
        else raise (TypeException ("Cannot apply a matrix of type " ^ (string_of_typ t1)
            ^ " to a vector of type " ^ (string_of_typ t2)))

    (* Matrix * Matrix Multiplication *)
    | TransTyp (m1, m2), TransTyp (m3, m4) ->
        (* Check for a cast match between m2 and m3 *)
        least_common_parent m1 m4 d |> ignore;
        TransTyp (m3, m2)
    | _ -> raise (TypeException ("Invalid types for multiplication: "
        ^ (string_of_typ t1) ^ " and " ^ (string_of_typ t2)))

(* Type checking division operations (/) *)
(* Types are closed under scalar division *)
let check_division_exp (t1: typ) (t2: typ) (d: delta) : typ =
    debug_print ">> check_division";
    match (t1, t2) with 
    | IntTyp, IntTyp -> IntTyp
    | FloatTyp, IntTyp
    | IntTyp, FloatTyp
    | FloatTyp, FloatTyp -> FloatTyp
    | TagTyp a, IntTyp
    | TagTyp a, FloatTyp -> TagTyp a
    | _ -> 
        (raise (TypeException ("Invalid expressions for division: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2))))

let check_index_exp (t1: typ) (t2: typ) (d: delta) (pm: parametrization): typ =
    debug_print ">> check_index_exp";
    match (t1, t2) with 
    | TagTyp t, IntTyp -> FloatTyp
    | TransTyp (u, v), IntTyp -> TagTyp (TopTyp (vec_dim v d pm))
    | _ -> 
        (raise (TypeException ("Invalid expressions for division: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2))))


(* Type check parameter; make sure there are no name-shadowed parameter names *)
(* TODO : parametrized types *)
let check_param ((id, t, t'): (string * typ * typ option)) (g: gamma) (d: delta) : gamma = 
    debug_print ">> check_param";
    if Assoc.mem id g 
    then raise (TypeException ("Duplicate parameter name in function declaration: " ^ id))
    else (
        match t with
        TagTyp (VarTyp v) -> 
            if Assoc.mem v d then Assoc.update id t g 
            else raise (TypeException ("Tag in parameter not defined : " ^ v))
        | _ -> Assoc.update id t g
    )
    
(* Get list of parameters from param list *)
let check_params (pl : (id * typ * typ option) list) (d : delta) (pm : parametrization) : TypedAst.params * gamma = 
    debug_print ">> check_params";
    let g = List.fold_left (fun (g: gamma) p -> check_param p g d) Assoc.empty pl in 
    let p = List.map (fun (i, t, t') -> (i, tag_erase t d pm)) pl in 
    (p, g)

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
        fun a b c d -> f a b c
    in
    let req_parametrizations2 f =
     fun a b c  -> f a b
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
    let (_, rt, pm) = 
        if Assoc.mem i p
        then Assoc.lookup i p
        else raise (TypeException ("Invocated function " ^ i ^ " not found")) in
    let args' = List.map (fun a -> check_exp a d g pm p) args in (* todo: fix parametrization argument *)
    let args_exp = List.map fst args' in
    let args_typ = List.map snd args' in
    (* find definition for function in phi *)
    (* looks through overloaded all possible definitions of the function *)
    let rec find_fn_inv ((params, rt, pr) : fn_type) : fn_type = (* TODO *)
        let params_typ = List.map (fun (_,a,_) -> a) params in
        let pr_typ = List.map fst pr in
        (* print_endline (String.concat "," (List.map TagAstPrinter.string_of_typ pr_typ)); *)
        if List.length pr_typ != List.length pml then raise (TypeException "Mismatched number of parametrizations") else
        (* check number of arg and param types match *)
        if List.length args_typ == List.length params_typ then
            if List.fold_left2 (fun acc arg param -> acc && is_subtype arg param d pm) true args_typ params_typ 
            then (params, rt, pr) 
            else raise (TypeException "function invocation argument type mismatch")
        else raise (TypeException ("function invocation argument count mismatch: expected :"
                ^ (args_typ |> List.length |> string_of_int) ^ "arguments, found: " ^ 
                (params_typ |> List.length |> string_of_int)))
                
    in
    let rec rt_map pm'' pml'' r = 
        ( match (pm'', pml'') with 
        | ([], []) -> raise (TypeException "abstraction type not found in function definition")
        | (AbsTyp s, _)::t, (at::t') -> if r = s then at else (rt_map t t' r)
        (* | (AbsTyp s, _)::t, [] -> if r = s then at else (rt_map t t' r) *)
        | (_, _) -> raise (TypeException "Expected abstraction type for parametrization") ) 
    in 
    let rt = match rt with
        | AbsTyp rt' -> rt_map pm pml rt'
        | _ -> rt
    in
    let (_, _, _) = 
        if Assoc.mem i p then find_fn_inv (Assoc.lookup i p) 
        else raise (TypeException ("Function not found: " ^ i)) in
    ((i, args_exp), rt)

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
        let result = check_exp e d g pm p in
        let t' = (match t with | AutoTyp -> 
            (match (snd result) with
                | TagTyp (BotTyp _) -> raise (TypeException "Cannot infer the type of a vector literal")
                | TransTyp (TopTyp _, BotTyp _) -> raise (TypeException "Cannot infer the type of a matrix literal")
                | t' -> t')
            | _ -> t) in
        begin
            try
            (TypedAst.Decl (tag_erase t' d pm, s, (exp_to_texp result d pm)), (check_assign t' s (snd result) d g p pm))
            with | AbstractTypeException -> (TypedAst.Decl (tag_erase (snd result) d pm, s, (exp_to_texp result d pm)), (check_assign t' s (snd result) d g p pm))
            | e -> raise e
        end
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
    | TransTyp (VarTyp t1, VarTyp t2) -> if not (Assoc.mem t1 d)
        then raise (TypeException ("Unknown tag " ^ t2))
        else if not (Assoc.mem t2 d) then raise (TypeException ("unknown tag " ^ t1))
    | TagTyp (VarTyp t')
    | TransTyp (VarTyp t', _)
    | TransTyp (_, VarTyp t') ->
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
        begin 
        match (t, etyp) with
        | (BoolTyp, BoolTyp)
        | (IntTyp, IntTyp)
        | (FloatTyp, FloatTyp) 
        | (GenTyp, GenTyp) -> Assoc.update s t g
        | (TagTyp t1, TagTyp t2) -> 
            least_common_parent t1 t2 d |> ignore;
            if subsumes_to t2 t1 d pm then Assoc.update s t g
            else raise (TypeException ("Mismatched linear type for var decl: " ^ s))
        | (TransTyp (t1, t2), TransTyp (t3, t4)) ->
            begin
                if is_tag_subtype t1 t3 d pm && is_tag_subtype t4 t2 d pm then Assoc.update s t g
                else raise (TypeException ("No possible upcast for var decl: " ^ s))  
            end
        | (AbsTyp s1, AbsTyp s2) -> 
            if s1 = s2 then Assoc.update s t g
            else raise (TypeException ("Abstraction type for var decl for " ^ s ^ " mismatched"))
        | _ -> raise (TypeException ("Mismatched types for var decl for " ^ s ^  ": expected " ^ (string_of_typ t) ^ ", found " ^ (string_of_typ etyp)))
        end

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

let check_fn_decl (d: delta) ((id, t): fn_decl) (p: phi) : phi =
    debug_print (">> check_fn_decl : " ^ id);
    let (pl, _, _) = t in
    let _ = check_params pl d in 
    if Assoc.mem id p 
    then raise (TypeException ("Function of duplicate name has been found: " ^ id))
    else Assoc.update id t p

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
        let raise_return_exception b =
            if b then () 
            else raise (TypeException ("Mismatched return types, expected: " ^ 
            (string_of_typ t) ^ ", found: " ^ (string_of_typ rt)))
        in
        match (t,rt) with 
        | (TagTyp t1, TagTyp t2) -> subsumes_to t2 t1 d pm |> raise_return_exception
        | (SamplerTyp i1, SamplerTyp i2) -> i1 = i2 |> raise_return_exception 
        | (BoolTyp, BoolTyp)
        | (IntTyp, IntTyp)
        | (FloatTyp, FloatTyp)
        | (TagTyp _, GenVecTyp)
        | (TransTyp _, GenMatTyp)
        | (AutoTyp, _) -> ()
        | (TransTyp (t1, t2), TransTyp (t3, t4)) -> 
            (is_tag_subtype t3 t1 d pm && is_tag_subtype t2 t4 d pm) |> raise_return_exception
        | (AbsTyp s1, AbsTyp s2) -> s1 != s2 |> raise_return_exception
        | _ -> false |> raise_return_exception
        )
    | _ -> ()

let rec check_fn (((id, (pl, r, pr)), cl): fn) (d: delta) (p: phi) : TypedAst.fn * phi = 
    debug_print (">> check_fn : " ^ id);
    (* fn := fn_decl * comm list *)
    let (pl', g') = check_params pl d pr in
    let (cl', g'') = check_comm_lst cl d g' pr p in 
    (* update phi with function declaration *)
    let p' = check_fn_decl d (id, (pl, r, pr)) p in 
    (* check that the last command is a return statement *)
    match r with
    | UnitTyp -> List.iter check_void_return cl; ((((id, (pl', TypedAst.UnitTyp)), cl')), p')
    (* TODO: might want to check that there is exactly one return statement at the end *)
    | t -> List.iter (check_return t d g'' pr p) cl; ((((id, (pl', tag_erase t d pr)), cl')), p')
and check_fn_lst (fl: fn list) (d: delta) (p: phi) : TypedAst.prog * phi =
    debug_print ">> check_fn_lst";
    match fl with
    | [] -> ([], p)
    | h::t -> let (fn', p') = check_fn h d p in
        let (fn'', p'') = check_fn_lst t d p' in 
        ((fn' :: fn''), p'')

(* Check that there is a void main() defined *)
let check_main_fn (p: phi) (d: delta) =
    debug_print ">> check_main_fn";
    let (params, ret_type, parameterization) = Assoc.lookup "main" p in 
    debug_print (">> check_main_fn_2" ^ (string_of_params params) ^ (string_of_parameterization parameterization));
    match ret_type with
        | UnitTyp -> check_params params d parameterization |> fst
        | _ -> raise (TypeException ("Expected main function to return void"))

(* Returns the list of fn's which represent the program 
 * and params of the void main() fn *)
let check_prog (e: prog) : TypedAst.prog * TypedAst.params =
    debug_print ">> check_prog";
    match e with
    | Prog (dl, t, f) -> (*(d: delta) ((id, t): fn_decl) (p: phi) *)
        (* delta from tag declarations *)
        let d = check_tags t Assoc.empty in 
        let p = List.fold_left 
            (fun (a: phi) (dl': fn_decl) -> check_fn_decl d dl' a) 
            Assoc.empty dl in
        let (e', p') = check_fn_lst f d p in 
        let pr = check_main_fn p' d in 
        debug_print "===================";
        debug_print "Type Check Complete";
        debug_print "===================\n";
        (e', pr)
