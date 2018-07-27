open CoreAst
open TagAst
open TagAstHelper
open Assoc
open Util
open Printf
open Str

exception TypeException of string
exception DimensionException of int * int

(* Variable defs *)
type gamma = (string, typ) Assoc.context

(* Tags defs *)
type delta = (string, tag_typ) Assoc.context

let trans_top (n1: int) (n2: int) : typ =
    TransTyp ((BotTyp n1), (TopTyp n2))

let trans_bot (n1: int) (n2: int) : typ =
    TransTyp ((TopTyp n1), (BotTyp n2))

let rec vec_dim (t: tag_typ) (d: delta) : int =
    match t with
    | TopTyp n
    | BotTyp n -> n
    | VarTyp s -> vec_dim (lookup s d) d

let rec get_ancestor_list (t: tag_typ) (d: delta) : id list =
    match t with 
    | TopTyp _ -> []
    | BotTyp _ -> raise (TypeException "Bad failure -- Ancestor list somehow includes the bottom type")
    | VarTyp s -> s :: (get_ancestor_list (lookup s d) d)

let is_tag_subtype (to_check: tag_typ) (target: tag_typ) (d: delta) : bool =
    match (to_check, target) with
    | BotTyp n1, BotTyp n2
    | BotTyp n1, TopTyp n2
    | TopTyp n1, TopTyp n2 -> n1 = n2
    | BotTyp n, VarTyp s -> n = (vec_dim target d)
    | VarTyp _, BotTyp _ -> false
    | VarTyp _, VarTyp s2 -> List.mem s2 (get_ancestor_list to_check d)
    | VarTyp s, TopTyp n -> (vec_dim to_check d) = n
    | TopTyp _, _ -> false

let least_common_parent (t1: tag_typ) (t2: tag_typ) (d: delta) : tag_typ =
    let check_dim (n1: int) (n2: int) : unit =
        if n1 = n2 then () else (raise (DimensionException (n1, n2)))
    in
    let rec lub (anc_list1: id list) (anc_list2: id list) : id =
        match anc_list1 with
        | [] -> raise (TypeException "Cannot implicitly cast to the top vector type")
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
        check_dim (vec_dim (VarTyp s) d) n1;
        raise (TypeException "Cannot implicitly cast to the top vector type")
    | VarTyp s, BotTyp n1
    | BotTyp n1, VarTyp s ->
        check_dim (vec_dim (VarTyp s) d) n1; VarTyp s
    | VarTyp s1, VarTyp s2 ->
        check_dim (vec_dim (VarTyp s1) d) (vec_dim (VarTyp s2) d);
        (if s1 = s2 then VarTyp s1
        else VarTyp (lub (get_ancestor_list t1 d) (get_ancestor_list t2 d)))

let greatest_common_child (t1: tag_typ) (t2: tag_typ) (d: delta) : tag_typ =
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
        check_dim (vec_dim (VarTyp s) d) n1; VarTyp s
    | VarTyp s, BotTyp n1
    | BotTyp n1, VarTyp s ->
        check_dim (vec_dim (VarTyp s) d) n1; BotTyp n1
    | VarTyp s1, VarTyp s2 ->
        let bot_dim = vec_dim (VarTyp s1) d in
        check_dim bot_dim (vec_dim (VarTyp s2) d);
        (* This works since each tag can only have one parent *)
        (if is_tag_subtype t1 t2 d then t1
        else if is_tag_subtype t2 t1 d then t2
        else BotTyp bot_dim)

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

let check_tag_typ (tag: tag_typ) (d: delta) : unit =
    match tag with
    | TopTyp n
    | BotTyp n -> (if (n > 0) then ()
        else raise (TypeException "Cannot declare a type with dimension less than 0"))
    | VarTyp s -> (if Assoc.mem s d then ()
        else raise (TypeException ("Undeclared tag" ^ s)))

let check_typ_exp (t: typ) (d: delta) : unit =
    debug_print ">> check_typ";
    match t with
    | UnitTyp
    | BoolTyp
    | IntTyp
    | FloatTyp 
    | SamplerTyp _ 
    | VoidTyp -> ()
    | TagTyp s -> check_tag_typ s d; ()
    | TransTyp (s1, s2) -> check_tag_typ s1 d; check_tag_typ s2 d; ()

(* "scalar linear exp", (i.e. ctimes) returns generalized MatTyp *)
let check_ctimes_exp (t1: typ) (t2: typ) (d: delta) : typ = 
    debug_print ">> check_scalar_linear_exp";
    match (t1, t2) with 
    | TransTyp (m1, m2), TransTyp (m3, m4) ->
        let left = (vec_dim m1 d) in
        let right = (vec_dim m2 d) in
        if left = (vec_dim m3 d) && right = (vec_dim m4 d)
        then trans_top left right
        else (raise (TypeException "dimension mismatch in ctimes operator"))
    | TagTyp l, TagTyp r -> (
        check_tag_typ l d; check_tag_typ r d;
        let ldim = vec_dim l d in
        let rdim = vec_dim r d in 
        if ldim = rdim 
        then TagTyp (TopTyp (vec_dim l d))
        else (raise (TypeException "dimension mismatch in ctimes operator"))
    )
    | _ -> (raise (TypeException ("expected linear types for ctimes operator, found: "^(string_of_typ t1)^", "^(string_of_typ t2))))

(* Type check norm expressions *)
let rec check_norm_exp (t: typ) (d: delta) : typ = 
    debug_print ">> check_norm_exp";
    match t with
    | TagTyp a -> t
    | _ -> (raise (TypeException "expected linear type for norm operator"))

(* Type check binary bool operators (i.e. &&, ||) *)
let check_bool_binop (t1: typ) (t2: typ) (d: delta) : typ = 
    debug_print ">> check_bool_binop";
    match (t1, t2) with 
    | BoolTyp, BoolTyp -> BoolTyp
    | _ -> raise (TypeException "expected boolean expression for binop")

(* Type check unary bool operators (i.e. !) *)
let check_bool_unop (t1: typ) (d: delta) : typ =
    debug_print ">> check_bool_unop";
    match t1 with 
    | BoolTyp -> BoolTyp
    | _ -> raise (TypeException "expected boolean expression")

(* Type check equality (==) *)
(* Only bool, int, float are comparable *)
let check_equality_exp (t1: typ) (t2: typ) (d: delta) : typ = 
    debug_print ">> check_comp_binop";
    match (t1, t2) with
    | BoolTyp, BoolTyp -> BoolTyp
    | IntTyp, IntTyp -> BoolTyp
    | FloatTyp, FloatTyp -> BoolTyp
    | _ -> raise (TypeException "unexpected type for binary comparator operations")

(* Type check comparative binary operators (i.e. <. <=) *)
(* Only int and float are comparable *)
let check_comp_binop (t1: typ) (t2: typ) (d: delta) : typ = 
    debug_print ">> check_comp_binop";
    match (t1, t2) with
    | IntTyp, IntTyp -> BoolTyp
    | FloatTyp, FloatTyp -> BoolTyp
    | _ -> raise (TypeException "unexpected type for binary comparator operations")

let check_dot_exp (t1: typ) (t2: typ) (d: delta): typ = 
    match (t1, t2) with 
    | TagTyp a1, TagTyp a2 ->  
        if vec_dim a1 d = vec_dim a2 d 
        then FloatTyp 
        else raise (TypeException "expected tag type of same dimension for dot product exp")
    | _ -> raise (TypeException "unexpected type for dot product exp")

(* Type checking addition operations on scalar (int, float) expressions *)
(* Types are closed under addition and scalar multiplication *)
let check_addition_exp (t1: typ) (t2: typ) (d: delta) : typ =
    debug_print ">> check_addition";
    match (t1, t2) with 
    | IntTyp, IntTyp -> IntTyp
    | FloatTyp, IntTyp
    | IntTyp, FloatTyp
    | FloatTyp, FloatTyp -> FloatTyp
    | TagTyp a1, TagTyp a2 -> TagTyp (least_common_parent a1 a2 d)
    | TransTyp (m1, m2), TransTyp (m3, m4) -> 
        TransTyp (greatest_common_child m1 m3 d, least_common_parent m2 m4 d)
    | _ -> 
        (raise (TypeException ("invalid expressions for addition: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2))))

(* Type checking times operator - on scalar mult & matrix transformations *)
let check_times_exp (t1: typ) (t2: typ) (d: delta) : typ = 
    debug_print ">> check_times_exp";
    match (t1, t2) with
    | IntTyp, IntTyp -> IntTyp
    | FloatTyp, IntTyp
    | IntTyp, FloatTyp
    | FloatTyp, FloatTyp -> FloatTyp
    | (TagTyp _, TagTyp _) -> raise (TypeException "cannot multiply vectors together")

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
    | TransTyp (m1, m2), TagTyp t -> if is_tag_subtype t m1 d then (TagTyp m2)
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
    debug_print ">> check_addition";
    match (t1, t2) with 
    | IntTyp, IntTyp -> IntTyp
    | FloatTyp, IntTyp
    | IntTyp, FloatTyp
    | FloatTyp, FloatTyp -> FloatTyp
    | TagTyp a, IntTyp
    | TagTyp a, FloatTyp -> TagTyp a
    | _ -> 
        (raise (TypeException ("invalid expressions for division: "
        ^ (string_of_typ t1) ^ ", " ^ (string_of_typ t2))))

let tag_erase (t : typ) (d : delta) : TypedAst.etyp =
    match t with
    | UnitTyp -> TypedAst.UnitTyp
    | BoolTyp -> TypedAst.BoolTyp
    | IntTyp -> TypedAst.IntTyp
    | FloatTyp -> TypedAst.FloatTyp
    | TagTyp tag -> (match tag with
        | TopTyp n
        | BotTyp n -> TypedAst.VecTyp n
        | VarTyp _ -> TypedAst.VecTyp (vec_dim tag d))
    | TransTyp (s1, s2) -> TypedAst.MatTyp ((vec_dim s2 d), (vec_dim s1 d))
    | SamplerTyp i -> TypedAst.SamplerTyp i
    | VoidTyp -> TypedAst.VoidTyp
    
let exp_to_texp (checked_exp : TypedAst.exp * typ) (d : delta) : TypedAst.texp = 
    ((fst checked_exp), (tag_erase (snd checked_exp) d))

let rec check_exp (e: exp) (d: delta) (g: gamma) : TypedAst.exp * typ = 
    debug_print ">> check_exp";
    
    let build_unop (op : unop) (e': exp) (check_fun: typ->delta->typ)
        : TypedAst.exp * typ =
        let result = check_exp e' d g in
            (TypedAst.Unop(op, exp_to_texp result d), check_fun (snd result) d)
    in
    let build_binop (op : binop) (e1: exp) (e2: exp) (check_fun: typ->typ->delta->typ)
        : TypedAst.exp * typ =
        let e1r = check_exp e1 d g in
        let e2r = check_exp e2 d g in
            (TypedAst.Binop(op, exp_to_texp e1r d, exp_to_texp e2r d), check_fun (snd e1r) (snd e2r) d)
    in 
    match e with
    | Val v -> (TypedAst.Val v, check_val v d)
    | Var v -> "\tVar "^v |> debug_print;
        (TypedAst.Var v, Assoc.lookup v g)
    | Unop (op, e') -> (match op with
        | Not -> build_unop op e' check_bool_unop)
    | Binop (op, e1, e2) -> (match op with
        | Eq -> build_binop op e1 e2 check_equality_exp
        | Leq -> build_binop op e1 e2 check_comp_binop
        | Or | And -> build_binop op e1 e2 check_bool_binop
        | Plus | Minus -> build_binop op e1 e2 check_addition_exp
        | Times -> build_binop op e1 e2 check_times_exp
        | Div  -> build_binop op e1 e2 check_division_exp
        | CTimes -> build_binop op e1 e2 check_ctimes_exp
    )
    | VecTrans (i, tag) -> failwith "Unimplemented"
    | _ -> failwith "Unimplemented"


let rec check_decl (t: typ) (s: string) (etyp : typ) (d: delta) (g: gamma) : gamma =
    debug_print (">> check_decl <<"^s^">>");
    if Assoc.mem s d then 
        raise (TypeException "variable declared as tag")
    else (
        match (t, etyp) with
        | (BoolTyp, BoolTyp)
        | (IntTyp, IntTyp)
        | (FloatTyp, FloatTyp) -> Assoc.update s t g
        | (TagTyp t1, TagTyp t2) ->
            least_common_parent t1 t2 d |> ignore;
            if is_tag_subtype t2 t1 d then Assoc.update s t g
            else raise (TypeException ("mismatched linear type for var decl: " ^ s))
        | (TransTyp (t1, t2), TransTyp (t3, t4)) ->
            if is_tag_subtype t1 t3 d && is_tag_subtype t4 t2 d then Assoc.update s t g
            else raise (TypeException ("no possible upcast for var decl: " ^ s))
        | _ -> raise (TypeException ("mismatched types for var decl: expected " ^ (string_of_typ t) ^ " " ^ s ^ ", found " ^ (string_of_typ etyp) ))
    )

let rec check_comm (c: comm) (d: delta) (g: gamma) : TypedAst.comm * gamma = 
    debug_print ">> check_comm";
    match c with
    | Skip -> (TypedAst.Skip, g)
    | Print e -> (TypedAst.Print (exp_to_texp (check_exp e d g) d), g)
    | Decl (t, s, e) -> 
        if Assoc.mem s g then raise (TypeException "variable name shadowing is illegal")
        else let result = check_exp e d g in
            (TypedAst.Decl (tag_erase t d, s, (exp_to_texp result d)), (check_decl t s (snd result) d g))

    | Assign (s, e) -> 
        if Assoc.mem s g then 
            let t = Assoc.lookup s g in
            let result = check_exp e d g in
            (TypedAst.Assign (s, (exp_to_texp result d)), check_decl t s (snd result) d g)
        else raise (TypeException "assignment to undeclared variable")

    | If (b, c1, c2) ->
        let result = (check_exp b d g) in
        let c1r = check_comm_lst c1 d g in
        let c2r = check_comm_lst c2 d g in
        (match (snd result) with 
        | BoolTyp -> (TypedAst.If ((exp_to_texp result d), (fst c1r), (fst c2r)), g)
        | _ -> raise (TypeException "expected boolean expression for if condition"))

and check_comm_lst (cl : comm list) (d: delta) (g: gamma): TypedAst.comm list * gamma = 
    debug_print ">> check_comm_lst";
    match cl with
    | [] -> ([], g)
    | h::t -> let context = check_comm h d g in
        let result = check_comm_lst t d (snd context) in 
        ((fst context) :: (fst result), (snd result))

let check_tag (s: string) (l: tag_typ) (d: delta) : delta = 
    if Assoc.mem s d then raise (TypeException "cannot redeclare tag")
            else Assoc.update s l d

let rec check_tags (t : tag_decl list) (d: delta): delta =
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
                else raise (TypeException "tag undefined")
            )
            | _ -> check_tag s l d |> check_tags t
        )
        | _ -> raise (TypeException "expected linear type for tag declaration")

let rec check_fn_lst (fl: fn list) (d: delta) (g: gamma): TypedAst.fn list * gamma =
    failwith "Unimplemented"

let check_prog (e : prog) : TypedAst.fn list =
    debug_print ">> check_prog";
    match e with
    | Prog (t, c) -> let d = check_tags t Assoc.empty in 
        (fst (check_fn_lst c d Assoc.empty))